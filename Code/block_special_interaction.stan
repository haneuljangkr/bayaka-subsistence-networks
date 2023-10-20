data{   
  //# Array dimension variables                                   
    int N_id;                                  //# Number of people                                                                                                   
    int N_responses;                           //# Number of outcome networks
    int N_params [3];                          //# Number of focal, target, and dyadic predictors

  //# Block predictor variables 
    int N_group_vars;                          //# Number of block structure variables
    int max_N_groups;                          //# Max number of group labels in any variable
    int N_groups_per_var[N_group_vars];        //# Number of group labels, per variable type
    int block_set[N_id, N_group_vars];         //# Dataframe holding the group ID codes for each person (rows) for each variable type (cols)

  //# Focal, target, and dyadic predictor variables                                                                                                      
    matrix[N_id, N_params[1]] focal_set;       //# Focal slash decider predictor variables    
    matrix[N_id, N_params[2]] target_set;      //# Target slash alter predictor variables
    real dyad_set[N_id, N_id, N_params[3]];    //# Dyadic predictor variables

    matrix[N_id, N_id] Relatedness;

  //# Outcome and exposure data
    int outcomes[N_id,N_id,N_responses];       //# Outcome network of binary ties
    int exposure[N_id,N_id,N_responses];       //# Exposure for each outcome
    int Z[N_id,N_id, N_responses];             //# Mask layer

  //# Accessory paramters 
    matrix[22, 2] priors;                      //# Priors in a matrix, see details in the make_priors() function
    int export_network;                        //# Controls export of predictions
    int outcome_mode;                           //# Are outcomes binomial
}

transformed data{
  //# Refactor to the first predictor slot, becuase it is unity
    matrix[N_id, N_params[1]-1] focal_predictors;     //# Same as focal_set without first column
    matrix[N_id, N_params[2]-1] target_predictors;    //# Same as target_set without first column
    real dyad_predictors[N_id, N_id, N_params[3]-1];  //# Same as dyad_set without first shelf

  //# Store some key indexes
    int N_per_group [max_N_groups, N_group_vars];     //# Number of people in each block-type for each group variable
    int block_indexes[N_group_vars+1];                //# The indexes of each block parameter when stored as a vector instead of ragged array
    int block_param_size;                             //# Total number of block-level parameters

  //# Get size of parameters for block model
    block_param_size = 0;                             //# Start at zero
    block_indexes[1] = 0;                             //# Start at zero for first index 
    
    for(q in 1: N_group_vars){
     block_param_size += N_groups_per_var[q]*N_groups_per_var[q];                      //# Count up number of parameters in each K by K block matrix and add to total
     block_indexes[1+q] = N_groups_per_var[q]*N_groups_per_var[q] + block_indexes[q];  //# Create cummulative sum of block indices, by adding new sum to old sum
     }

  //# First fill with scrap
    for(q in 1: N_group_vars){
    for(k in 1: max_N_groups){
     N_per_group[k, q] = 0;   
     }}

  //# Now fill in real values
    for(q in 1: N_group_vars){
    for(i in 1:N_id){
     N_per_group[block_set[i,q],q] += 1;
     }}

  //# Make pruned data for predictor variables, by dropping first column
    if(N_params[1]>1){
     for(i in 2:N_params[1]){
     focal_predictors[ , i-1] = focal_set[,i];  
     }}

    if(N_params[2]>1){
     for(i in 2:N_params[2]){
     target_predictors[ , i-1] = target_set[,i];  
     }}

    if(N_params[3]>1){
     for(i in 2:N_params[3]){
     dyad_predictors[ , , i-1] = dyad_set[,,i];  
     }}
}

parameters{
    //# Block effects, stored as a vector to save space
    vector[block_param_size] block_effects;

    //# Effects of covariate
    vector[N_params[1]-1] focal_effects;
    vector[N_params[2]-1] target_effects;
    vector[N_params[3]-1] dyad_effects;    
}

model{
  //# Local storage to make code more readable
    matrix[max_N_groups, max_N_groups] B [N_group_vars];  //# Block effects, in array form
    vector[N_group_vars] br;                              //# Sum of block effects per dyad    
    vector[2] scrap;                                      //# Local storage  
    vector[N_responses] checksum;                         //# Check sum     
    real dr_sum;            
    
    //# The first step, is to transform the vector of block effects into a list of matrices
    for(q in 1:N_group_vars){
      B[q,1:N_groups_per_var[q], 1:N_groups_per_var[q]] = to_matrix(block_effects[(block_indexes[q]+1):(block_indexes[q+1])], N_groups_per_var[q], N_groups_per_var[q]);
    }

    //# Then put priors on B, which scale loosely with the block size
    for ( q in 1:N_group_vars ){
    for ( i in 1:N_groups_per_var[q] ){
        for ( j in 1:N_groups_per_var[q] ) {
            if ( i==j ) {
                B[q,i,j] ~ normal(logit(priors[10,1]/sqrt(N_per_group[i,q])), priors[10,2]);   //# transfers more likely within groups
            } else {
              if ( j > i ) {
                B[q,i,j] ~ normal(logit(priors[11,1]/sqrt(N_per_group[i,q]*0.5 + N_per_group[j,q]*0.5)), priors[11,2]); //# transfers less likely between groups
                          } else{
                B[q,i,j] ~ normal(B[q,j,i], 0.01);          //# mirror symetry           
                          }

            }
        }}
    }

    //# Priors on effects of covariates
     focal_effects ~ normal(priors[12,1], priors[12,2]);
     target_effects ~ normal(priors[13,1], priors[13,2]);
     dyad_effects ~ normal(priors[14,1], priors[14,2]);

    //# Sender-receiver priors for social relations model

    //# likelihood
    for(i in 1:(N_id-1)){
    for(j in (i+1):N_id){
 
        for(q in 1:N_group_vars){
          if(q<3){
          br[q] = B[q,block_set[i,q], block_set[j,q]]; //# Extract all of the block components for this dyad
           } 
          if(q==3){
          br[q] = B[q,block_set[i,q], block_set[j,q]]*Relatedness[i,j];   
          }
         }

        dr_sum = dot_product(dyad_effects,  to_vector(dyad_predictors[i, j, ]));

     for(k in 1:N_responses){
      checksum[k] = 0;

      if(Z[i,j,k] == 1){ 
        if(exposure[i,j,k] == 1){
         checksum[k] = bernoulli_logit_lupmf(outcomes[i,j,k] | sum(br) + dr_sum);  
        }}

       }

       target += sum(checksum);

     
     }
    }


 
}

