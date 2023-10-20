######################################################################## Age-graded
fill_other_triangle = function(x){
  x[lower.tri(x)] = t(x)[lower.tri(x)]
  return(x)
}

G = 4
V = 1
B2 = matrix(NA, nrow=G, ncol=G)

B2[1,1] = -1
B2[1,2] = -1.5
B2[1,3] = -3
B2[1,4] = -4

B2[2,1] = B2[1,2]
B2[2,2] = -1
B2[2,3] = -1.5
B2[2,4] = -3

B2[3,1] = B2[1,3]
B2[3,2] = B2[2,3]
B2[3,3] = -1
B2[3,4] = -1.5

B2[4,1] = B2[1,4]
B2[4,2] = B2[2,4]
B2[4,3] = B2[3,4]
B2[4,4] = -1

B2 = B2 - 1.05

N_id = 60
test_age = model_dat_nofocal$block_predictors$Age

test_dyadic = array(NA,c(N_id, N_id, 4))
test_dyadic[,,1] = model_dat_nofocal$dyadic_predictors$Relatedness
test_dyadic[,,2] = model_dat_nofocal$dyadic_predictors$Same_Female
test_dyadic[,,3] = model_dat_nofocal$dyadic_predictors$Different_Sex
test_dyadic[,,4] = model_dat_nofocal$dyadic_predictors$Coresidence

dyadic_ests = c(1.5, 1, 0.5, -0.75)



A = simulate_sbm_network(N_id = N_id, B=list(B=B2), V=V, groups = data.frame(Age=as.numeric(as.factor(test_age))),
                         dyadic_predictors = test_dyadic,  
                         dyadic_effects = dyadic_ests,
                         mode="bernoulli"
                               )

Net = graph_from_adjacency_matrix(A$network, mode = c("directed"))
V(Net)$color = c("turquoise4","gray13", "goldenrod3","darkred")[A$group_ids$Age]

plot(Net, edge.arrow.size =0.1, edge.curved = 0.3, vertex.label=NA, vertex.size = 5)


############################ Iterate over days
N_days = 230

Z_twostep_test = array(0, c(N_id, N_id, N_days))

daily_foraging_test = vector("list", N_days)
daily_camp_test = model_dat_nofocal$exposure


for(i in 1:N_days){
      A = simulate_sbm_network(N_id = N_id, B=list(B=B2), V=V, groups = data.frame(Age=as.numeric(as.factor(test_age))),
                         dyadic_predictors = test_dyadic,  
                         dyadic_effects = dyadic_ests,
                         mode="bernoulli"
                               )

  daily_foraging_test[[i]] = fill_other_triangle(A$network)
}


# Two-step masking
Z_twostep_test = array(0, c(N, N, N_days))

for (l in 1:N_days) {
  Z_twostep_test[which(name_list == focal_on_day[l]), , l] = 1
  Z_twostep_test[ , which(name_list == focal_on_day[l]), l] = 1

  alters_on_day = which( daily_foraging_test[[l]][which(name_list == focal_on_day[l]),]  == 1)

  for( j in 1:length(alters_on_day)){
    Z_twostep_test[alters_on_day[j], , l] = 1
    Z_twostep_test[ , alters_on_day[j], l] = 1
  }
}

Age = data.frame(Age=factor(test_age))

dyad = list(Relatedness = r, Same_Female = both_female, Different_Sex = different_sex, Coresidence = coresidence) 


model_dat_twostep_test = make_strand_data_special(self_report=daily_foraging_test, exposure = daily_camp_test, 
                                                  block_covariates=Age, individual_covariates=NULL, 
                                                  dyadic_covariates=dyad, outcome_mode="bernoulli")


model_dat_twostep_test$Relatedness = r
model_dat_twostep_test$Z = Z_twostep_test


############## Two step mask
n_warm = 1000
n_samp = 1000
n_chains = 1
n_cores = 1

fit_twostep_test = fit_block_special(data=model_dat_twostep_test,
                                block_regression = ~ Age,
                                focal_regression = ~ 1,
                                target_regression = ~ 1,
                                dyad_regression = ~ Relatedness + Same_Female + Different_Sex + Coresidence,
                                mode="mcmc",
                                stan_mcmc_parameters = list(seed = 1, chains = n_chains, parallel_chains = n_cores, refresh = 1, iter_warmup = n_warm,
                                                            iter_sampling = n_samp, max_treedepth = NULL, adapt_delta = 0.9)
)

model_twostep_test  = rstan::read_stan_csv(fit_twostep_test$fit$output_files())
res_twostep_test = summarize_results_special(fit_twostep_test)

res_twostep_test$summary_list
res_twostep_test$summary_list$`Dyadic effects`

source("Code/code_plots.R")
dyad_plot(res_twostep_test,"Dyadic_TwoStep_Test.pdf")
age_class_plot(fit_twostep_test,"AgeClass_TwoStep_Test.pdf")
heatmap_plot(fit_twostep_test,"Heatmap_TwoStep_Test.pdf")


