##################################################################################################### Normal dyadic plot
dyad_plot = function(res1, title){
dyad = as.data.frame(res1$summary_list$`Dyadic effects`)
dyad = dyad[-1,]
dyad$Variable = c("relatedness", "female-female dyads", "different sex dyads", "coresidence")
ordered_names = c("relatedness", "female-female dyads", "different sex dyads", "coresidence")
dyad$Variable = factor(dyad$Variable, levels = rev(ordered_names))

dyad$Median = as.numeric(dyad$Median)
dyad$lowerHPDI = as.numeric(dyad$'HPDI:L')
dyad$upperHPDI = as.numeric(dyad$'HPDI:H')

p_dyad = ggplot(data=dyad, aes(x=Variable, y=Median, ymin=as.numeric(lowerHPDI), ymax=as.numeric(upperHPDI))) +
  geom_pointrange(position=position_dodge(width = 0.5)) +
  geom_hline(yintercept= 0, lty=2)+
  coord_flip() +
  xlab("") + ylab("Change in log-odds of a tie") +
  theme_bw() +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.text=element_text(size=15))
  plot(p_dyad)
  ggsave(p_dyad, filename = title, width = 7, height = 5)
}

##################################################################################################### Kin interaction dyadic plot
dyad_plot_interation = function(res1, title){
dyad = as.data.frame(res1$summary_list$`Dyadic effects`)
dyad = dyad[-1,]
dyad$Variable = c("female-female dyads", "different sex dyads", "coresidence")
ordered_names = c("female-female dyads", "different sex dyads", "coresidence")
dyad$Variable = factor(dyad$Variable, levels = rev(ordered_names))

dyad$Median = as.numeric(dyad$Median)
dyad$lowerHPDI = as.numeric(dyad$'HPDI:L')
dyad$upperHPDI = as.numeric(dyad$'HPDI:H')

p_dyad = ggplot(data=dyad, aes(x=Variable, y=Median, ymin=as.numeric(lowerHPDI), ymax=as.numeric(upperHPDI))) +
  geom_pointrange(position=position_dodge(width = 0.5)) +
  geom_hline(yintercept= 0, lty=2)+
  coord_flip() +
  xlab("") + ylab("Change in log-odds of a tie") +
  theme_bw() +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=15), legend.text=element_text(size=15))
  plot(p_dyad)
  ggsave(p_dyad, filename = title, width = 7, height = 7)
}

##################################################################################################### Normal age class plots
quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

age_class_process = function(fit, reference_category, category_name){
  stan_object = quiet(summarize_results_special(fit))

  Intercept = stan_object$samples$srm_model_samples$block_parameters[[1]]
  B = stan_object$samples$srm_model_samples$block_parameters[[2]]
  slopes = stan_object$samples$srm_model_samples$dyadic_coeffs 
  
  n_samps = dim(B)[1]
  age_samples = B                
  
  for(i in 1:n_samps){
    age_samples[i, , ] = Intercept[i] + B[i,,] #+ slopes[i,1]  
  }
  
  return(data.frame(
    median = apply(age_samples[,reference_category,], 2, median),
    HL = apply(age_samples[,reference_category,], 2, HPDI)[1,],
    HH = apply(age_samples[,reference_category,], 2, HPDI)[2,],
    focal = rep(category_name, 4)
  ))
}


age_class_plot = function(model1, title){
 ordered_names = attr(model1$data,"group_ids_levels")[[1]]
 age_class_results = list()
 
  for( i in 1:4){
  age_class_results[[i]] = age_class_process(model1, reference_category=i, category_name=ordered_names[i])
  }

 block_res = do.call(rbind, age_class_results)
 block_res$alter = rep(ordered_names, 4)
 block_res$x = rep(1:4,4)
 block_res = block_res[order(factor(block_res$focal, levels = ordered_names)),]
 block_res$alter = factor(block_res$alter, levels = rev(ordered_names))

 age_block_fig = ggplot(data=block_res, 
                       aes(x=alter, y=median, 
                           ymin= HL, ymax= HH)) +
  geom_pointrange(position=position_dodge(width = 0.5)) +
  facet_wrap(vars(fct_relevel(focal, ordered_names)), nrow = 4) +
  # geom_hline(yintercept= 0, lty=2) +
  coord_flip() +
  ggtitle("          Focal age class") +
  xlab("Alter age class\n") + ylab("Log-odds of network tie (intercept)") +
  scale_color_image(image=wonder, discrete=TRUE, choice=min, n=4) +
  theme_bw() +
  theme(plot.title = element_text(size=14),
        axis.text=element_text(size=14), 
        axis.title=element_text(size=14), 
        legend.text=element_text(size=14), 
        strip.text = element_text(size = 14, face = "bold")
  )


  plot(age_block_fig)
  ggsave(age_block_fig, filename = title, width = 6, height = 8)
}

##################################################################################################### Normal heatmaps
heatmap_plot = function(model1, title){
 ordered_names = attr(model1$data,"group_ids_levels")[[1]]
 age_class_results = list()
  for( i in 1:4){
  age_class_results[[i]] = age_class_process(model1, reference_category=i, category_name=ordered_names[i])
  }

 block_res = do.call(rbind, age_class_results)
 block_res$alter = rep(ordered_names, 4)
 block_res$x = rep(1:4, 4)
 block_res = block_res[order(factor(block_res$focal, levels = ordered_names)),]
 block_res$alter = factor(block_res$alter, levels = rev(ordered_names))

 block_res$focal = factor(block_res$focal)
 block_res$focal = factor(block_res$focal, levels = (ordered_names))
 block_res$alter = factor(block_res$alter, levels = (ordered_names))

 block_res$median[which(block_res$alter == "early childhood" & block_res$focal == "adulthood")] = NA
 block_res$median[which(block_res$alter == "middle childhood" & block_res$focal == "adulthood")] = NA
 block_res$median[which(block_res$alter == "adolescence" & block_res$focal == "adulthood")] = NA

 block_res$median[which(block_res$alter == "early childhood" & block_res$focal == "adolescence")] = NA
 block_res$median[which(block_res$alter == "middle childhood" & block_res$focal == "adolescence")] = NA

 block_res$median[which(block_res$alter == "early childhood" & block_res$focal == "middle childhood")] = NA

 block_res$median2 = round(logistic(block_res$median),2)

levels(block_res$alter) = c("early\nchildhood",  "middle\nchildhood", "adolescence", "adulthood")
levels(block_res$focal) = c("early\nchildhood",  "middle\nchildhood", "adolescence", "adulthood")

 heat_map = ggplot(block_res, aes(focal, alter, fill=median2, label=median2)) +
  geom_tile(aes(width=0.96, height=0.96), size=1.5) +
  labs(x = NULL, y = NULL, fill = "Prob.", title="Between age-class tie probabilities") + 
  geom_text(color="black",size=12*0.36) +
  theme_classic() + scale_fill_gradientn(colours =c("#F2F2F2", "#04379C") )+
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) + guides(color = "none") + theme(plot.title = element_text(size = 14))  +
  theme(axis.text = element_text(size = 14))
 
   plot(heat_map)
  ggsave(heat_map, filename = title, width = 6.5, height = 6)
}

##################################################################################################### Kin interaction age class plots
age_class_process_interaction = function(stan_object, reference_category, category_name, kin = 0.5){
  A = res_twostep_interaction$samples$srm_model_samples$block_parameters[[1]]
  B = res_twostep_interaction$samples$srm_model_samples$block_parameters[[2]]
  Y = res_twostep_interaction$samples$srm_model_samples$block_parameters[[3]]

  slopes = res_twostep_interaction$samples$srm_model_samples$dyadic_coeffs
  age_samples = B # Create storage
  n_samps = dim(B)[1]

  for(i in 1:n_samps){
    age_samples[i, , ] = A[i] + B[i, , ] + Y[i,  , ] * kin + slopes[i,1]
  }
  
  return(data.frame(
    median = apply(age_samples[,reference_category,], 2, median),
    HL = apply(age_samples[,reference_category,], 2, HPDI)[1,],
    HH = apply(age_samples[,reference_category,], 2, HPDI)[2,],
    focal = rep(category_name, 4)
  ))
}


age_class_plot_interaction = function(model2, title){
 ordered_names = c("early childhood", "middle childhood","adolescence", "adulthood")
 age_class_results2 = list()
 for( i in 1:4){
  age_class_results2[[i]] = age_class_process_interaction(model2, reference_category=i, category_name=ordered_names[i], kin = 0.0)
 }

 block_res = do.call(rbind, age_class_results2)
 block_res$alter = rep(ordered_names, 4)
 block_res$x = rep(1:4,4)

 block_res = block_res[order(factor(block_res$focal, levels = ordered_names)),]
 block_res$alter = factor(block_res$alter, levels = rev(ordered_names))
 block_res$kin = "non-kin"

 age_class_results2_kin = list()
 for( i in 1:4){
  age_class_results2_kin[[i]] = age_class_process_interaction(model2, reference_category=i, category_name=ordered_names[i], kin = 0.5)
 }

 kin_block_res = do.call(rbind, age_class_results2_kin)
 kin_block_res$alter = rep(ordered_names, 4)
 kin_block_res$x = rep(1:4,4)
 ordered_names = c("early childhood", "middle childhood","adolescence", "adulthood")
 kin_block_res = kin_block_res[order(factor(kin_block_res$focal, levels = ordered_names)),]
 kin_block_res$alter = factor(kin_block_res$alter, levels = rev(ordered_names))
 kin_block_res$kin = "first-order kin"

 final_block_res = rbind(block_res, kin_block_res)

 final_age_block_fig = ggplot(data=final_block_res, 
                             aes(x=alter, y=median, 
                                 ymin= HL, ymax= HH, fill= kin, color = kin)) +
  geom_pointrange(position=position_dodge(width = 0.5)) +
  facet_wrap(vars(fct_relevel(focal, ordered_names)), nrow = 4) +
  #geom_hline(yintercept= 0, lty=2) +
  coord_flip() +
  ggtitle("Focal age class") +
  xlab("Alter age class\n") + ylab("Log-odds of network tie (intercept)") +
  #scale_color_image(image=wonder, discrete=TRUE, choice=min,n=4)
  theme_bw() + scale_color_manual(values =c("#bb6526", "#04379C") ) + theme(legend.position="bottom") + 
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text=element_text(size=15), 
        axis.title=element_text(size=16), 
        legend.text=element_text(size=15), 
        legend.title = element_blank(),
        strip.text = element_text(size = 15, face = "bold"))


  plot(final_age_block_fig)
  ggsave(final_age_block_fig, filename = title, width = 6, height = 8)
}


get_kin_diff = function(stan_object,  kin = 0.5){
  A = stan_object$samples$srm_model_samples$block_parameters[[1]]
  B = stan_object$samples$srm_model_samples$block_parameters[[2]]
  Y = stan_object$samples$srm_model_samples$block_parameters[[3]]

  slopes = stan_object$samples$srm_model_samples$dyadic_coeffs
  age_samples_2 = age_samples_1 = B # Create storage
  n_samps = dim(B)[1]
  
  for(i in 1:dim(B)[1] ){ 
    age_samples_1[i, , ] = A[i] + B[i, , ] + Y[i,  , ] * 0   + slopes[i,1]
    age_samples_2[i, , ] = A[i] + B[i, , ] + Y[i,  , ] * kin + slopes[i,1]
  }
  
  age_samples = age_samples_2 - age_samples_1
  
  names = c("early childhood", "middle childhood","adolescence", "adulthood")
  
  f_names = rbind(names,names,names,names)
  a_names = cbind(names,names,names,names)
  
  return(data.frame(
    median = c(apply(age_samples, 2:3, median)),
    HL = c(apply(age_samples, 2:3, HPDI)[1,,]),
    HH = c(apply(age_samples, 2:3, HPDI)[2,,]),
    focal = c(f_names),
    alter = c(a_names)
  )
  )
}


kin_diff_plot_interaction = function(model2, title){
res_contrast = get_kin_diff(model2, kin=0.5)
####################################################### need to update labels and make rows in correct order
ordered_names = c("early childhood", "middle childhood","adolescence", "adulthood")
res_contrast$alter = rep(ordered_names, 4)
res_contrast = res_contrast[order(factor(res_contrast$focal, levels = ordered_names)),]
res_contrast$alter = factor(res_contrast$alter, levels = rev(ordered_names))

age_block_cont_kin = ggplot(data=res_contrast, 
                            aes(x=alter, y=median, 
                                ymin= HL, ymax= HH)) +
  geom_pointrange(position=position_dodge(width = 0.5)) +
  facet_wrap(vars(fct_relevel(focal, ordered_names)), nrow = 4) +
  # geom_hline(yintercept= 0, lty=2) +
  coord_flip() +
  ggtitle("Focal age class") +
  xlab("Alter age class\n") + ylab("Contrast (difference between kin and non-kin)") +
  scale_color_image(image=wonder, discrete=TRUE, choice=min, n=4) +
  theme_bw() + geom_hline(yintercept= 0, lty=2) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text=element_text(size=15), 
        axis.title=element_text(size=16), 
        legend.text=element_text(size=15), 
        strip.text = element_text(size = 15, face = "bold")
  )


  plot(age_block_cont_kin)
  ggsave(age_block_cont_kin, filename = title, width = 6, height = 8)
}





