# rm(list=ls())
library(devtools)
#install_github('ctross/STRAND')

library(STRAND)
library(rethinking)
library(reshape2)
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(igraph)
library(ggraph)
library(RImagePalette)
library(jpeg)
library(ggplot2)

# set working directory
setwd("C:\\Users\\cody_ross\\Dropbox\\Open Papers\\Womens Foraging Networks\\PublicWorkflow\\")
load("PublicVersion.RData")
############# Run Models
source("Code/fit_block_special_base.R")
source("Code/build_results.R")

############## First run quality test on simulated data
source("Code/test_model.R")

############## Then real models with robustness checks
n_warm = 1000
n_samp = 1000
n_chains = 1
n_cores = 1

############## Only focal
fit_onlyfocal = fit_block_special(data=model_dat_onlyfocal,
                                  block_regression = ~ Age,
                                  focal_regression = ~ 1,
                                  target_regression = ~ 1,
                                  dyad_regression = ~ Relatedness + Same_Female + Different_Sex + Coresidence,
                                  mode="mcmc",
                                  return_predicted_network=FALSE,
                                  stan_mcmc_parameters = list(seed = 1, chains = n_chains, parallel_chains = n_cores, refresh = 1, iter_warmup = n_warm,
                                   iter_sampling = n_samp, max_treedepth = NULL, adapt_delta = 0.9)
)

source("Code/build_results.R")
model_onlyfocal  = rstan::read_stan_csv(fit_onlyfocal$fit$output_files())
res_onlyfocal = summarize_results_special(fit_onlyfocal)

res_onlyfocal$summary_list
res_onlyfocal$summary_list$`Dyadic effects`

source("Code/code_plots.R")
dyad_plot(res_onlyfocal,"Dyadic_OnlyFocal.pdf")
age_class_plot(fit_onlyfocal,"AgeClass_OnlyFocal.pdf")
heatmap_plot(fit_onlyfocal,"Heatmap_OnlyFocal.pdf")

############## Two step mask
fit_twostep = fit_block_special(data=model_dat_twostep,
                                block_regression = ~ Age,
                                focal_regression = ~ 1,
                                target_regression = ~ 1,
                                dyad_regression = ~ Relatedness + Same_Female + Different_Sex + Coresidence,
                                mode="mcmc",
                                stan_mcmc_parameters = list(seed = 1, chains = n_chains, parallel_chains = n_cores, refresh = 1, iter_warmup = n_warm,
                                                            iter_sampling = n_samp, max_treedepth = NULL, adapt_delta = 0.9)
)

model_twostep  = rstan::read_stan_csv(fit_twostep$fit$output_files())
res_twostep = summarize_results_special(fit_twostep)

res_twostep$summary_list
res_twostep$summary_list$`Dyadic effects`

source("Code/code_plots.R")
dyad_plot(res_twostep,"Dyadic_TwoStep.pdf")
age_class_plot(fit_twostep,"AgeClass_TwoStep.pdf")
heatmap_plot(fit_twostep,"Heatmap_TwoStep.pdf")

############## Two step mask no focal
fit_twostep_nf = fit_block_special(data=model_dat_twostep_nf,
                                              block_regression = ~ Age,
                                              focal_regression = ~ 1,
                                              target_regression = ~ 1,
                                              dyad_regression = ~ Relatedness + Same_Female + Different_Sex + Coresidence,
                                              mode="mcmc",
                                              stan_mcmc_parameters = list(seed = 1, chains = n_chains, parallel_chains = n_cores, refresh = 1, iter_warmup = n_warm,
                                                                          iter_sampling = n_samp, max_treedepth = NULL, adapt_delta = 0.9)
)

model_twostep_nf  = rstan::read_stan_csv(fit_twostep_nf$fit$output_files())
res_twostep_nf = summarize_results_special(fit_twostep_nf)

res_twostep_nf$summary_list
res_twostep_nf$summary_list$`Dyadic effects`

source("Code/code_plots.R")
dyad_plot(res_twostep_nf,"Dyadic_TwoStepNF.pdf")
age_class_plot(fit_twostep_nf,"AgeClass_TwoStepNF.pdf")
heatmap_plot(fit_twostep_nf,"Heatmap_TwoStepNF.pdf")

############## No focal
fit_nofocal = fit_block_special(data=model_dat_nofocal,
                                              block_regression = ~ Age,
                                              focal_regression = ~ 1,
                                              target_regression = ~ 1,
                                              dyad_regression = ~ Relatedness + Same_Female + Different_Sex + Coresidence,
                                              mode="mcmc",
                                              stan_mcmc_parameters = list(seed = 1, chains = n_chains, parallel_chains = n_cores, refresh = 1, iter_warmup = n_warm,
                                                                          iter_sampling = n_samp, max_treedepth = NULL, adapt_delta = 0.9)
)

model_nofocal  = rstan::read_stan_csv(fit_nofocal$fit$output_files())
res_nofocal = summarize_results_special(fit_nofocal)

res_nofocal$summary_list
res_nofocal$summary_list$`Dyadic effects`

source("Code/code_plots.R")
dyad_plot(res_nofocal,"Dyadic_NoFocal.pdf")
age_class_plot(fit_nofocal,"AgeClass_NoFocal.pdf")
heatmap_plot(fit_nofocal,"Heatmap_NoFocal.pdf")

############## No mask
fit_nomask = fit_block_special(data=model_dat_nomask,
                                              block_regression = ~ Age,
                                              focal_regression = ~ 1,
                                              target_regression = ~ 1,
                                              dyad_regression = ~ Relatedness + Same_Female + Different_Sex + Coresidence,
                                              mode="mcmc",
                                              stan_mcmc_parameters = list(seed = 1, chains = n_chains, parallel_chains = n_cores, refresh = 1, iter_warmup = n_warm,
                                                                          iter_sampling = n_samp, max_treedepth = NULL, adapt_delta = 0.9)
)

model_nomask  = rstan::read_stan_csv(fit_nomask$fit$output_files())
res_nomask = summarize_results_special(fit_nomask)

res_nomask$summary_list
res_nomask$summary_list$`Dyadic effects`

source("Code/code_plots.R")
dyad_plot(res_nomask,"Dyadic_NoMask.pdf")
age_class_plot(fit_nomask,"AgeClass_NoMask.pdf")
heatmap_plot(fit_nomask,"Heatmap_NoMask.pdf")



#######################################################################
## Second model with an interaction between relatedness and age blocks
source("Code/fit_block_special_interaction.R")
 Age_Kin = Age
 colnames(Age_Kin) = "Age_Kin"
 model_dat_twostep_interaction = make_strand_data_special(self_report=daily_foraging, exposure = daily_camp, 
                                                             block_covariates=data.frame(Age=Age, Age_Kin = Age_Kin),
                                                                    individual_covariates=NULL, dyadic_covariates=dyad, 
                                                                    outcome_mode="bernoulli")
 model_dat_twostep_interaction$Z = Z_twostep
 model_dat_twostep_interaction$Relatedness = r

############## Two step mask
fit_twostep_interation = fit_block_special_interaction(data=model_dat_twostep_interaction,
                                              block_regression = ~ Age + Age_Kin,
                                              focal_regression = ~ 1,
                                              target_regression = ~ 1,
                                              dyad_regression = ~ Same_Female + Different_Sex + Coresidence,
                                              mode="mcmc",
                                              stan_mcmc_parameters = list(seed = 1, chains = n_chains, parallel_chains = n_cores, refresh = 1, iter_warmup = n_warm,
                                                                          iter_sampling = n_samp, max_treedepth = 12, adapt_delta = 0.9)
)


model_twostep_interaction  = rstan::read_stan_csv(fit_twostep_interation$fit$output_files())
res_twostep_interaction = summarize_results_special(fit_twostep_interation)

res_twostep_interaction$summary_list
res_twostep_interaction$summary_list$`Dyadic effects`

source("Code/code_plots.R")
dyad_plot_interation(res_twostep_interaction,"Dyadic_TwoStep_Interaction.pdf")
age_class_plot_interaction(res_twostep_interaction,"AgeClass_TwoStep_Interaction.pdf")
kin_diff_plot_interaction(res_twostep_interaction,"KinDiff_TwoStep_Interaction.pdf")


