library("lme4"); library(tidyverse); nest <- read_rds('Data/active/success-cleaned.rds')
mod<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet * NewLU1 + NewLU1 * cavity_binary + Tmax_std_gridmet * cavity_binary + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))
write_rds(mod,"results/q3/success~cavity.tmax.LRT.AK.rds")
