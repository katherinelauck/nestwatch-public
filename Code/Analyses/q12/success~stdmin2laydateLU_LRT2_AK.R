library("lme4"); library(tidyverse); nest <- read_rds('Documents/nestwatch/Data/active/success-cleaned.rds')
nest$Tmin_std_gridmet_sq<-nest$Tmin_std_gridmet^2
mod<-glmer(formula = at_least_one_success ~ Tmin_std_gridmet*NewLU1 + Tmin_std_gridmet_sq + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))
write_rds(mod,"~/Documents/nestwatch/results/q12/success~stdmin2laydateLU_LRT2_AK.rds")
