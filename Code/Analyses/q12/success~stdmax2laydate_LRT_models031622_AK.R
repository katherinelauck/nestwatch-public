library("lme4"); library(tidyverse)
nest <- read_rds('Data/active/success-cleaned.rds')

# mod1<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet + Tmax_std_gridmet_sq + NewLU1 + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))
# 
# write_rds(mod1,"results/q12/success~stdmax2laydateLU_LRT_AK.rds")

mod2<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet*NewLU1 + Tmax_std_gridmet_sq + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))

write_rds(mod2,"results/q12/success~stdmax2laydateLU_LRT2_AK.rds")