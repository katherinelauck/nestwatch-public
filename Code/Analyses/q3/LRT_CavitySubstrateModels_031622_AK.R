library(lme4)
library(tidyverse)
nest <- read_rds('Data/active/success-cleaned.rds')

mod1<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet * NewLU1 * substrate_binary + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))

saveRDS(mod1, "results/q3/success~stdmaxsubstratelaydate.AK.RDS")

mod2<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet * NewLU1 + NewLU1 * substrate_binary + Tmax_std_gridmet * substrate_binary + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))

saveRDS(mod2, "results/q3/success~substrate.tmaxlaydate.LRT.AK.RDS")

mod3<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet * NewLU1 * cavity_binary + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))

saveRDS(mod3, "results/q3/success~stdmaxcavitylaydate.AK.RDS")

  
mod4<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet * NewLU1 + NewLU1 * cavity_binary + Tmax_std_gridmet * cavity_binary + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))

saveRDS(mod4, "results/q3/success~cavity.tmaxlaydate.LRT.AK.RDS")

  