library("lme4"); library(tidyverse); nest <- read_rds('Data/active/failure-cleaned.rds')
mod<-glmer(formula = at_least_one_failure ~ Tmax_std_gridmet * NewLU1 * substrate_binary + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + laydate_scaled + (1 | year) + (1 | Region/UnCoor), data = nest.f, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))
write_rds(mod,"results/q3/failure~Substr_stdmaxlaydate3way.AK.rds")
