library("lme4"); library(tidyverse); nest <- read_rds('Data/active/success-cleaned.rds')
mod<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet + Tmax_std_gridmet_sq + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region/UnCoor), data = filter(nest, NewLU1 == "Human"), family = binomial(link = "logit"), control = glmerControl(optimizer = c("bobyqa"), optCtrl = list(maxfun = 2e+09)))
write_rds(mod,"results/q12/success~stdmax2_human.rds")
