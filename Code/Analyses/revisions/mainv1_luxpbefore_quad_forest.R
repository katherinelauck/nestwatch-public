library("lme4"); library(tidyverse); nest <- read_rds('Data/active/success-cleaned.rds')
mod<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet + pcpbefore_raw_gridmet + pcpbefore_raw_gridmet_sq + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region/UnCoor), data = filter(nest,NewLU1 == "Forest"), family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))
write_rds(mod,"results/revisions/mainv1_luxpbefore_quad_forest.rds")
