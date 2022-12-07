library("lme4"); library(tidyverse)
data <- read_rds("results/revisions/mainv1_res16data.rds")
mod<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet + Tmax_std_gridmet_sq + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region), data = filter(data,NewLU1 == "Forest"), family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06)))
write_rds(mod,paste0("results/revisions/mainv1_res16_forest_quad.rds"))