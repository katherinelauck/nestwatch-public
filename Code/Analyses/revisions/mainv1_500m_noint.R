library("lme4"); library(tidyverse); nest <- read_rds('Data/active/success-cleaned.rds')
mod<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet + NewLU1 + Tmax_std_gridmet_sq + pcpbefore_raw_gridmet + NLCD_p_forest_500m + NLCD_p_human_500m + NLCD_p_ag_500m + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region150/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))
write_rds(mod,"results/revisions/mainv1_500m_noint.rds")
