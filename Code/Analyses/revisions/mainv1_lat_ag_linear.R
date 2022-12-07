library("lme4"); library(tidyverse); data <- read_rds('Data/active/success-cleaned.rds')
mod<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + lat + (1 | species) + (1 | year) + (1 | Region/UnCoor), data = filter(data,NewLU1 == "Ag"), family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))
write_rds(mod,"results/revisions/mainv1_lat_ag_linear.rds")
