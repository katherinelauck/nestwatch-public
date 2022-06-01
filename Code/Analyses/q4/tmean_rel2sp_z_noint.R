library("lme4")
library(tidyverse)

setwd('/home/kslauck/projects/nestwatch')

nest <- read_rds('Data/active/success-cleaned.rds')

#success, temp anomalies, 365 precip, regional temp * max anomaly * NewLU1
mod17<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet * NewLU1 + Tmax_std_gridmet_sq * NewLU1 + tmean_rel2sp_z + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region/UnCoor), data = nest, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05)))

write_rds(mod17,'results/spatial/success~tmean_rel2sp_z_noint.rds')
