
library("lme4")
library(tidyverse)

setwd('/home/kslauck/projects/nestwatch')

nest <- read_rds('Data/active/success-cleaned.rds')

#success, temp anomalies, 365 precip, regional temp * max anomaly * NewLU1
mod17<-glmer(at_least_one_success ~ Tmax_std_gridmet * tmean_rel2sp_z + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human +  NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region/UnCoor),data=filter(nest,NewLU1=='Forest'),family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

write_rds(mod17,'results/spatial/success~tmean_rel2sp_z_Forest_3way.rds')
