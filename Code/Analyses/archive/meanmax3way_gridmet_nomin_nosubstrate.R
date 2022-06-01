library("lme4")
library(tidyverse)

setwd('/home/kslauck/projects/nestwatch')

nest <- read_rds('Data/data-scaled.rds')

#success, temp anomalies, 365 precip, site mean max temp gridmet * max anomaly * NewLU1
mod20<-glmer(at_least_one_success~Tmax_std_gridmet*tnestpd_meanmax_gridmet*NewLU1 +pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=2e9)))

#refit20 <- all_fit(mod20)

write_rds(mod20,'results/success~meanmax3way_gridmet_nomin_nosubstrate.rds')