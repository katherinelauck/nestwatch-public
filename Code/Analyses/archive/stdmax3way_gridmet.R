library("lme4")
library(tidyverse)

nest <- read_rds('Data/data-scaled.rds')

#success, temp anomalies, 365 precip, site std max temp gridmet * max anomaly * NewLU1
mod21<-glmer(at_least_one_success~Tmax_std_gridmet*tnestpd_stdmaxsp_gridmet*NewLU1 + Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=2e9)))

#refit21 <- all_fit(mod21)

write_rds(mod21,'results/success~stdmax3way_gridmet.rds')