library("lme4")
library(tidyverse)

#setwd('/home/kslauck/projects/nestwatch')

nest <- read_rds('Data/success-cleaned.rds')

mod20<-glmer(at_least_one_success~Tmin_std_gridmet*tnestpd_meanmin_gridmet*NewLU1 + laydate_scaled + pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=2e9)))

write_rds(mod20,'results/success~meanmin3way_gridmet_subbin_laydate.rds')