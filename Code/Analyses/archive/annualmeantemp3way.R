library("lme4")
library(tidyverse)

# setwd('/home/kslauck/projects/nestwatch')

nest <- read_rds('Data/active/success-cleaned.rds')

#success, temp anomalies, 365 precip, regional temp * max anomaly * NewLU1
mod17<-glmer(at_least_one_success~zmaxanomalytemp*avetemp*NewLU1 + zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

write_rds(mod17,'results/q4/success~annualmeantemp3way.rds')