library("lme4")
library("sjPlot")
library("lubridate")
setwd("~/Documents/nestwatch/Data/")
nest<-readRDS("success-cleaned.rds")
nest.f<-readRDS("failure-cleaned.rds")

conscore<-read.csv("NABCI_ConservationScores.csv")

nest.c<-merge(nest,conscore,by.x="species",by.y="CommonName")

##look into squared effect of temp on the basic model
nest$Tmax_std_gridmet_sq<-(nest$Tmax_std_gridmet)^2
nest$Tmin_std_gridmet_sq<-(nest$Tmin_std_gridmet)^2
nest.f$Tmax_std_gridmet_sq<-(nest.f$Tmax_std_gridmet)^2
nest.f$Tmin_std_gridmet_sq<-(nest.f$Tmin_std_gridmet)^2

##the laydate models that didn't converge:
# model_bbs.tmin.layd.s
# model_substrate.tmax.layd.s


#basic model success, max temp anomaly, and with laydate
#model_baseline.tmax.s<-glmer(at_least_one_success~Tmax_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

# model_baseline.tmax.layd.s<-glmer(at_least_one_success~Tmax_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# setwd("~/Documents/nestwatch/results/Question 1-2/")
# saveRDS(model_baseline.tmax.layd.s, "success~stdmaxlaydate2way.AK.RDS")

##success, no interactions whatsoever, with squared tmax and regular tmax
model_LRT.tmax.layd.s<-glmer(at_least_one_success~Tmax_std_gridmet+Tmax_std_gridmet_sq+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_LRT.tmax.layd.s, "success~stdmax2laydate_LRT_AK.RDS")

#success, no interactions whatsoever, with squared tmin and regular tmin
model_LRT.tmin.layd.s<-glmer(at_least_one_success~Tmin_std_gridmet+Tmin_std_gridmet_sq+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_LRT.tmin.layd.s, "success~stdmin2laydate_LRT_AK.RDS")

##failure, no interactions whatsoever, with squared tmax and regular tmax
model_LRT.tmax.layd.f<-glmer(at_least_one_failure~Tmax_std_gridmet+Tmax_std_gridmet_sq+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_LRT.tmax.layd.f, "failure~stdmax2laydate_LRT_AK.RDS")

##failure, no interactions whatsoever, with squared tmin and regular tmin
model_LRT.tmin.layd.f<-glmer(at_least_one_failure~Tmin_std_gridmet+Tmin_std_gridmet_sq+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_LRT.tmin.layd.f, "failure~stdmin2laydate_LRT_AK.RDS")

#model_baseline.tmax.f<-glmer(at_least_one_failure~Tmax_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
model_baseline.tmax.layd.f<-glmer(at_least_one_failure~Tmax_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_baseline.tmax.layd.f, "failure~stdmaxlaydate2way.AK.RDS")


#basic model success, min temp anomaly
#model_baseline.tmin.s<-glmer(at_least_one_success~Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e7)))

model_baseline.tmin.layd.s<-glmer(at_least_one_success~Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e7)))

saveRDS(model_baseline.tmin.layd.s, "success~stdminlaydate2way.AK.RDS")

#model_baseline.tmin.f<-glmer(at_least_one_failure~Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

model_baseline.tmin.layd.f<-glmer(at_least_one_failure~Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


saveRDS(model_baseline.tmax.layd.f, "failure~stdmaxlaydate2way.AK.RDS")

#basic model failure, min temp anomaly
#model_baseline.tmin.f<-glmer(at_least_one_failure~Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#saveRDS(model_baseline.tmin.f, "failure~stdmin2way.AK.RDS")

#model with 3-way interaction with BBS trend success, max temp anomaly, laydate 
model_bbs.tmax.layd.s<-glmer(at_least_one_success~Tmax_std_gridmet*NewLU1*Trend.scaled+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_bbs.tmax.layd.s, "success~BBSstdmaxlaydate3way.AK.RDS")

#model with 3-way interaction with BBS trend success, min temp anomaly, laydate 
model_bbs.tmin.layd.s<-glmer(at_least_one_success~Tmin_std_gridmet*NewLU1*Trend.scaled+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e7)))
##DID NOT CONVERGE
#saveRDS(model_bbs.tmin.layd.s, "success~BBSstdminlaydate3way.AK.RDS")

#model with 3-way interaction with BBS trend failure, max temp anomaly, laydate 
model_bbs.tmax.layd.f<-glmer(at_least_one_failure~Tmax_std_gridmet*NewLU1*Trend.scaled+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_bbs.tmax.layd.f, "failure~BBSstdmaxlaydate3way.AK.RDS")

#model with 3-way interaction with BBS trend failure, min temp anomaly, laydate 

model_bbs.tmin.layd.f<-glmer(at_least_one_failure~Tmin_std_gridmet*NewLU1*Trend.scaled+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_bbs.tmin.layd.f, "failure~BBSstdminlaydate3way.AK.RDS")

#model with 3-way interaction with substrate trend success, max temp anomaly, laydate 
model_substrate.tmax.layd.s<-glmer(at_least_one_success~Tmax_std_gridmet*NewLU1*substrate_binary+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+laydate_scaled+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
#DID NOT CONVERGE
#saveRDS(model_substrate.tmax.layd.s, "success~Substr_stdmaxlaydate3way.AK.RDS")

#model with 3-way interaction with substrate trend failure, max temp anomaly, laydate 
model_substrate.tmax.layd.f<-glmer(at_least_one_failure~Tmax_std_gridmet*NewLU1*substrate_binary+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+laydate_scaled+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_substrate.tmax.layd.f, "failure~Substr_stdmaxlaydate3way.AK.RDS")

#model with 3-way interaction with substrate trend success, min temp anomaly, laydate 
model_substrate.tmin.layd.s<-glmer(at_least_one_success~Tmin_std_gridmet*NewLU1*substrate_binary+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+laydate_scaled+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_substrate.tmin.layd.s, "success~Substr_stdminlaydate3way.AK.RDS")

#model with 3-way interaction with substrate trend failure, min temp anomaly, laydate 
model_substrate.tmin.layd.f<-glmer(at_least_one_failure~Tmin_std_gridmet*NewLU1*substrate_binary+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+laydate_scaled+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_substrate.tmin.layd.f, "failure~Substr_stdminlaydate3way.AK.RDS")
#based on the original model, we know that the land uses are sig. different from each other when comparing them between substrates. Which of them is sig. different from itself in diff substrates?



##likelihood ratio tests for bbs for those that converged
#DID NOT CONVERGE anova(model_baseline.tmax.layd.s, model_substrate.tmax.layd.s) 
anova(model_baseline.tmax.layd.f, model_bbs.tmax.layd.f) #2-way
anova(model_baseline.tmin.layd.s, model_substrate.tmin.layd.s) #2-way
anova(model_baseline.tmin.layd.f, model_substrate.tmin.layd.f) #2-way


##likelihood ratio tests for substrate for those that converged

#run models to compare the substrate model to
model_substrateLRT.tmax.layd.f<-glmer(at_least_one_failure~Tmax_std_gridmet*NewLU1+NewLU1*substrate_binary+Tmax_std_gridmet*substrate_binary+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+laydate_scaled+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_substrateLRT.tmax.layd.f, "failure~Substr_stdmaxlaydateLRT.AK.RDS")

#RERAN
model_substrateLRT.tmin.layd.s<-glmer(at_least_one_success~Tmin_std_gridmet*NewLU1+NewLU1*substrate_binary+Tmin_std_gridmet*substrate_binary+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+laydate_scaled+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_substrateLRT.tmin.layd.s, "success~Substr_stdminlaydateLRT.AK.RDS")

#RERAN
model_substrateLRT.tmin.layd.f<-glmer(at_least_one_failure~Tmin_std_gridmet*NewLU1+NewLU1*substrate_binary+Tmin_std_gridmet*substrate_binary+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+laydate_scaled+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_substrateLRT.tmin.layd.f, "failure~Substr_stdminlaydateLRT.AK.RDS")

setwd("~/Documents/nestwatch/results/Question 3/")

test<-readRDS("failure~Substr_stdmaxlaydate3way.AK.RDS")
anova(model_substrateLRT.tmax.layd.f, test)
#there is no significant difference between models

test<-readRDS("success~Substr_stdminlaydate3way.AK.RDS")
anova(model_substrateLRT.tmin.layd.s, test)
#there is no significant difference between models

test<-readRDS("failure~Substr_stdminlaydate3way.AK.RDS")
anova(model_substrateLRT.tmin.layd.f, test)
#there is no significant difference between models


##likelihood ratio tests for bbs for those that converged

#re-run bbs models with the more limited data

nest.bbs<-nest[is.na(nest$Trend.scaled)==F,]

#model with 3-way interaction with BBS trend success, max temp anomaly, laydate 
model_bbsLRT.tmax.layd.s<-glmer(at_least_one_success~Tmax_std_gridmet*NewLU1+NewLU1*Trend.scaled+Tmax_std_gridmet*Trend.scaled+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 4/")

saveRDS(model_bbsLRT.tmax.layd.s, "success~BBSstdmaxlaydate_LRT.AK.RDS")

#model with 3-way interaction with BBS trend failure, max temp anomaly, laydate 
model_bbsLRT.tmax.layd.f<-glmer(at_least_one_failure~Tmax_std_gridmet*NewLU1+NewLU1*Trend.scaled+Tmax_std_gridmet*Trend.scaled+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_bbsLRT.tmax.layd.f, "failure~BBSstdmaxlaydate_LRT.AK.RDS")

#model with 3-way interaction with BBS trend failure, min temp anomaly, laydate 

model_bbsLRT.tmin.layd.f<-glmer(at_least_one_failure~Tmin_std_gridmet*NewLU1+NewLU1*Trend.scaled+Tmin_std_gridmet*Trend.scaled+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_bbsLRT.tmin.layd.f, "failure~BBSstdminlaydate_LRT.AK.RDS")

##do LRT
test<-readRDS("success~BBSstdmaxlaydate3way.AK.RDS")
anova(test, model_bbsLRT.tmax.layd.s) 
#there is no significant difference between models

test<-readRDS("failure~BBSstdmaxlaydate3way.AK.RDS")
anova(test, model_bbsLRT.tmax.layd.f) 
#there is no significant difference between models

test<-readRDS("failure~BBSstdminlaydate3way.AK.RDS")
anova(test, model_bbsLRT.tmin.layd.f) 
#there is no significant difference between models


#anova to see if linear is better
setwd("~/Documents/nestwatch/results/Question 1-2/")
test<-readRDS("success~stdmaxlaydate2way.AK.RDS")
anova(test,model_baseline.tmax2.layd.s)

###look into significance of land uses in substrate models
substrate.success<-readRDS("~/Documents/nestwatch/results/Question 3/success~stdmaxsubstrate.AK.RDS")
plot_model(substrate.success,type="int")


###run some basic models replacing tmax anom and tmin anom with squared values

model_baseline.tmax2.layd.s<-glmer(at_least_one_success~Tmax_std_gridmet*NewLU1+Tmax_std_gridmet_sq*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#the corresponding model for the LRT, full model without interaction between tmax_sq and land use
model_baseline.tmax2.layd.s.LRT<-glmer(at_least_one_success~Tmax_std_gridmet*NewLU1+Tmax_std_gridmet_sq+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_baseline.tmax2.layd.s.LRT, "success~stdmax2laydate2way.LRT.AK.RDS")

##for the LRT, full model without interaction between tmin_sq and land use
model_baseline.tmin2.layd.s<-glmer(at_least_one_success~Tmin_std_gridmet*NewLU1+Tmin_std_gridmet_sq*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_baseline.tmin2.layd.s, "success~stdmin2laydate2way.AK.RDS")


#the corresponding model for the LRT, failure, full model without interaction between tmax_sq and land use
model_baseline.tmax2.layd.f.LRT<-glmer(at_least_one_failure~Tmax_std_gridmet*NewLU1+Tmax_std_gridmet_sq+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_baseline.tmax2.layd.f.LRT, "failure~stdmax2laydate2way.LRT.AK.RDS")

#the corresponding model for the LRT, failure, full model without interaction between tmin_sq and land use
model_baseline.tmin2.layd.f.LRT<-glmer(at_least_one_failure~Tmin_std_gridmet*NewLU1+Tmin_std_gridmet_sq+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_baseline.tmin2.layd.f.LRT, "failure~stdmin2laydate2way.LRT.AK.RDS")



#the corresponding model for the LRT
model_baseline.tmin2.layd.s.LRT<-glmer(at_least_one_success~Tmin_std_gridmet*NewLU1+Tmin_std_gridmet_sq+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_baseline.tmin2.layd.s.LRT, "success~stdmin2laydate2way.LRT.AK.RDS")


##do LRT without interaction
modsq<-readRDS("success~stdmax2laydate2way.AK.RDS")
anova(modsq,model_baseline.tmax2.layd.s.LRT)

#for success and max, the model with the squared interaction is marginally not better, p value is 0.053

##run it for tmax squared, all interactions, failure
model_baseline.tmax2.layd.f<-glmer(at_least_one_failure~Tmax_std_gridmet*NewLU1+Tmax_std_gridmet_sq*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_baseline.tmax2.layd.f, "failure~stdmax2laydate2way.AK.RDS")




##run it for tmin squared, all interactions, failure
model_baseline.tmin2.layd.f<-glmer(at_least_one_failure~Tmin_std_gridmet*NewLU1+Tmin_std_gridmet_sq*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate_binary+laydate_scaled+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.f,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 1-2/")
saveRDS(model_baseline.tmin2.layd.f, "failure~stdmin2laydate2way.AK.RDS")



modsq_min<-readRDS("success~stdmin2laydate2way.AK.RDS")
anova(modsq_min,model_baseline.tmin2.layd.s.LRT)

#for success and min, the model with the squared interaction is marginally not better, p value is 0.050


#model with 3-way interaction with BBS trend success, 3-way with temp squared, max temp anomaly, laydate 
model_bbs.tmax2.layd.s<-glmer(at_least_one_success~
                               Tmax_std_gridmet*NewLU1*Trend.scaled +
                               Tmax_std_gridmet_sq*NewLU1*Trend.scaled +
                               pcpbefore_raw_gridmet +
                               NLCD_p_forest +
                               NLCD_p_human +
                               NLCD_p_ag +
                               substrate_binary +
                               laydate_scaled +
                               (1|species) +
                               (1|year) +
                               (1|Region/UnCoor),
                             data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#did not save
setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_bbs.tmax2.layd.s, "success~BBSstdmax2laydate3way.AK.RDS")

#model with 3-way interaction with BBS trend success, no 3-way with temp squared, max temp anomaly, laydate 
model_bbs.tmax2.layd.LRT.s<-glmer(at_least_one_success~
                                Tmax_std_gridmet*NewLU1*Trend.scaled +
                                Tmax_std_gridmet_sq*NewLU1 +
                                pcpbefore_raw_gridmet +
                                NLCD_p_forest +
                                NLCD_p_human +
                                NLCD_p_ag +
                                substrate_binary +
                                laydate_scaled +
                                (1|species) +
                                (1|year) +
                                (1|Region/UnCoor),
                              data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#did not converge
setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_bbs.tmax.layd.LRT.s, "success~BBSstdmax2laydate3way.LRT.AK.RDS")

#model with 3-way interaction with BBS trend success, no temp squared at all, max temp anomaly, laydate 
model_bbs.tmax.layd.LRT.s<-glmer(at_least_one_success~
                                    Tmax_std_gridmet*NewLU1*Trend.scaled +
                                    pcpbefore_raw_gridmet +
                                    NLCD_p_forest +
                                    NLCD_p_human +
                                    NLCD_p_ag +
                                    substrate_binary +
                                    laydate_scaled +
                                    (1|species) +
                                    (1|year) +
                                    (1|Region/UnCoor),
                                  data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#saved
setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_bbs.tmax.layd.LRT.s, "success~BBSstdmaxlaydate3way.LRT.AK.RDS")



#model with 3-way interaction with BBS trend success, 3-way with temp squared, max temp anomaly, laydate 
model_bbs.tmax2.layd.s<-glmer(at_least_one_success~
                                Tmax_std_gridmet*NewLU1*Trend.scaled +
                                Tmax_std_gridmet_sq*NewLU1*Trend.scaled +
                                pcpbefore_raw_gridmet +
                                NLCD_p_forest +
                                NLCD_p_human +
                                NLCD_p_ag +
                                substrate_binary +
                                laydate_scaled +
                                (1|species) +
                                (1|year) +
                                (1|Region/UnCoor),
                              data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#saved
setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_bbs.tmax.layd.s, "success~BBSstdmax2laydate3way.AK.RDS")

#model with 3-way interaction with BBS trend success, no 3-way with temp squared, max temp anomaly, laydate 
model_bbs.tmax2.layd.LRT.s<-glmer(at_least_one_success~
                                    Tmax_std_gridmet*NewLU1*Trend.scaled +
                                    Tmax_std_gridmet_sq*NewLU1 +
                                    pcpbefore_raw_gridmet +
                                    NLCD_p_forest +
                                    NLCD_p_human +
                                    NLCD_p_ag +
                                    substrate_binary +
                                    laydate_scaled +
                                    (1|species) +
                                    (1|year) +
                                    (1|Region/UnCoor),
                                  data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#did not save
setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_bbs.tmax.layd.LRT.s, "success~BBSstdmax2laydate3way.LRT.AK.RDS")




#model with 3-way interaction with conservation score success, 3-way with temp squared, max temp anomaly, laydate 
model_conscore.tmax2.layd.s<-glmer(at_least_one_success~
                                     Tmax_std_gridmet*NewLU1*ConservationScore.scaled +
                                     Tmax_std_gridmet_sq*NewLU1*ConservationScore.scaled +
                                     pcpbefore_raw_gridmet +
                                     NLCD_p_forest +
                                     NLCD_p_human +
                                     NLCD_p_ag +
                                     substrate_binary +
                                     laydate_scaled +
                                     (1|species) +
                                     (1|year) +
                                     (1|Region/UnCoor),
                                   data=nest.c,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#did not save
setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_conscore.tmax2.layd.s, "success~Conscore.stdmax2laydate3way.AK.RDS")

plot_model(model_conscore.tmax2.layd.s,type="int")

#model with 3-way interaction with conscore trend success, no 3-way with temp squared, max temp anomaly, laydate 
model_conscore.tmax2.layd.LRT.s<-glmer(at_least_one_success~
                                         Tmax_std_gridmet*NewLU1*ConservationScore.scaled +
                                         Tmax_std_gridmet_sq*NewLU1 +
                                         pcpbefore_raw_gridmet +
                                         NLCD_p_forest +
                                         NLCD_p_human +
                                         NLCD_p_ag +
                                         substrate_binary +
                                         laydate_scaled +
                                         (1|species) +
                                         (1|year) +
                                         (1|Region/UnCoor),
                                       data=nest.c,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#did not converge
setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_conscore.tmax2.layd.LRT.s, "success~Conscore.stdmax2laydate3way.AK.LRT.RDS")


#try model with 3-way interaction with conscore trend success, no temp squared at all, max temp anomaly, laydate 
model_conscore.tmax.layd.LRT.s<-glmer(at_least_one_success~
                                        Tmax_std_gridmet*NewLU1*ConservationScore.scaled +
                                        pcpbefore_raw_gridmet +
                                        NLCD_p_forest +
                                        NLCD_p_human +
                                        NLCD_p_ag +
                                        substrate_binary +
                                        laydate_scaled +
                                        (1|species) +
                                        (1|year) +
                                        (1|Region/UnCoor),
                                      data=nest.c,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#did not save
setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_conscore.tmax.layd.LRT.s, "success~Conscore.stdmaxlaydate3way.AK.LRT.RDS")
