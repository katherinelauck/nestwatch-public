# 1. Determine whether squared terms are important at all by comparing model with and without ANY squared terms via LRT and AIC
# 2. We know that it is not important, so then reduce model to have NO squared terms
# 3. Start with model with no squared terms and compare it to a model where you drop tmax:lu:BBS
# 4. Report LRT and AIC to see if that interaction is important
# Approach 2 for 3 way interactions
# 1. Start with full model with squared terms and 3 way interactions
# 2. Drop BOTH tmax:lu:BBS AND tmax2:lu:BBS
# 3. Compare with LRT and AIC
library("lme4")
library("lubridate")
setwd("~/Documents/nestwatch/Data/")
nest<-readRDS("success-cleaned.rds")
nest$Tmax_std_gridmet_sq<-(nest$Tmax_std_gridmet)^2

setwd("~/Documents/nestwatch/results/Question 3/")
#run full models
model_substrate.tmax2.layd.s<-glmer(at_least_one_success~
                                      Tmax_std_gridmet*NewLU1*substrate_binary +
                                      Tmax_std_gridmet_sq*NewLU1*substrate_binary +
                                      pcpbefore_raw_gridmet +
                                      NLCD_p_forest +
                                      NLCD_p_human +
                                      NLCD_p_ag +
                                      laydate_scaled +
                                      (1|year) +
                                      (1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(model_substrate.tmax2.layd.s, "success~SubstrateStdmax2laydate.AK.RDS")

full_sub<-readRDS("success~SubstrateStdmax2laydate.AK.RDS")
###LRTs

# 1. Determine whether squared terms are important at all by comparing model with and without ANY squared terms via LRT and AIC

#also take out species random effect and laydate

#the LRT model
model_substrate.tmax.s.LRT<-glmer(at_least_one_success~
                                      Tmax_std_gridmet*NewLU1*substrate_binary +
                                      pcpbefore_raw_gridmet +
                                      NLCD_p_forest +
                                      NLCD_p_human +
                                      NLCD_p_ag +
                                      (1|year) +
                                      (1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_substrate.tmax.layd.s.LRT, "success~SubstrateStdmaxlaydate.LRT.AK.RDS")

alt1_sub<-readRDS("success~SubstrateStdmaxlaydate.LRT.AK.RDS")

anova(full_sub, alt1_sub)

##################################################


##for cavity binary
success.stdmaxcavity<-glmer(at_least_one_success~
                                      Tmax_std_gridmet*NewLU1*cavity_binary +
                                      pcpbefore_raw_gridmet +
                                      NLCD_p_forest +
                                      NLCD_p_human +
                                      NLCD_p_ag +
                                      (1|year) +
                                      (1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxcavity, "success~stdmaxcavity.AK.RDS")

nest.ag<-nest[nest$NewLU1=="Ag",]
nest.for<-nest[nest$NewLU1=="Forest",]
nest.natop<-nest[nest$NewLU1=="Natural_open",]
nest.human<-nest[nest$NewLU1=="Human",]
# 
# success.stdmaxcavity.ag<-glmer(at_least_one_success~
#                               Tmax_std_gridmet*cavity_binary +
#                               pcpbefore_raw_gridmet +
#                               NLCD_p_forest +
#                               NLCD_p_human +
#                               NLCD_p_ag +
#                               (1|year) +
#                               (1|Region/UnCoor),data=nest.ag,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# setwd("~/Documents/nestwatch/results/Question 3/")
# saveRDS(success.stdmaxcavity.ag, "success~stdmaxcavity.ag.AK.RDS")


success.stdmaxcavity.laydate.ag<-glmer(at_least_one_success~
                                 Tmax_std_gridmet*cavity_binary +
                                 pcpbefore_raw_gridmet +
                                 NLCD_p_forest +
                                 NLCD_p_human +
                                 NLCD_p_ag +
                                   laydate_scaled +
                                 (1|year) +
                                 (1|Region/UnCoor),data=nest.ag,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxcavity.laydate.ag, "success~stdmaxcavity.laydate.ag.AK.RDS")


# success.stdmaxcavity.for<-glmer(at_least_one_success~
#                                  Tmax_std_gridmet*cavity_binary +
#                                  pcpbefore_raw_gridmet +
#                                  NLCD_p_forest +
#                                  NLCD_p_human +
#                                  NLCD_p_ag +
#                                  (1|year) +
#                                  (1|Region/UnCoor),data=nest.for,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# setwd("~/Documents/nestwatch/results/Question 3/")
# saveRDS(success.stdmaxcavity.for, "success~stdmaxcavity.forest.AK.RDS")


success.stdmaxcavity.laydate.for<-glmer(at_least_one_success~
                                  Tmax_std_gridmet*cavity_binary +
                                  pcpbefore_raw_gridmet +
                                  NLCD_p_forest +
                                  NLCD_p_human +
                                  NLCD_p_ag +
                                  laydate_scaled +
                                  (1|year) +
                                  (1|Region/UnCoor),data=nest.for,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxcavity.laydate.for, "success~stdmaxcavity.laydate.forest.AK.RDS")


# success.stdmaxcavity.human<-glmer(at_least_one_success~
#                                   Tmax_std_gridmet*cavity_binary +
#                                   pcpbefore_raw_gridmet +
#                                   NLCD_p_forest +
#                                   NLCD_p_human +
#                                   NLCD_p_ag +
#                                   (1|year) +
#                                   (1|Region/UnCoor),data=nest.human,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# setwd("~/Documents/nestwatch/results/Question 3/")
# saveRDS(success.stdmaxcavity.human, "success~stdmaxcavity.human.AK.RDS")



success.stdmaxcavity.laydate.human<-glmer(at_least_one_success~
                                    Tmax_std_gridmet*cavity_binary +
                                    pcpbefore_raw_gridmet +
                                    NLCD_p_forest +
                                    NLCD_p_human +
                                    NLCD_p_ag +
                                    laydate_scaled +
                                    (1|year) +
                                    (1|Region/UnCoor),data=nest.human,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxcavity.laydate.human, "success~stdmaxcavity.laydate.human.AK.RDS")


# success.stdmaxcavity.natop<-glmer(at_least_one_success~
#                                   Tmax_std_gridmet*cavity_binary +
#                                   pcpbefore_raw_gridmet +
#                                   NLCD_p_forest +
#                                   NLCD_p_human +
#                                   NLCD_p_ag +
#                                   (1|year) +
#                                   (1|Region/UnCoor),data=nest.natop,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# setwd("~/Documents/nestwatch/results/Question 3/")
# saveRDS(success.stdmaxcavity.natop, "success~stdmaxcavity.natop.AK.RDS")


success.stdmaxcavity.laydate.natop<-glmer(at_least_one_success~
                                    Tmax_std_gridmet*cavity_binary +
                                    pcpbefore_raw_gridmet +
                                    NLCD_p_forest +
                                    NLCD_p_human +
                                    NLCD_p_ag +
                                    laydate_scaled +
                                    (1|year) +
                                    (1|Region/UnCoor),data=nest.natop,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxcavity.laydate.natop, "success~stdmaxcavity.laydate.natop.AK.RDS")

#test<-readRDS("~/Documents/nestwatch/results/Question 3/success~Substr_stdmaxAg.AK.RDS")



##run individual models for substrate instead of cavity, with laydate

success.stdmaxsubstrate.laydate.ag<-glmer(at_least_one_success~
                                         Tmax_std_gridmet*substrate_binary +
                                         pcpbefore_raw_gridmet +
                                         NLCD_p_forest +
                                         NLCD_p_human +
                                         NLCD_p_ag +
                                          laydate_scaled +
                                         (1|year) +
                                         (1|Region/UnCoor),data=nest.ag,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxsubstrate.laydate.ag, "success~stdmaxsubstrate.laydate.ag.AK.RDS")

success.stdmaxsubstrate.laydate.for<-glmer(at_least_one_success~
                                          Tmax_std_gridmet*substrate_binary +
                                          pcpbefore_raw_gridmet +
                                          NLCD_p_forest +
                                          NLCD_p_human +
                                          NLCD_p_ag +
                                            laydate_scaled +
                                          (1|year) +
                                          (1|Region/UnCoor),data=nest.for,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxsubstrate.laydate.for, "success~stdmaxsubstrate.laydate.forest.AK.RDS")


success.stdmaxsubstrate.laydate.human<-glmer(at_least_one_success~
                                            Tmax_std_gridmet*substrate_binary +
                                            pcpbefore_raw_gridmet +
                                            NLCD_p_forest +
                                            NLCD_p_human +
                                            NLCD_p_ag +
                                              laydate_scaled +
                                            (1|year) +
                                            (1|Region/UnCoor),data=nest.human,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxsubstrate.laydate.human, "success~stdmaxsubstrate.laydate.human.AK.RDS")


success.stdmaxsubstrate.laydate.natop<-glmer(at_least_one_success~
                                            Tmax_std_gridmet*substrate_binary +
                                            pcpbefore_raw_gridmet +
                                            NLCD_p_forest +
                                            NLCD_p_human +
                                            NLCD_p_ag +
                                              laydate_scaled +
                                            (1|year) +
                                            (1|Region/UnCoor),data=nest.natop,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxsubstrate.laydate.natop, "success~stdmaxsubstrate.laydate.natop.AK.RDS")



##run substrate/cavity models for LRT to see if substrate is important.

#no squared term, no laydate, and just take out the one interaction term.

model_substrate.tmax.LRT.s<-glmer(at_least_one_success~
                                      Tmax_std_gridmet*NewLU1+
                                      NewLU1*substrate_binary+
                                      Tmax_std_gridmet*substrate_binary+
                                      pcpbefore_raw_gridmet +
                                      NLCD_p_forest +
                                      NLCD_p_human +
                                      NLCD_p_ag +
                                      (1|year) +
                                      (1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(model_substrate.tmax.LRT.s, "success~substrate.tmax.LRT.AK.RDS")


model_cavity.tmax.LRT.s<-glmer(at_least_one_success~
                                    Tmax_std_gridmet*NewLU1+
                                    NewLU1*cavity_binary+
                                    Tmax_std_gridmet*cavity_binary+
                                    pcpbefore_raw_gridmet +
                                    NLCD_p_forest +
                                    NLCD_p_human +
                                    NLCD_p_ag +
                                    (1|year) +
                                    (1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(model_cavity.tmax.LRT.s, "success~cavity.tmax.LRT.AK.RDS")

test<-readRDS("~/Documents/nestwatch/results/Question 3/success~stdmaxsubstrate.AK.RDS")

anova(test,model_substrate.tmax.LRT.s)


###run LRT models with individual land uses and no 2-way interactions with substrate
success.stdmaxcavity.laydate.ag.LRT<-glmer(at_least_one_success~
                                         Tmax_std_gridmet +
                                         cavity_binary +
                                         pcpbefore_raw_gridmet +
                                         NLCD_p_forest +
                                         NLCD_p_human +
                                         NLCD_p_ag +
                                         laydate_scaled +
                                         (1|year) +
                                         (1|Region/UnCoor),data=nest.ag,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxcavity.laydate.ag.LRT, "success~stdmaxcavity.laydate.ag.LRT.AK.RDS")

success.stdmaxcavity.laydate.for.LRT<-glmer(at_least_one_success~
                                             Tmax_std_gridmet +
                                             cavity_binary +
                                             pcpbefore_raw_gridmet +
                                              NLCD_p_forest +
                                              NLCD_p_human +
                                              NLCD_p_ag +
                                             laydate_scaled +
                                             (1|year) +
                                             (1|Region/UnCoor),data=nest.for,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxcavity.laydate.for.LRT, "success~stdmaxcavity.laydate.for.LRT.AK.RDS")


success.stdmaxcavity.laydate.human.LRT<-glmer(at_least_one_success~
                                              Tmax_std_gridmet +
                                              cavity_binary +
                                                pcpbefore_raw_gridmet +
                                                NLCD_p_forest +
                                                NLCD_p_human +
                                                NLCD_p_ag +
                                              laydate_scaled +
                                              (1|year) +
                                              (1|Region/UnCoor),data=nest.human,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxcavity.laydate.human.LRT, "success~stdmaxcavity.laydate.human.LRT.AK.RDS")


success.stdmaxcavity.laydate.natop.LRT<-glmer(at_least_one_success~
                                                Tmax_std_gridmet +
                                                cavity_binary +
                                                pcpbefore_raw_gridmet +
                                                NLCD_p_forest +
                                                NLCD_p_human +
                                                NLCD_p_ag +
                                                laydate_scaled +
                                                (1|year) +
                                                (1|Region/UnCoor),data=nest.natop,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxcavity.laydate.natop.LRT, "success~stdmaxcavity.laydate.natop.LRT.AK.RDS")


##substrate binary

success.stdmaxsubstrate.laydate.ag.LRT<-glmer(at_least_one_success~
                                             Tmax_std_gridmet +
                                               substrate_binary +
                                             pcpbefore_raw_gridmet +
                                             NLCD_p_forest +
                                             NLCD_p_human +
                                             NLCD_p_ag +
                                             laydate_scaled +
                                             (1|year) +
                                             (1|Region/UnCoor),data=nest.ag,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxsubstrate.laydate.ag.LRT, "success~stdmaxsubstrate.laydate.ag.LRT.AK.RDS")

success.stdmaxsubstrate.laydate.for.LRT<-glmer(at_least_one_success~
                                              Tmax_std_gridmet +
                                                substrate_binary +
                                              pcpbefore_raw_gridmet +
                                              NLCD_p_forest +
                                              NLCD_p_human +
                                              NLCD_p_ag +
                                              laydate_scaled +
                                              (1|year) +
                                              (1|Region/UnCoor),data=nest.for,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxsubstrate.laydate.for.LRT, "success~stdmaxsubstrate.laydate.for.LRT.AK.RDS")


success.stdmaxsubstrate.laydate.human.LRT<-glmer(at_least_one_success~
                                                Tmax_std_gridmet +
                                                  substrate_binary +
                                                pcpbefore_raw_gridmet +
                                                NLCD_p_forest +
                                                NLCD_p_human +
                                                NLCD_p_ag +
                                                laydate_scaled +
                                                (1|year) +
                                                (1|Region/UnCoor),data=nest.human,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxsubstrate.laydate.human.LRT, "success~stdmaxsubstrate.laydate.human.LRT.AK.RDS")


success.stdmaxsubstrate.laydate.natop.LRT<-glmer(at_least_one_success~
                                                Tmax_std_gridmet +
                                                  substrate_binary +
                                                pcpbefore_raw_gridmet +
                                                NLCD_p_forest +
                                                NLCD_p_human +
                                                NLCD_p_ag +
                                                laydate_scaled +
                                                (1|year) +
                                                (1|Region/UnCoor),data=nest.natop,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

setwd("~/Documents/nestwatch/results/Question 3/")
saveRDS(success.stdmaxsubstrate.laydate.natop.LRT, "success~stdmaxsubstrate.laydate.natop.LRT.AK.RDS")

