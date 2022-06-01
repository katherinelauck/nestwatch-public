# 1. Determine whether squared terms are important at all by comparing model with and without ANY squared terms via LRT and AIC
# 2. We know that it is not important, so then reduce model to have NO squared terms
# 3. Start with model with no squared terms and compare it to a model where you drop tmax:lu:BBS
# 4. Report LRT and AIC to see if that interaction is important
# Approach 2 for 3 way interactions
# 1. Start with full model with squared terms and 3 way interactions
# 2. Drop BOTH tmax:lu:BBS AND tmax2:lu:BBS
# 3. Compare with LRT and AIC
library(lme4)
setwd("~/Documents/nestwatch/results/Question 4/")

#load full models
full_bbs_s<-readRDS("success~BBSstdmax2laydate3way.AK.RDS")
full_con_s<-readRDS("success~Conscore.stdmax2laydate3way.AK.RDS")

###BBS and Conservation Score LRTs

# 1. Determine whether squared terms are important at all by comparing model with and without ANY squared terms via LRT and AIC
alt1_bbs<-readRDS("success~BBSstdmaxlaydate3way.LRT.AK.RDS")
alt1_con<-readRDS("success~Conscore.stdmaxlaydate3way.AK.LRT.RDS")

anova(alt1_bbs, full_bbs_s)
anova(alt1_con, full_con_s)

# 2. We know that it is not important, so then reduce model to have NO squared terms

# 3. Start with model with no squared terms and compare it to a model where you drop tmax:lu:BBS

nest<-readRDS("~/Documents/nestwatch/Data/success-cleaned.rds")
nest$Tmax_std_gridmet_sq<-(nest$Tmax_std_gridmet)^2
conscore<-read.csv("~/Documents/nestwatch/Data/NABCI_ConservationScores.csv")
nest.c<-merge(nest,conscore,by.x="species",by.y="CommonName")
nest.c$ConservationScore.scaled<-scale(nest.c$ConservationScore)

model_bbs.tmax.layd.LRT2.s<-glmer(at_least_one_success~
                                   Tmax_std_gridmet*NewLU1+
                                   Tmax_std_gridmet*Trend.scaled +
                                   NewLU1*Trend.scaled +
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

setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_bbs.tmax.layd.LRT2.s, "success~BBSstdmaxlaydate_LRT2.AK.RDS")

model_conscore.tmax.layd.LRT2.s<-glmer(at_least_one_success~
                                    Tmax_std_gridmet*NewLU1+
                                    Tmax_std_gridmet*ConservationScore.scaled +
                                    NewLU1*ConservationScore.scaled +
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

setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_conscore.tmax.layd.LRT2.s, "success~ConScoreStdmaxlaydate_LRT2.AK.RDS")

alt2_bbs<-readRDS("success~BBSstdmaxlaydate_LRT2.AK.RDS")
alt2_con<-readRDS("success~ConScoreStdmaxlaydate_LRT2.AK.RDS")

anova(alt2_bbs, alt1_bbs)
anova(alt2_con, alt1_con)

# Approach 2 for 3 way interactions
# 1. Start with full model with squared terms and 3 way interactions
# 2. Drop BOTH tmax:lu:BBS AND tmax2:lu:BBS
  
  
model_bbs.tmax.layd.LRT3.s<-glmer(at_least_one_success~
                                    Tmax_std_gridmet_sq*NewLU1 +
                                    Tmax_std_gridmet_sq*Trend.scaled +
                                    Tmax_std_gridmet*NewLU1 +
                                    Tmax_std_gridmet*Trend.scaled +
                                    NewLU1*Trend.scaled +
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

setwd("~/Documents/nestwatch/results/Question 4/")
##did not converge
saveRDS(model_bbs.tmax.layd.LRT3.s, "success~BBSstdmaxlaydate_LRT3.AK.RDS")

model_conscore.tmax.layd.LRT3.s<-glmer(at_least_one_success~
                                         Tmax_std_gridmet_sq*NewLU1 +
                                         Tmax_std_gridmet_sq*ConservationScore.scaled +
                                         Tmax_std_gridmet*NewLU1 +
                                         Tmax_std_gridmet*ConservationScore.scaled +
                                         NewLU1*ConservationScore.scaled +
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
###did not converge
setwd("~/Documents/nestwatch/results/Question 4/")
saveRDS(model_conscore.tmax.layd.LRT3.s, "success~ConScoreStdmaxlaydate_LRT3.AK.RDS")
