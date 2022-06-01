#use this code to build a summary table
#columns: response (success, failure), category (random effects, precip, temp, NLCD), specific predictors, AIC, p-value from anova

rm(list=ls())


library(lme4)
library(tidyverse)
library(lubridate)

setwd("/Volumes/GoogleDrive/My Drive/NestWatchProject/Data"); getwd()

load("oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet.RData") #use new data file
nest<-processed


#NLCD categories: human, forest, non-forest
nest$NLCD_p_forest_2km<-nest$NLCD_decidious_forest_2km+nest$NLCD_evergreen_forest_2km+nest$NLCD_mixed_forest_2km
nest$NLCD_p_human_2km<-nest$NLCD_developed_open_2km+nest$NLCD_developed_low_2km+nest$NLCD_developed_high_2km+nest$NLCD_developed_medium_2km
nest$NLCD_p_ag_2km<-nest$NLCD_cultivated_crops_2km+nest$NLCD_pasture_2km

nest$NLCD_p_forest_1km<-nest$NLCD_decidious_forest_1km+nest$NLCD_evergreen_forest_1km+nest$NLCD_mixed_forest_1km
nest$NLCD_p_human_1km<-nest$NLCD_developed_open_1km+nest$NLCD_developed_low_1km+nest$NLCD_developed_high_1km+nest$NLCD_developed_medium_1km
nest$NLCD_p_ag_1km<-nest$NLCD_cultivated_crops_1km+nest$NLCD_pasture_1km

nest$NLCD_p_forest_500m<-nest$NLCD_decidious_forest_500m+nest$NLCD_evergreen_forest_500m+nest$NLCD_mixed_forest_500m
nest$NLCD_p_human_500m<-nest$NLCD_developed_open_500m+nest$NLCD_developed_low_500m+nest$NLCD_developed_high_500m+nest$NLCD_developed_medium_500m
nest$NLCD_p_ag_500m<-nest$NLCD_cultivated_crops_500m+nest$NLCD_pasture_500m


#scale everything that's relevant
nest[,c(122:156)] <- scale(nest[,c(122:156)])

nest$UnCoor<-factor(nest$UnCoor)
nest$Region<-factor(nest$Region200)
nest$Region250<-factor(nest$Region250)
nest$year<-factor(nest$year)
nest$species<-factor(nest$species)

#look for NAs
#remove rows where response is NA
nest %>%
  filter(!is.na(at_least_one_failure))->nest.failure
nest %>%
  filter(!is.na(at_least_one_success))->nest.success

nest %>%
  filter(!is.na(at_least_one_success) & !is.na(noanomoly_mean_precip_Next45))->nest.success.precip
nest %>%
  filter(!is.na(at_least_one_failure) & !is.na(noanomoly_mean_precip_Next45))->nest.failure.precip

nest %>%
  filter(!is.na(at_least_one_success) & !is.na(noanomoly_max_tmax_Next45))->nest.success.temp
nest %>%
  filter(!is.na(at_least_one_failure) & !is.na(noanomoly_max_tmax_Next45))->nest.failure.temp

nest %>%
  filter(!is.na(at_least_one_success) & !is.na(NLCD_p_forest_500m))->nest.success.NLCD
nest %>%
  filter(!is.na(at_least_one_failure) & !is.na(NLCD_p_forest_500m))->nest.failure.NLCD

#compare random effect structure for at leat one success
mod0.success.RE<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|UnCoor),data=nest.success,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.success.RE<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.success,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.success.RE<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|Region250/UnCoor),data=nest.success,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#compare random effect structure for at leat one failure
mod0.failure.RE<-glmer(at_least_one_failure~ 1+(1|species)+(1|year)+(1|UnCoor),data=nest.failure,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.failure.RE<-glmer(at_least_one_failure~ 1+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.failure,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.failure.RE<-glmer(at_least_one_failure~ 1+(1|species)+(1|year)+(1|Region250/UnCoor),data=nest.failure,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#nest success precipitation models
mod0.success.precip<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.success.precip<-glmer(at_least_one_success~ noanomoly_mean_precip_Next45+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.success.precip<-glmer(at_least_one_success~ mean_anamoly_mean_precip_Next45+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod3.success.precip<-glmer(at_least_one_success~ z_anamoly_mean_precip_Next45+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod4.success.precip<-glmer(at_least_one_success~ noanomoly_mean_precip_Prior365+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod5.success.precip<-glmer(at_least_one_success~ z_anamoly_mean_precip_Prior365+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod6.success.precip<-glmer(at_least_one_success~ mean_anamoly_mean_precip_Prior365+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#choose model with lowest Pr(>Chisq)
anova(mod0.success.precip, mod1.success.precip) #209023, 0.7115
anova(mod0.success.precip, mod2.success.precip) #209023, 0.5454
anova(mod0.success.precip, mod3.success.precip) #209017, 0.01274
anova(mod0.success.precip, mod4.success.precip)
anova(mod0.success.precip, mod5.success.precip)
anova(mod0.success.precip, mod6.success.precip)

AIC(mod1.success.precip, mod2.success.precip, mod3.success.precip,
    mod4.success.precip, mod5.success.precip, mod6.success.precip)


#nest failure precipitation models
mod0.fail.precip<-glmer(at_least_one_failure~ 1+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.failure.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.fail.precip<-glmer(at_least_one_failure~ noanomoly_mean_precip_Next45+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.failure.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.fail.precip<-glmer(at_least_one_failure~ mean_anamoly_mean_precip_Next45+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.failure.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod3.fail.precip<-glmer(at_least_one_failure~ z_anamoly_mean_precip_Next45+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.failure.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod4.fail.precip<-glmer(at_least_one_failure~ noanomoly_mean_precip_Prior365+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.failure.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod5.fail.precip<-glmer(at_least_one_failure~ z_anamoly_mean_precip_Prior365+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.failure.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod6.fail.precip<-glmer(at_least_one_failure~ mean_anamoly_mean_precip_Prior365+(1|species)+(1|year)+(1|Region/UnCoor),data=nest.failure.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))






#Choose: Anamoly (mean versus z)
#Choose: Scale of land use

#Responses: 2
#at_least_one_failure
#at_least_one_success



#######Predictors: precipitation
#noanomoly_mean_precip_Next45
#mean_anamoly_mean_precip_Next45
#z_anamoly_mean_precip_Next45

#noanomoly_mean_precip_Prior365
#z_anamoly_mean_precip_Prior365
#mean_anamoly_mean_precip_Prior365



#######Predictors: temperature
#noanomoly_max_tmax_Next45
#noanomoly_min_tmin_Next45
#mean_anamoly_max_tmax_Next45
#mean_anamoly_min_tmin_Next45
#mean_anamoly_mean_tmean_Next45
#z_anamoly_max_tmax_Next45
#z_anamoly_mean_tmin_Next45
#z_anamoly_mean_tmean_Next45
#noanomoly_max_tmax_Prior365
#noanomoly_min_tmin_Prior365
#noanomoly_mean_tmean_Prior365
#mean_anamoly_max_tmax_Prior365
#mean_anamoly_min_tmin_Prior365
#mean_anamoly_mean_tmean_Prior365
#z_anamoly_max_tmax_Prior365
#z_anamoly_mean_tmin_Prior365
#z_anamoly_mean_tmean_Prior365







#####NLCD
#NLCD_p_forest_2km
#NLCD_p_human_2km
#NLCD_p_ag_2km

#NLCD_p_forest_1km
#NLCD_p_human_1km
#NLCD_p_ag_1km

#NLCD_p_forest_500m
#NLCD_p_human_500m
#NLCD_p_ag_500m

