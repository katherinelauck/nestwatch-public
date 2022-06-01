#use this code to build a summary table
#columns: response (success, failure), category (random effects, precip, temp, NLCD), specific predictors, AIC, p-value from anova

rm(list=ls())


library(lme4)
library(tidyverse)
library(lubridate)

setwd("/Volumes/GoogleDrive/My Drive/NestWatchProject/Data"); getwd()

load("oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet.RData") #use new data file
nest<-processed

#LU categories:agriculture, forest, open natural habitats, urban  
nest$NewLU1<-NA
nest$NewLU1[nest$habitat1 %in% c("NW-ag","NW-xmas","NW-orch-vin")]<-"Ag"
nest$NewLU1[nest$habitat1 %in% c("NW-airport","NW-campus","NW-cem","NW-cmpgrd","NW-com-ind","NW-golf","NW-human","NW-park","NW-pit","NW-pwrln","NW-road","NW-ry")]<-"Human"
nest$NewLU1[nest$habitat1 %in% c("NW-for")]<-"Forest"
nest$NewLU1[nest$habitat1 %in% c("NW-burn","NW-chap","NW-des","NW-grass","NW-fw","NW-sw","NW-beach","NW-clrcut")]<-"Natural_open"

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
nest[,c(122:147,149:156)] <- scale(nest[,c(122:147,149:156)])

nest$UnCoor<-factor(nest$UnCoor)
nest$Region<-factor(nest$Region200)
nest$Region250<-factor(nest$Region250)
nest$year<-factor(nest$year)
nest$species<-factor(nest$species)
nest$NewLU1<-factor(nest$NewLU1); summary(nest$NewLU1)
nest$substrate<-factor(nest$substrate); summary(nest$substrate)

############### build datasets for each response and predictor category combination ################

#remove rows where response is NA
nest %>%
  filter(!is.na(at_least_one_failure)) %>%
  filter(!substrate=="")->nest.failure
nest.failure$substrate<-as.factor(as.character(nest.failure$substrate))

nest %>%
  filter(!is.na(at_least_one_success)) %>%
  filter(!substrate=="")->nest.success
nest.success$substrate<-as.factor(as.character(nest.success$substrate))


#remove rows where response or predictor is NA
nest %>%
  filter(!is.na(at_least_one_success) & !is.na(noanomoly_mean_precip_Next45))->nest.success.precip
nest %>%
  filter(!is.na(at_least_one_failure) & !is.na(noanomoly_mean_precip_Next45))->nest.fail.precip

nest %>%
  filter(!is.na(at_least_one_success) & !is.na(noanomoly_max_tmax_Next45))->nest.success.temp
nest %>%
  filter(!is.na(at_least_one_failure) & !is.na(noanomoly_max_tmax_Next45))->nest.fail.temp

nest %>%
  filter(!is.na(at_least_one_success) & !is.na(NLCD_p_forest_500m))->nest.success.NLCD
nest %>%
  filter(!is.na(at_least_one_failure) & !is.na(NLCD_p_forest_500m))->nest.fail.NLCD




################### compare random effect structure for at leat one success ######################

mod0.success.RE<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|substrate)+(1|UnCoor),data=nest.success,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.success.RE<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.success.RE<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|substrate)+(1|Region250/UnCoor),data=nest.success,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

anova(mod0.success.RE, mod1.success.RE)
anova(mod0.success.RE, mod2.success.RE)
AIC(mod0.success.RE, mod1.success.RE, mod2.success.RE)


################### compare random effect structure for at leat one failure ######################

mod0.fail.RE<-glmer(at_least_one_failure~ 1+(1|species)+(1|year)+(1|substrate)+(1|UnCoor),data=nest.failure,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.fail.RE<-glmer(at_least_one_failure~ 1+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.failure,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.fail.RE<-glmer(at_least_one_failure~ 1+(1|species)+(1|year)+(1|substrate)+(1|Region250/UnCoor),data=nest.failure,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#extract Pr(>Chisq) value
anova(mod0.fail.RE, mod1.fail.RE)
anova(mod0.fail.RE, mod2.fail.RE)
#extract AIC scores
AIC(mod0.fail.RE, mod1.fail.RE, mod2.fail.RE)


################### compare nest success precipitation models ######################

mod0.success.precip<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.success.precip<-glmer(at_least_one_success~ noanomoly_mean_precip_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.success.precip<-glmer(at_least_one_success~ mean_anamoly_mean_precip_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod3.success.precip<-glmer(at_least_one_success~ z_anamoly_mean_precip_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod4.success.precip<-glmer(at_least_one_success~ noanomoly_mean_precip_Prior365+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod5.success.precip<-glmer(at_least_one_success~ z_anamoly_mean_precip_Prior365+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod6.success.precip<-glmer(at_least_one_success~ mean_anamoly_mean_precip_Prior365+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#extract Pr(>Chisq) value
anova(mod0.success.precip, mod1.success.precip)
anova(mod0.success.precip, mod2.success.precip)
anova(mod0.success.precip, mod3.success.precip)
anova(mod0.success.precip, mod4.success.precip)
anova(mod0.success.precip, mod5.success.precip)
anova(mod0.success.precip, mod6.success.precip)
#extract AIC scores
AIC(mod1.success.precip, mod2.success.precip, mod3.success.precip,
    mod4.success.precip, mod5.success.precip, mod6.success.precip)


################### compare nest failure precipitation models ######################

mod0.fail.precip<-glmer(at_least_one_failure~ 1+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.fail.precip<-glmer(at_least_one_failure~ noanomoly_mean_precip_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.fail.precip<-glmer(at_least_one_failure~ mean_anamoly_mean_precip_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod3.fail.precip<-glmer(at_least_one_failure~ z_anamoly_mean_precip_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod4.fail.precip<-glmer(at_least_one_failure~ noanomoly_mean_precip_Prior365+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod5.fail.precip<-glmer(at_least_one_failure~ z_anamoly_mean_precip_Prior365+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod6.fail.precip<-glmer(at_least_one_failure~ mean_anamoly_mean_precip_Prior365+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.precip,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#extract Pr(>Chisq) value
anova(mod0.fail.precip, mod1.fail.precip)
anova(mod0.fail.precip, mod2.fail.precip)
anova(mod0.fail.precip, mod3.fail.precip)
anova(mod0.fail.precip, mod4.fail.precip)
anova(mod0.fail.precip, mod5.fail.precip)
anova(mod0.fail.precip, mod6.fail.precip)
#extract AIC scores
AIC(mod1.fail.precip, mod2.fail.precip, mod3.fail.precip,
    mod4.fail.precip, mod5.fail.precip, mod6.fail.precip)




################### compare nest success temperature models ######################

mod0.success.temp<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.success.temp<-glmer(at_least_one_success~ noanomoly_mean_tmean_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.success.temp<-glmer(at_least_one_success~ noanomoly_max_tmax_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod3.success.temp<-glmer(at_least_one_success~ noanomoly_min_tmin_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod4.success.temp<-glmer(at_least_one_success~ mean_anamoly_max_tmax_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod5.success.temp<-glmer(at_least_one_success~ mean_anamoly_min_tmin_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod6.success.temp<-glmer(at_least_one_success~ mean_anamoly_mean_tmean_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod7.success.temp<-glmer(at_least_one_success~ z_anamoly_max_tmax_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod8.success.temp<-glmer(at_least_one_success~ z_anamoly_mean_tmin_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod9.success.temp<-glmer(at_least_one_success~ z_anamoly_mean_tmean_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#extract Pr(>Chisq) value
anova(mod0.success.temp, mod1.success.temp)
anova(mod0.success.temp, mod2.success.temp)
anova(mod0.success.temp, mod3.success.temp)
anova(mod0.success.temp, mod4.success.temp)
anova(mod0.success.temp, mod5.success.temp)
anova(mod0.success.temp, mod6.success.temp)
#extract AIC scores
AIC(mod1.success.temp, mod2.success.temp, mod3.success.temp,
    mod4.success.temp, mod5.success.temp, mod6.success.temp)


################### compare nest failure temperature models ######################

mod0.fail.temp<-glmer(at_least_one_failure~ 1+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.fail.temp<-glmer(at_least_one_failure~ noanomoly_mean_tmean_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.fail.temp<-glmer(at_least_one_failure~ noanomoly_max_tmax_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod3.fail.temp<-glmer(at_least_one_failure~ noanomoly_min_tmin_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod4.fail.temp<-glmer(at_least_one_failure~ mean_anamoly_max_tmax_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod5.fail.temp<-glmer(at_least_one_failure~ mean_anamoly_min_tmin_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod6.fail.temp<-glmer(at_least_one_failure~ mean_anamoly_mean_tmean_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod7.fail.temp<-glmer(at_least_one_failure~ z_anamoly_max_tmax_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod8.fail.temp<-glmer(at_least_one_failure~ z_anamoly_mean_tmin_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod9.fail.temp<-glmer(at_least_one_failure~ z_anamoly_mean_tmean_Next45+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.fail.temp,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#extract Pr(>Chisq) value
anova(mod0.fail.temp, mod1.fail.temp)
anova(mod0.fail.temp, mod2.fail.temp)
anova(mod0.fail.temp, mod3.fail.temp)
anova(mod0.fail.temp, mod4.fail.temp)
anova(mod0.fail.temp, mod5.fail.temp)
anova(mod0.fail.temp, mod6.fail.temp)
#extract AIC scores
AIC(mod1.fail.temp, mod2.fail.temp, mod3.fail.temp,
    mod4.fail.temp, mod5.fail.temp, mod6.fail.temp)










################### compare nest success NLCD models ######################

mod0.success.NLCD<-glmer(at_least_one_success~ 1+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.NLCD,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod1.success.NLCD<-glmer(at_least_one_success~ NLCD_p_human_2km+NLCD_p_forest_2km+NLCD_p_ag_2km+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.NLCD,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod2.success.NLCD<-glmer(at_least_one_success~ NLCD_p_human_1km+NLCD_p_forest_1km+NLCD_p_ag_1km+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.NLCD,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod3.success.NLCD<-glmer(at_least_one_success~ NLCD_p_human_500m+NLCD_p_forest_500m+NLCD_p_ag_500m+(1|species)+(1|year)+(1|substrate)+(1|Region/UnCoor),data=nest.success.NLCD,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#extract Pr(>Chisq) value
anova(mod0.success.NLCD, mod1.success.NLCD)
anova(mod0.success.NLCD, mod2.success.NLCD)
anova(mod0.success.NLCD, mod3.success.NLCD)
#extract AIC scores
AIC(mod1.success.NLCD, mod2.success.NLCD, mod3.success.NLCD)





#Choose: Anamoly (mean versus z)
#Choose: Scale of land use

#Responses: 2
#at_least_one_failure
#at_least_one_success



#######Predictors: precipitation
#noanomoly_mean_precip_Next45 #optional
#mean_anamoly_mean_precip_Next45 #optional
#z_anamoly_mean_precip_Next45 #optional
#noanomoly_mean_precip_Prior365
#z_anamoly_mean_precip_Prior365
#mean_anamoly_mean_precip_Prior365



#######Predictors: temperature
#noanomoly_mean_tmean_Next45
#noanomoly_max_tmax_Next45
#noanomoly_min_tmin_Next45

#mean_anamoly_max_tmax_Next45
#mean_anamoly_min_tmin_Next45
#mean_anamoly_mean_tmean_Next45

#z_anamoly_max_tmax_Next45
#z_anamoly_mean_tmin_Next45
#z_anamoly_mean_tmean_Next45


#####NLCD- choose scale
#NLCD_p_forest_2km+NLCD_p_human_2km+NLCD_p_ag_2km
#NLCD_p_forest_1km+NLCD_p_human_1km+NLCD_p_ag_1km
#NLCD_p_forest_500m+NLCD_p_human_500m+NLCD_p_ag_500m

