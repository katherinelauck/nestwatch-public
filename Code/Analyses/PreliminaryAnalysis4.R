library("lme4")
library("sjPlot")
library("lubridate")
library(ggplot2)
library(tidyverse)
library(GGally)
library(afex)

load("Data/oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet_meanNestpdTemp_gridmet.RData")
nest <- processed


nest$UnCoor<-factor(nest$UnCoor)
nest$Region<-factor(nest$Region200)
#NLCD categories: one that is forest vs not
#one that is human, forest, non-forest
#LU categories:agriculture, forest, open natural habitats, urban  

nest$NLCD_p_forest<-nest$NLCD_decidious_forest_2km+nest$NLCD_evergreen_forest_2km+nest$NLCD_mixed_forest_2km
nest$NLCD_p_human<-nest$NLCD_developed_open_2km+nest$NLCD_developed_low_2km+nest$NLCD_developed_high_2km+nest$NLCD_developed_medium_2km
nest$NLCD_p_ag<-nest$NLCD_cultivated_crops_2km+nest$NLCD_pasture_2km

nest$NewLU1<-NA
nest$NewLU1[nest$habitat1 %in% c("NW-ag","NW-xmas","NW-orch-vin")]<-"Ag"
nest$NewLU1[nest$habitat1 %in% c("NW-airport","NW-campus","NW-cem","NW-cmpgrd","NW-com-ind","NW-golf","NW-human","NW-park","NW-pit","NW-pwrln","NW-road","NW-ry")]<-"Human"
nest$NewLU1[nest$habitat1 %in% c("NW-for")]<-"Forest"
nest$NewLU1[nest$habitat1 %in% c("NW-burn","NW-chap","NW-des","NW-grass","NW-fw","NW-sw","NW-beach","NW-clrcut")]<-"Natural_open"
#what is com-ind, tun
nest$NewLU1<-factor(nest$NewLU1) # relevel factor
nest$substrate<-factor(nest$substrate)
summary(nest$NewLU1)

#temp variables: tmax_degc_next45, tmin45, tave45,tmax/ave/minprior365,
#precip var: precip_mmday_next45,precip_prior365

#modify: response variable, temperature max min, precip prior and next. create another variable that's temp max minus min?
nest$temprange<-nest$noanomoly_max_tmax_Next45-nest$noanomoly_min_tmin_Next45
nest$avetemp <- c(scale(nest$WorldClim_AveAnnualTemp))
nest$temprange<-c(scale(nest$temprange))
nest$tmax_degc_Next45<-c(scale(nest$noanomoly_max_tmax_Next45))
nest$tmin_degc_Next45<-c(scale(nest$noanomoly_min_tmin_Next45))
nest$Tmax_std_gridmet <- c(scale(nest$Tmax_std))
nest$Tmin_std_gridmet <- c(scale(nest$Tmin_std))
nest$pcpbefore_raw_gridmet <- c(scale(nest$PcpBefore_raw))
nest$pcpafter_raw_gridmet <- c(scale(nest$Pcp45dAfter_raw))
nest$pcpbefore_std_gridmet <- c(scale(nest$PcpBefore_std))
nest$pcpafter_std_gridmet <- c(scale(nest$Pcp45dAfter_std))
nest$tmean_nestpd <- c(scale(nest$mean_nestpd_temp))
nest$tnestpd_zanamoly_sp <- c(scale(nest$z_nestpd_temp))
nest$tnestpd_meanmax_gridmet <- c(scale(((nest$Tmeanmax/10)-32)*(5/9)))
nest$tnestpd_stdmaxsp_gridmet <- c(scale(nest$Tstdmax_sp))
nest$tnestpd_meanmax_gridmet_exp <- c(scale(exp(((nest$Tmeanmax/10)-32)*(5/9))))
nest$tnestpd_stdmaxsp_gridmet_exp <- c(scale(exp(nest$Tstdmax_sp)))
nest$precip_mmday_Prior365<-c(scale(nest$noanomoly_mean_precip_Prior365))
nest$precip_mmday_Next45<-c(scale(nest$noanomoly_mean_precip_Next45))
nest$zmaxanomalytemp<-c(scale(nest$z_anamoly_max_tmax_Next45))
nest$zminanomalytemp<-c(scale(nest$z_anamoly_mean_tmin_Next45))
nest$NLCD_p_ag<-c(scale(nest$NLCD_p_ag))
nest$NLCD_p_forest<-c(scale(nest$NLCD_p_forest))
nest$NLCD_p_human<-c(scale(nest$NLCD_p_human))

ggsave("figures/model-selection-pairs.png",plot = ggpairs(nest[,c("avetemp",'tmean_nestpd','tnestpd_zanamoly_sp','tnestpd_meanmax_gridmet','tnestpd_stdmaxsp_gridmet')]),width = 6,height = 4,dpi = 'retina')

#success, max temp, prior precip
mod1<-glmer(at_least_one_success~tmax_degc_Next45*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(mod1)

mod1a<-glmer(at_least_one_success~tmax_degc_Next45*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(mod1)

#success, min temp, prior precip
#mod2<-glmer(at_least_one_success~tmin_degc_Next45*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#success, temp range, prior precip
#mod3<-glmer(at_least_one_success~temprange*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#success, max temp, 45precip
#mod4<-glmer(at_least_one_success~tmax_degc_Next45*NewLU1+precip_mmday_Next45*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#success, min temp, 45precip
#mod5<-glmer(at_least_one_success~tmin_degc_Next45*NewLU1+precip_mmday_Next45*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#success, temp range, 45precip
#mod6<-glmer(at_least_one_success~temprange*NewLU1+precip_mmday_Next45*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, max temp, prior precip
mod7<-glmer(at_least_one_failure~tmax_degc_Next45*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod7a<-glmer(at_least_one_failure~tmax_degc_Next45*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, min temp, prior precip
#mod8<-glmer(at_least_one_failure~tmin_degc_Next45*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, temp range, prior precip
#mod9<-glmer(at_least_one_failure~temprange*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, max temp, 45precip
#mod10<-glmer(at_least_one_failure~tmax_degc_Next45*NewLU1+precip_mmday_Next45*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, min temp, 45precip
#mod11<-glmer(at_least_one_failure~tmin_degc_Next45*NewLU1+precip_mmday_Next45*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, temp range, 45precip
#mod12<-glmer(at_least_one_failure~temprange*NewLU1+precip_mmday_Next45*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#success, temp anomalies, 45 precip
#mod13<-glmer(at_least_one_success~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Next45*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#success, temp anomalies, 365 precip
mod14<-glmer(at_least_one_success~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod14a<-glmer(at_least_one_success~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, temp anomalies, 45 precip
#mod15<-glmer(at_least_one_failure~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Next45*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, temp anomalies, 365 precip
mod16<-glmer(at_least_one_failure~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod16a<-glmer(at_least_one_failure~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#success, temp anomalies, 365 precip, regional temp * max anomaly * NewLU1
mod17<-glmer(at_least_one_success~zmaxanomalytemp*avetemp*NewLU1 + zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

write_rds(mod17,"results/success~annualmeantemp3way.rds")

#success, temp anomalies, 365 precip, site avg temp * max anomaly * NewLU1
mod18<-glmer(at_least_one_success~zmaxanomalytemp*tmean_nestpd*NewLU1 + zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

write_rds(mod18,"results/success~meanmax3way_daymet.rds")

#success, temp anomalies, 365 precip, site z anamoly per sp * max anomaly * NewLU1
mod19<-glmer(at_least_one_success~zmaxanomalytemp*tnestpd_zanamoly_sp*NewLU1 + zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

write_rds(mod19,"results/success~stdmax3way_daymet.rds")

#success, temp anomalies, 365 precip, site mean max temp gridmet * max anomaly * NewLU1
mod20<-glmer(at_least_one_success~Tmax_std_gridmet*tnestpd_meanmax_gridmet*NewLU1 + Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=2e7)))

#refit20 <- all_fit(mod20)

write_rds(mod20,"results/success~meanmax3way_gridmet.rds")

#success, temp anomalies, 365 precip, site std max temp gridmet * max anomaly * NewLU1
mod21<-glmer(at_least_one_success~Tmax_std_gridmet*tnestpd_stdmaxsp_gridmet*NewLU1 + Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=2e7)))

#refit21 <- all_fit(mod21)

write_rds(mod21,"results/success~stdmax3way_gridmet.rds")

#success, temp anomalies, 365 precip, exp(site mean max temp gridmet) * max anomaly * NewLU1
mod22<-glmer(at_least_one_success~Tmax_std_gridmet*tnestpd_meanmax_gridmet_exp*NewLU1 + Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=2e7)))

#refit22 <- all_fit(mod22)

write_rds(mod22,"results/success~meanmax3way_gridmet_exp.rds")

#success, temp anomalies, 365 precip, exp(site std max temp gridmet) * max anomaly * NewLU1
mod23<-glmer(at_least_one_success~Tmax_std_gridmet*tnestpd_stdmaxsp_gridmet_exp*NewLU1 + Tmin_std_gridmet*NewLU1+pcpbefore_raw_gridmet+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer=c("bobyqa"),optCtrl=list(maxfun=2e7)))

#refit23 <- all_fit(mod23)

write_rds(mod23,"results/success~stdmax3way_gridmet_exp.rds")


# Convergence exploration: gridmet vs daymet. Hypothesis: gridmet meanmax temp unit is F*10

hist(nest$Tmeanmax)
hist(((nest$Tmeanmax/10)-32)*(5/9))
hist(nest$mean_nestpd_temp)

#plot_model(mod1,type = "est")
#plot_model(mod1,type = "pred") #this makes multiple plots, click the "back" arrow to see others



summary(mod17)
summary(mod19)
plot_model(mod1,type = "int")
plot_model(mod7,type = "int")
plot_model(mod16,type = "int")
plot_model(mod17,type = "int")
plot_model(mod18,type = "int")
plot_model(mod19,type = "int")
plot_model(mod20,type = "int")
plot_model(mod21,type = "int")

