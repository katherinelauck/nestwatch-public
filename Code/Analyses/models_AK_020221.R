library("lme4")
library("sjPlot")
library("lubridate")
setwd("~/Google Drive/NestWatch Alison/")
load("oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet_meanNestpdTemp_gridmet.RData")
nest<-processed
nest2<-processed
include.sp<-read.csv("IncludeSpecies.csv")

nest$UnCoor<-factor(nest$UnCoor)
nest$Region<-factor(nest$Region200)
nest<-nest[nest$species %in% c(include.sp$Species),]
#NLCD categories: one that is forest vs not
#one that is human, forest, non-forest
#LU categories:agriculture, forest, open natural habitats, urban  

nest$NLCD_p_forest<-nest$NLCD_decidious_forest_2km+nest$NLCD_evergreen_forest_2km+nest$NLCD_mixed_forest_2km
nest$NLCD_p_human<-nest$NLCD_developed_open_2km+nest$NLCD_developed_low_2km+nest$NLCD_developed_high_2km+nest$NLCD_developed_medium_2km
nest$NLCD_p_ag<-nest$NLCD_cultivated_crops_2km+nest$NLCD_pasture_2km

nest$NewLU1<-NA
nest$NewLU1[nest$habitat1 %in% c("NW-ag","NW-xmas","NW-orch-vin")]<-"Ag"
nest$NewLU1[nest$habitat1 %in% c("NW-airport","NW-campus","NW-cem","NW-cmpgrd","NW-com-ind","NW-golf","NW-human","NW-park","NW-pit","NW-pwrln","NW-road","NW-ry","NW-airprt")]<-"Human"
nest$NewLU1[nest$habitat1 %in% c("NW-for")]<-"Forest"
nest$NewLU1[nest$habitat1 %in% c("NW-burn","NW-chap","NW-des","NW-grass","NW-fw","NW-sw","NW-beach","NW-clrcut")]<-"Natural_open"
#what is com-ind, tun
nest$NewLU1<-factor(nest$NewLU1)
nest$substrate<-factor(nest$substrate)
summary(nest$NewLU1)

#temp variables: tmax_degc_next45, tmin45, tave45,tmax/ave/minprior365,
#precip var: precip_mmday_next45,precip_prior365

#modify: response variable, temperature max min, precip prior and next. create another variable that's temp max minus min?
#nest$temprange<-nest$noanomoly_max_tmax_Next45-nest$noanomoly_min_tmin_Next45
#nest$temprange<-c(scale(nest$temprange))
nest$tmax_degc_Next45<-c(scale(nest$noanomoly_max_tmax_Next45))
nest$Tmax_raw<-c(scale(nest$Tmax_raw))
nest$tmin_degc_Next45<-c(scale(nest$noanomoly_min_tmin_Next45))
nest$Tmin_raw<-c(scale(nest$Tmin_raw))
nest$precip_mmday_Prior365<-c(scale(nest$noanomoly_mean_precip_Prior365))
nest$PcpBefore_raw<-c(scale(nest$PcpBefore_raw))
nest$precip_mmday_Next45<-c(scale(nest$noanomoly_mean_precip_Next45))
nest$zmaxanomalytemp<-c(scale(nest$z_anamoly_max_tmax_Next45))
nest$Tmax_anom<-c(scale(nest$Tmax_anom))
nest$zminanomalytemp<-c(scale(nest$z_anamoly_mean_tmin_Next45))
nest$Tmin_anom<-c(scale(nest$Tmin_anom))
nest$NLCD_p_ag<-c(scale(nest$NLCD_p_ag))
nest$NLCD_p_forest<-c(scale(nest$NLCD_p_forest))
nest$NLCD_p_human<-c(scale(nest$NLCD_p_human))

nest$substrate2<-0
nest$substrate2[nest$substrate=="nesbox"]<-1

bbs<-read.csv("~/Google Drive/NestWatch Alison/BBS data.csv")
codes<-read.csv("~/Google Drive/NestWatch Alison/BirdCodes.csv")
all<-unique(processed$species)

codes$codes.simp<-gsub("\\d+","",codes$eBird.species.code.2019)

code.df<-data.frame(codes.simp=codes$codes.simp,english=codes$English.name)

code.merge<-merge(nest,codes,by.x="species",by.y="eBird.species.code.2019", all.x=T, all.y=F)

#check.names<-code.merge[!duplicated(code.merge$species), ]
#chk.names<-data.frame(check.names$species,check.names$English.name)
#write.csv(chk.names,"~/Downloads/check.names.csv")

library(dplyr)

bbs$Species.Name
codes$English.name
test<-gsub(" ","",bbs$Species.Name)
test2<-gsub(" ","",codes$English.name)

cnkey<-read.csv("~/Google Drive/NestWatch Alison/sp-key-commonname.csv")
cnkey$nospace<-gsub(" ","",cnkey$ebird_name)
bbs$nospace<-gsub(" ","",bbs$Species.Name)
send<-cnkey[which(cnkey$nospace %in% bbs$nospace==F),]

code.merge$nospace<-gsub(" ","",code.merge$species)

bbs.merge<-merge(code.merge,bbs,"nospace")

bbs.merge2<-bbs.merge[bbs.merge$species!="Eurasian Collared-Dove",]

max(bbs.merge$Trend)
bbs.merge$species[which(bbs.merge$Trend>29)]
#eurasian collared dove has very high trend
bbs.merge$Trend.scaled<-scale(bbs.merge$Trend)
bbs.merge2$Trend.scaled<-scale(bbs.merge2$Trend)


#success, max temp, prior precip with substrate
mod1<-glmer(at_least_one_success~tmax_degc_Next45*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(mod1)


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

#success, temp anomalies, 365 precip with substrate
mod14a.2way.sub<-glmer(at_least_one_success~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod14a.2way.nosub<-glmer(at_least_one_success~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


mod14a.2way.sub2<-glmer(at_least_one_success~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+substrate2,data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


#success, temp anomalies, 3 way interaction w BBS, 365 precip with substrate
#take out eurasian collared dove

mod14a<-glmer(at_least_one_success~Tmax_anom*NewLU1*Trend.scaled+Tmin_anom*NewLU1*Trend.scaled+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region.x/UnCoor)+(1|substrate),data=bbs.merge,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod14a<-glmer(at_least_one_success~Tmax_anom*NewLU1*Trend.scaled+Tmin_anom*NewLU1*Trend.scaled+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region.x/UnCoor),data=bbs.merge2,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
library("sjstats")
library("MuMIn")
r2(mod14a)
r.squaredGLMM(mod14a)

r.squaredGLMM(mod14a.2way.sub2)
r.squaredGLMM(mod14a.2way.nosub)

rtmod14<-glmer(at_least_one_success~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


#mod14a<-glmer(at_least_one_success~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, temp anomalies, 45 precip
#mod15<-glmer(at_least_one_failure~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Next45*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|loc_id),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, temp anomalies, 365 precip
#mod16<-glmer(at_least_one_failure~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod15a<-glmer(at_least_one_failure~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod15<-glmer(at_least_one_failure~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


###does substrate matter? with basic model. fixed effect.
mod.s.anom.substr<-glmer(at_least_one_success~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod.f.anom.substr<-glmer(at_least_one_failure~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


###does substrate matter? with basic model. take out sp random effect
mod.s.anom.substr<-glmer(at_least_one_success~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mod.f.anom.substr<-glmer(at_least_one_failure~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

###does substrate matter? with basic model. 3 way interaction. take out min
mod.s.anom.substr.3way<-glmer(at_least_one_success~Tmax_anom*NewLU1*substrate2+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(mod.s.anom.substr.3way,"mod.s.anom.substr.3way.RDS")
##try it with species
mod.s.anom.substr.wsp.3way<-glmer(at_least_one_success~Tmax_anom*NewLU1*substrate2+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|year)+(1|Region/UnCoor)+(1|species),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


mod.f.anom.substr.3way<-glmer(at_least_one_success~Tmax_anom*NewLU1*substrate2+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#species model, narrow species to those with 100 obs or more. with and without substrate.
sp.inc<-unique(nest$species)[summary(factor(nest$species))>99]
nest3<-nest[nest$species%in%sp.inc,]

#with substrate
mod.s.anom.sp.substr<-glmer(at_least_one_success~Tmax_anom*NewLU1*species+Tmin_anom*NewLU1*species+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|year)+(1|Region/UnCoor),data=nest3,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


#basic model success, max temp anomaly, 365 precip with substrate
model_baseline.tmax<-glmer(at_least_one_success~Tmax_std*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#basic model success, min temp anomaly, 365 precip with substrate
model_baseline.tmin<-glmer(at_least_one_success~Tmin_std*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

saveRDS(model_baseline.tmax,"success~stdmax2way.nomin.AK.RDS")
saveRDS(model_baseline.tmin,"success~stdmin2way.nomax.AK.RDS")

readRDS()
#mod16a<-glmer(at_least_one_failure~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#plot_model(mod1,type = "est")
#plot_model(mod1,type = "pred") #this makes multiple plots, click the "back" arrow to see others
summary(mod.f.anom.substr)
plot_model(model_baseline,type = "int")
dput(mod.f.anom.substr,"substrate.model.failure.R")
plot_model(mod7,type = "int")
plot_model(mod15a,type = "int")
plot_model(mod16,type = "int")


saveRDS(nest,"~/Downloads/baseline_model_df.RDS")
