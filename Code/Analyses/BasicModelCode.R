library("lme4")
library("lubridate")
setwd("~/Google Drive/NestWatch Alison/")
load("oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet_meanNestpdTemp_gridmet.RData")
nest<-processed
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

#success, temp anomalies, 365 precip with substrate fixed effect
mod.success<-glmer(at_least_one_success~Tmax_anom*NewLU1+Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
