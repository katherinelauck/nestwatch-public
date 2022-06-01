rm(list=ls())

library(tidyverse)
library(lme4)
library(boot)
library(sjPlot)
library(cowplot)
library(ggpubr)
library(reshape)
library("scales")
library("ggbeeswarm")

##Set the plot theme
my.theme = theme(
  title= element_text(size = 14),
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.line = element_line(size = 0.5, colour = "black"),
  panel.background = element_rect(fill = "white"),
  plot.margin = margin(4,4,4,4))


setwd('/Users/DrBohemio/Documents/Git/nestwatch'); getwd()

success_projections <- read.csv("Data/active/success_projections.csv")
str(success_projections)

coor <- data.frame(UnCoor = success_projections$UnCoor)
coor.unique <- unique(coor)
coor.sep <- coor.unique %>% separate(UnCoor, c("lon","lat"), sep = "(_)",remove = F)

success_projections <- left_join(success_projections, coor.sep,by ="UnCoor")
success_projections$lat <- as.numeric(success_projections$lat)
success_projections$lon <- as.numeric(success_projections$lon)


success_projections$Tmax_std_gridmet_sq <- success_projections$Tmax_std_gridmet* success_projections$Tmax_std_gridmet

m1 <- readRDS('results/spatial/success~tnestpd_meanmax_gridmet_tmax.rds')
#need to get updated model from Alison where temperature anomalies are used
#once the temperature z-scores are used, those do not need to be transformed or scaled
#precipitation is raw, so Alison scales precip, I use those values to transform raw data from climate success_projections
#from climate success_projections, use temperature standardized z-scores and raw precipitation data

#mod.success.Tmax <- glmer(at_least_one_success~Tmax_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#mod.success.Tmin <- glmer(at_least_one_success~Tmin_anom*NewLU1+PcpBefore_raw+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+substrate2+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#2 climate scenarios * 2 time periods * 3 parts of distribution = 12 success_projections



###Getting models parameters
fixef <- as.data.frame(fixef(m1))

intercept <- fixef[1,1]

Tmax_std_gridmet <- fixef[2,1]

Forest <- 0
Ag <- fixef[3,1]
Natural_open <- fixef[4,1]
Human <- fixef[5,1]

Tmax_std_gridmet_sq <- fixef[6,1]

tnestpd_meanmax_gridmet <- fixef[7,1]

PcpBefore_raw_gridmet <- fixef[8,1]

NLCD_p_forest <- fixef[9,1]
NLCD_p_human <- fixef[10,1]
NLCD_p_ag <- fixef[11,1]

substrate <- fixef[12,1]

laydate_scaled <- fixef[13,1]

int.Tmax.forest <- 1
int.Tmax.ag<- fixef[14,1]
int.Tmax.natural.open <- fixef[15,1]
int.Tmax.human <- fixef[16,1]

int.Tmax_sq.forest <- 1
int.Tmax_sq.ag<- fixef[17,1]
int.Tmax_sq.natural.open <- fixef[18,1]
int.Tmax_sq.human <- fixef[19,1]

int.Tmax.tnestpd_meanmax <- fixef[20,1]

##For better predict I incorporate the estimated values for the land uses and the interations of them with temperature
success_projections$LU <- 0

for (i in 1:nrow(success_projections)){
  if (is.na(success_projections$NewLU1[i]) == T) {success_projections$LU[i] <- NA 
  
  } else if (success_projections$NewLU1[i] == "Forest") {success_projections$LU[i] <- 0 
  } else if (success_projections$NewLU1[i]== "Ag") {success_projections$LU[i] <- fixef[3,1]
  } else if (success_projections$NewLU1[i]== "Natural_open") {success_projections$LU[i] <- fixef[4,1]
  } else {success_projections$LU[i] <- fixef[5,1]}
}

success_projections$int <- 0

for (i in 1:nrow(success_projections)){
  if (is.na(success_projections$NewLU1[i]) == T) {success_projections$int[i] <- NA 
  
  } else if (success_projections$NewLU1[i] == "Forest") {success_projections$int[i] <- 0 
  } else if (success_projections$NewLU1[i]== "Ag") {success_projections$int[i] <- fixef[14,1]
  } else if (success_projections$NewLU1[i]== "Natural_open") {success_projections$int[i] <- fixef[15,1]
  } else {success_projections$int[i] <- fixef[16,1]}
}


success_projections$int_sq <- 0

for (i in 1:nrow(success_projections)){
  if (is.na(success_projections$NewLU1[i]) == T) {success_projections$int_sq[i] <- NA 
  
  } else if (success_projections$NewLU1[i] == "Forest") {success_projections$int_sq[i] <- 0 
  } else if (success_projections$NewLU1[i]== "Ag") {success_projections$int_sq[i] <- fixef[17,1]
  } else if (success_projections$NewLU1[i]== "Natural_open") {success_projections$int_sq[i] <- fixef[18,1]
  } else {success_projections$int_sq[i] <- fixef[19,1]}
}



#####How Tmax and Precipitation affect succes----

plot_model(m1,type="pred",terms = "pcpbefore_raw_gridmet[all]")+my.theme+
  ggtitle("Effect of precipitation on success") + xlab("Precipitation") + ylab("Success")

plot_model(m1,type="pred",terms = "Tmax_std_gridmet[all]")+my.theme+
  ggtitle("Effect of temperature on success") + xlab("Max temperature") + ylab("Success")


####Projections now
success_projections$y.now <- 0
success_projections$y.now <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet_sq + 
                                                     tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                     PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + success_projections$laydate_scaled * laydate_scaled +
                                                     NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                     substrate*success_projections$substrate_binary +
                                                     success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet +
                                                     success_projections$int_sq*success_projections$Tmax_std_gridmet_sq+
                                                     int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet*success_projections$tnestpd_meanmax_gridmet)


####GFDL
#RCP 45e mean gfdl-----------------------------------------------------------
###Create success_projections
success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean_sq <- success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean * success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean


success_projections$y.fut.rcp45e.gfdl.mean <- 0
success_projections$y.fut.rcp45e.gfdl.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean_sq + 
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.gfdl.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45e.gfdl.mean <- success_projections$y.fut.rcp45e.gfdl.mean - success_projections$y.now

###10%
###Create success_projections
success_projections$Tmax_std_gridmet.rcp45e.gfdl.10_sq <- success_projections$Tmax_std_gridmet.rcp45e.gfdl.10 * success_projections$Tmax_std_gridmet.rcp45e.gfdl.10


success_projections$y.fut.rcp45e.gfdl.10 <- 0
success_projections$y.fut.rcp45e.gfdl.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.gfdl.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.gfdl.10_sq +
                                                         tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                         PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.gfdl.10  + success_projections$laydate_scaled * laydate_scaled +
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.gfdl.10 +
                                                         success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.gfdl.10_sq+
                                                         int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.gfdl.10*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45e.gfdl.10 <- success_projections$y.fut.rcp45e.gfdl.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45e.gfdl.90_sq <- success_projections$Tmax_std_gridmet.rcp45e.gfdl.90 * success_projections$Tmax_std_gridmet.rcp45e.gfdl.90


success_projections$y.fut.rcp45e.gfdl.90 <- 0
success_projections$y.fut.rcp45e.gfdl.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.gfdl.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.gfdl.90_sq + tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                         PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.gfdl.90  + success_projections$laydate_scaled * laydate_scaled +
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.gfdl.90 +
                                                         success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.gfdl.90_sq +
                                                         int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.gfdl.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45e.gfdl.90 <- success_projections$y.fut.rcp45e.gfdl.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean_sq <- success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean * success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean

success_projections$y.fut.rcp45m.gfdl.mean <- 0
success_projections$y.fut.rcp45m.gfdl.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean_sq + 
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.gfdl.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean_sq +
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp45m.gfdl.mean <- success_projections$y.fut.rcp45m.gfdl.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp45m.gfdl.10_sq <- success_projections$Tmax_std_gridmet.rcp45m.gfdl.10 * success_projections$Tmax_std_gridmet.rcp45m.gfdl.10


success_projections$y.fut.rcp45m.gfdl.10 <- 0
success_projections$y.fut.rcp45m.gfdl.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.gfdl.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.gfdl.10_sq +
                                                         tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                         PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.gfdl.10  + success_projections$laydate_scaled * laydate_scaled +
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.gfdl.10 +
                                                         success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.gfdl.10_sq+
                                                         int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.gfdl.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp45m.gfdl.10 <- success_projections$y.fut.rcp45m.gfdl.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45m.gfdl.90_sq <- success_projections$Tmax_std_gridmet.rcp45m.gfdl.90 * success_projections$Tmax_std_gridmet.rcp45m.gfdl.90


success_projections$y.fut.rcp45m.gfdl.90 <- 0
success_projections$y.fut.rcp45m.gfdl.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.gfdl.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.gfdl.90_sq +
                                                         tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                         PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.gfdl.90  + success_projections$laydate_scaled * laydate_scaled +
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.gfdl.90 +
                                                         success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.gfdl.90_sq+
                                                         int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.gfdl.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45m.gfdl.90 <- success_projections$y.fut.rcp45m.gfdl.90 - success_projections$y.now


#RCP 85 gfdl----
##End of the century

##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean_sq <- success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean * success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean

success_projections$y.fut.rcp85e.gfdl.mean <- 0
success_projections$y.fut.rcp85e.gfdl.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.gfdl.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85e.gfdl.mean <- success_projections$y.fut.rcp85e.gfdl.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85e.gfdl.10_sq <- success_projections$Tmax_std_gridmet.rcp85e.gfdl.10 * success_projections$Tmax_std_gridmet.rcp85e.gfdl.10


success_projections$y.fut.rcp85e.gfdl.10 <- 0
success_projections$y.fut.rcp85e.gfdl.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.gfdl.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.gfdl.10_sq +
                                                         tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                         PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.gfdl.10  + success_projections$laydate_scaled * laydate_scaled +
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.gfdl.10 +
                                                         success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.gfdl.10_sq +
                                                         int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.gfdl.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85e.gfdl.10 <- success_projections$y.fut.rcp85e.gfdl.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85e.gfdl.90_sq <- success_projections$Tmax_std_gridmet.rcp85e.gfdl.90 * success_projections$Tmax_std_gridmet.rcp85e.gfdl.90


success_projections$y.fut.rcp85e.gfdl.90 <- 0
success_projections$y.fut.rcp85e.gfdl.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.gfdl.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.gfdl.90_sq +
                                                         tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                         PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.gfdl.90  + success_projections$laydate_scaled * laydate_scaled +
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.gfdl.90 +
                                                         success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.gfdl.90_sq+
                                                         int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.gfdl.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85e.gfdl.90 <- success_projections$y.fut.rcp85e.gfdl.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean_sq <- success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean * success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean

success_projections$y.fut.rcp85m.gfdl.mean <- 0
success_projections$y.fut.rcp85m.gfdl.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.gfdl.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean_sq +
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85m.gfdl.mean <- success_projections$y.fut.rcp85m.gfdl.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85m.gfdl.10_sq <- success_projections$Tmax_std_gridmet.rcp85m.gfdl.10 * success_projections$Tmax_std_gridmet.rcp85m.gfdl.10


success_projections$y.fut.rcp85m.gfdl.10 <- 0
success_projections$y.fut.rcp85m.gfdl.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.gfdl.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.gfdl.10_sq + 
                                                         tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                         PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.gfdl.10  + success_projections$laydate_scaled * laydate_scaled +
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.gfdl.10 +
                                                         success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.gfdl.10_sq+
                                                         int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.gfdl.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85m.gfdl.10 <- success_projections$y.fut.rcp85m.gfdl.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85m.gfdl.90_sq <- success_projections$Tmax_std_gridmet.rcp85m.gfdl.90 * success_projections$Tmax_std_gridmet.rcp85m.gfdl.90


success_projections$y.fut.rcp85m.gfdl.90 <- 0
success_projections$y.fut.rcp85m.gfdl.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.gfdl.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.gfdl.90_sq + 
                                                         tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                         PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.gfdl.90  + success_projections$laydate_scaled * laydate_scaled +
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.gfdl.90 +
                                                         success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.gfdl.90_sq+
                                                         int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.gfdl.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85m.gfdl.90 <- success_projections$y.fut.rcp85m.gfdl.90 - success_projections$y.now



####canesm
#RCP 45 canesm----
##End of the century

##Projection for the mean
success_projections$Tmax_std_gridmet.rcp45e.canesm.mean_sq <- success_projections$Tmax_std_gridmet.rcp45e.canesm.mean * success_projections$Tmax_std_gridmet.rcp45e.canesm.mean

success_projections$y.fut.rcp45e.canesm.mean <- 0
success_projections$y.fut.rcp45e.canesm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.canesm.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.canesm.mean_sq +
                                                             tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                             PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.canesm.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.canesm.mean +
                                                             success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.canesm.mean_sq +
                                                             int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.canesm.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp45e.canesm.mean <- success_projections$y.fut.rcp45e.canesm.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp45e.canesm.10_sq <- success_projections$Tmax_std_gridmet.rcp45e.canesm.10 * success_projections$Tmax_std_gridmet.rcp45e.canesm.10


success_projections$y.fut.rcp45e.canesm.10 <- 0
success_projections$y.fut.rcp45e.canesm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.canesm.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.canesm.10_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.canesm.10  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.canesm.10 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.canesm.10_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.canesm.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp45e.canesm.10 <- success_projections$y.fut.rcp45e.canesm.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45e.canesm.90_sq <- success_projections$Tmax_std_gridmet.rcp45e.canesm.90 * success_projections$Tmax_std_gridmet.rcp45e.canesm.90


success_projections$y.fut.rcp45e.canesm.90 <- 0
success_projections$y.fut.rcp45e.canesm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.canesm.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.canesm.90_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.canesm.90  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.canesm.90 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.canesm.90_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.canesm.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45e.canesm.90 <- success_projections$y.fut.rcp45e.canesm.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp45m.canesm.mean_sq <- success_projections$Tmax_std_gridmet.rcp45m.canesm.mean * success_projections$Tmax_std_gridmet.rcp45m.canesm.mean

success_projections$y.fut.rcp45m.canesm.mean <- 0
success_projections$y.fut.rcp45m.canesm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.canesm.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.canesm.mean_sq + 
                                                             tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                             PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.canesm.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.canesm.mean +
                                                             success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.canesm.mean_sq +
                                                             int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.canesm.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp45m.canesm.mean <- success_projections$y.fut.rcp45m.canesm.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp45m.canesm.10_sq <- success_projections$Tmax_std_gridmet.rcp45m.canesm.10 * success_projections$Tmax_std_gridmet.rcp45m.canesm.10


success_projections$y.fut.rcp45m.canesm.10 <- 0
success_projections$y.fut.rcp45m.canesm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.canesm.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.canesm.10_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.canesm.10  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.canesm.10 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.canesm.10_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.canesm.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp45m.canesm.10 <- success_projections$y.fut.rcp45m.canesm.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45m.canesm.90_sq <- success_projections$Tmax_std_gridmet.rcp45m.canesm.90 * success_projections$Tmax_std_gridmet.rcp45m.canesm.90


success_projections$y.fut.rcp45m.canesm.90 <- 0
success_projections$y.fut.rcp45m.canesm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.canesm.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.canesm.90_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.canesm.90  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.canesm.90 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.canesm.90_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.canesm.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45m.canesm.90 <- success_projections$y.fut.rcp45m.canesm.90 - success_projections$y.now


#RCP 85 canesm----
##End of the century

##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85e.canesm.mean_sq <- success_projections$Tmax_std_gridmet.rcp85e.canesm.mean * success_projections$Tmax_std_gridmet.rcp85e.canesm.mean

success_projections$y.fut.rcp85e.canesm.mean <- 0
success_projections$y.fut.rcp85e.canesm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.canesm.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.canesm.mean_sq +
                                                             tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                             PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.canesm.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.canesm.mean +
                                                             success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.canesm.mean_sq +
                                                             int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.canesm.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85e.canesm.mean <- success_projections$y.fut.rcp85e.canesm.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85e.canesm.10_sq <- success_projections$Tmax_std_gridmet.rcp85e.canesm.10 * success_projections$Tmax_std_gridmet.rcp85e.canesm.10


success_projections$y.fut.rcp85e.canesm.10 <- 0
success_projections$y.fut.rcp85e.canesm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.canesm.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.canesm.10_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.canesm.10  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.canesm.10 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.canesm.10_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.canesm.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85e.canesm.10 <- success_projections$y.fut.rcp85e.canesm.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85e.canesm.90_sq <- success_projections$Tmax_std_gridmet.rcp85e.canesm.90 * success_projections$Tmax_std_gridmet.rcp85e.canesm.90


success_projections$y.fut.rcp85e.canesm.90 <- 0
success_projections$y.fut.rcp85e.canesm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.canesm.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.canesm.90_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.canesm.90  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.canesm.90 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.canesm.90_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.canesm.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85e.canesm.90 <- success_projections$y.fut.rcp85e.canesm.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85m.canesm.mean_sq <- success_projections$Tmax_std_gridmet.rcp85m.canesm.mean * success_projections$Tmax_std_gridmet.rcp85m.canesm.mean

success_projections$y.fut.rcp85m.canesm.mean <- 0
success_projections$y.fut.rcp85m.canesm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.canesm.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.canesm.mean_sq +
                                                             tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                             PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.canesm.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.canesm.mean +
                                                             success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.canesm.mean_sq +
                                                             int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.canesm.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85m.canesm.mean <- success_projections$y.fut.rcp85m.canesm.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85m.canesm.10_sq <- success_projections$Tmax_std_gridmet.rcp85m.canesm.10 * success_projections$Tmax_std_gridmet.rcp85m.canesm.10


success_projections$y.fut.rcp85m.canesm.10 <- 0
success_projections$y.fut.rcp85m.canesm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.canesm.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.canesm.10_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.canesm.10  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.canesm.10 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.canesm.10_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.canesm.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85m.canesm.10 <- success_projections$y.fut.rcp85m.canesm.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85m.canesm.90_sq <- success_projections$Tmax_std_gridmet.rcp85m.canesm.90 * success_projections$Tmax_std_gridmet.rcp85m.canesm.90


success_projections$y.fut.rcp85m.canesm.90 <- 0
success_projections$y.fut.rcp85m.canesm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.canesm.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.canesm.90_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.canesm.90  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.canesm.90 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.canesm.90_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.canesm.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85m.canesm.90 <- success_projections$y.fut.rcp85m.canesm.90 - success_projections$y.now






















####mri
#RCP 45 mri----
##End of the century

##Projection for the mean
success_projections$Tmax_std_gridmet.rcp45e.mri.mean_sq <- success_projections$Tmax_std_gridmet.rcp45e.mri.mean * success_projections$Tmax_std_gridmet.rcp45e.mri.mean

success_projections$y.fut.rcp45e.mri.mean <- 0
success_projections$y.fut.rcp45e.mri.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.mri.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.mri.mean_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.mri.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.mri.mean +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.mri.mean_sq +
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.mri.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp45e.mri.mean <- success_projections$y.fut.rcp45e.mri.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp45e.mri.10_sq <- success_projections$Tmax_std_gridmet.rcp45e.mri.10 * success_projections$Tmax_std_gridmet.rcp45e.mri.10


success_projections$y.fut.rcp45e.mri.10 <- 0
success_projections$y.fut.rcp45e.mri.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.mri.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.mri.10_sq +
                                                        tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                        PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.mri.10  + success_projections$laydate_scaled * laydate_scaled +
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.mri.10 +
                                                        success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.mri.10_sq+
                                                        int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.mri.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp45e.mri.10 <- success_projections$y.fut.rcp45e.mri.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45e.mri.90_sq <- success_projections$Tmax_std_gridmet.rcp45e.mri.90 * success_projections$Tmax_std_gridmet.rcp45e.mri.90


success_projections$y.fut.rcp45e.mri.90 <- 0
success_projections$y.fut.rcp45e.mri.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.mri.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.mri.90_sq +
                                                        tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                        PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.mri.90  + success_projections$laydate_scaled * laydate_scaled +
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.mri.90 +
                                                        success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.mri.90_sq+
                                                        int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.mri.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45e.mri.90 <- success_projections$y.fut.rcp45e.mri.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp45m.mri.mean_sq <- success_projections$Tmax_std_gridmet.rcp45m.mri.mean * success_projections$Tmax_std_gridmet.rcp45m.mri.mean

success_projections$y.fut.rcp45m.mri.mean <- 0
success_projections$y.fut.rcp45m.mri.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.mri.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.mri.mean_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.mri.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.mri.mean +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.mri.mean_sq +
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.mri.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp45m.mri.mean <- success_projections$y.fut.rcp45m.mri.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp45m.mri.10_sq <- success_projections$Tmax_std_gridmet.rcp45m.mri.10 * success_projections$Tmax_std_gridmet.rcp45m.mri.10


success_projections$y.fut.rcp45m.mri.10 <- 0
success_projections$y.fut.rcp45m.mri.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.mri.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.mri.10_sq +
                                                        tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                        PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.mri.10  + success_projections$laydate_scaled * laydate_scaled +
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.mri.10 +
                                                        success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.mri.10_sq+
                                                        int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.mri.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp45m.mri.10 <- success_projections$y.fut.rcp45m.mri.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45m.mri.90_sq <- success_projections$Tmax_std_gridmet.rcp45m.mri.90 * success_projections$Tmax_std_gridmet.rcp45m.mri.90


success_projections$y.fut.rcp45m.mri.90 <- 0
success_projections$y.fut.rcp45m.mri.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.mri.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.mri.90_sq +
                                                        tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                        PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.mri.90  + success_projections$laydate_scaled * laydate_scaled +
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.mri.90 +
                                                        success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.mri.90_sq+
                                                        int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.mri.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45m.mri.90 <- success_projections$y.fut.rcp45m.mri.90 - success_projections$y.now


#RCP 85 mri----
##End of the century

##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85e.mri.mean_sq <- success_projections$Tmax_std_gridmet.rcp85e.mri.mean * success_projections$Tmax_std_gridmet.rcp85e.mri.mean

success_projections$y.fut.rcp85e.mri.mean <- 0
success_projections$y.fut.rcp85e.mri.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.mri.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.mri.mean_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.mri.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.mri.mean +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.mri.mean_sq +
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.mri.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85e.mri.mean <- success_projections$y.fut.rcp85e.mri.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85e.mri.10_sq <- success_projections$Tmax_std_gridmet.rcp85e.mri.10 * success_projections$Tmax_std_gridmet.rcp85e.mri.10


success_projections$y.fut.rcp85e.mri.10 <- 0
success_projections$y.fut.rcp85e.mri.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.mri.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.mri.10_sq +
                                                        tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                        PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.mri.10  + success_projections$laydate_scaled * laydate_scaled +
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.mri.10 +
                                                        success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.mri.10_sq+
                                                        int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.mri.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85e.mri.10 <- success_projections$y.fut.rcp85e.mri.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85e.mri.90_sq <- success_projections$Tmax_std_gridmet.rcp85e.mri.90 * success_projections$Tmax_std_gridmet.rcp85e.mri.90


success_projections$y.fut.rcp85e.mri.90 <- 0
success_projections$y.fut.rcp85e.mri.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.mri.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.mri.90_sq +
                                                        tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                        PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.mri.90  + success_projections$laydate_scaled * laydate_scaled +
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.mri.90 +
                                                        success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.mri.90_sq+
                                                        int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.mri.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85e.mri.90 <- success_projections$y.fut.rcp85e.mri.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85m.mri.mean_sq <- success_projections$Tmax_std_gridmet.rcp85m.mri.mean * success_projections$Tmax_std_gridmet.rcp85m.mri.mean

success_projections$y.fut.rcp85m.mri.mean <- 0
success_projections$y.fut.rcp85m.mri.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.mri.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.mri.mean_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.mri.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.mri.mean +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.mri.mean_sq +
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.mri.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85m.mri.mean <- success_projections$y.fut.rcp85m.mri.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85m.mri.10_sq <- success_projections$Tmax_std_gridmet.rcp85m.mri.10 * success_projections$Tmax_std_gridmet.rcp85m.mri.10


success_projections$y.fut.rcp85m.mri.10 <- 0
success_projections$y.fut.rcp85m.mri.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.mri.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.mri.10_sq +
                                                        tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                        PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.mri.10  + success_projections$laydate_scaled * laydate_scaled +
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.mri.10 +
                                                        success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.mri.10_sq+
                                                        int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.mri.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85m.mri.10 <- success_projections$y.fut.rcp85m.mri.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85m.mri.90_sq <- success_projections$Tmax_std_gridmet.rcp85m.mri.90 * success_projections$Tmax_std_gridmet.rcp85m.mri.90


success_projections$y.fut.rcp85m.mri.90 <- 0
success_projections$y.fut.rcp85m.mri.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.mri.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.mri.90_sq +
                                                        tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                        PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.mri.90  + success_projections$laydate_scaled * laydate_scaled +
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.mri.90 +
                                                        success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.mri.90_sq+
                                                        int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.mri.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85m.mri.90 <- success_projections$y.fut.rcp85m.mri.90 - success_projections$y.now

























####miroc
#RCP 45 miroc----
##End of the century

##Projection for the mean
success_projections$Tmax_std_gridmet.rcp45e.miroc.mean_sq <- success_projections$Tmax_std_gridmet.rcp45e.miroc.mean * success_projections$Tmax_std_gridmet.rcp45e.miroc.mean

success_projections$y.fut.rcp45e.miroc.mean <- 0
success_projections$y.fut.rcp45e.miroc.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.miroc.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.miroc.mean_sq +
                                                            tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                            PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.miroc.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                            NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                            substrate*success_projections$substrate_binary +
                                                            success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.miroc.mean +
                                                            success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.miroc.mean_sq +
                                                            int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.miroc.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp45e.miroc.mean <- success_projections$y.fut.rcp45e.miroc.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp45e.miroc.10_sq <- success_projections$Tmax_std_gridmet.rcp45e.miroc.10 * success_projections$Tmax_std_gridmet.rcp45e.miroc.10


success_projections$y.fut.rcp45e.miroc.10 <- 0
success_projections$y.fut.rcp45e.miroc.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.miroc.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.miroc.10_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.miroc.10  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.miroc.10 +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.miroc.10_sq+
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.miroc.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp45e.miroc.10 <- success_projections$y.fut.rcp45e.miroc.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45e.miroc.90_sq <- success_projections$Tmax_std_gridmet.rcp45e.miroc.90 * success_projections$Tmax_std_gridmet.rcp45e.miroc.90


success_projections$y.fut.rcp45e.miroc.90 <- 0
success_projections$y.fut.rcp45e.miroc.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.miroc.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.miroc.90_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.miroc.90  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.miroc.90 +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.miroc.90_sq+
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.miroc.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45e.miroc.90 <- success_projections$y.fut.rcp45e.miroc.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp45m.miroc.mean_sq <- success_projections$Tmax_std_gridmet.rcp45m.miroc.mean * success_projections$Tmax_std_gridmet.rcp45m.miroc.mean

success_projections$y.fut.rcp45m.miroc.mean <- 0
success_projections$y.fut.rcp45m.miroc.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.miroc.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.miroc.mean_sq +
                                                            tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                            PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.miroc.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                            NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                            substrate*success_projections$substrate_binary +
                                                            success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.miroc.mean +
                                                            success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.miroc.mean_sq +
                                                            int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.miroc.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp45m.miroc.mean <- success_projections$y.fut.rcp45m.miroc.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp45m.miroc.10_sq <- success_projections$Tmax_std_gridmet.rcp45m.miroc.10 * success_projections$Tmax_std_gridmet.rcp45m.miroc.10


success_projections$y.fut.rcp45m.miroc.10 <- 0
success_projections$y.fut.rcp45m.miroc.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.miroc.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.miroc.10_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.miroc.10  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.miroc.10 +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.miroc.10_sq+
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.miroc.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp45m.miroc.10 <- success_projections$y.fut.rcp45m.miroc.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45m.miroc.90_sq <- success_projections$Tmax_std_gridmet.rcp45m.miroc.90 * success_projections$Tmax_std_gridmet.rcp45m.miroc.90


success_projections$y.fut.rcp45m.miroc.90 <- 0
success_projections$y.fut.rcp45m.miroc.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.miroc.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.miroc.90_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.miroc.90  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.miroc.90 +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.miroc.90_sq+
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.miroc.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45m.miroc.90 <- success_projections$y.fut.rcp45m.miroc.90 - success_projections$y.now


#RCP 85 miroc----
##End of the century

##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85e.miroc.mean_sq <- success_projections$Tmax_std_gridmet.rcp85e.miroc.mean * success_projections$Tmax_std_gridmet.rcp85e.miroc.mean

success_projections$y.fut.rcp85e.miroc.mean <- 0
success_projections$y.fut.rcp85e.miroc.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.miroc.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.miroc.mean_sq +
                                                            tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                            PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.miroc.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                            NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                            substrate*success_projections$substrate_binary +
                                                            success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.miroc.mean +
                                                            success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.miroc.mean_sq +
                                                            int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.miroc.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85e.miroc.mean <- success_projections$y.fut.rcp85e.miroc.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85e.miroc.10_sq <- success_projections$Tmax_std_gridmet.rcp85e.miroc.10 * success_projections$Tmax_std_gridmet.rcp85e.miroc.10


success_projections$y.fut.rcp85e.miroc.10 <- 0
success_projections$y.fut.rcp85e.miroc.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.miroc.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.miroc.10_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.miroc.10  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.miroc.10 +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.miroc.10_sq +
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.miroc.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85e.miroc.10 <- success_projections$y.fut.rcp85e.miroc.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85e.miroc.90_sq <- success_projections$Tmax_std_gridmet.rcp85e.miroc.90 * success_projections$Tmax_std_gridmet.rcp85e.miroc.90


success_projections$y.fut.rcp85e.miroc.90 <- 0
success_projections$y.fut.rcp85e.miroc.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.miroc.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.miroc.90_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.miroc.90  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.miroc.90 +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.miroc.90_sq +
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.miroc.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85e.miroc.90 <- success_projections$y.fut.rcp85e.miroc.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85m.miroc.mean_sq <- success_projections$Tmax_std_gridmet.rcp85m.miroc.mean * success_projections$Tmax_std_gridmet.rcp85m.miroc.mean

success_projections$y.fut.rcp85m.miroc.mean <- 0
success_projections$y.fut.rcp85m.miroc.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.miroc.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.miroc.mean_sq +
                                                            tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                            PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.miroc.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                            NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                            substrate*success_projections$substrate_binary +
                                                            success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.miroc.mean +
                                                            success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.miroc.mean_sq +
                                                            int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.miroc.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85m.miroc.mean <- success_projections$y.fut.rcp85m.miroc.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85m.miroc.10_sq <- success_projections$Tmax_std_gridmet.rcp85m.miroc.10 * success_projections$Tmax_std_gridmet.rcp85m.miroc.10


success_projections$y.fut.rcp85m.miroc.10 <- 0
success_projections$y.fut.rcp85m.miroc.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.miroc.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.miroc.10_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.miroc.10  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.miroc.10 +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.miroc.10_sq+
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.miroc.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85m.miroc.10 <- success_projections$y.fut.rcp85m.miroc.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85m.miroc.90_sq <- success_projections$Tmax_std_gridmet.rcp85m.miroc.90 * success_projections$Tmax_std_gridmet.rcp85m.miroc.90


success_projections$y.fut.rcp85m.miroc.90 <- 0
success_projections$y.fut.rcp85m.miroc.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.miroc.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.miroc.90_sq +
                                                          tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                          PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.miroc.90  + success_projections$laydate_scaled * laydate_scaled +
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.miroc.90 +
                                                          success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.miroc.90_sq+
                                                          int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.miroc.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85m.miroc.90 <- success_projections$y.fut.rcp85m.miroc.90 - success_projections$y.now




















####noresm
#RCP 45 noresm----
##End of the century

##Projection for the mean
success_projections$Tmax_std_gridmet.rcp45e.noresm.mean_sq <- success_projections$Tmax_std_gridmet.rcp45e.noresm.mean * success_projections$Tmax_std_gridmet.rcp45e.noresm.mean

success_projections$y.fut.rcp45e.noresm.mean <- 0
success_projections$y.fut.rcp45e.noresm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.noresm.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.noresm.mean_sq +
                                                             tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                             PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.noresm.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.noresm.mean +
                                                             success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.noresm.mean_sq +
                                                             int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.noresm.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp45e.noresm.mean <- success_projections$y.fut.rcp45e.noresm.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp45e.noresm.10_sq <- success_projections$Tmax_std_gridmet.rcp45e.noresm.10 * success_projections$Tmax_std_gridmet.rcp45e.noresm.10


success_projections$y.fut.rcp45e.noresm.10 <- 0
success_projections$y.fut.rcp45e.noresm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.noresm.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.noresm.10_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.noresm.10  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.noresm.10 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.noresm.10_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.noresm.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp45e.noresm.10 <- success_projections$y.fut.rcp45e.noresm.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45e.noresm.90_sq <- success_projections$Tmax_std_gridmet.rcp45e.noresm.90 * success_projections$Tmax_std_gridmet.rcp45e.noresm.90


success_projections$y.fut.rcp45e.noresm.90 <- 0
success_projections$y.fut.rcp45e.noresm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.noresm.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45e.noresm.90_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.noresm.90  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.noresm.90 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45e.noresm.90_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45e.noresm.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45e.noresm.90 <- success_projections$y.fut.rcp45e.noresm.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp45m.noresm.mean_sq <- success_projections$Tmax_std_gridmet.rcp45m.noresm.mean * success_projections$Tmax_std_gridmet.rcp45m.noresm.mean

success_projections$y.fut.rcp45m.noresm.mean <- 0
success_projections$y.fut.rcp45m.noresm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.noresm.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.noresm.mean_sq +
                                                             tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                             PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.noresm.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.noresm.mean +
                                                             success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.noresm.mean_sq +
                                                             int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.noresm.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp45m.noresm.mean <- success_projections$y.fut.rcp45m.noresm.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp45m.noresm.10_sq <- success_projections$Tmax_std_gridmet.rcp45m.noresm.10 * success_projections$Tmax_std_gridmet.rcp45m.noresm.10


success_projections$y.fut.rcp45m.noresm.10 <- 0
success_projections$y.fut.rcp45m.noresm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.noresm.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.noresm.10_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.noresm.10  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.noresm.10 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.noresm.10_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.noresm.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp45m.noresm.10 <- success_projections$y.fut.rcp45m.noresm.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp45m.noresm.90_sq <- success_projections$Tmax_std_gridmet.rcp45m.noresm.90 * success_projections$Tmax_std_gridmet.rcp45m.noresm.90


success_projections$y.fut.rcp45m.noresm.90 <- 0
success_projections$y.fut.rcp45m.noresm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.noresm.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp45m.noresm.90_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.noresm.90  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.noresm.90 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp45m.noresm.90_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp45m.noresm.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp45m.noresm.90 <- success_projections$y.fut.rcp45m.noresm.90 - success_projections$y.now


#RCP 85 noresm----
##End of the century

##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85e.noresm.mean_sq <- success_projections$Tmax_std_gridmet.rcp85e.noresm.mean * success_projections$Tmax_std_gridmet.rcp85e.noresm.mean

success_projections$y.fut.rcp85e.noresm.mean <- 0
success_projections$y.fut.rcp85e.noresm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.noresm.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.noresm.mean_sq +
                                                             tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                             PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.noresm.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.noresm.mean +
                                                             success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.noresm.mean_sq +
                                                             int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.noresm.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85e.noresm.mean <- success_projections$y.fut.rcp85e.noresm.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85e.noresm.10_sq <- success_projections$Tmax_std_gridmet.rcp85e.noresm.10 * success_projections$Tmax_std_gridmet.rcp85e.noresm.10


success_projections$y.fut.rcp85e.noresm.10 <- 0
success_projections$y.fut.rcp85e.noresm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.noresm.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.noresm.10_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.noresm.10  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.noresm.10 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.noresm.10_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.noresm.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85e.noresm.10 <- success_projections$y.fut.rcp85e.noresm.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85e.noresm.90_sq <- success_projections$Tmax_std_gridmet.rcp85e.noresm.90 * success_projections$Tmax_std_gridmet.rcp85e.noresm.90


success_projections$y.fut.rcp85e.noresm.90 <- 0
success_projections$y.fut.rcp85e.noresm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.noresm.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85e.noresm.90_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.noresm.90  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.noresm.90 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85e.noresm.90_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85e.noresm.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85e.noresm.90 <- success_projections$y.fut.rcp85e.noresm.90 - success_projections$y.now





#Middle of the century
##Projection for the mean
success_projections$Tmax_std_gridmet.rcp85m.noresm.mean_sq <- success_projections$Tmax_std_gridmet.rcp85m.noresm.mean * success_projections$Tmax_std_gridmet.rcp85m.noresm.mean

success_projections$y.fut.rcp85m.noresm.mean <- 0
success_projections$y.fut.rcp85m.noresm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.noresm.mean + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.noresm.mean_sq +
                                                             tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                             PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.noresm.mean  + success_projections$laydate_scaled * laydate_scaled +
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.noresm.mean +
                                                             success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.noresm.mean_sq +
                                                             int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.noresm.mean*success_projections$tnestpd_meanmax_gridmet)




success_projections$y.dif.rcp85m.noresm.mean <- success_projections$y.fut.rcp85m.noresm.mean - success_projections$y.now

###Projection for the 10%
success_projections$Tmax_std_gridmet.rcp85m.noresm.10_sq <- success_projections$Tmax_std_gridmet.rcp85m.noresm.10 * success_projections$Tmax_std_gridmet.rcp85m.noresm.10


success_projections$y.fut.rcp85m.noresm.10 <- 0
success_projections$y.fut.rcp85m.noresm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.noresm.10 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.noresm.10_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.noresm.10  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.noresm.10 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.noresm.10_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.noresm.10*success_projections$tnestpd_meanmax_gridmet)


success_projections$y.dif.rcp85m.noresm.10 <- success_projections$y.fut.rcp85m.noresm.10 - success_projections$y.now

###Projection for the 90%
success_projections$Tmax_std_gridmet.rcp85m.noresm.90_sq <- success_projections$Tmax_std_gridmet.rcp85m.noresm.90 * success_projections$Tmax_std_gridmet.rcp85m.noresm.90


success_projections$y.fut.rcp85m.noresm.90 <- 0
success_projections$y.fut.rcp85m.noresm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.noresm.90 + Tmax_std_gridmet_sq*success_projections$Tmax_std_gridmet.rcp85m.noresm.90_sq +
                                                           tnestpd_meanmax_gridmet*success_projections$tnestpd_meanmax_gridmet +
                                                           PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.noresm.90  + success_projections$laydate_scaled * laydate_scaled +
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.noresm.90 +
                                                           success_projections$int_sq*success_projections$Tmax_std_gridmet.rcp85m.noresm.90_sq+
                                                           int.Tmax.tnestpd_meanmax*success_projections$Tmax_std_gridmet.rcp85m.noresm.90*success_projections$tnestpd_meanmax_gridmet)



success_projections$y.dif.rcp85m.noresm.90 <- success_projections$y.fut.rcp85m.noresm.90 - success_projections$y.now






















###Remove NA values
na <- is.na(success_projections$y.dif.rcp45e.gfdl.mean)
rm.na <- which(na =="TRUE")

success_projections <- success_projections[-rm.na,]


###Average between models-----
#####The mean of means
########RCP 45
success_projections$y.dif.average.mean.rcp.45e <- 0
  
for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.mean.rcp.45e[i] <-mean(success_projections$y.dif.rcp45e.gfdl.mean[i],success_projections$y.dif.rcp45e.canesm.mean[i],success_projections$y.dif.rcp45e.mri.mean[i],
       success_projections$y.dif.rcp45e.miroc.mean[i],success_projections$y.dif.rcp45e.noresm.mean[i])
}
  
success_projections$y.dif.average.mean.rcp.45m <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.mean.rcp.45m[i] <-mean(success_projections$y.dif.rcp45m.gfdl.mean[i],success_projections$y.dif.rcp45m.canesm.mean[i],success_projections$y.dif.rcp45m.mri.mean[i],
                                                      success_projections$y.dif.rcp45m.miroc.mean[i],success_projections$y.dif.rcp45m.noresm.mean[i])
}

########RCP 85
success_projections$y.dif.average.mean.rcp.85e <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.mean.rcp.85e[i] <-mean(success_projections$y.dif.rcp85e.gfdl.mean[i],success_projections$y.dif.rcp85e.canesm.mean[i],success_projections$y.dif.rcp85e.mri.mean[i],
                                                      success_projections$y.dif.rcp85e.miroc.mean[i],success_projections$y.dif.rcp85e.noresm.mean[i])
}

success_projections$y.dif.average.mean.rcp.85m <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.mean.rcp.85m[i] <-mean(success_projections$y.dif.rcp85m.gfdl.mean[i],success_projections$y.dif.rcp85m.canesm.mean[i],success_projections$y.dif.rcp85m.mri.mean[i],
                                                      success_projections$y.dif.rcp85m.miroc.mean[i],success_projections$y.dif.rcp85m.noresm.mean[i])
}

#####10%
########RCP 45
success_projections$y.dif.average.10.rcp.45e <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.10.rcp.45e[i] <-mean(success_projections$y.dif.rcp45e.gfdl.10[i],success_projections$y.dif.rcp45e.canesm.10[i],success_projections$y.dif.rcp45e.mri.10[i],
                                                         success_projections$y.dif.rcp45e.miroc.10[i],success_projections$y.dif.rcp45e.noresm.10[i])
}

success_projections$y.dif.average.10.rcp.45m <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.10.rcp.45m[i] <-mean(success_projections$y.dif.rcp45m.gfdl.10[i],success_projections$y.dif.rcp45m.canesm.10[i],success_projections$y.dif.rcp45m.mri.10[i],
                                                         success_projections$y.dif.rcp45m.miroc.10[i],success_projections$y.dif.rcp45m.noresm.10[i])
}

########RCP 85
success_projections$y.dif.average.10.rcp.85e <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.10.rcp.85e[i] <-mean(success_projections$y.dif.rcp85e.gfdl.10[i],success_projections$y.dif.rcp85e.canesm.10[i],success_projections$y.dif.rcp85e.mri.10[i],
                                                         success_projections$y.dif.rcp85e.miroc.10[i],success_projections$y.dif.rcp85e.noresm.10[i])
}

success_projections$y.dif.average.10.rcp.85m <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.10.rcp.85m[i] <-mean(success_projections$y.dif.rcp85m.gfdl.10[i],success_projections$y.dif.rcp85m.canesm.10[i],success_projections$y.dif.rcp85m.mri.10[i],
                                                         success_projections$y.dif.rcp85m.miroc.10[i],success_projections$y.dif.rcp85m.noresm.10[i])
}


#####90%
########RCP 45
success_projections$y.dif.average.90.rcp.45e <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.90.rcp.45e[i] <-mean(success_projections$y.dif.rcp45e.gfdl.90[i],success_projections$y.dif.rcp45e.canesm.90[i],success_projections$y.dif.rcp45e.mri.90[i],
                                                         success_projections$y.dif.rcp45e.miroc.90[i],success_projections$y.dif.rcp45e.noresm.90[i])
}

success_projections$y.dif.average.90.rcp.45m <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.90.rcp.45m[i] <-mean(success_projections$y.dif.rcp45m.gfdl.90[i],success_projections$y.dif.rcp45m.canesm.90[i],success_projections$y.dif.rcp45m.mri.90[i],
                                                         success_projections$y.dif.rcp45m.miroc.90[i],success_projections$y.dif.rcp45m.noresm.90[i])
}

########RCP 85
success_projections$y.dif.average.90.rcp.85e <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.90.rcp.85e[i] <-mean(success_projections$y.dif.rcp85e.gfdl.90[i],success_projections$y.dif.rcp85e.canesm.90[i],success_projections$y.dif.rcp85e.mri.90[i],
                                                         success_projections$y.dif.rcp85e.miroc.90[i],success_projections$y.dif.rcp85e.noresm.90[i])
}

success_projections$y.dif.average.90.rcp.85m <- 0

for(i in 1:nrow(success_projections)){
  success_projections$y.dif.average.90.rcp.85m[i] <-mean(success_projections$y.dif.rcp85m.gfdl.90[i],success_projections$y.dif.rcp85m.canesm.90[i],success_projections$y.dif.rcp85m.mri.90[i],
                                                         success_projections$y.dif.rcp85m.miroc.90[i],success_projections$y.dif.rcp85m.noresm.90[i])
}

###Getting uncertainty

success_projections$rcp.45e.uncertainty <- abs(success_projections$y.dif.average.10.rcp.45e - success_projections$y.dif.average.90.rcp.45e)
success_projections$rcp.45m.uncertainty <- abs(success_projections$y.dif.average.10.rcp.45m - success_projections$y.dif.average.90.rcp.45e)

success_projections$rcp.85e.uncertainty <- abs(success_projections$y.dif.average.10.rcp.85e - success_projections$y.dif.average.90.rcp.85e)
success_projections$rcp.85m.uncertainty <- abs(success_projections$y.dif.average.10.rcp.85m - success_projections$y.dif.average.90.rcp.85e)


write.csv(success_projections,"Data/active/success_projections_computed.csv")



