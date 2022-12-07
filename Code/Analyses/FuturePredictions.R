rm(list=ls())

library(tidyverse)
library(lme4)
library(boot)
library(sjPlot)
library(cowplot)
library(ggpubr)

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

m1 <- readRDS('Results/Question 1-2/success~stdmax2way.AK.RDS')
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

PcpBefore_raw_gridmet <- fixef[6,1]

NLCD_p_forest <- fixef[7,1]
NLCD_p_human <- fixef[8,1]
NLCD_p_ag <- fixef[9,1]

substrate <- fixef[10,1]

int.Tmax.forest <- 1
int.Tmax.ag<- fixef[11,1]
int.Tmax.natural.open <- fixef[12,1]
int.Tmax.human <- fixef[13,1]


##Fort better predict I incorporate the estimated values for the land uses and the interations of them with temperature

success_projections$int <- 0

for (i in 1:nrow(success_projections)){
  if (is.na(success_projections$NewLU1[i]) == T) {success_projections$int[i] <- NA 
  
  } else if (success_projections$NewLU1[i] == "Forest") {success_projections$int[i] <- 0 
  } else if (success_projections$NewLU1[i]== "Ag") {success_projections$int[i] <- fixef[11,1]
  } else if (success_projections$NewLU1[i]== "Natural_open") {success_projections$int[i] <- fixef[12,1]
  } else {success_projections$int[i] <- fixef[13,1]}
}

success_projections$LU <- 0

for (i in 1:nrow(success_projections)){
  if (is.na(success_projections$NewLU1[i]) == T) {success_projections$LU[i] <- NA 
  
  } else if (success_projections$NewLU1[i] == "Forest") {success_projections$LU[i] <- 0 
  } else if (success_projections$NewLU1[i]== "Ag") {success_projections$LU[i] <- fixef[3,1]
  } else if (success_projections$NewLU1[i]== "Natural_open") {success_projections$LU[i] <- fixef[4,1]
  } else {success_projections$LU[i] <- fixef[5,1]}
}

#####How Tmax and Precipitation affect succes

plot_model(m1,type="pred",terms = "pcpbefore_raw_gridmet[all]")+my.theme+
  ggtitle("Effect of precipitation on success") + xlab("Precipitation") + ylab("Success")

plot_model(m1,type="pred",terms = "Tmax_std_gridmet[all]")+my.theme+
  ggtitle("Effect of temperature on success") + xlab("Max temperature") + ylab("Success")

####GFDL
#RCP 45e mean gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45e.gfdl.mean <- 0
success_projections$y.fut.rcp45e.gfdl.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.gfdl.mean  + 
                                       NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                       substrate*success_projections$substrate_binary +
                                      success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.gfdl.mean )

success_projections$y.now.rcp45e.gfdl.mean<- 0
success_projections$y.now.rcp45e.gfdl.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                              NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                              substrate*success_projections$substrate_binary +
                                              success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45e.gfdl.mean <- success_projections$y.fut.rcp45e.gfdl.mean - success_projections$y.now.rcp45e.gfdl.mean


###Scatter plot with x = now y = future
g.now.rcp45e.gfdl.mean <- ggplot(success_projections, aes(x=y.now.rcp45e.gfdl.mean, y = y.fut.rcp45e.gfdl.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 mean gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45e.gfdl.mean


###Density plot of the difference future - now
g.density.y.dif.rcp45e.gfdl.mean <- ggplot(success_projections, aes(x=y.dif.rcp45e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 mean gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45e.gfdl.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45e.gfdl.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45e.gfdl.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45e.gfdl.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45e.gfdl.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45e.gfdl.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45e.gfdl.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45e.gfdl.mean.ag <- ggplot(success_projections.ag.rcp45e.gfdl.mean , aes(x=y.dif.rcp45e.gfdl.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.mean.ag

g.density.y.dif.rcp45e.gfdl.mean.forest <- ggplot(success_projections.forest.rcp45e.gfdl.mean, aes(x=y.dif.rcp45e.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.mean.forest

g.density.y.dif.rcp45e.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp45e.gfdl.mean, aes(x=y.dif.rcp45e.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.mean.natural.open

g.density.y.dif.rcp45e.gfdl.mean.human <- ggplot(success_projections.human.rcp45e.gfdl.mean, aes(x=y.dif.rcp45e.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.mean.human

difference.density.by.landuse.rcp45e.gfdl.mean <- ggarrange(g.density.y.dif.rcp45e.gfdl.mean.ag,g.density.y.dif.rcp45e.gfdl.mean.forest,
                                           g.density.y.dif.rcp45e.gfdl.mean.human,g.density.y.dif.rcp45e.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45e.gfdl.mean


####Difference of temperatures within land uses
success_projections.ag.rcp45e.gfdl.mean$y.dif.Tmax_std_gridmet.rcp45e.gfdl.mean <- success_projections.ag.rcp45e.gfdl.mean$Tmax_std_gridmet.rcp45e.gfdl.mean - success_projections.ag.rcp45e.gfdl.mean$Tmax_std_gridmet
success_projections.forest.rcp45e.gfdl.mean$y.dif.Tmax_std_gridmet.rcp45e.gfdl.mean<- success_projections.forest.rcp45e.gfdl.mean$Tmax_std_gridmet.rcp45e.gfdl.mean - success_projections.forest.rcp45e.gfdl.mean$Tmax_std_gridmet
success_projections.natural.open.rcp45e.gfdl.mean$y.dif.Tmax_std_gridmet.rcp45e.gfdl.mean <- success_projections.natural.open.rcp45e.gfdl.mean$Tmax_std_gridmet.rcp45e.gfdl.mean - success_projections.natural.open.rcp45e.gfdl.mean$Tmax_std_gridmet
success_projections.human.rcp45e.gfdl.mean$y.dif.Tmax_std_gridmet.rcp45e.gfdl.mean <- success_projections.human.rcp45e.gfdl.mean$Tmax_std_gridmet.rcp45e.gfdl.mean - success_projections.human.rcp45e.gfdl.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45e.gfdl.mean.ag <- ggplot(success_projections.ag.rcp45e.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.mean.ag

g.Tmax.y.dif.rcp45e.gfdl.mean.forest <- ggplot(success_projections.forest.rcp45e.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.mean.forest

g.Tmax.y.dif.rcp45e.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp45e.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.mean.natural.open

g.Tmax.y.dif.rcp45e.gfdl.mean.human <- ggplot(success_projections.human.rcp45e.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.mean.human

difference.Tmax.by.landuse.rcp45e.gfdl.mean <- ggarrange(g.Tmax.y.dif.rcp45e.gfdl.mean.ag,g.Tmax.y.dif.rcp45e.gfdl.mean.forest,
                                           g.Tmax.y.dif.rcp45e.gfdl.mean.human,g.Tmax.y.dif.rcp45e.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45e.gfdl.mean



####Difference of precipitation within land uses
success_projections.ag.rcp45e.gfdl.mean$y.dif.pcp_std_gridmet.rcp45e.gfdl.mean <- success_projections.ag.rcp45e.gfdl.mean$pcpbefore_raw_gridmet.rcp45e.gfdl.mean - success_projections.ag.rcp45e.gfdl.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp45e.gfdl.mean$y.dif.pcp_std_gridmet.rcp45e.gfdl.mean<- success_projections.forest.rcp45e.gfdl.mean$pcpbefore_raw_gridmet.rcp45e.gfdl.mean - success_projections.forest.rcp45e.gfdl.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45e.gfdl.mean$y.dif.pcp_std_gridmet.rcp45e.gfdl.mean <- success_projections.natural.open.rcp45e.gfdl.mean$pcpbefore_raw_gridmet.rcp45e.gfdl.mean - success_projections.natural.open.rcp45e.gfdl.mean$pcpbefore_raw_gridmet
success_projections.human.rcp45e.gfdl.mean$y.dif.pcp_std_gridmet.rcp45e.gfdl.mean <- success_projections.human.rcp45e.gfdl.mean$pcpbefore_raw_gridmet.rcp45e.gfdl.mean - success_projections.human.rcp45e.gfdl.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45e.gfdl.mean.ag <- ggplot(success_projections.ag.rcp45e.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.mean.ag

g.pcp.y.dif.rcp45e.gfdl.mean.forest <- ggplot(success_projections.forest.rcp45e.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.mean.forest

g.pcp.y.dif.rcp45e.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp45e.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.mean.natural.open

g.pcp.y.dif.rcp45e.gfdl.mean.human <- ggplot(success_projections.human.rcp45e.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.mean.human

difference.pcp.by.landuse.rcp45e.gfdl.mean <- ggarrange(g.pcp.y.dif.rcp45e.gfdl.mean.ag,g.pcp.y.dif.rcp45e.gfdl.mean.forest,
                                       g.pcp.y.dif.rcp45e.gfdl.mean.human,g.pcp.y.dif.rcp45e.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45e.gfdl.mean

#RCP 45e 10% gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45e.gfdl.10 <- 0
success_projections$y.fut.rcp45e.gfdl.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.gfdl.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.gfdl.10  + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.gfdl.10 )

success_projections$y.now.rcp45e.gfdl.10<- 0
success_projections$y.now.rcp45e.gfdl.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45e.gfdl.10 <- success_projections$y.fut.rcp45e.gfdl.10 - success_projections$y.now.rcp45e.gfdl.10


###Scatter plot with x = now y = future
g.now.rcp45e.gfdl.10 <- ggplot(success_projections, aes(x=y.now.rcp45e.gfdl.10, y = y.fut.rcp45e.gfdl.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 10% gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45e.gfdl.10


###Density plot of the difference future - now
g.density.y.dif.rcp45e.gfdl.10 <- ggplot(success_projections, aes(x=y.dif.rcp45e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 10% gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45e.gfdl.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45e.gfdl.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45e.gfdl.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45e.gfdl.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45e.gfdl.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45e.gfdl.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45e.gfdl.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45e.gfdl.10.ag <- ggplot(success_projections.ag.rcp45e.gfdl.10 , aes(x=y.dif.rcp45e.gfdl.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.10.ag

g.density.y.dif.rcp45e.gfdl.10.forest <- ggplot(success_projections.forest.rcp45e.gfdl.10, aes(x=y.dif.rcp45e.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.10.forest

g.density.y.dif.rcp45e.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp45e.gfdl.10, aes(x=y.dif.rcp45e.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.10.natural.open

g.density.y.dif.rcp45e.gfdl.10.human <- ggplot(success_projections.human.rcp45e.gfdl.10, aes(x=y.dif.rcp45e.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.10.human

difference.density.by.landuse.rcp45e.gfdl.10 <- ggarrange(g.density.y.dif.rcp45e.gfdl.10.ag,g.density.y.dif.rcp45e.gfdl.10.forest,
                                                          g.density.y.dif.rcp45e.gfdl.10.human,g.density.y.dif.rcp45e.gfdl.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45e.gfdl.10


####Difference of temperatures within land uses
success_projections.ag.rcp45e.gfdl.10$y.dif.Tmax_std_gridmet.rcp45e.gfdl.10 <- success_projections.ag.rcp45e.gfdl.10$Tmax_std_gridmet.rcp45e.gfdl.10 - success_projections.ag.rcp45e.gfdl.10$Tmax_std_gridmet
success_projections.forest.rcp45e.gfdl.10$y.dif.Tmax_std_gridmet.rcp45e.gfdl.10<- success_projections.forest.rcp45e.gfdl.10$Tmax_std_gridmet.rcp45e.gfdl.10 - success_projections.forest.rcp45e.gfdl.10$Tmax_std_gridmet
success_projections.natural.open.rcp45e.gfdl.10$y.dif.Tmax_std_gridmet.rcp45e.gfdl.10 <- success_projections.natural.open.rcp45e.gfdl.10$Tmax_std_gridmet.rcp45e.gfdl.10 - success_projections.natural.open.rcp45e.gfdl.10$Tmax_std_gridmet
success_projections.human.rcp45e.gfdl.10$y.dif.Tmax_std_gridmet.rcp45e.gfdl.10 <- success_projections.human.rcp45e.gfdl.10$Tmax_std_gridmet.rcp45e.gfdl.10 - success_projections.human.rcp45e.gfdl.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45e.gfdl.10.ag <- ggplot(success_projections.ag.rcp45e.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.10.ag

g.Tmax.y.dif.rcp45e.gfdl.10.forest <- ggplot(success_projections.forest.rcp45e.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.10.forest

g.Tmax.y.dif.rcp45e.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp45e.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.10.natural.open

g.Tmax.y.dif.rcp45e.gfdl.10.human <- ggplot(success_projections.human.rcp45e.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.10.human

difference.Tmax.by.landuse.rcp45e.gfdl.10 <- ggarrange(g.Tmax.y.dif.rcp45e.gfdl.10.ag,g.Tmax.y.dif.rcp45e.gfdl.10.forest,
                                                       g.Tmax.y.dif.rcp45e.gfdl.10.human,g.Tmax.y.dif.rcp45e.gfdl.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45e.gfdl.10



####Difference of precipitation within land uses
success_projections.ag.rcp45e.gfdl.10$y.dif.pcp_std_gridmet.rcp45e.gfdl.10 <- success_projections.ag.rcp45e.gfdl.10$pcpbefore_raw_gridmet.rcp45e.gfdl.10 - success_projections.ag.rcp45e.gfdl.10$pcpbefore_raw_gridmet
success_projections.forest.rcp45e.gfdl.10$y.dif.pcp_std_gridmet.rcp45e.gfdl.10<- success_projections.forest.rcp45e.gfdl.10$pcpbefore_raw_gridmet.rcp45e.gfdl.10 - success_projections.forest.rcp45e.gfdl.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45e.gfdl.10$y.dif.pcp_std_gridmet.rcp45e.gfdl.10 <- success_projections.natural.open.rcp45e.gfdl.10$pcpbefore_raw_gridmet.rcp45e.gfdl.10 - success_projections.natural.open.rcp45e.gfdl.10$pcpbefore_raw_gridmet
success_projections.human.rcp45e.gfdl.10$y.dif.pcp_std_gridmet.rcp45e.gfdl.10 <- success_projections.human.rcp45e.gfdl.10$pcpbefore_raw_gridmet.rcp45e.gfdl.10 - success_projections.human.rcp45e.gfdl.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45e.gfdl.10.ag <- ggplot(success_projections.ag.rcp45e.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.10.ag

g.pcp.y.dif.rcp45e.gfdl.10.forest <- ggplot(success_projections.forest.rcp45e.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.10.forest

g.pcp.y.dif.rcp45e.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp45e.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.10.natural.open

g.pcp.y.dif.rcp45e.gfdl.10.human <- ggplot(success_projections.human.rcp45e.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.10.human

difference.pcp.by.landuse.rcp45e.gfdl.10 <- ggarrange(g.pcp.y.dif.rcp45e.gfdl.10.ag,g.pcp.y.dif.rcp45e.gfdl.10.forest,
                                                      g.pcp.y.dif.rcp45e.gfdl.10.human,g.pcp.y.dif.rcp45e.gfdl.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45e.gfdl.10

#RCP 45e 90% gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45e.gfdl.90 <- 0
success_projections$y.fut.rcp45e.gfdl.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.gfdl.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.gfdl.90  + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.gfdl.90 )

success_projections$y.now.rcp45e.gfdl.90<- 0
success_projections$y.now.rcp45e.gfdl.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45e.gfdl.90 <- success_projections$y.fut.rcp45e.gfdl.90 - success_projections$y.now.rcp45e.gfdl.90


###Scatter plot with x = now y = future
g.now.rcp45e.gfdl.90 <- ggplot(success_projections, aes(x=y.now.rcp45e.gfdl.90, y = y.fut.rcp45e.gfdl.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 90% gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45e.gfdl.90


###Density plot of the difference future - now
g.density.y.dif.rcp45e.gfdl.90 <- ggplot(success_projections, aes(x=y.dif.rcp45e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 90% gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45e.gfdl.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45e.gfdl.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45e.gfdl.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45e.gfdl.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45e.gfdl.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45e.gfdl.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45e.gfdl.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45e.gfdl.90.ag <- ggplot(success_projections.ag.rcp45e.gfdl.90 , aes(x=y.dif.rcp45e.gfdl.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.90.ag

g.density.y.dif.rcp45e.gfdl.90.forest <- ggplot(success_projections.forest.rcp45e.gfdl.90, aes(x=y.dif.rcp45e.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.90.forest

g.density.y.dif.rcp45e.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp45e.gfdl.90, aes(x=y.dif.rcp45e.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.90.natural.open

g.density.y.dif.rcp45e.gfdl.90.human <- ggplot(success_projections.human.rcp45e.gfdl.90, aes(x=y.dif.rcp45e.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.gfdl.90.human

difference.density.by.landuse.rcp45e.gfdl.90 <- ggarrange(g.density.y.dif.rcp45e.gfdl.90.ag,g.density.y.dif.rcp45e.gfdl.90.forest,
                                                          g.density.y.dif.rcp45e.gfdl.90.human,g.density.y.dif.rcp45e.gfdl.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45e.gfdl.90


####Difference of temperatures within land uses
success_projections.ag.rcp45e.gfdl.90$y.dif.Tmax_std_gridmet.rcp45e.gfdl.90 <- success_projections.ag.rcp45e.gfdl.90$Tmax_std_gridmet.rcp45e.gfdl.90 - success_projections.ag.rcp45e.gfdl.90$Tmax_std_gridmet
success_projections.forest.rcp45e.gfdl.90$y.dif.Tmax_std_gridmet.rcp45e.gfdl.90<- success_projections.forest.rcp45e.gfdl.90$Tmax_std_gridmet.rcp45e.gfdl.90 - success_projections.forest.rcp45e.gfdl.90$Tmax_std_gridmet
success_projections.natural.open.rcp45e.gfdl.90$y.dif.Tmax_std_gridmet.rcp45e.gfdl.90 <- success_projections.natural.open.rcp45e.gfdl.90$Tmax_std_gridmet.rcp45e.gfdl.90 - success_projections.natural.open.rcp45e.gfdl.90$Tmax_std_gridmet
success_projections.human.rcp45e.gfdl.90$y.dif.Tmax_std_gridmet.rcp45e.gfdl.90 <- success_projections.human.rcp45e.gfdl.90$Tmax_std_gridmet.rcp45e.gfdl.90 - success_projections.human.rcp45e.gfdl.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45e.gfdl.90.ag <- ggplot(success_projections.ag.rcp45e.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.90.ag

g.Tmax.y.dif.rcp45e.gfdl.90.forest <- ggplot(success_projections.forest.rcp45e.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.90.forest

g.Tmax.y.dif.rcp45e.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp45e.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.90.natural.open

g.Tmax.y.dif.rcp45e.gfdl.90.human <- ggplot(success_projections.human.rcp45e.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.gfdl.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.gfdl.90.human

difference.Tmax.by.landuse.rcp45e.gfdl.90 <- ggarrange(g.Tmax.y.dif.rcp45e.gfdl.90.ag,g.Tmax.y.dif.rcp45e.gfdl.90.forest,
                                                       g.Tmax.y.dif.rcp45e.gfdl.90.human,g.Tmax.y.dif.rcp45e.gfdl.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45e.gfdl.90



####Difference of precipitation within land uses
success_projections.ag.rcp45e.gfdl.90$y.dif.pcp_std_gridmet.rcp45e.gfdl.90 <- success_projections.ag.rcp45e.gfdl.90$pcpbefore_raw_gridmet.rcp45e.gfdl.90 - success_projections.ag.rcp45e.gfdl.90$pcpbefore_raw_gridmet
success_projections.forest.rcp45e.gfdl.90$y.dif.pcp_std_gridmet.rcp45e.gfdl.90<- success_projections.forest.rcp45e.gfdl.90$pcpbefore_raw_gridmet.rcp45e.gfdl.90 - success_projections.forest.rcp45e.gfdl.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45e.gfdl.90$y.dif.pcp_std_gridmet.rcp45e.gfdl.90 <- success_projections.natural.open.rcp45e.gfdl.90$pcpbefore_raw_gridmet.rcp45e.gfdl.90 - success_projections.natural.open.rcp45e.gfdl.90$pcpbefore_raw_gridmet
success_projections.human.rcp45e.gfdl.90$y.dif.pcp_std_gridmet.rcp45e.gfdl.90 <- success_projections.human.rcp45e.gfdl.90$pcpbefore_raw_gridmet.rcp45e.gfdl.90 - success_projections.human.rcp45e.gfdl.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45e.gfdl.90.ag <- ggplot(success_projections.ag.rcp45e.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.90.ag

g.pcp.y.dif.rcp45e.gfdl.90.forest <- ggplot(success_projections.forest.rcp45e.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.90.forest

g.pcp.y.dif.rcp45e.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp45e.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.90.natural.open

g.pcp.y.dif.rcp45e.gfdl.90.human <- ggplot(success_projections.human.rcp45e.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.gfdl.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.gfdl.90.human

difference.pcp.by.landuse.rcp45e.gfdl.90 <- ggarrange(g.pcp.y.dif.rcp45e.gfdl.90.ag,g.pcp.y.dif.rcp45e.gfdl.90.forest,
                                                      g.pcp.y.dif.rcp45e.gfdl.90.human,g.pcp.y.dif.rcp45e.gfdl.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45e.gfdl.90


#RCP 45m mean gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45m.gfdl.mean <- 0
success_projections$y.fut.rcp45m.gfdl.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.gfdl.mean  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.gfdl.mean )

success_projections$y.now.rcp45m.gfdl.mean<- 0
success_projections$y.now.rcp45m.gfdl.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45m.gfdl.mean <- success_projections$y.fut.rcp45m.gfdl.mean - success_projections$y.now.rcp45m.gfdl.mean


###Scatter plot with x = now y = future
g.now.rcp45m.gfdl.mean <- ggplot(success_projections, aes(x=y.now.rcp45m.gfdl.mean, y = y.fut.rcp45m.gfdl.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 mean gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45m.gfdl.mean


###Density plot of the difference future - now
g.density.y.dif.rcp45m.gfdl.mean <- ggplot(success_projections, aes(x=y.dif.rcp45m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 mean gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45m.gfdl.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45m.gfdl.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45m.gfdl.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45m.gfdl.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45m.gfdl.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45m.gfdl.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45m.gfdl.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45m.gfdl.mean.ag <- ggplot(success_projections.ag.rcp45m.gfdl.mean , aes(x=y.dif.rcp45m.gfdl.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.mean.ag

g.density.y.dif.rcp45m.gfdl.mean.forest <- ggplot(success_projections.forest.rcp45m.gfdl.mean, aes(x=y.dif.rcp45m.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.mean.forest

g.density.y.dif.rcp45m.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp45m.gfdl.mean, aes(x=y.dif.rcp45m.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.mean.natural.open

g.density.y.dif.rcp45m.gfdl.mean.human <- ggplot(success_projections.human.rcp45m.gfdl.mean, aes(x=y.dif.rcp45m.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.mean.human

difference.density.by.landuse.rcp45m.gfdl.mean <- ggarrange(g.density.y.dif.rcp45m.gfdl.mean.ag,g.density.y.dif.rcp45m.gfdl.mean.forest,
                                                            g.density.y.dif.rcp45m.gfdl.mean.human,g.density.y.dif.rcp45m.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45m.gfdl.mean


####Difference of temperatures within land uses
success_projections.ag.rcp45m.gfdl.mean$y.dif.Tmax_std_gridmet.rcp45m.gfdl.mean <- success_projections.ag.rcp45m.gfdl.mean$Tmax_std_gridmet.rcp45m.gfdl.mean - success_projections.ag.rcp45m.gfdl.mean$Tmax_std_gridmet
success_projections.forest.rcp45m.gfdl.mean$y.dif.Tmax_std_gridmet.rcp45m.gfdl.mean<- success_projections.forest.rcp45m.gfdl.mean$Tmax_std_gridmet.rcp45m.gfdl.mean - success_projections.forest.rcp45m.gfdl.mean$Tmax_std_gridmet
success_projections.natural.open.rcp45m.gfdl.mean$y.dif.Tmax_std_gridmet.rcp45m.gfdl.mean <- success_projections.natural.open.rcp45m.gfdl.mean$Tmax_std_gridmet.rcp45m.gfdl.mean - success_projections.natural.open.rcp45m.gfdl.mean$Tmax_std_gridmet
success_projections.human.rcp45m.gfdl.mean$y.dif.Tmax_std_gridmet.rcp45m.gfdl.mean <- success_projections.human.rcp45m.gfdl.mean$Tmax_std_gridmet.rcp45m.gfdl.mean - success_projections.human.rcp45m.gfdl.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45m.gfdl.mean.ag <- ggplot(success_projections.ag.rcp45m.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.mean.ag

g.Tmax.y.dif.rcp45m.gfdl.mean.forest <- ggplot(success_projections.forest.rcp45m.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.mean.forest

g.Tmax.y.dif.rcp45m.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp45m.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.mean.natural.open

g.Tmax.y.dif.rcp45m.gfdl.mean.human <- ggplot(success_projections.human.rcp45m.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.mean.human

difference.Tmax.by.landuse.rcp45m.gfdl.mean <- ggarrange(g.Tmax.y.dif.rcp45m.gfdl.mean.ag,g.Tmax.y.dif.rcp45m.gfdl.mean.forest,
                                                         g.Tmax.y.dif.rcp45m.gfdl.mean.human,g.Tmax.y.dif.rcp45m.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45m.gfdl.mean



####Difference of precipitation within land uses
success_projections.ag.rcp45m.gfdl.mean$y.dif.pcp_std_gridmet.rcp45m.gfdl.mean <- success_projections.ag.rcp45m.gfdl.mean$pcpbefore_raw_gridmet.rcp45m.gfdl.mean - success_projections.ag.rcp45m.gfdl.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp45m.gfdl.mean$y.dif.pcp_std_gridmet.rcp45m.gfdl.mean<- success_projections.forest.rcp45m.gfdl.mean$pcpbefore_raw_gridmet.rcp45m.gfdl.mean - success_projections.forest.rcp45m.gfdl.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45m.gfdl.mean$y.dif.pcp_std_gridmet.rcp45m.gfdl.mean <- success_projections.natural.open.rcp45m.gfdl.mean$pcpbefore_raw_gridmet.rcp45m.gfdl.mean - success_projections.natural.open.rcp45m.gfdl.mean$pcpbefore_raw_gridmet
success_projections.human.rcp45m.gfdl.mean$y.dif.pcp_std_gridmet.rcp45m.gfdl.mean <- success_projections.human.rcp45m.gfdl.mean$pcpbefore_raw_gridmet.rcp45m.gfdl.mean - success_projections.human.rcp45m.gfdl.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45m.gfdl.mean.ag <- ggplot(success_projections.ag.rcp45m.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.mean.ag

g.pcp.y.dif.rcp45m.gfdl.mean.forest <- ggplot(success_projections.forest.rcp45m.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.mean.forest

g.pcp.y.dif.rcp45m.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp45m.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.mean.natural.open

g.pcp.y.dif.rcp45m.gfdl.mean.human <- ggplot(success_projections.human.rcp45m.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.mean.human

difference.pcp.by.landuse.rcp45m.gfdl.mean <- ggarrange(g.pcp.y.dif.rcp45m.gfdl.mean.ag,g.pcp.y.dif.rcp45m.gfdl.mean.forest,
                                                        g.pcp.y.dif.rcp45m.gfdl.mean.human,g.pcp.y.dif.rcp45m.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45m.gfdl.mean

#RCP 45m 10% gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45m.gfdl.10 <- 0
success_projections$y.fut.rcp45m.gfdl.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.gfdl.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.gfdl.10  + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.gfdl.10 )

success_projections$y.now.rcp45m.gfdl.10<- 0
success_projections$y.now.rcp45m.gfdl.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45m.gfdl.10 <- success_projections$y.fut.rcp45m.gfdl.10 - success_projections$y.now.rcp45m.gfdl.10


###Scatter plot with x = now y = future
g.now.rcp45m.gfdl.10 <- ggplot(success_projections, aes(x=y.now.rcp45m.gfdl.10, y = y.fut.rcp45m.gfdl.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 10% gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45m.gfdl.10


###Density plot of the difference future - now
g.density.y.dif.rcp45m.gfdl.10 <- ggplot(success_projections, aes(x=y.dif.rcp45m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 10% gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45m.gfdl.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45m.gfdl.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45m.gfdl.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45m.gfdl.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45m.gfdl.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45m.gfdl.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45m.gfdl.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45m.gfdl.10.ag <- ggplot(success_projections.ag.rcp45m.gfdl.10 , aes(x=y.dif.rcp45m.gfdl.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.10.ag

g.density.y.dif.rcp45m.gfdl.10.forest <- ggplot(success_projections.forest.rcp45m.gfdl.10, aes(x=y.dif.rcp45m.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.10.forest

g.density.y.dif.rcp45m.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp45m.gfdl.10, aes(x=y.dif.rcp45m.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.10.natural.open

g.density.y.dif.rcp45m.gfdl.10.human <- ggplot(success_projections.human.rcp45m.gfdl.10, aes(x=y.dif.rcp45m.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.10.human

difference.density.by.landuse.rcp45m.gfdl.10 <- ggarrange(g.density.y.dif.rcp45m.gfdl.10.ag,g.density.y.dif.rcp45m.gfdl.10.forest,
                                                          g.density.y.dif.rcp45m.gfdl.10.human,g.density.y.dif.rcp45m.gfdl.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45m.gfdl.10


####Difference of temperatures within land uses
success_projections.ag.rcp45m.gfdl.10$y.dif.Tmax_std_gridmet.rcp45m.gfdl.10 <- success_projections.ag.rcp45m.gfdl.10$Tmax_std_gridmet.rcp45m.gfdl.10 - success_projections.ag.rcp45m.gfdl.10$Tmax_std_gridmet
success_projections.forest.rcp45m.gfdl.10$y.dif.Tmax_std_gridmet.rcp45m.gfdl.10<- success_projections.forest.rcp45m.gfdl.10$Tmax_std_gridmet.rcp45m.gfdl.10 - success_projections.forest.rcp45m.gfdl.10$Tmax_std_gridmet
success_projections.natural.open.rcp45m.gfdl.10$y.dif.Tmax_std_gridmet.rcp45m.gfdl.10 <- success_projections.natural.open.rcp45m.gfdl.10$Tmax_std_gridmet.rcp45m.gfdl.10 - success_projections.natural.open.rcp45m.gfdl.10$Tmax_std_gridmet
success_projections.human.rcp45m.gfdl.10$y.dif.Tmax_std_gridmet.rcp45m.gfdl.10 <- success_projections.human.rcp45m.gfdl.10$Tmax_std_gridmet.rcp45m.gfdl.10 - success_projections.human.rcp45m.gfdl.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45m.gfdl.10.ag <- ggplot(success_projections.ag.rcp45m.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.10.ag

g.Tmax.y.dif.rcp45m.gfdl.10.forest <- ggplot(success_projections.forest.rcp45m.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.10.forest

g.Tmax.y.dif.rcp45m.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp45m.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.10.natural.open

g.Tmax.y.dif.rcp45m.gfdl.10.human <- ggplot(success_projections.human.rcp45m.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.10.human

difference.Tmax.by.landuse.rcp45m.gfdl.10 <- ggarrange(g.Tmax.y.dif.rcp45m.gfdl.10.ag,g.Tmax.y.dif.rcp45m.gfdl.10.forest,
                                                       g.Tmax.y.dif.rcp45m.gfdl.10.human,g.Tmax.y.dif.rcp45m.gfdl.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45m.gfdl.10



####Difference of precipitation within land uses
success_projections.ag.rcp45m.gfdl.10$y.dif.pcp_std_gridmet.rcp45m.gfdl.10 <- success_projections.ag.rcp45m.gfdl.10$pcpbefore_raw_gridmet.rcp45m.gfdl.10 - success_projections.ag.rcp45m.gfdl.10$pcpbefore_raw_gridmet
success_projections.forest.rcp45m.gfdl.10$y.dif.pcp_std_gridmet.rcp45m.gfdl.10<- success_projections.forest.rcp45m.gfdl.10$pcpbefore_raw_gridmet.rcp45m.gfdl.10 - success_projections.forest.rcp45m.gfdl.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45m.gfdl.10$y.dif.pcp_std_gridmet.rcp45m.gfdl.10 <- success_projections.natural.open.rcp45m.gfdl.10$pcpbefore_raw_gridmet.rcp45m.gfdl.10 - success_projections.natural.open.rcp45m.gfdl.10$pcpbefore_raw_gridmet
success_projections.human.rcp45m.gfdl.10$y.dif.pcp_std_gridmet.rcp45m.gfdl.10 <- success_projections.human.rcp45m.gfdl.10$pcpbefore_raw_gridmet.rcp45m.gfdl.10 - success_projections.human.rcp45m.gfdl.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45m.gfdl.10.ag <- ggplot(success_projections.ag.rcp45m.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.10.ag

g.pcp.y.dif.rcp45m.gfdl.10.forest <- ggplot(success_projections.forest.rcp45m.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.10.forest

g.pcp.y.dif.rcp45m.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp45m.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.10.natural.open

g.pcp.y.dif.rcp45m.gfdl.10.human <- ggplot(success_projections.human.rcp45m.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.10.human

difference.pcp.by.landuse.rcp45m.gfdl.10 <- ggarrange(g.pcp.y.dif.rcp45m.gfdl.10.ag,g.pcp.y.dif.rcp45m.gfdl.10.forest,
                                                      g.pcp.y.dif.rcp45m.gfdl.10.human,g.pcp.y.dif.rcp45m.gfdl.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45m.gfdl.10

#RCP 45m 90% gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45m.gfdl.90 <- 0
success_projections$y.fut.rcp45m.gfdl.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.gfdl.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.gfdl.90  + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.gfdl.90 )

success_projections$y.now.rcp45m.gfdl.90<- 0
success_projections$y.now.rcp45m.gfdl.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45m.gfdl.90 <- success_projections$y.fut.rcp45m.gfdl.90 - success_projections$y.now.rcp45m.gfdl.90


###Scatter plot with x = now y = future
g.now.rcp45m.gfdl.90 <- ggplot(success_projections, aes(x=y.now.rcp45m.gfdl.90, y = y.fut.rcp45m.gfdl.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 90% gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45m.gfdl.90


###Density plot of the difference future - now
g.density.y.dif.rcp45m.gfdl.90 <- ggplot(success_projections, aes(x=y.dif.rcp45m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 90% gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45m.gfdl.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45m.gfdl.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45m.gfdl.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45m.gfdl.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45m.gfdl.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45m.gfdl.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45m.gfdl.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45m.gfdl.90.ag <- ggplot(success_projections.ag.rcp45m.gfdl.90 , aes(x=y.dif.rcp45m.gfdl.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.90.ag

g.density.y.dif.rcp45m.gfdl.90.forest <- ggplot(success_projections.forest.rcp45m.gfdl.90, aes(x=y.dif.rcp45m.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.90.forest

g.density.y.dif.rcp45m.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp45m.gfdl.90, aes(x=y.dif.rcp45m.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.90.natural.open

g.density.y.dif.rcp45m.gfdl.90.human <- ggplot(success_projections.human.rcp45m.gfdl.90, aes(x=y.dif.rcp45m.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.gfdl.90.human

difference.density.by.landuse.rcp45m.gfdl.90 <- ggarrange(g.density.y.dif.rcp45m.gfdl.90.ag,g.density.y.dif.rcp45m.gfdl.90.forest,
                                                          g.density.y.dif.rcp45m.gfdl.90.human,g.density.y.dif.rcp45m.gfdl.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45m.gfdl.90


####Difference of temperatures within land uses
success_projections.ag.rcp45m.gfdl.90$y.dif.Tmax_std_gridmet.rcp45m.gfdl.90 <- success_projections.ag.rcp45m.gfdl.90$Tmax_std_gridmet.rcp45m.gfdl.90 - success_projections.ag.rcp45m.gfdl.90$Tmax_std_gridmet
success_projections.forest.rcp45m.gfdl.90$y.dif.Tmax_std_gridmet.rcp45m.gfdl.90<- success_projections.forest.rcp45m.gfdl.90$Tmax_std_gridmet.rcp45m.gfdl.90 - success_projections.forest.rcp45m.gfdl.90$Tmax_std_gridmet
success_projections.natural.open.rcp45m.gfdl.90$y.dif.Tmax_std_gridmet.rcp45m.gfdl.90 <- success_projections.natural.open.rcp45m.gfdl.90$Tmax_std_gridmet.rcp45m.gfdl.90 - success_projections.natural.open.rcp45m.gfdl.90$Tmax_std_gridmet
success_projections.human.rcp45m.gfdl.90$y.dif.Tmax_std_gridmet.rcp45m.gfdl.90 <- success_projections.human.rcp45m.gfdl.90$Tmax_std_gridmet.rcp45m.gfdl.90 - success_projections.human.rcp45m.gfdl.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45m.gfdl.90.ag <- ggplot(success_projections.ag.rcp45m.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.90.ag

g.Tmax.y.dif.rcp45m.gfdl.90.forest <- ggplot(success_projections.forest.rcp45m.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.90.forest

g.Tmax.y.dif.rcp45m.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp45m.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.90.natural.open

g.Tmax.y.dif.rcp45m.gfdl.90.human <- ggplot(success_projections.human.rcp45m.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.gfdl.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.gfdl.90.human

difference.Tmax.by.landuse.rcp45m.gfdl.90 <- ggarrange(g.Tmax.y.dif.rcp45m.gfdl.90.ag,g.Tmax.y.dif.rcp45m.gfdl.90.forest,
                                                       g.Tmax.y.dif.rcp45m.gfdl.90.human,g.Tmax.y.dif.rcp45m.gfdl.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45m.gfdl.90



####Difference of precipitation within land uses
success_projections.ag.rcp45m.gfdl.90$y.dif.pcp_std_gridmet.rcp45m.gfdl.90 <- success_projections.ag.rcp45m.gfdl.90$pcpbefore_raw_gridmet.rcp45m.gfdl.90 - success_projections.ag.rcp45m.gfdl.90$pcpbefore_raw_gridmet
success_projections.forest.rcp45m.gfdl.90$y.dif.pcp_std_gridmet.rcp45m.gfdl.90<- success_projections.forest.rcp45m.gfdl.90$pcpbefore_raw_gridmet.rcp45m.gfdl.90 - success_projections.forest.rcp45m.gfdl.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45m.gfdl.90$y.dif.pcp_std_gridmet.rcp45m.gfdl.90 <- success_projections.natural.open.rcp45m.gfdl.90$pcpbefore_raw_gridmet.rcp45m.gfdl.90 - success_projections.natural.open.rcp45m.gfdl.90$pcpbefore_raw_gridmet
success_projections.human.rcp45m.gfdl.90$y.dif.pcp_std_gridmet.rcp45m.gfdl.90 <- success_projections.human.rcp45m.gfdl.90$pcpbefore_raw_gridmet.rcp45m.gfdl.90 - success_projections.human.rcp45m.gfdl.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45m.gfdl.90.ag <- ggplot(success_projections.ag.rcp45m.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.90.ag

g.pcp.y.dif.rcp45m.gfdl.90.forest <- ggplot(success_projections.forest.rcp45m.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.90.forest

g.pcp.y.dif.rcp45m.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp45m.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.90.natural.open

g.pcp.y.dif.rcp45m.gfdl.90.human <- ggplot(success_projections.human.rcp45m.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.gfdl.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.gfdl.90.human

difference.pcp.by.landuse.rcp45m.gfdl.90 <- ggarrange(g.pcp.y.dif.rcp45m.gfdl.90.ag,g.pcp.y.dif.rcp45m.gfdl.90.forest,
                                                      g.pcp.y.dif.rcp45m.gfdl.90.human,g.pcp.y.dif.rcp45m.gfdl.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45m.gfdl.90


#RCP 85e mean gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85e.gfdl.mean <- 0
success_projections$y.fut.rcp85e.gfdl.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.gfdl.mean  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.gfdl.mean )

success_projections$y.now.rcp85e.gfdl.mean<- 0
success_projections$y.now.rcp85e.gfdl.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85e.gfdl.mean <- success_projections$y.fut.rcp85e.gfdl.mean - success_projections$y.now.rcp85e.gfdl.mean


###Scatter plot with x = now y = future
g.now.rcp85e.gfdl.mean <- ggplot(success_projections, aes(x=y.now.rcp85e.gfdl.mean, y = y.fut.rcp85e.gfdl.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 mean gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85e.gfdl.mean


###Density plot of the difference future - now
g.density.y.dif.rcp85e.gfdl.mean <- ggplot(success_projections, aes(x=y.dif.rcp85e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 mean gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85e.gfdl.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85e.gfdl.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85e.gfdl.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85e.gfdl.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85e.gfdl.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85e.gfdl.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85e.gfdl.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85e.gfdl.mean.ag <- ggplot(success_projections.ag.rcp85e.gfdl.mean , aes(x=y.dif.rcp85e.gfdl.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.mean.ag

g.density.y.dif.rcp85e.gfdl.mean.forest <- ggplot(success_projections.forest.rcp85e.gfdl.mean, aes(x=y.dif.rcp85e.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.mean.forest

g.density.y.dif.rcp85e.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp85e.gfdl.mean, aes(x=y.dif.rcp85e.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.mean.natural.open

g.density.y.dif.rcp85e.gfdl.mean.human <- ggplot(success_projections.human.rcp85e.gfdl.mean, aes(x=y.dif.rcp85e.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.mean.human

difference.density.by.landuse.rcp85e.gfdl.mean <- ggarrange(g.density.y.dif.rcp85e.gfdl.mean.ag,g.density.y.dif.rcp85e.gfdl.mean.forest,
                                                            g.density.y.dif.rcp85e.gfdl.mean.human,g.density.y.dif.rcp85e.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85e.gfdl.mean


####Difference of temperatures within land uses
success_projections.ag.rcp85e.gfdl.mean$y.dif.Tmax_std_gridmet.rcp85e.gfdl.mean <- success_projections.ag.rcp85e.gfdl.mean$Tmax_std_gridmet.rcp85e.gfdl.mean - success_projections.ag.rcp85e.gfdl.mean$Tmax_std_gridmet
success_projections.forest.rcp85e.gfdl.mean$y.dif.Tmax_std_gridmet.rcp85e.gfdl.mean<- success_projections.forest.rcp85e.gfdl.mean$Tmax_std_gridmet.rcp85e.gfdl.mean - success_projections.forest.rcp85e.gfdl.mean$Tmax_std_gridmet
success_projections.natural.open.rcp85e.gfdl.mean$y.dif.Tmax_std_gridmet.rcp85e.gfdl.mean <- success_projections.natural.open.rcp85e.gfdl.mean$Tmax_std_gridmet.rcp85e.gfdl.mean - success_projections.natural.open.rcp85e.gfdl.mean$Tmax_std_gridmet
success_projections.human.rcp85e.gfdl.mean$y.dif.Tmax_std_gridmet.rcp85e.gfdl.mean <- success_projections.human.rcp85e.gfdl.mean$Tmax_std_gridmet.rcp85e.gfdl.mean - success_projections.human.rcp85e.gfdl.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85e.gfdl.mean.ag <- ggplot(success_projections.ag.rcp85e.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.mean.ag

g.Tmax.y.dif.rcp85e.gfdl.mean.forest <- ggplot(success_projections.forest.rcp85e.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.mean.forest

g.Tmax.y.dif.rcp85e.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp85e.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.mean.natural.open

g.Tmax.y.dif.rcp85e.gfdl.mean.human <- ggplot(success_projections.human.rcp85e.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.mean.human

difference.Tmax.by.landuse.rcp85e.gfdl.mean <- ggarrange(g.Tmax.y.dif.rcp85e.gfdl.mean.ag,g.Tmax.y.dif.rcp85e.gfdl.mean.forest,
                                                         g.Tmax.y.dif.rcp85e.gfdl.mean.human,g.Tmax.y.dif.rcp85e.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85e.gfdl.mean



####Difference of precipitation within land uses
success_projections.ag.rcp85e.gfdl.mean$y.dif.pcp_std_gridmet.rcp85e.gfdl.mean <- success_projections.ag.rcp85e.gfdl.mean$pcpbefore_raw_gridmet.rcp85e.gfdl.mean - success_projections.ag.rcp85e.gfdl.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp85e.gfdl.mean$y.dif.pcp_std_gridmet.rcp85e.gfdl.mean<- success_projections.forest.rcp85e.gfdl.mean$pcpbefore_raw_gridmet.rcp85e.gfdl.mean - success_projections.forest.rcp85e.gfdl.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85e.gfdl.mean$y.dif.pcp_std_gridmet.rcp85e.gfdl.mean <- success_projections.natural.open.rcp85e.gfdl.mean$pcpbefore_raw_gridmet.rcp85e.gfdl.mean - success_projections.natural.open.rcp85e.gfdl.mean$pcpbefore_raw_gridmet
success_projections.human.rcp85e.gfdl.mean$y.dif.pcp_std_gridmet.rcp85e.gfdl.mean <- success_projections.human.rcp85e.gfdl.mean$pcpbefore_raw_gridmet.rcp85e.gfdl.mean - success_projections.human.rcp85e.gfdl.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85e.gfdl.mean.ag <- ggplot(success_projections.ag.rcp85e.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.mean.ag

g.pcp.y.dif.rcp85e.gfdl.mean.forest <- ggplot(success_projections.forest.rcp85e.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.mean.forest

g.pcp.y.dif.rcp85e.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp85e.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.mean.natural.open

g.pcp.y.dif.rcp85e.gfdl.mean.human <- ggplot(success_projections.human.rcp85e.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.mean.human

difference.pcp.by.landuse.rcp85e.gfdl.mean <- ggarrange(g.pcp.y.dif.rcp85e.gfdl.mean.ag,g.pcp.y.dif.rcp85e.gfdl.mean.forest,
                                                        g.pcp.y.dif.rcp85e.gfdl.mean.human,g.pcp.y.dif.rcp85e.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85e.gfdl.mean

#RCP 85e 10% gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85e.gfdl.10 <- 0
success_projections$y.fut.rcp85e.gfdl.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.gfdl.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.gfdl.10  + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.gfdl.10 )

success_projections$y.now.rcp85e.gfdl.10<- 0
success_projections$y.now.rcp85e.gfdl.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85e.gfdl.10 <- success_projections$y.fut.rcp85e.gfdl.10 - success_projections$y.now.rcp85e.gfdl.10


###Scatter plot with x = now y = future
g.now.rcp85e.gfdl.10 <- ggplot(success_projections, aes(x=y.now.rcp85e.gfdl.10, y = y.fut.rcp85e.gfdl.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 10% gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85e.gfdl.10


###Density plot of the difference future - now
g.density.y.dif.rcp85e.gfdl.10 <- ggplot(success_projections, aes(x=y.dif.rcp85e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 10% gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85e.gfdl.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85e.gfdl.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85e.gfdl.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85e.gfdl.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85e.gfdl.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85e.gfdl.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85e.gfdl.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85e.gfdl.10.ag <- ggplot(success_projections.ag.rcp85e.gfdl.10 , aes(x=y.dif.rcp85e.gfdl.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.10.ag

g.density.y.dif.rcp85e.gfdl.10.forest <- ggplot(success_projections.forest.rcp85e.gfdl.10, aes(x=y.dif.rcp85e.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.10.forest

g.density.y.dif.rcp85e.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp85e.gfdl.10, aes(x=y.dif.rcp85e.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.10.natural.open

g.density.y.dif.rcp85e.gfdl.10.human <- ggplot(success_projections.human.rcp85e.gfdl.10, aes(x=y.dif.rcp85e.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.10.human

difference.density.by.landuse.rcp85e.gfdl.10 <- ggarrange(g.density.y.dif.rcp85e.gfdl.10.ag,g.density.y.dif.rcp85e.gfdl.10.forest,
                                                          g.density.y.dif.rcp85e.gfdl.10.human,g.density.y.dif.rcp85e.gfdl.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85e.gfdl.10


####Difference of temperatures within land uses
success_projections.ag.rcp85e.gfdl.10$y.dif.Tmax_std_gridmet.rcp85e.gfdl.10 <- success_projections.ag.rcp85e.gfdl.10$Tmax_std_gridmet.rcp85e.gfdl.10 - success_projections.ag.rcp85e.gfdl.10$Tmax_std_gridmet
success_projections.forest.rcp85e.gfdl.10$y.dif.Tmax_std_gridmet.rcp85e.gfdl.10<- success_projections.forest.rcp85e.gfdl.10$Tmax_std_gridmet.rcp85e.gfdl.10 - success_projections.forest.rcp85e.gfdl.10$Tmax_std_gridmet
success_projections.natural.open.rcp85e.gfdl.10$y.dif.Tmax_std_gridmet.rcp85e.gfdl.10 <- success_projections.natural.open.rcp85e.gfdl.10$Tmax_std_gridmet.rcp85e.gfdl.10 - success_projections.natural.open.rcp85e.gfdl.10$Tmax_std_gridmet
success_projections.human.rcp85e.gfdl.10$y.dif.Tmax_std_gridmet.rcp85e.gfdl.10 <- success_projections.human.rcp85e.gfdl.10$Tmax_std_gridmet.rcp85e.gfdl.10 - success_projections.human.rcp85e.gfdl.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85e.gfdl.10.ag <- ggplot(success_projections.ag.rcp85e.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.10.ag

g.Tmax.y.dif.rcp85e.gfdl.10.forest <- ggplot(success_projections.forest.rcp85e.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.10.forest

g.Tmax.y.dif.rcp85e.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp85e.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.10.natural.open

g.Tmax.y.dif.rcp85e.gfdl.10.human <- ggplot(success_projections.human.rcp85e.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.10.human

difference.Tmax.by.landuse.rcp85e.gfdl.10 <- ggarrange(g.Tmax.y.dif.rcp85e.gfdl.10.ag,g.Tmax.y.dif.rcp85e.gfdl.10.forest,
                                                       g.Tmax.y.dif.rcp85e.gfdl.10.human,g.Tmax.y.dif.rcp85e.gfdl.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85e.gfdl.10



####Difference of precipitation within land uses
success_projections.ag.rcp85e.gfdl.10$y.dif.pcp_std_gridmet.rcp85e.gfdl.10 <- success_projections.ag.rcp85e.gfdl.10$pcpbefore_raw_gridmet.rcp85e.gfdl.10 - success_projections.ag.rcp85e.gfdl.10$pcpbefore_raw_gridmet
success_projections.forest.rcp85e.gfdl.10$y.dif.pcp_std_gridmet.rcp85e.gfdl.10<- success_projections.forest.rcp85e.gfdl.10$pcpbefore_raw_gridmet.rcp85e.gfdl.10 - success_projections.forest.rcp85e.gfdl.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85e.gfdl.10$y.dif.pcp_std_gridmet.rcp85e.gfdl.10 <- success_projections.natural.open.rcp85e.gfdl.10$pcpbefore_raw_gridmet.rcp85e.gfdl.10 - success_projections.natural.open.rcp85e.gfdl.10$pcpbefore_raw_gridmet
success_projections.human.rcp85e.gfdl.10$y.dif.pcp_std_gridmet.rcp85e.gfdl.10 <- success_projections.human.rcp85e.gfdl.10$pcpbefore_raw_gridmet.rcp85e.gfdl.10 - success_projections.human.rcp85e.gfdl.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85e.gfdl.10.ag <- ggplot(success_projections.ag.rcp85e.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.10.ag

g.pcp.y.dif.rcp85e.gfdl.10.forest <- ggplot(success_projections.forest.rcp85e.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.10.forest

g.pcp.y.dif.rcp85e.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp85e.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.10.natural.open

g.pcp.y.dif.rcp85e.gfdl.10.human <- ggplot(success_projections.human.rcp85e.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.10.human

difference.pcp.by.landuse.rcp85e.gfdl.10 <- ggarrange(g.pcp.y.dif.rcp85e.gfdl.10.ag,g.pcp.y.dif.rcp85e.gfdl.10.forest,
                                                      g.pcp.y.dif.rcp85e.gfdl.10.human,g.pcp.y.dif.rcp85e.gfdl.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85e.gfdl.10

#RCP 85e 90% gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85e.gfdl.90 <- 0
success_projections$y.fut.rcp85e.gfdl.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.gfdl.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.gfdl.90  + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.gfdl.90 )

success_projections$y.now.rcp85e.gfdl.90<- 0
success_projections$y.now.rcp85e.gfdl.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85e.gfdl.90 <- success_projections$y.fut.rcp85e.gfdl.90 - success_projections$y.now.rcp85e.gfdl.90


###Scatter plot with x = now y = future
g.now.rcp85e.gfdl.90 <- ggplot(success_projections, aes(x=y.now.rcp85e.gfdl.90, y = y.fut.rcp85e.gfdl.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 90% gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85e.gfdl.90


###Density plot of the difference future - now
g.density.y.dif.rcp85e.gfdl.90 <- ggplot(success_projections, aes(x=y.dif.rcp85e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 90% gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85e.gfdl.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85e.gfdl.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85e.gfdl.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85e.gfdl.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85e.gfdl.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85e.gfdl.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85e.gfdl.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85e.gfdl.90.ag <- ggplot(success_projections.ag.rcp85e.gfdl.90 , aes(x=y.dif.rcp85e.gfdl.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.90.ag

g.density.y.dif.rcp85e.gfdl.90.forest <- ggplot(success_projections.forest.rcp85e.gfdl.90, aes(x=y.dif.rcp85e.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.90.forest

g.density.y.dif.rcp85e.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp85e.gfdl.90, aes(x=y.dif.rcp85e.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.90.natural.open

g.density.y.dif.rcp85e.gfdl.90.human <- ggplot(success_projections.human.rcp85e.gfdl.90, aes(x=y.dif.rcp85e.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.gfdl.90.human

difference.density.by.landuse.rcp85e.gfdl.90 <- ggarrange(g.density.y.dif.rcp85e.gfdl.90.ag,g.density.y.dif.rcp85e.gfdl.90.forest,
                                                          g.density.y.dif.rcp85e.gfdl.90.human,g.density.y.dif.rcp85e.gfdl.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85e.gfdl.90


####Difference of temperatures within land uses
success_projections.ag.rcp85e.gfdl.90$y.dif.Tmax_std_gridmet.rcp85e.gfdl.90 <- success_projections.ag.rcp85e.gfdl.90$Tmax_std_gridmet.rcp85e.gfdl.90 - success_projections.ag.rcp85e.gfdl.90$Tmax_std_gridmet
success_projections.forest.rcp85e.gfdl.90$y.dif.Tmax_std_gridmet.rcp85e.gfdl.90<- success_projections.forest.rcp85e.gfdl.90$Tmax_std_gridmet.rcp85e.gfdl.90 - success_projections.forest.rcp85e.gfdl.90$Tmax_std_gridmet
success_projections.natural.open.rcp85e.gfdl.90$y.dif.Tmax_std_gridmet.rcp85e.gfdl.90 <- success_projections.natural.open.rcp85e.gfdl.90$Tmax_std_gridmet.rcp85e.gfdl.90 - success_projections.natural.open.rcp85e.gfdl.90$Tmax_std_gridmet
success_projections.human.rcp85e.gfdl.90$y.dif.Tmax_std_gridmet.rcp85e.gfdl.90 <- success_projections.human.rcp85e.gfdl.90$Tmax_std_gridmet.rcp85e.gfdl.90 - success_projections.human.rcp85e.gfdl.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85e.gfdl.90.ag <- ggplot(success_projections.ag.rcp85e.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.90.ag

g.Tmax.y.dif.rcp85e.gfdl.90.forest <- ggplot(success_projections.forest.rcp85e.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.90.forest

g.Tmax.y.dif.rcp85e.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp85e.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.90.natural.open

g.Tmax.y.dif.rcp85e.gfdl.90.human <- ggplot(success_projections.human.rcp85e.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.gfdl.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.gfdl.90.human

difference.Tmax.by.landuse.rcp85e.gfdl.90 <- ggarrange(g.Tmax.y.dif.rcp85e.gfdl.90.ag,g.Tmax.y.dif.rcp85e.gfdl.90.forest,
                                                       g.Tmax.y.dif.rcp85e.gfdl.90.human,g.Tmax.y.dif.rcp85e.gfdl.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85e.gfdl.90



####Difference of precipitation within land uses
success_projections.ag.rcp85e.gfdl.90$y.dif.pcp_std_gridmet.rcp85e.gfdl.90 <- success_projections.ag.rcp85e.gfdl.90$pcpbefore_raw_gridmet.rcp85e.gfdl.90 - success_projections.ag.rcp85e.gfdl.90$pcpbefore_raw_gridmet
success_projections.forest.rcp85e.gfdl.90$y.dif.pcp_std_gridmet.rcp85e.gfdl.90<- success_projections.forest.rcp85e.gfdl.90$pcpbefore_raw_gridmet.rcp85e.gfdl.90 - success_projections.forest.rcp85e.gfdl.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85e.gfdl.90$y.dif.pcp_std_gridmet.rcp85e.gfdl.90 <- success_projections.natural.open.rcp85e.gfdl.90$pcpbefore_raw_gridmet.rcp85e.gfdl.90 - success_projections.natural.open.rcp85e.gfdl.90$pcpbefore_raw_gridmet
success_projections.human.rcp85e.gfdl.90$y.dif.pcp_std_gridmet.rcp85e.gfdl.90 <- success_projections.human.rcp85e.gfdl.90$pcpbefore_raw_gridmet.rcp85e.gfdl.90 - success_projections.human.rcp85e.gfdl.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85e.gfdl.90.ag <- ggplot(success_projections.ag.rcp85e.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.90.ag

g.pcp.y.dif.rcp85e.gfdl.90.forest <- ggplot(success_projections.forest.rcp85e.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.90.forest

g.pcp.y.dif.rcp85e.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp85e.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.90.natural.open

g.pcp.y.dif.rcp85e.gfdl.90.human <- ggplot(success_projections.human.rcp85e.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.gfdl.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.gfdl.90.human

difference.pcp.by.landuse.rcp85e.gfdl.90 <- ggarrange(g.pcp.y.dif.rcp85e.gfdl.90.ag,g.pcp.y.dif.rcp85e.gfdl.90.forest,
                                                      g.pcp.y.dif.rcp85e.gfdl.90.human,g.pcp.y.dif.rcp85e.gfdl.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85e.gfdl.90


#RCP 85m mean gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85m.gfdl.mean <- 0
success_projections$y.fut.rcp85m.gfdl.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.gfdl.mean  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.gfdl.mean )

success_projections$y.now.rcp85m.gfdl.mean<- 0
success_projections$y.now.rcp85m.gfdl.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85m.gfdl.mean <- success_projections$y.fut.rcp85m.gfdl.mean - success_projections$y.now.rcp85m.gfdl.mean


###Scatter plot with x = now y = future
g.now.rcp85m.gfdl.mean <- ggplot(success_projections, aes(x=y.now.rcp85m.gfdl.mean, y = y.fut.rcp85m.gfdl.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 mean gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85m.gfdl.mean


###Density plot of the difference future - now
g.density.y.dif.rcp85m.gfdl.mean <- ggplot(success_projections, aes(x=y.dif.rcp85m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 mean gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85m.gfdl.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85m.gfdl.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85m.gfdl.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85m.gfdl.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85m.gfdl.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85m.gfdl.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85m.gfdl.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85m.gfdl.mean.ag <- ggplot(success_projections.ag.rcp85m.gfdl.mean , aes(x=y.dif.rcp85m.gfdl.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.mean.ag

g.density.y.dif.rcp85m.gfdl.mean.forest <- ggplot(success_projections.forest.rcp85m.gfdl.mean, aes(x=y.dif.rcp85m.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.mean.forest

g.density.y.dif.rcp85m.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp85m.gfdl.mean, aes(x=y.dif.rcp85m.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.mean.natural.open

g.density.y.dif.rcp85m.gfdl.mean.human <- ggplot(success_projections.human.rcp85m.gfdl.mean, aes(x=y.dif.rcp85m.gfdl.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.mean.human

difference.density.by.landuse.rcp85m.gfdl.mean <- ggarrange(g.density.y.dif.rcp85m.gfdl.mean.ag,g.density.y.dif.rcp85m.gfdl.mean.forest,
                                                            g.density.y.dif.rcp85m.gfdl.mean.human,g.density.y.dif.rcp85m.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85m.gfdl.mean


####Difference of temperatures within land uses
success_projections.ag.rcp85m.gfdl.mean$y.dif.Tmax_std_gridmet.rcp85m.gfdl.mean <- success_projections.ag.rcp85m.gfdl.mean$Tmax_std_gridmet.rcp85m.gfdl.mean - success_projections.ag.rcp85m.gfdl.mean$Tmax_std_gridmet
success_projections.forest.rcp85m.gfdl.mean$y.dif.Tmax_std_gridmet.rcp85m.gfdl.mean<- success_projections.forest.rcp85m.gfdl.mean$Tmax_std_gridmet.rcp85m.gfdl.mean - success_projections.forest.rcp85m.gfdl.mean$Tmax_std_gridmet
success_projections.natural.open.rcp85m.gfdl.mean$y.dif.Tmax_std_gridmet.rcp85m.gfdl.mean <- success_projections.natural.open.rcp85m.gfdl.mean$Tmax_std_gridmet.rcp85m.gfdl.mean - success_projections.natural.open.rcp85m.gfdl.mean$Tmax_std_gridmet
success_projections.human.rcp85m.gfdl.mean$y.dif.Tmax_std_gridmet.rcp85m.gfdl.mean <- success_projections.human.rcp85m.gfdl.mean$Tmax_std_gridmet.rcp85m.gfdl.mean - success_projections.human.rcp85m.gfdl.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85m.gfdl.mean.ag <- ggplot(success_projections.ag.rcp85m.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.mean.ag

g.Tmax.y.dif.rcp85m.gfdl.mean.forest <- ggplot(success_projections.forest.rcp85m.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.mean.forest

g.Tmax.y.dif.rcp85m.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp85m.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.mean.natural.open

g.Tmax.y.dif.rcp85m.gfdl.mean.human <- ggplot(success_projections.human.rcp85m.gfdl.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.mean.human

difference.Tmax.by.landuse.rcp85m.gfdl.mean <- ggarrange(g.Tmax.y.dif.rcp85m.gfdl.mean.ag,g.Tmax.y.dif.rcp85m.gfdl.mean.forest,
                                                         g.Tmax.y.dif.rcp85m.gfdl.mean.human,g.Tmax.y.dif.rcp85m.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85m.gfdl.mean



####Difference of precipitation within land uses
success_projections.ag.rcp85m.gfdl.mean$y.dif.pcp_std_gridmet.rcp85m.gfdl.mean <- success_projections.ag.rcp85m.gfdl.mean$pcpbefore_raw_gridmet.rcp85m.gfdl.mean - success_projections.ag.rcp85m.gfdl.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp85m.gfdl.mean$y.dif.pcp_std_gridmet.rcp85m.gfdl.mean<- success_projections.forest.rcp85m.gfdl.mean$pcpbefore_raw_gridmet.rcp85m.gfdl.mean - success_projections.forest.rcp85m.gfdl.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85m.gfdl.mean$y.dif.pcp_std_gridmet.rcp85m.gfdl.mean <- success_projections.natural.open.rcp85m.gfdl.mean$pcpbefore_raw_gridmet.rcp85m.gfdl.mean - success_projections.natural.open.rcp85m.gfdl.mean$pcpbefore_raw_gridmet
success_projections.human.rcp85m.gfdl.mean$y.dif.pcp_std_gridmet.rcp85m.gfdl.mean <- success_projections.human.rcp85m.gfdl.mean$pcpbefore_raw_gridmet.rcp85m.gfdl.mean - success_projections.human.rcp85m.gfdl.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85m.gfdl.mean.ag <- ggplot(success_projections.ag.rcp85m.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.mean.ag

g.pcp.y.dif.rcp85m.gfdl.mean.forest <- ggplot(success_projections.forest.rcp85m.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.mean.forest

g.pcp.y.dif.rcp85m.gfdl.mean.natural.open <- ggplot(success_projections.natural.open.rcp85m.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.mean.natural.open

g.pcp.y.dif.rcp85m.gfdl.mean.human <- ggplot(success_projections.human.rcp85m.gfdl.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.mean.human

difference.pcp.by.landuse.rcp85m.gfdl.mean <- ggarrange(g.pcp.y.dif.rcp85m.gfdl.mean.ag,g.pcp.y.dif.rcp85m.gfdl.mean.forest,
                                                        g.pcp.y.dif.rcp85m.gfdl.mean.human,g.pcp.y.dif.rcp85m.gfdl.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85m.gfdl.mean

#RCP 85e 10% gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85m.gfdl.10 <- 0
success_projections$y.fut.rcp85m.gfdl.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.gfdl.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.gfdl.10  + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.gfdl.10 )

success_projections$y.now.rcp85m.gfdl.10<- 0
success_projections$y.now.rcp85m.gfdl.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85m.gfdl.10 <- success_projections$y.fut.rcp85m.gfdl.10 - success_projections$y.now.rcp85m.gfdl.10


###Scatter plot with x = now y = future
g.now.rcp85m.gfdl.10 <- ggplot(success_projections, aes(x=y.now.rcp85m.gfdl.10, y = y.fut.rcp85m.gfdl.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 10% gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85m.gfdl.10


###Density plot of the difference future - now
g.density.y.dif.rcp85m.gfdl.10 <- ggplot(success_projections, aes(x=y.dif.rcp85m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 10% gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85m.gfdl.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85m.gfdl.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85m.gfdl.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85m.gfdl.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85m.gfdl.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85m.gfdl.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85m.gfdl.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85m.gfdl.10.ag <- ggplot(success_projections.ag.rcp85m.gfdl.10 , aes(x=y.dif.rcp85m.gfdl.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.10.ag

g.density.y.dif.rcp85m.gfdl.10.forest <- ggplot(success_projections.forest.rcp85m.gfdl.10, aes(x=y.dif.rcp85m.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.10.forest

g.density.y.dif.rcp85m.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp85m.gfdl.10, aes(x=y.dif.rcp85m.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.10.natural.open

g.density.y.dif.rcp85m.gfdl.10.human <- ggplot(success_projections.human.rcp85m.gfdl.10, aes(x=y.dif.rcp85m.gfdl.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.10.human

difference.density.by.landuse.rcp85m.gfdl.10 <- ggarrange(g.density.y.dif.rcp85m.gfdl.10.ag,g.density.y.dif.rcp85m.gfdl.10.forest,
                                                          g.density.y.dif.rcp85m.gfdl.10.human,g.density.y.dif.rcp85m.gfdl.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85m.gfdl.10


####Difference of temperatures within land uses
success_projections.ag.rcp85m.gfdl.10$y.dif.Tmax_std_gridmet.rcp85m.gfdl.10 <- success_projections.ag.rcp85m.gfdl.10$Tmax_std_gridmet.rcp85m.gfdl.10 - success_projections.ag.rcp85m.gfdl.10$Tmax_std_gridmet
success_projections.forest.rcp85m.gfdl.10$y.dif.Tmax_std_gridmet.rcp85m.gfdl.10<- success_projections.forest.rcp85m.gfdl.10$Tmax_std_gridmet.rcp85m.gfdl.10 - success_projections.forest.rcp85m.gfdl.10$Tmax_std_gridmet
success_projections.natural.open.rcp85m.gfdl.10$y.dif.Tmax_std_gridmet.rcp85m.gfdl.10 <- success_projections.natural.open.rcp85m.gfdl.10$Tmax_std_gridmet.rcp85m.gfdl.10 - success_projections.natural.open.rcp85m.gfdl.10$Tmax_std_gridmet
success_projections.human.rcp85m.gfdl.10$y.dif.Tmax_std_gridmet.rcp85m.gfdl.10 <- success_projections.human.rcp85m.gfdl.10$Tmax_std_gridmet.rcp85m.gfdl.10 - success_projections.human.rcp85m.gfdl.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85m.gfdl.10.ag <- ggplot(success_projections.ag.rcp85m.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.10.ag

g.Tmax.y.dif.rcp85m.gfdl.10.forest <- ggplot(success_projections.forest.rcp85m.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.10.forest

g.Tmax.y.dif.rcp85m.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp85m.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.10.natural.open

g.Tmax.y.dif.rcp85m.gfdl.10.human <- ggplot(success_projections.human.rcp85m.gfdl.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.10.human

difference.Tmax.by.landuse.rcp85m.gfdl.10 <- ggarrange(g.Tmax.y.dif.rcp85m.gfdl.10.ag,g.Tmax.y.dif.rcp85m.gfdl.10.forest,
                                                       g.Tmax.y.dif.rcp85m.gfdl.10.human,g.Tmax.y.dif.rcp85m.gfdl.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85m.gfdl.10



####Difference of precipitation within land uses
success_projections.ag.rcp85m.gfdl.10$y.dif.pcp_std_gridmet.rcp85m.gfdl.10 <- success_projections.ag.rcp85m.gfdl.10$pcpbefore_raw_gridmet.rcp85m.gfdl.10 - success_projections.ag.rcp85m.gfdl.10$pcpbefore_raw_gridmet
success_projections.forest.rcp85m.gfdl.10$y.dif.pcp_std_gridmet.rcp85m.gfdl.10<- success_projections.forest.rcp85m.gfdl.10$pcpbefore_raw_gridmet.rcp85m.gfdl.10 - success_projections.forest.rcp85m.gfdl.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85m.gfdl.10$y.dif.pcp_std_gridmet.rcp85m.gfdl.10 <- success_projections.natural.open.rcp85m.gfdl.10$pcpbefore_raw_gridmet.rcp85m.gfdl.10 - success_projections.natural.open.rcp85m.gfdl.10$pcpbefore_raw_gridmet
success_projections.human.rcp85m.gfdl.10$y.dif.pcp_std_gridmet.rcp85m.gfdl.10 <- success_projections.human.rcp85m.gfdl.10$pcpbefore_raw_gridmet.rcp85m.gfdl.10 - success_projections.human.rcp85m.gfdl.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85m.gfdl.10.ag <- ggplot(success_projections.ag.rcp85m.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.10.ag

g.pcp.y.dif.rcp85m.gfdl.10.forest <- ggplot(success_projections.forest.rcp85m.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.10.forest

g.pcp.y.dif.rcp85m.gfdl.10.natural.open <- ggplot(success_projections.natural.open.rcp85m.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.10.natural.open

g.pcp.y.dif.rcp85m.gfdl.10.human <- ggplot(success_projections.human.rcp85m.gfdl.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.10.human

difference.pcp.by.landuse.rcp85m.gfdl.10 <- ggarrange(g.pcp.y.dif.rcp85m.gfdl.10.ag,g.pcp.y.dif.rcp85m.gfdl.10.forest,
                                                      g.pcp.y.dif.rcp85m.gfdl.10.human,g.pcp.y.dif.rcp85m.gfdl.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85m.gfdl.10

#RCP 85e 90% gfdl-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85m.gfdl.90 <- 0
success_projections$y.fut.rcp85m.gfdl.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.gfdl.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.gfdl.90  + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.gfdl.90 )

success_projections$y.now.rcp85m.gfdl.90<- 0
success_projections$y.now.rcp85m.gfdl.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85m.gfdl.90 <- success_projections$y.fut.rcp85m.gfdl.90 - success_projections$y.now.rcp85m.gfdl.90


###Scatter plot with x = now y = future
g.now.rcp85m.gfdl.90 <- ggplot(success_projections, aes(x=y.now.rcp85m.gfdl.90, y = y.fut.rcp85m.gfdl.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 90% gfdl")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85m.gfdl.90


###Density plot of the difference future - now
g.density.y.dif.rcp85m.gfdl.90 <- ggplot(success_projections, aes(x=y.dif.rcp85m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 90% gfdl ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85m.gfdl.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85m.gfdl.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85m.gfdl.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85m.gfdl.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85m.gfdl.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85m.gfdl.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85m.gfdl.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85m.gfdl.90.ag <- ggplot(success_projections.ag.rcp85m.gfdl.90 , aes(x=y.dif.rcp85m.gfdl.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.90.ag

g.density.y.dif.rcp85m.gfdl.90.forest <- ggplot(success_projections.forest.rcp85m.gfdl.90, aes(x=y.dif.rcp85m.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.90.forest

g.density.y.dif.rcp85m.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp85m.gfdl.90, aes(x=y.dif.rcp85m.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.90.natural.open

g.density.y.dif.rcp85m.gfdl.90.human <- ggplot(success_projections.human.rcp85m.gfdl.90, aes(x=y.dif.rcp85m.gfdl.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.gfdl.90.human

difference.density.by.landuse.rcp85m.gfdl.90 <- ggarrange(g.density.y.dif.rcp85m.gfdl.90.ag,g.density.y.dif.rcp85m.gfdl.90.forest,
                                                          g.density.y.dif.rcp85m.gfdl.90.human,g.density.y.dif.rcp85m.gfdl.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85m.gfdl.90


####Difference of temperatures within land uses
success_projections.ag.rcp85m.gfdl.90$y.dif.Tmax_std_gridmet.rcp85m.gfdl.90 <- success_projections.ag.rcp85m.gfdl.90$Tmax_std_gridmet.rcp85m.gfdl.90 - success_projections.ag.rcp85m.gfdl.90$Tmax_std_gridmet
success_projections.forest.rcp85m.gfdl.90$y.dif.Tmax_std_gridmet.rcp85m.gfdl.90<- success_projections.forest.rcp85m.gfdl.90$Tmax_std_gridmet.rcp85m.gfdl.90 - success_projections.forest.rcp85m.gfdl.90$Tmax_std_gridmet
success_projections.natural.open.rcp85m.gfdl.90$y.dif.Tmax_std_gridmet.rcp85m.gfdl.90 <- success_projections.natural.open.rcp85m.gfdl.90$Tmax_std_gridmet.rcp85m.gfdl.90 - success_projections.natural.open.rcp85m.gfdl.90$Tmax_std_gridmet
success_projections.human.rcp85m.gfdl.90$y.dif.Tmax_std_gridmet.rcp85m.gfdl.90 <- success_projections.human.rcp85m.gfdl.90$Tmax_std_gridmet.rcp85m.gfdl.90 - success_projections.human.rcp85m.gfdl.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85m.gfdl.90.ag <- ggplot(success_projections.ag.rcp85m.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.90.ag

g.Tmax.y.dif.rcp85m.gfdl.90.forest <- ggplot(success_projections.forest.rcp85m.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.90.forest

g.Tmax.y.dif.rcp85m.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp85m.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.90.natural.open

g.Tmax.y.dif.rcp85m.gfdl.90.human <- ggplot(success_projections.human.rcp85m.gfdl.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.gfdl.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.gfdl.90.human

difference.Tmax.by.landuse.rcp85m.gfdl.90 <- ggarrange(g.Tmax.y.dif.rcp85m.gfdl.90.ag,g.Tmax.y.dif.rcp85m.gfdl.90.forest,
                                                       g.Tmax.y.dif.rcp85m.gfdl.90.human,g.Tmax.y.dif.rcp85m.gfdl.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85m.gfdl.90



####Difference of precipitation within land uses
success_projections.ag.rcp85m.gfdl.90$y.dif.pcp_std_gridmet.rcp85m.gfdl.90 <- success_projections.ag.rcp85m.gfdl.90$pcpbefore_raw_gridmet.rcp85m.gfdl.90 - success_projections.ag.rcp85m.gfdl.90$pcpbefore_raw_gridmet
success_projections.forest.rcp85m.gfdl.90$y.dif.pcp_std_gridmet.rcp85m.gfdl.90<- success_projections.forest.rcp85m.gfdl.90$pcpbefore_raw_gridmet.rcp85m.gfdl.90 - success_projections.forest.rcp85m.gfdl.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85m.gfdl.90$y.dif.pcp_std_gridmet.rcp85m.gfdl.90 <- success_projections.natural.open.rcp85m.gfdl.90$pcpbefore_raw_gridmet.rcp85m.gfdl.90 - success_projections.natural.open.rcp85m.gfdl.90$pcpbefore_raw_gridmet
success_projections.human.rcp85m.gfdl.90$y.dif.pcp_std_gridmet.rcp85m.gfdl.90 <- success_projections.human.rcp85m.gfdl.90$pcpbefore_raw_gridmet.rcp85m.gfdl.90 - success_projections.human.rcp85m.gfdl.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85m.gfdl.90.ag <- ggplot(success_projections.ag.rcp85m.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.90.ag

g.pcp.y.dif.rcp85m.gfdl.90.forest <- ggplot(success_projections.forest.rcp85m.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.90.forest

g.pcp.y.dif.rcp85m.gfdl.90.natural.open <- ggplot(success_projections.natural.open.rcp85m.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.90.natural.open

g.pcp.y.dif.rcp85m.gfdl.90.human <- ggplot(success_projections.human.rcp85m.gfdl.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.gfdl.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.gfdl.90.human

difference.pcp.by.landuse.rcp85m.gfdl.90 <- ggarrange(g.pcp.y.dif.rcp85m.gfdl.90.ag,g.pcp.y.dif.rcp85m.gfdl.90.forest,
                                                      g.pcp.y.dif.rcp85m.gfdl.90.human,g.pcp.y.dif.rcp85m.gfdl.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85m.gfdl.90









####MRI
#RCP 45e mean mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45e.mri.mean <- 0
success_projections$y.fut.rcp45e.mri.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.mri.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.mri.mean  + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.mri.mean )

success_projections$y.now.rcp45e.mri.mean<- 0
success_projections$y.now.rcp45e.mri.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45e.mri.mean <- success_projections$y.fut.rcp45e.mri.mean - success_projections$y.now.rcp45e.mri.mean


###Scatter plot with x = now y = future
g.now.rcp45e.mri.mean <- ggplot(success_projections, aes(x=y.now.rcp45e.mri.mean, y = y.fut.rcp45e.mri.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 mean mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45e.mri.mean


###Density plot of the difference future - now
g.density.y.dif.rcp45e.mri.mean <- ggplot(success_projections, aes(x=y.dif.rcp45e.mri.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 mean mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45e.mri.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45e.mri.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45e.mri.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45e.mri.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45e.mri.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45e.mri.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45e.mri.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45e.mri.mean.ag <- ggplot(success_projections.ag.rcp45e.mri.mean , aes(x=y.dif.rcp45e.mri.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.mean.ag

g.density.y.dif.rcp45e.mri.mean.forest <- ggplot(success_projections.forest.rcp45e.mri.mean, aes(x=y.dif.rcp45e.mri.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.mean.forest

g.density.y.dif.rcp45e.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp45e.mri.mean, aes(x=y.dif.rcp45e.mri.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.mean.natural.open

g.density.y.dif.rcp45e.mri.mean.human <- ggplot(success_projections.human.rcp45e.mri.mean, aes(x=y.dif.rcp45e.mri.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.mean.human

difference.density.by.landuse.rcp45e.mri.mean <- ggarrange(g.density.y.dif.rcp45e.mri.mean.ag,g.density.y.dif.rcp45e.mri.mean.forest,
                                                           g.density.y.dif.rcp45e.mri.mean.human,g.density.y.dif.rcp45e.mri.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45e.mri.mean


####Difference of temperatures within land uses
success_projections.ag.rcp45e.mri.mean$y.dif.Tmax_std_gridmet.rcp45e.mri.mean <- success_projections.ag.rcp45e.mri.mean$Tmax_std_gridmet.rcp45e.mri.mean - success_projections.ag.rcp45e.mri.mean$Tmax_std_gridmet
success_projections.forest.rcp45e.mri.mean$y.dif.Tmax_std_gridmet.rcp45e.mri.mean<- success_projections.forest.rcp45e.mri.mean$Tmax_std_gridmet.rcp45e.mri.mean - success_projections.forest.rcp45e.mri.mean$Tmax_std_gridmet
success_projections.natural.open.rcp45e.mri.mean$y.dif.Tmax_std_gridmet.rcp45e.mri.mean <- success_projections.natural.open.rcp45e.mri.mean$Tmax_std_gridmet.rcp45e.mri.mean - success_projections.natural.open.rcp45e.mri.mean$Tmax_std_gridmet
success_projections.human.rcp45e.mri.mean$y.dif.Tmax_std_gridmet.rcp45e.mri.mean <- success_projections.human.rcp45e.mri.mean$Tmax_std_gridmet.rcp45e.mri.mean - success_projections.human.rcp45e.mri.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45e.mri.mean.ag <- ggplot(success_projections.ag.rcp45e.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.mean.ag

g.Tmax.y.dif.rcp45e.mri.mean.forest <- ggplot(success_projections.forest.rcp45e.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.mean.forest

g.Tmax.y.dif.rcp45e.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp45e.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.mean.natural.open

g.Tmax.y.dif.rcp45e.mri.mean.human <- ggplot(success_projections.human.rcp45e.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.mean.human

difference.Tmax.by.landuse.rcp45e.mri.mean <- ggarrange(g.Tmax.y.dif.rcp45e.mri.mean.ag,g.Tmax.y.dif.rcp45e.mri.mean.forest,
                                                        g.Tmax.y.dif.rcp45e.mri.mean.human,g.Tmax.y.dif.rcp45e.mri.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45e.mri.mean



####Difference of precipitation within land uses
success_projections.ag.rcp45e.mri.mean$y.dif.pcp_std_gridmet.rcp45e.mri.mean <- success_projections.ag.rcp45e.mri.mean$pcpbefore_raw_gridmet.rcp45e.mri.mean - success_projections.ag.rcp45e.mri.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp45e.mri.mean$y.dif.pcp_std_gridmet.rcp45e.mri.mean<- success_projections.forest.rcp45e.mri.mean$pcpbefore_raw_gridmet.rcp45e.mri.mean - success_projections.forest.rcp45e.mri.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45e.mri.mean$y.dif.pcp_std_gridmet.rcp45e.mri.mean <- success_projections.natural.open.rcp45e.mri.mean$pcpbefore_raw_gridmet.rcp45e.mri.mean - success_projections.natural.open.rcp45e.mri.mean$pcpbefore_raw_gridmet
success_projections.human.rcp45e.mri.mean$y.dif.pcp_std_gridmet.rcp45e.mri.mean <- success_projections.human.rcp45e.mri.mean$pcpbefore_raw_gridmet.rcp45e.mri.mean - success_projections.human.rcp45e.mri.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45e.mri.mean.ag <- ggplot(success_projections.ag.rcp45e.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.mean.ag

g.pcp.y.dif.rcp45e.mri.mean.forest <- ggplot(success_projections.forest.rcp45e.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.mean.forest

g.pcp.y.dif.rcp45e.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp45e.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.mean.natural.open

g.pcp.y.dif.rcp45e.mri.mean.human <- ggplot(success_projections.human.rcp45e.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.mean.human

difference.pcp.by.landuse.rcp45e.mri.mean <- ggarrange(g.pcp.y.dif.rcp45e.mri.mean.ag,g.pcp.y.dif.rcp45e.mri.mean.forest,
                                                       g.pcp.y.dif.rcp45e.mri.mean.human,g.pcp.y.dif.rcp45e.mri.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45e.mri.mean

#RCP 45e 10% mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45e.mri.10 <- 0
success_projections$y.fut.rcp45e.mri.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.mri.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.mri.10  + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.mri.10 )

success_projections$y.now.rcp45e.mri.10<- 0
success_projections$y.now.rcp45e.mri.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                       NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                       substrate*success_projections$substrate_binary +
                                                       success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45e.mri.10 <- success_projections$y.fut.rcp45e.mri.10 - success_projections$y.now.rcp45e.mri.10


###Scatter plot with x = now y = future
g.now.rcp45e.mri.10 <- ggplot(success_projections, aes(x=y.now.rcp45e.mri.10, y = y.fut.rcp45e.mri.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 10% mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45e.mri.10


###Density plot of the difference future - now
g.density.y.dif.rcp45e.mri.10 <- ggplot(success_projections, aes(x=y.dif.rcp45e.mri.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 10% mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45e.mri.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45e.mri.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45e.mri.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45e.mri.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45e.mri.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45e.mri.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45e.mri.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45e.mri.10.ag <- ggplot(success_projections.ag.rcp45e.mri.10 , aes(x=y.dif.rcp45e.mri.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.10.ag

g.density.y.dif.rcp45e.mri.10.forest <- ggplot(success_projections.forest.rcp45e.mri.10, aes(x=y.dif.rcp45e.mri.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.10.forest

g.density.y.dif.rcp45e.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp45e.mri.10, aes(x=y.dif.rcp45e.mri.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.10.natural.open

g.density.y.dif.rcp45e.mri.10.human <- ggplot(success_projections.human.rcp45e.mri.10, aes(x=y.dif.rcp45e.mri.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.10.human

difference.density.by.landuse.rcp45e.mri.10 <- ggarrange(g.density.y.dif.rcp45e.mri.10.ag,g.density.y.dif.rcp45e.mri.10.forest,
                                                         g.density.y.dif.rcp45e.mri.10.human,g.density.y.dif.rcp45e.mri.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45e.mri.10


####Difference of temperatures within land uses
success_projections.ag.rcp45e.mri.10$y.dif.Tmax_std_gridmet.rcp45e.mri.10 <- success_projections.ag.rcp45e.mri.10$Tmax_std_gridmet.rcp45e.mri.10 - success_projections.ag.rcp45e.mri.10$Tmax_std_gridmet
success_projections.forest.rcp45e.mri.10$y.dif.Tmax_std_gridmet.rcp45e.mri.10<- success_projections.forest.rcp45e.mri.10$Tmax_std_gridmet.rcp45e.mri.10 - success_projections.forest.rcp45e.mri.10$Tmax_std_gridmet
success_projections.natural.open.rcp45e.mri.10$y.dif.Tmax_std_gridmet.rcp45e.mri.10 <- success_projections.natural.open.rcp45e.mri.10$Tmax_std_gridmet.rcp45e.mri.10 - success_projections.natural.open.rcp45e.mri.10$Tmax_std_gridmet
success_projections.human.rcp45e.mri.10$y.dif.Tmax_std_gridmet.rcp45e.mri.10 <- success_projections.human.rcp45e.mri.10$Tmax_std_gridmet.rcp45e.mri.10 - success_projections.human.rcp45e.mri.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45e.mri.10.ag <- ggplot(success_projections.ag.rcp45e.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.10.ag

g.Tmax.y.dif.rcp45e.mri.10.forest <- ggplot(success_projections.forest.rcp45e.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.10.forest

g.Tmax.y.dif.rcp45e.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp45e.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.10.natural.open

g.Tmax.y.dif.rcp45e.mri.10.human <- ggplot(success_projections.human.rcp45e.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.10.human

difference.Tmax.by.landuse.rcp45e.mri.10 <- ggarrange(g.Tmax.y.dif.rcp45e.mri.10.ag,g.Tmax.y.dif.rcp45e.mri.10.forest,
                                                      g.Tmax.y.dif.rcp45e.mri.10.human,g.Tmax.y.dif.rcp45e.mri.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45e.mri.10



####Difference of precipitation within land uses
success_projections.ag.rcp45e.mri.10$y.dif.pcp_std_gridmet.rcp45e.mri.10 <- success_projections.ag.rcp45e.mri.10$pcpbefore_raw_gridmet.rcp45e.mri.10 - success_projections.ag.rcp45e.mri.10$pcpbefore_raw_gridmet
success_projections.forest.rcp45e.mri.10$y.dif.pcp_std_gridmet.rcp45e.mri.10<- success_projections.forest.rcp45e.mri.10$pcpbefore_raw_gridmet.rcp45e.mri.10 - success_projections.forest.rcp45e.mri.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45e.mri.10$y.dif.pcp_std_gridmet.rcp45e.mri.10 <- success_projections.natural.open.rcp45e.mri.10$pcpbefore_raw_gridmet.rcp45e.mri.10 - success_projections.natural.open.rcp45e.mri.10$pcpbefore_raw_gridmet
success_projections.human.rcp45e.mri.10$y.dif.pcp_std_gridmet.rcp45e.mri.10 <- success_projections.human.rcp45e.mri.10$pcpbefore_raw_gridmet.rcp45e.mri.10 - success_projections.human.rcp45e.mri.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45e.mri.10.ag <- ggplot(success_projections.ag.rcp45e.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.10.ag

g.pcp.y.dif.rcp45e.mri.10.forest <- ggplot(success_projections.forest.rcp45e.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.10.forest

g.pcp.y.dif.rcp45e.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp45e.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.10.natural.open

g.pcp.y.dif.rcp45e.mri.10.human <- ggplot(success_projections.human.rcp45e.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.10.human

difference.pcp.by.landuse.rcp45e.mri.10 <- ggarrange(g.pcp.y.dif.rcp45e.mri.10.ag,g.pcp.y.dif.rcp45e.mri.10.forest,
                                                     g.pcp.y.dif.rcp45e.mri.10.human,g.pcp.y.dif.rcp45e.mri.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45e.mri.10

#RCP 45e 90% mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45e.mri.90 <- 0
success_projections$y.fut.rcp45e.mri.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.mri.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.mri.90  + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.mri.90 )

success_projections$y.now.rcp45e.mri.90<- 0
success_projections$y.now.rcp45e.mri.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                       NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                       substrate*success_projections$substrate_binary +
                                                       success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45e.mri.90 <- success_projections$y.fut.rcp45e.mri.90 - success_projections$y.now.rcp45e.mri.90


###Scatter plot with x = now y = future
g.now.rcp45e.mri.90 <- ggplot(success_projections, aes(x=y.now.rcp45e.mri.90, y = y.fut.rcp45e.mri.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 90% mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45e.mri.90


###Density plot of the difference future - now
g.density.y.dif.rcp45e.mri.90 <- ggplot(success_projections, aes(x=y.dif.rcp45e.mri.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 90% mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45e.mri.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45e.mri.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45e.mri.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45e.mri.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45e.mri.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45e.mri.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45e.mri.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45e.mri.90.ag <- ggplot(success_projections.ag.rcp45e.mri.90 , aes(x=y.dif.rcp45e.mri.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.90.ag

g.density.y.dif.rcp45e.mri.90.forest <- ggplot(success_projections.forest.rcp45e.mri.90, aes(x=y.dif.rcp45e.mri.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.90.forest

g.density.y.dif.rcp45e.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp45e.mri.90, aes(x=y.dif.rcp45e.mri.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.90.natural.open

g.density.y.dif.rcp45e.mri.90.human <- ggplot(success_projections.human.rcp45e.mri.90, aes(x=y.dif.rcp45e.mri.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.mri.90.human

difference.density.by.landuse.rcp45e.mri.90 <- ggarrange(g.density.y.dif.rcp45e.mri.90.ag,g.density.y.dif.rcp45e.mri.90.forest,
                                                         g.density.y.dif.rcp45e.mri.90.human,g.density.y.dif.rcp45e.mri.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45e.mri.90


####Difference of temperatures within land uses
success_projections.ag.rcp45e.mri.90$y.dif.Tmax_std_gridmet.rcp45e.mri.90 <- success_projections.ag.rcp45e.mri.90$Tmax_std_gridmet.rcp45e.mri.90 - success_projections.ag.rcp45e.mri.90$Tmax_std_gridmet
success_projections.forest.rcp45e.mri.90$y.dif.Tmax_std_gridmet.rcp45e.mri.90<- success_projections.forest.rcp45e.mri.90$Tmax_std_gridmet.rcp45e.mri.90 - success_projections.forest.rcp45e.mri.90$Tmax_std_gridmet
success_projections.natural.open.rcp45e.mri.90$y.dif.Tmax_std_gridmet.rcp45e.mri.90 <- success_projections.natural.open.rcp45e.mri.90$Tmax_std_gridmet.rcp45e.mri.90 - success_projections.natural.open.rcp45e.mri.90$Tmax_std_gridmet
success_projections.human.rcp45e.mri.90$y.dif.Tmax_std_gridmet.rcp45e.mri.90 <- success_projections.human.rcp45e.mri.90$Tmax_std_gridmet.rcp45e.mri.90 - success_projections.human.rcp45e.mri.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45e.mri.90.ag <- ggplot(success_projections.ag.rcp45e.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.90.ag

g.Tmax.y.dif.rcp45e.mri.90.forest <- ggplot(success_projections.forest.rcp45e.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.90.forest

g.Tmax.y.dif.rcp45e.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp45e.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.90.natural.open

g.Tmax.y.dif.rcp45e.mri.90.human <- ggplot(success_projections.human.rcp45e.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.mri.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.mri.90.human

difference.Tmax.by.landuse.rcp45e.mri.90 <- ggarrange(g.Tmax.y.dif.rcp45e.mri.90.ag,g.Tmax.y.dif.rcp45e.mri.90.forest,
                                                      g.Tmax.y.dif.rcp45e.mri.90.human,g.Tmax.y.dif.rcp45e.mri.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45e.mri.90



####Difference of precipitation within land uses
success_projections.ag.rcp45e.mri.90$y.dif.pcp_std_gridmet.rcp45e.mri.90 <- success_projections.ag.rcp45e.mri.90$pcpbefore_raw_gridmet.rcp45e.mri.90 - success_projections.ag.rcp45e.mri.90$pcpbefore_raw_gridmet
success_projections.forest.rcp45e.mri.90$y.dif.pcp_std_gridmet.rcp45e.mri.90<- success_projections.forest.rcp45e.mri.90$pcpbefore_raw_gridmet.rcp45e.mri.90 - success_projections.forest.rcp45e.mri.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45e.mri.90$y.dif.pcp_std_gridmet.rcp45e.mri.90 <- success_projections.natural.open.rcp45e.mri.90$pcpbefore_raw_gridmet.rcp45e.mri.90 - success_projections.natural.open.rcp45e.mri.90$pcpbefore_raw_gridmet
success_projections.human.rcp45e.mri.90$y.dif.pcp_std_gridmet.rcp45e.mri.90 <- success_projections.human.rcp45e.mri.90$pcpbefore_raw_gridmet.rcp45e.mri.90 - success_projections.human.rcp45e.mri.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45e.mri.90.ag <- ggplot(success_projections.ag.rcp45e.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.90.ag

g.pcp.y.dif.rcp45e.mri.90.forest <- ggplot(success_projections.forest.rcp45e.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.90.forest

g.pcp.y.dif.rcp45e.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp45e.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.90.natural.open

g.pcp.y.dif.rcp45e.mri.90.human <- ggplot(success_projections.human.rcp45e.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.mri.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.mri.90.human

difference.pcp.by.landuse.rcp45e.mri.90 <- ggarrange(g.pcp.y.dif.rcp45e.mri.90.ag,g.pcp.y.dif.rcp45e.mri.90.forest,
                                                     g.pcp.y.dif.rcp45e.mri.90.human,g.pcp.y.dif.rcp45e.mri.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45e.mri.90


#RCP 45m mean mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45m.mri.mean <- 0
success_projections$y.fut.rcp45m.mri.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.mri.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.mri.mean  + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.mri.mean )

success_projections$y.now.rcp45m.mri.mean<- 0
success_projections$y.now.rcp45m.mri.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45m.mri.mean <- success_projections$y.fut.rcp45m.mri.mean - success_projections$y.now.rcp45m.mri.mean


###Scatter plot with x = now y = future
g.now.rcp45m.mri.mean <- ggplot(success_projections, aes(x=y.now.rcp45m.mri.mean, y = y.fut.rcp45m.mri.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 mean mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45m.mri.mean


###Density plot of the difference future - now
g.density.y.dif.rcp45m.mri.mean <- ggplot(success_projections, aes(x=y.dif.rcp45m.mri.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 mean mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45m.mri.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45m.mri.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45m.mri.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45m.mri.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45m.mri.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45m.mri.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45m.mri.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45m.mri.mean.ag <- ggplot(success_projections.ag.rcp45m.mri.mean , aes(x=y.dif.rcp45m.mri.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.mean.ag

g.density.y.dif.rcp45m.mri.mean.forest <- ggplot(success_projections.forest.rcp45m.mri.mean, aes(x=y.dif.rcp45m.mri.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.mean.forest

g.density.y.dif.rcp45m.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp45m.mri.mean, aes(x=y.dif.rcp45m.mri.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.mean.natural.open

g.density.y.dif.rcp45m.mri.mean.human <- ggplot(success_projections.human.rcp45m.mri.mean, aes(x=y.dif.rcp45m.mri.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.mean.human

difference.density.by.landuse.rcp45m.mri.mean <- ggarrange(g.density.y.dif.rcp45m.mri.mean.ag,g.density.y.dif.rcp45m.mri.mean.forest,
                                                           g.density.y.dif.rcp45m.mri.mean.human,g.density.y.dif.rcp45m.mri.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45m.mri.mean


####Difference of temperatures within land uses
success_projections.ag.rcp45m.mri.mean$y.dif.Tmax_std_gridmet.rcp45m.mri.mean <- success_projections.ag.rcp45m.mri.mean$Tmax_std_gridmet.rcp45m.mri.mean - success_projections.ag.rcp45m.mri.mean$Tmax_std_gridmet
success_projections.forest.rcp45m.mri.mean$y.dif.Tmax_std_gridmet.rcp45m.mri.mean<- success_projections.forest.rcp45m.mri.mean$Tmax_std_gridmet.rcp45m.mri.mean - success_projections.forest.rcp45m.mri.mean$Tmax_std_gridmet
success_projections.natural.open.rcp45m.mri.mean$y.dif.Tmax_std_gridmet.rcp45m.mri.mean <- success_projections.natural.open.rcp45m.mri.mean$Tmax_std_gridmet.rcp45m.mri.mean - success_projections.natural.open.rcp45m.mri.mean$Tmax_std_gridmet
success_projections.human.rcp45m.mri.mean$y.dif.Tmax_std_gridmet.rcp45m.mri.mean <- success_projections.human.rcp45m.mri.mean$Tmax_std_gridmet.rcp45m.mri.mean - success_projections.human.rcp45m.mri.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45m.mri.mean.ag <- ggplot(success_projections.ag.rcp45m.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.mean.ag

g.Tmax.y.dif.rcp45m.mri.mean.forest <- ggplot(success_projections.forest.rcp45m.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.mean.forest

g.Tmax.y.dif.rcp45m.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp45m.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.mean.natural.open

g.Tmax.y.dif.rcp45m.mri.mean.human <- ggplot(success_projections.human.rcp45m.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.mean.human

difference.Tmax.by.landuse.rcp45m.mri.mean <- ggarrange(g.Tmax.y.dif.rcp45m.mri.mean.ag,g.Tmax.y.dif.rcp45m.mri.mean.forest,
                                                        g.Tmax.y.dif.rcp45m.mri.mean.human,g.Tmax.y.dif.rcp45m.mri.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45m.mri.mean



####Difference of precipitation within land uses
success_projections.ag.rcp45m.mri.mean$y.dif.pcp_std_gridmet.rcp45m.mri.mean <- success_projections.ag.rcp45m.mri.mean$pcpbefore_raw_gridmet.rcp45m.mri.mean - success_projections.ag.rcp45m.mri.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp45m.mri.mean$y.dif.pcp_std_gridmet.rcp45m.mri.mean<- success_projections.forest.rcp45m.mri.mean$pcpbefore_raw_gridmet.rcp45m.mri.mean - success_projections.forest.rcp45m.mri.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45m.mri.mean$y.dif.pcp_std_gridmet.rcp45m.mri.mean <- success_projections.natural.open.rcp45m.mri.mean$pcpbefore_raw_gridmet.rcp45m.mri.mean - success_projections.natural.open.rcp45m.mri.mean$pcpbefore_raw_gridmet
success_projections.human.rcp45m.mri.mean$y.dif.pcp_std_gridmet.rcp45m.mri.mean <- success_projections.human.rcp45m.mri.mean$pcpbefore_raw_gridmet.rcp45m.mri.mean - success_projections.human.rcp45m.mri.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45m.mri.mean.ag <- ggplot(success_projections.ag.rcp45m.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.mean.ag

g.pcp.y.dif.rcp45m.mri.mean.forest <- ggplot(success_projections.forest.rcp45m.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.mean.forest

g.pcp.y.dif.rcp45m.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp45m.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.mean.natural.open

g.pcp.y.dif.rcp45m.mri.mean.human <- ggplot(success_projections.human.rcp45m.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.mean.human

difference.pcp.by.landuse.rcp45m.mri.mean <- ggarrange(g.pcp.y.dif.rcp45m.mri.mean.ag,g.pcp.y.dif.rcp45m.mri.mean.forest,
                                                       g.pcp.y.dif.rcp45m.mri.mean.human,g.pcp.y.dif.rcp45m.mri.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45m.mri.mean

#RCP 45m 10% mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45m.mri.10 <- 0
success_projections$y.fut.rcp45m.mri.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.mri.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.mri.10  + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.mri.10 )

success_projections$y.now.rcp45m.mri.10<- 0
success_projections$y.now.rcp45m.mri.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                       NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                       substrate*success_projections$substrate_binary +
                                                       success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45m.mri.10 <- success_projections$y.fut.rcp45m.mri.10 - success_projections$y.now.rcp45m.mri.10


###Scatter plot with x = now y = future
g.now.rcp45m.mri.10 <- ggplot(success_projections, aes(x=y.now.rcp45m.mri.10, y = y.fut.rcp45m.mri.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 10% mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45m.mri.10


###Density plot of the difference future - now
g.density.y.dif.rcp45m.mri.10 <- ggplot(success_projections, aes(x=y.dif.rcp45m.mri.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 10% mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45m.mri.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45m.mri.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45m.mri.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45m.mri.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45m.mri.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45m.mri.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45m.mri.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45m.mri.10.ag <- ggplot(success_projections.ag.rcp45m.mri.10 , aes(x=y.dif.rcp45m.mri.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.10.ag

g.density.y.dif.rcp45m.mri.10.forest <- ggplot(success_projections.forest.rcp45m.mri.10, aes(x=y.dif.rcp45m.mri.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.10.forest

g.density.y.dif.rcp45m.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp45m.mri.10, aes(x=y.dif.rcp45m.mri.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.10.natural.open

g.density.y.dif.rcp45m.mri.10.human <- ggplot(success_projections.human.rcp45m.mri.10, aes(x=y.dif.rcp45m.mri.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.10.human

difference.density.by.landuse.rcp45m.mri.10 <- ggarrange(g.density.y.dif.rcp45m.mri.10.ag,g.density.y.dif.rcp45m.mri.10.forest,
                                                         g.density.y.dif.rcp45m.mri.10.human,g.density.y.dif.rcp45m.mri.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45m.mri.10


####Difference of temperatures within land uses
success_projections.ag.rcp45m.mri.10$y.dif.Tmax_std_gridmet.rcp45m.mri.10 <- success_projections.ag.rcp45m.mri.10$Tmax_std_gridmet.rcp45m.mri.10 - success_projections.ag.rcp45m.mri.10$Tmax_std_gridmet
success_projections.forest.rcp45m.mri.10$y.dif.Tmax_std_gridmet.rcp45m.mri.10<- success_projections.forest.rcp45m.mri.10$Tmax_std_gridmet.rcp45m.mri.10 - success_projections.forest.rcp45m.mri.10$Tmax_std_gridmet
success_projections.natural.open.rcp45m.mri.10$y.dif.Tmax_std_gridmet.rcp45m.mri.10 <- success_projections.natural.open.rcp45m.mri.10$Tmax_std_gridmet.rcp45m.mri.10 - success_projections.natural.open.rcp45m.mri.10$Tmax_std_gridmet
success_projections.human.rcp45m.mri.10$y.dif.Tmax_std_gridmet.rcp45m.mri.10 <- success_projections.human.rcp45m.mri.10$Tmax_std_gridmet.rcp45m.mri.10 - success_projections.human.rcp45m.mri.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45m.mri.10.ag <- ggplot(success_projections.ag.rcp45m.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.10.ag

g.Tmax.y.dif.rcp45m.mri.10.forest <- ggplot(success_projections.forest.rcp45m.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.10.forest

g.Tmax.y.dif.rcp45m.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp45m.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.10.natural.open

g.Tmax.y.dif.rcp45m.mri.10.human <- ggplot(success_projections.human.rcp45m.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.10.human

difference.Tmax.by.landuse.rcp45m.mri.10 <- ggarrange(g.Tmax.y.dif.rcp45m.mri.10.ag,g.Tmax.y.dif.rcp45m.mri.10.forest,
                                                      g.Tmax.y.dif.rcp45m.mri.10.human,g.Tmax.y.dif.rcp45m.mri.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45m.mri.10



####Difference of precipitation within land uses
success_projections.ag.rcp45m.mri.10$y.dif.pcp_std_gridmet.rcp45m.mri.10 <- success_projections.ag.rcp45m.mri.10$pcpbefore_raw_gridmet.rcp45m.mri.10 - success_projections.ag.rcp45m.mri.10$pcpbefore_raw_gridmet
success_projections.forest.rcp45m.mri.10$y.dif.pcp_std_gridmet.rcp45m.mri.10<- success_projections.forest.rcp45m.mri.10$pcpbefore_raw_gridmet.rcp45m.mri.10 - success_projections.forest.rcp45m.mri.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45m.mri.10$y.dif.pcp_std_gridmet.rcp45m.mri.10 <- success_projections.natural.open.rcp45m.mri.10$pcpbefore_raw_gridmet.rcp45m.mri.10 - success_projections.natural.open.rcp45m.mri.10$pcpbefore_raw_gridmet
success_projections.human.rcp45m.mri.10$y.dif.pcp_std_gridmet.rcp45m.mri.10 <- success_projections.human.rcp45m.mri.10$pcpbefore_raw_gridmet.rcp45m.mri.10 - success_projections.human.rcp45m.mri.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45m.mri.10.ag <- ggplot(success_projections.ag.rcp45m.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.10.ag

g.pcp.y.dif.rcp45m.mri.10.forest <- ggplot(success_projections.forest.rcp45m.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.10.forest

g.pcp.y.dif.rcp45m.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp45m.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.10.natural.open

g.pcp.y.dif.rcp45m.mri.10.human <- ggplot(success_projections.human.rcp45m.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.10.human

difference.pcp.by.landuse.rcp45m.mri.10 <- ggarrange(g.pcp.y.dif.rcp45m.mri.10.ag,g.pcp.y.dif.rcp45m.mri.10.forest,
                                                     g.pcp.y.dif.rcp45m.mri.10.human,g.pcp.y.dif.rcp45m.mri.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45m.mri.10

#RCP 45m 90% mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45m.mri.90 <- 0
success_projections$y.fut.rcp45m.mri.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.mri.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.mri.90  + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.mri.90 )

success_projections$y.now.rcp45m.mri.90<- 0
success_projections$y.now.rcp45m.mri.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                       NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                       substrate*success_projections$substrate_binary +
                                                       success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45m.mri.90 <- success_projections$y.fut.rcp45m.mri.90 - success_projections$y.now.rcp45m.mri.90


###Scatter plot with x = now y = future
g.now.rcp45m.mri.90 <- ggplot(success_projections, aes(x=y.now.rcp45m.mri.90, y = y.fut.rcp45m.mri.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 90% mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45m.mri.90


###Density plot of the difference future - now
g.density.y.dif.rcp45m.mri.90 <- ggplot(success_projections, aes(x=y.dif.rcp45m.mri.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 90% mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45m.mri.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45m.mri.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45m.mri.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45m.mri.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45m.mri.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45m.mri.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45m.mri.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45m.mri.90.ag <- ggplot(success_projections.ag.rcp45m.mri.90 , aes(x=y.dif.rcp45m.mri.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.90.ag

g.density.y.dif.rcp45m.mri.90.forest <- ggplot(success_projections.forest.rcp45m.mri.90, aes(x=y.dif.rcp45m.mri.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.90.forest

g.density.y.dif.rcp45m.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp45m.mri.90, aes(x=y.dif.rcp45m.mri.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.90.natural.open

g.density.y.dif.rcp45m.mri.90.human <- ggplot(success_projections.human.rcp45m.mri.90, aes(x=y.dif.rcp45m.mri.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.mri.90.human

difference.density.by.landuse.rcp45m.mri.90 <- ggarrange(g.density.y.dif.rcp45m.mri.90.ag,g.density.y.dif.rcp45m.mri.90.forest,
                                                         g.density.y.dif.rcp45m.mri.90.human,g.density.y.dif.rcp45m.mri.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45m.mri.90


####Difference of temperatures within land uses
success_projections.ag.rcp45m.mri.90$y.dif.Tmax_std_gridmet.rcp45m.mri.90 <- success_projections.ag.rcp45m.mri.90$Tmax_std_gridmet.rcp45m.mri.90 - success_projections.ag.rcp45m.mri.90$Tmax_std_gridmet
success_projections.forest.rcp45m.mri.90$y.dif.Tmax_std_gridmet.rcp45m.mri.90<- success_projections.forest.rcp45m.mri.90$Tmax_std_gridmet.rcp45m.mri.90 - success_projections.forest.rcp45m.mri.90$Tmax_std_gridmet
success_projections.natural.open.rcp45m.mri.90$y.dif.Tmax_std_gridmet.rcp45m.mri.90 <- success_projections.natural.open.rcp45m.mri.90$Tmax_std_gridmet.rcp45m.mri.90 - success_projections.natural.open.rcp45m.mri.90$Tmax_std_gridmet
success_projections.human.rcp45m.mri.90$y.dif.Tmax_std_gridmet.rcp45m.mri.90 <- success_projections.human.rcp45m.mri.90$Tmax_std_gridmet.rcp45m.mri.90 - success_projections.human.rcp45m.mri.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45m.mri.90.ag <- ggplot(success_projections.ag.rcp45m.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.90.ag

g.Tmax.y.dif.rcp45m.mri.90.forest <- ggplot(success_projections.forest.rcp45m.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.90.forest

g.Tmax.y.dif.rcp45m.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp45m.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.90.natural.open

g.Tmax.y.dif.rcp45m.mri.90.human <- ggplot(success_projections.human.rcp45m.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.mri.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.mri.90.human

difference.Tmax.by.landuse.rcp45m.mri.90 <- ggarrange(g.Tmax.y.dif.rcp45m.mri.90.ag,g.Tmax.y.dif.rcp45m.mri.90.forest,
                                                      g.Tmax.y.dif.rcp45m.mri.90.human,g.Tmax.y.dif.rcp45m.mri.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45m.mri.90



####Difference of precipitation within land uses
success_projections.ag.rcp45m.mri.90$y.dif.pcp_std_gridmet.rcp45m.mri.90 <- success_projections.ag.rcp45m.mri.90$pcpbefore_raw_gridmet.rcp45m.mri.90 - success_projections.ag.rcp45m.mri.90$pcpbefore_raw_gridmet
success_projections.forest.rcp45m.mri.90$y.dif.pcp_std_gridmet.rcp45m.mri.90<- success_projections.forest.rcp45m.mri.90$pcpbefore_raw_gridmet.rcp45m.mri.90 - success_projections.forest.rcp45m.mri.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45m.mri.90$y.dif.pcp_std_gridmet.rcp45m.mri.90 <- success_projections.natural.open.rcp45m.mri.90$pcpbefore_raw_gridmet.rcp45m.mri.90 - success_projections.natural.open.rcp45m.mri.90$pcpbefore_raw_gridmet
success_projections.human.rcp45m.mri.90$y.dif.pcp_std_gridmet.rcp45m.mri.90 <- success_projections.human.rcp45m.mri.90$pcpbefore_raw_gridmet.rcp45m.mri.90 - success_projections.human.rcp45m.mri.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45m.mri.90.ag <- ggplot(success_projections.ag.rcp45m.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.90.ag

g.pcp.y.dif.rcp45m.mri.90.forest <- ggplot(success_projections.forest.rcp45m.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.90.forest

g.pcp.y.dif.rcp45m.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp45m.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.90.natural.open

g.pcp.y.dif.rcp45m.mri.90.human <- ggplot(success_projections.human.rcp45m.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.mri.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.mri.90.human

difference.pcp.by.landuse.rcp45m.mri.90 <- ggarrange(g.pcp.y.dif.rcp45m.mri.90.ag,g.pcp.y.dif.rcp45m.mri.90.forest,
                                                     g.pcp.y.dif.rcp45m.mri.90.human,g.pcp.y.dif.rcp45m.mri.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45m.mri.90


#RCP 85e mean mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85e.mri.mean <- 0
success_projections$y.fut.rcp85e.mri.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.mri.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.mri.mean  + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.mri.mean )

success_projections$y.now.rcp85e.mri.mean<- 0
success_projections$y.now.rcp85e.mri.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85e.mri.mean <- success_projections$y.fut.rcp85e.mri.mean - success_projections$y.now.rcp85e.mri.mean


###Scatter plot with x = now y = future
g.now.rcp85e.mri.mean <- ggplot(success_projections, aes(x=y.now.rcp85e.mri.mean, y = y.fut.rcp85e.mri.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 mean mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85e.mri.mean


###Density plot of the difference future - now
g.density.y.dif.rcp85e.mri.mean <- ggplot(success_projections, aes(x=y.dif.rcp85e.mri.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 mean mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85e.mri.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85e.mri.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85e.mri.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85e.mri.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85e.mri.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85e.mri.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85e.mri.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85e.mri.mean.ag <- ggplot(success_projections.ag.rcp85e.mri.mean , aes(x=y.dif.rcp85e.mri.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.mean.ag

g.density.y.dif.rcp85e.mri.mean.forest <- ggplot(success_projections.forest.rcp85e.mri.mean, aes(x=y.dif.rcp85e.mri.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.mean.forest

g.density.y.dif.rcp85e.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp85e.mri.mean, aes(x=y.dif.rcp85e.mri.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.mean.natural.open

g.density.y.dif.rcp85e.mri.mean.human <- ggplot(success_projections.human.rcp85e.mri.mean, aes(x=y.dif.rcp85e.mri.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.mean.human

difference.density.by.landuse.rcp85e.mri.mean <- ggarrange(g.density.y.dif.rcp85e.mri.mean.ag,g.density.y.dif.rcp85e.mri.mean.forest,
                                                           g.density.y.dif.rcp85e.mri.mean.human,g.density.y.dif.rcp85e.mri.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85e.mri.mean


####Difference of temperatures within land uses
success_projections.ag.rcp85e.mri.mean$y.dif.Tmax_std_gridmet.rcp85e.mri.mean <- success_projections.ag.rcp85e.mri.mean$Tmax_std_gridmet.rcp85e.mri.mean - success_projections.ag.rcp85e.mri.mean$Tmax_std_gridmet
success_projections.forest.rcp85e.mri.mean$y.dif.Tmax_std_gridmet.rcp85e.mri.mean<- success_projections.forest.rcp85e.mri.mean$Tmax_std_gridmet.rcp85e.mri.mean - success_projections.forest.rcp85e.mri.mean$Tmax_std_gridmet
success_projections.natural.open.rcp85e.mri.mean$y.dif.Tmax_std_gridmet.rcp85e.mri.mean <- success_projections.natural.open.rcp85e.mri.mean$Tmax_std_gridmet.rcp85e.mri.mean - success_projections.natural.open.rcp85e.mri.mean$Tmax_std_gridmet
success_projections.human.rcp85e.mri.mean$y.dif.Tmax_std_gridmet.rcp85e.mri.mean <- success_projections.human.rcp85e.mri.mean$Tmax_std_gridmet.rcp85e.mri.mean - success_projections.human.rcp85e.mri.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85e.mri.mean.ag <- ggplot(success_projections.ag.rcp85e.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.mean.ag

g.Tmax.y.dif.rcp85e.mri.mean.forest <- ggplot(success_projections.forest.rcp85e.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.mean.forest

g.Tmax.y.dif.rcp85e.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp85e.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.mean.natural.open

g.Tmax.y.dif.rcp85e.mri.mean.human <- ggplot(success_projections.human.rcp85e.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.mean.human

difference.Tmax.by.landuse.rcp85e.mri.mean <- ggarrange(g.Tmax.y.dif.rcp85e.mri.mean.ag,g.Tmax.y.dif.rcp85e.mri.mean.forest,
                                                        g.Tmax.y.dif.rcp85e.mri.mean.human,g.Tmax.y.dif.rcp85e.mri.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85e.mri.mean



####Difference of precipitation within land uses
success_projections.ag.rcp85e.mri.mean$y.dif.pcp_std_gridmet.rcp85e.mri.mean <- success_projections.ag.rcp85e.mri.mean$pcpbefore_raw_gridmet.rcp85e.mri.mean - success_projections.ag.rcp85e.mri.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp85e.mri.mean$y.dif.pcp_std_gridmet.rcp85e.mri.mean<- success_projections.forest.rcp85e.mri.mean$pcpbefore_raw_gridmet.rcp85e.mri.mean - success_projections.forest.rcp85e.mri.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85e.mri.mean$y.dif.pcp_std_gridmet.rcp85e.mri.mean <- success_projections.natural.open.rcp85e.mri.mean$pcpbefore_raw_gridmet.rcp85e.mri.mean - success_projections.natural.open.rcp85e.mri.mean$pcpbefore_raw_gridmet
success_projections.human.rcp85e.mri.mean$y.dif.pcp_std_gridmet.rcp85e.mri.mean <- success_projections.human.rcp85e.mri.mean$pcpbefore_raw_gridmet.rcp85e.mri.mean - success_projections.human.rcp85e.mri.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85e.mri.mean.ag <- ggplot(success_projections.ag.rcp85e.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.mean.ag

g.pcp.y.dif.rcp85e.mri.mean.forest <- ggplot(success_projections.forest.rcp85e.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.mean.forest

g.pcp.y.dif.rcp85e.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp85e.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.mean.natural.open

g.pcp.y.dif.rcp85e.mri.mean.human <- ggplot(success_projections.human.rcp85e.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.mean.human

difference.pcp.by.landuse.rcp85e.mri.mean <- ggarrange(g.pcp.y.dif.rcp85e.mri.mean.ag,g.pcp.y.dif.rcp85e.mri.mean.forest,
                                                       g.pcp.y.dif.rcp85e.mri.mean.human,g.pcp.y.dif.rcp85e.mri.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85e.mri.mean

#RCP 85e 10% mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85e.mri.10 <- 0
success_projections$y.fut.rcp85e.mri.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.mri.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.mri.10  + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.mri.10 )

success_projections$y.now.rcp85e.mri.10<- 0
success_projections$y.now.rcp85e.mri.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                       NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                       substrate*success_projections$substrate_binary +
                                                       success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85e.mri.10 <- success_projections$y.fut.rcp85e.mri.10 - success_projections$y.now.rcp85e.mri.10


###Scatter plot with x = now y = future
g.now.rcp85e.mri.10 <- ggplot(success_projections, aes(x=y.now.rcp85e.mri.10, y = y.fut.rcp85e.mri.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 10% mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85e.mri.10


###Density plot of the difference future - now
g.density.y.dif.rcp85e.mri.10 <- ggplot(success_projections, aes(x=y.dif.rcp85e.mri.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 10% mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85e.mri.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85e.mri.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85e.mri.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85e.mri.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85e.mri.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85e.mri.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85e.mri.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85e.mri.10.ag <- ggplot(success_projections.ag.rcp85e.mri.10 , aes(x=y.dif.rcp85e.mri.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.10.ag

g.density.y.dif.rcp85e.mri.10.forest <- ggplot(success_projections.forest.rcp85e.mri.10, aes(x=y.dif.rcp85e.mri.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.10.forest

g.density.y.dif.rcp85e.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp85e.mri.10, aes(x=y.dif.rcp85e.mri.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.10.natural.open

g.density.y.dif.rcp85e.mri.10.human <- ggplot(success_projections.human.rcp85e.mri.10, aes(x=y.dif.rcp85e.mri.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.10.human

difference.density.by.landuse.rcp85e.mri.10 <- ggarrange(g.density.y.dif.rcp85e.mri.10.ag,g.density.y.dif.rcp85e.mri.10.forest,
                                                         g.density.y.dif.rcp85e.mri.10.human,g.density.y.dif.rcp85e.mri.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85e.mri.10


####Difference of temperatures within land uses
success_projections.ag.rcp85e.mri.10$y.dif.Tmax_std_gridmet.rcp85e.mri.10 <- success_projections.ag.rcp85e.mri.10$Tmax_std_gridmet.rcp85e.mri.10 - success_projections.ag.rcp85e.mri.10$Tmax_std_gridmet
success_projections.forest.rcp85e.mri.10$y.dif.Tmax_std_gridmet.rcp85e.mri.10<- success_projections.forest.rcp85e.mri.10$Tmax_std_gridmet.rcp85e.mri.10 - success_projections.forest.rcp85e.mri.10$Tmax_std_gridmet
success_projections.natural.open.rcp85e.mri.10$y.dif.Tmax_std_gridmet.rcp85e.mri.10 <- success_projections.natural.open.rcp85e.mri.10$Tmax_std_gridmet.rcp85e.mri.10 - success_projections.natural.open.rcp85e.mri.10$Tmax_std_gridmet
success_projections.human.rcp85e.mri.10$y.dif.Tmax_std_gridmet.rcp85e.mri.10 <- success_projections.human.rcp85e.mri.10$Tmax_std_gridmet.rcp85e.mri.10 - success_projections.human.rcp85e.mri.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85e.mri.10.ag <- ggplot(success_projections.ag.rcp85e.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.10.ag

g.Tmax.y.dif.rcp85e.mri.10.forest <- ggplot(success_projections.forest.rcp85e.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.10.forest

g.Tmax.y.dif.rcp85e.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp85e.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.10.natural.open

g.Tmax.y.dif.rcp85e.mri.10.human <- ggplot(success_projections.human.rcp85e.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.10.human

difference.Tmax.by.landuse.rcp85e.mri.10 <- ggarrange(g.Tmax.y.dif.rcp85e.mri.10.ag,g.Tmax.y.dif.rcp85e.mri.10.forest,
                                                      g.Tmax.y.dif.rcp85e.mri.10.human,g.Tmax.y.dif.rcp85e.mri.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85e.mri.10



####Difference of precipitation within land uses
success_projections.ag.rcp85e.mri.10$y.dif.pcp_std_gridmet.rcp85e.mri.10 <- success_projections.ag.rcp85e.mri.10$pcpbefore_raw_gridmet.rcp85e.mri.10 - success_projections.ag.rcp85e.mri.10$pcpbefore_raw_gridmet
success_projections.forest.rcp85e.mri.10$y.dif.pcp_std_gridmet.rcp85e.mri.10<- success_projections.forest.rcp85e.mri.10$pcpbefore_raw_gridmet.rcp85e.mri.10 - success_projections.forest.rcp85e.mri.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85e.mri.10$y.dif.pcp_std_gridmet.rcp85e.mri.10 <- success_projections.natural.open.rcp85e.mri.10$pcpbefore_raw_gridmet.rcp85e.mri.10 - success_projections.natural.open.rcp85e.mri.10$pcpbefore_raw_gridmet
success_projections.human.rcp85e.mri.10$y.dif.pcp_std_gridmet.rcp85e.mri.10 <- success_projections.human.rcp85e.mri.10$pcpbefore_raw_gridmet.rcp85e.mri.10 - success_projections.human.rcp85e.mri.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85e.mri.10.ag <- ggplot(success_projections.ag.rcp85e.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.10.ag

g.pcp.y.dif.rcp85e.mri.10.forest <- ggplot(success_projections.forest.rcp85e.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.10.forest

g.pcp.y.dif.rcp85e.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp85e.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.10.natural.open

g.pcp.y.dif.rcp85e.mri.10.human <- ggplot(success_projections.human.rcp85e.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.10.human

difference.pcp.by.landuse.rcp85e.mri.10 <- ggarrange(g.pcp.y.dif.rcp85e.mri.10.ag,g.pcp.y.dif.rcp85e.mri.10.forest,
                                                     g.pcp.y.dif.rcp85e.mri.10.human,g.pcp.y.dif.rcp85e.mri.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85e.mri.10

#RCP 85e 90% mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85e.mri.90 <- 0
success_projections$y.fut.rcp85e.mri.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.mri.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.mri.90  + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.mri.90 )

success_projections$y.now.rcp85e.mri.90<- 0
success_projections$y.now.rcp85e.mri.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                       NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                       substrate*success_projections$substrate_binary +
                                                       success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85e.mri.90 <- success_projections$y.fut.rcp85e.mri.90 - success_projections$y.now.rcp85e.mri.90


###Scatter plot with x = now y = future
g.now.rcp85e.mri.90 <- ggplot(success_projections, aes(x=y.now.rcp85e.mri.90, y = y.fut.rcp85e.mri.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 90% mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85e.mri.90


###Density plot of the difference future - now
g.density.y.dif.rcp85e.mri.90 <- ggplot(success_projections, aes(x=y.dif.rcp85e.mri.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 90% mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85e.mri.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85e.mri.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85e.mri.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85e.mri.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85e.mri.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85e.mri.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85e.mri.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85e.mri.90.ag <- ggplot(success_projections.ag.rcp85e.mri.90 , aes(x=y.dif.rcp85e.mri.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.90.ag

g.density.y.dif.rcp85e.mri.90.forest <- ggplot(success_projections.forest.rcp85e.mri.90, aes(x=y.dif.rcp85e.mri.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.90.forest

g.density.y.dif.rcp85e.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp85e.mri.90, aes(x=y.dif.rcp85e.mri.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.90.natural.open

g.density.y.dif.rcp85e.mri.90.human <- ggplot(success_projections.human.rcp85e.mri.90, aes(x=y.dif.rcp85e.mri.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.mri.90.human

difference.density.by.landuse.rcp85e.mri.90 <- ggarrange(g.density.y.dif.rcp85e.mri.90.ag,g.density.y.dif.rcp85e.mri.90.forest,
                                                         g.density.y.dif.rcp85e.mri.90.human,g.density.y.dif.rcp85e.mri.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85e.mri.90


####Difference of temperatures within land uses
success_projections.ag.rcp85e.mri.90$y.dif.Tmax_std_gridmet.rcp85e.mri.90 <- success_projections.ag.rcp85e.mri.90$Tmax_std_gridmet.rcp85e.mri.90 - success_projections.ag.rcp85e.mri.90$Tmax_std_gridmet
success_projections.forest.rcp85e.mri.90$y.dif.Tmax_std_gridmet.rcp85e.mri.90<- success_projections.forest.rcp85e.mri.90$Tmax_std_gridmet.rcp85e.mri.90 - success_projections.forest.rcp85e.mri.90$Tmax_std_gridmet
success_projections.natural.open.rcp85e.mri.90$y.dif.Tmax_std_gridmet.rcp85e.mri.90 <- success_projections.natural.open.rcp85e.mri.90$Tmax_std_gridmet.rcp85e.mri.90 - success_projections.natural.open.rcp85e.mri.90$Tmax_std_gridmet
success_projections.human.rcp85e.mri.90$y.dif.Tmax_std_gridmet.rcp85e.mri.90 <- success_projections.human.rcp85e.mri.90$Tmax_std_gridmet.rcp85e.mri.90 - success_projections.human.rcp85e.mri.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85e.mri.90.ag <- ggplot(success_projections.ag.rcp85e.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.90.ag

g.Tmax.y.dif.rcp85e.mri.90.forest <- ggplot(success_projections.forest.rcp85e.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.90.forest

g.Tmax.y.dif.rcp85e.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp85e.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.90.natural.open

g.Tmax.y.dif.rcp85e.mri.90.human <- ggplot(success_projections.human.rcp85e.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.mri.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.mri.90.human

difference.Tmax.by.landuse.rcp85e.mri.90 <- ggarrange(g.Tmax.y.dif.rcp85e.mri.90.ag,g.Tmax.y.dif.rcp85e.mri.90.forest,
                                                      g.Tmax.y.dif.rcp85e.mri.90.human,g.Tmax.y.dif.rcp85e.mri.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85e.mri.90



####Difference of precipitation within land uses
success_projections.ag.rcp85e.mri.90$y.dif.pcp_std_gridmet.rcp85e.mri.90 <- success_projections.ag.rcp85e.mri.90$pcpbefore_raw_gridmet.rcp85e.mri.90 - success_projections.ag.rcp85e.mri.90$pcpbefore_raw_gridmet
success_projections.forest.rcp85e.mri.90$y.dif.pcp_std_gridmet.rcp85e.mri.90<- success_projections.forest.rcp85e.mri.90$pcpbefore_raw_gridmet.rcp85e.mri.90 - success_projections.forest.rcp85e.mri.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85e.mri.90$y.dif.pcp_std_gridmet.rcp85e.mri.90 <- success_projections.natural.open.rcp85e.mri.90$pcpbefore_raw_gridmet.rcp85e.mri.90 - success_projections.natural.open.rcp85e.mri.90$pcpbefore_raw_gridmet
success_projections.human.rcp85e.mri.90$y.dif.pcp_std_gridmet.rcp85e.mri.90 <- success_projections.human.rcp85e.mri.90$pcpbefore_raw_gridmet.rcp85e.mri.90 - success_projections.human.rcp85e.mri.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85e.mri.90.ag <- ggplot(success_projections.ag.rcp85e.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.90.ag

g.pcp.y.dif.rcp85e.mri.90.forest <- ggplot(success_projections.forest.rcp85e.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.90.forest

g.pcp.y.dif.rcp85e.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp85e.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.90.natural.open

g.pcp.y.dif.rcp85e.mri.90.human <- ggplot(success_projections.human.rcp85e.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.mri.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.mri.90.human

difference.pcp.by.landuse.rcp85e.mri.90 <- ggarrange(g.pcp.y.dif.rcp85e.mri.90.ag,g.pcp.y.dif.rcp85e.mri.90.forest,
                                                     g.pcp.y.dif.rcp85e.mri.90.human,g.pcp.y.dif.rcp85e.mri.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85e.mri.90


#RCP 85m mean mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85m.mri.mean <- 0
success_projections$y.fut.rcp85m.mri.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.mri.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.mri.mean  + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.mri.mean )

success_projections$y.now.rcp85m.mri.mean<- 0
success_projections$y.now.rcp85m.mri.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                         NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                         substrate*success_projections$substrate_binary +
                                                         success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85m.mri.mean <- success_projections$y.fut.rcp85m.mri.mean - success_projections$y.now.rcp85m.mri.mean


###Scatter plot with x = now y = future
g.now.rcp85m.mri.mean <- ggplot(success_projections, aes(x=y.now.rcp85m.mri.mean, y = y.fut.rcp85m.mri.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 mean mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85m.mri.mean


###Density plot of the difference future - now
g.density.y.dif.rcp85m.mri.mean <- ggplot(success_projections, aes(x=y.dif.rcp85m.mri.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 mean mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85m.mri.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85m.mri.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85m.mri.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85m.mri.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85m.mri.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85m.mri.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85m.mri.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85m.mri.mean.ag <- ggplot(success_projections.ag.rcp85m.mri.mean , aes(x=y.dif.rcp85m.mri.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.mean.ag

g.density.y.dif.rcp85m.mri.mean.forest <- ggplot(success_projections.forest.rcp85m.mri.mean, aes(x=y.dif.rcp85m.mri.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.mean.forest

g.density.y.dif.rcp85m.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp85m.mri.mean, aes(x=y.dif.rcp85m.mri.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.mean.natural.open

g.density.y.dif.rcp85m.mri.mean.human <- ggplot(success_projections.human.rcp85m.mri.mean, aes(x=y.dif.rcp85m.mri.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.mean.human

difference.density.by.landuse.rcp85m.mri.mean <- ggarrange(g.density.y.dif.rcp85m.mri.mean.ag,g.density.y.dif.rcp85m.mri.mean.forest,
                                                           g.density.y.dif.rcp85m.mri.mean.human,g.density.y.dif.rcp85m.mri.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85m.mri.mean


####Difference of temperatures within land uses
success_projections.ag.rcp85m.mri.mean$y.dif.Tmax_std_gridmet.rcp85m.mri.mean <- success_projections.ag.rcp85m.mri.mean$Tmax_std_gridmet.rcp85m.mri.mean - success_projections.ag.rcp85m.mri.mean$Tmax_std_gridmet
success_projections.forest.rcp85m.mri.mean$y.dif.Tmax_std_gridmet.rcp85m.mri.mean<- success_projections.forest.rcp85m.mri.mean$Tmax_std_gridmet.rcp85m.mri.mean - success_projections.forest.rcp85m.mri.mean$Tmax_std_gridmet
success_projections.natural.open.rcp85m.mri.mean$y.dif.Tmax_std_gridmet.rcp85m.mri.mean <- success_projections.natural.open.rcp85m.mri.mean$Tmax_std_gridmet.rcp85m.mri.mean - success_projections.natural.open.rcp85m.mri.mean$Tmax_std_gridmet
success_projections.human.rcp85m.mri.mean$y.dif.Tmax_std_gridmet.rcp85m.mri.mean <- success_projections.human.rcp85m.mri.mean$Tmax_std_gridmet.rcp85m.mri.mean - success_projections.human.rcp85m.mri.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85m.mri.mean.ag <- ggplot(success_projections.ag.rcp85m.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.mean.ag

g.Tmax.y.dif.rcp85m.mri.mean.forest <- ggplot(success_projections.forest.rcp85m.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.mean.forest

g.Tmax.y.dif.rcp85m.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp85m.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.mean.natural.open

g.Tmax.y.dif.rcp85m.mri.mean.human <- ggplot(success_projections.human.rcp85m.mri.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.mean.human

difference.Tmax.by.landuse.rcp85m.mri.mean <- ggarrange(g.Tmax.y.dif.rcp85m.mri.mean.ag,g.Tmax.y.dif.rcp85m.mri.mean.forest,
                                                        g.Tmax.y.dif.rcp85m.mri.mean.human,g.Tmax.y.dif.rcp85m.mri.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85m.mri.mean



####Difference of precipitation within land uses
success_projections.ag.rcp85m.mri.mean$y.dif.pcp_std_gridmet.rcp85m.mri.mean <- success_projections.ag.rcp85m.mri.mean$pcpbefore_raw_gridmet.rcp85m.mri.mean - success_projections.ag.rcp85m.mri.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp85m.mri.mean$y.dif.pcp_std_gridmet.rcp85m.mri.mean<- success_projections.forest.rcp85m.mri.mean$pcpbefore_raw_gridmet.rcp85m.mri.mean - success_projections.forest.rcp85m.mri.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85m.mri.mean$y.dif.pcp_std_gridmet.rcp85m.mri.mean <- success_projections.natural.open.rcp85m.mri.mean$pcpbefore_raw_gridmet.rcp85m.mri.mean - success_projections.natural.open.rcp85m.mri.mean$pcpbefore_raw_gridmet
success_projections.human.rcp85m.mri.mean$y.dif.pcp_std_gridmet.rcp85m.mri.mean <- success_projections.human.rcp85m.mri.mean$pcpbefore_raw_gridmet.rcp85m.mri.mean - success_projections.human.rcp85m.mri.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85m.mri.mean.ag <- ggplot(success_projections.ag.rcp85m.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.mean.ag

g.pcp.y.dif.rcp85m.mri.mean.forest <- ggplot(success_projections.forest.rcp85m.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.mean.forest

g.pcp.y.dif.rcp85m.mri.mean.natural.open <- ggplot(success_projections.natural.open.rcp85m.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.mean.natural.open

g.pcp.y.dif.rcp85m.mri.mean.human <- ggplot(success_projections.human.rcp85m.mri.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.mean.human

difference.pcp.by.landuse.rcp85m.mri.mean <- ggarrange(g.pcp.y.dif.rcp85m.mri.mean.ag,g.pcp.y.dif.rcp85m.mri.mean.forest,
                                                       g.pcp.y.dif.rcp85m.mri.mean.human,g.pcp.y.dif.rcp85m.mri.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85m.mri.mean

#RCP 85e 10% mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85m.mri.10 <- 0
success_projections$y.fut.rcp85m.mri.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.mri.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.mri.10  + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.mri.10 )

success_projections$y.now.rcp85m.mri.10<- 0
success_projections$y.now.rcp85m.mri.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                       NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                       substrate*success_projections$substrate_binary +
                                                       success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85m.mri.10 <- success_projections$y.fut.rcp85m.mri.10 - success_projections$y.now.rcp85m.mri.10


###Scatter plot with x = now y = future
g.now.rcp85m.mri.10 <- ggplot(success_projections, aes(x=y.now.rcp85m.mri.10, y = y.fut.rcp85m.mri.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 10% mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85m.mri.10


###Density plot of the difference future - now
g.density.y.dif.rcp85m.mri.10 <- ggplot(success_projections, aes(x=y.dif.rcp85m.mri.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 10% mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85m.mri.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85m.mri.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85m.mri.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85m.mri.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85m.mri.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85m.mri.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85m.mri.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85m.mri.10.ag <- ggplot(success_projections.ag.rcp85m.mri.10 , aes(x=y.dif.rcp85m.mri.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.10.ag

g.density.y.dif.rcp85m.mri.10.forest <- ggplot(success_projections.forest.rcp85m.mri.10, aes(x=y.dif.rcp85m.mri.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.10.forest

g.density.y.dif.rcp85m.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp85m.mri.10, aes(x=y.dif.rcp85m.mri.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.10.natural.open

g.density.y.dif.rcp85m.mri.10.human <- ggplot(success_projections.human.rcp85m.mri.10, aes(x=y.dif.rcp85m.mri.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.10.human

difference.density.by.landuse.rcp85m.mri.10 <- ggarrange(g.density.y.dif.rcp85m.mri.10.ag,g.density.y.dif.rcp85m.mri.10.forest,
                                                         g.density.y.dif.rcp85m.mri.10.human,g.density.y.dif.rcp85m.mri.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85m.mri.10


####Difference of temperatures within land uses
success_projections.ag.rcp85m.mri.10$y.dif.Tmax_std_gridmet.rcp85m.mri.10 <- success_projections.ag.rcp85m.mri.10$Tmax_std_gridmet.rcp85m.mri.10 - success_projections.ag.rcp85m.mri.10$Tmax_std_gridmet
success_projections.forest.rcp85m.mri.10$y.dif.Tmax_std_gridmet.rcp85m.mri.10<- success_projections.forest.rcp85m.mri.10$Tmax_std_gridmet.rcp85m.mri.10 - success_projections.forest.rcp85m.mri.10$Tmax_std_gridmet
success_projections.natural.open.rcp85m.mri.10$y.dif.Tmax_std_gridmet.rcp85m.mri.10 <- success_projections.natural.open.rcp85m.mri.10$Tmax_std_gridmet.rcp85m.mri.10 - success_projections.natural.open.rcp85m.mri.10$Tmax_std_gridmet
success_projections.human.rcp85m.mri.10$y.dif.Tmax_std_gridmet.rcp85m.mri.10 <- success_projections.human.rcp85m.mri.10$Tmax_std_gridmet.rcp85m.mri.10 - success_projections.human.rcp85m.mri.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85m.mri.10.ag <- ggplot(success_projections.ag.rcp85m.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.10.ag

g.Tmax.y.dif.rcp85m.mri.10.forest <- ggplot(success_projections.forest.rcp85m.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.10.forest

g.Tmax.y.dif.rcp85m.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp85m.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.10.natural.open

g.Tmax.y.dif.rcp85m.mri.10.human <- ggplot(success_projections.human.rcp85m.mri.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.10.human

difference.Tmax.by.landuse.rcp85m.mri.10 <- ggarrange(g.Tmax.y.dif.rcp85m.mri.10.ag,g.Tmax.y.dif.rcp85m.mri.10.forest,
                                                      g.Tmax.y.dif.rcp85m.mri.10.human,g.Tmax.y.dif.rcp85m.mri.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85m.mri.10



####Difference of precipitation within land uses
success_projections.ag.rcp85m.mri.10$y.dif.pcp_std_gridmet.rcp85m.mri.10 <- success_projections.ag.rcp85m.mri.10$pcpbefore_raw_gridmet.rcp85m.mri.10 - success_projections.ag.rcp85m.mri.10$pcpbefore_raw_gridmet
success_projections.forest.rcp85m.mri.10$y.dif.pcp_std_gridmet.rcp85m.mri.10<- success_projections.forest.rcp85m.mri.10$pcpbefore_raw_gridmet.rcp85m.mri.10 - success_projections.forest.rcp85m.mri.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85m.mri.10$y.dif.pcp_std_gridmet.rcp85m.mri.10 <- success_projections.natural.open.rcp85m.mri.10$pcpbefore_raw_gridmet.rcp85m.mri.10 - success_projections.natural.open.rcp85m.mri.10$pcpbefore_raw_gridmet
success_projections.human.rcp85m.mri.10$y.dif.pcp_std_gridmet.rcp85m.mri.10 <- success_projections.human.rcp85m.mri.10$pcpbefore_raw_gridmet.rcp85m.mri.10 - success_projections.human.rcp85m.mri.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85m.mri.10.ag <- ggplot(success_projections.ag.rcp85m.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.10.ag

g.pcp.y.dif.rcp85m.mri.10.forest <- ggplot(success_projections.forest.rcp85m.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.10.forest

g.pcp.y.dif.rcp85m.mri.10.natural.open <- ggplot(success_projections.natural.open.rcp85m.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.10.natural.open

g.pcp.y.dif.rcp85m.mri.10.human <- ggplot(success_projections.human.rcp85m.mri.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.10.human

difference.pcp.by.landuse.rcp85m.mri.10 <- ggarrange(g.pcp.y.dif.rcp85m.mri.10.ag,g.pcp.y.dif.rcp85m.mri.10.forest,
                                                     g.pcp.y.dif.rcp85m.mri.10.human,g.pcp.y.dif.rcp85m.mri.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85m.mri.10

#RCP 85e 90% mri-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85m.mri.90 <- 0
success_projections$y.fut.rcp85m.mri.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.mri.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.mri.90  + 
                                                        NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                        substrate*success_projections$substrate_binary +
                                                        success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.mri.90 )

success_projections$y.now.rcp85m.mri.90<- 0
success_projections$y.now.rcp85m.mri.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                       NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                       substrate*success_projections$substrate_binary +
                                                       success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85m.mri.90 <- success_projections$y.fut.rcp85m.mri.90 - success_projections$y.now.rcp85m.mri.90


###Scatter plot with x = now y = future
g.now.rcp85m.mri.90 <- ggplot(success_projections, aes(x=y.now.rcp85m.mri.90, y = y.fut.rcp85m.mri.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 90% mri")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85m.mri.90


###Density plot of the difference future - now
g.density.y.dif.rcp85m.mri.90 <- ggplot(success_projections, aes(x=y.dif.rcp85m.mri.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 90% mri ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85m.mri.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85m.mri.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85m.mri.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85m.mri.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85m.mri.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85m.mri.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85m.mri.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85m.mri.90.ag <- ggplot(success_projections.ag.rcp85m.mri.90 , aes(x=y.dif.rcp85m.mri.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.90.ag

g.density.y.dif.rcp85m.mri.90.forest <- ggplot(success_projections.forest.rcp85m.mri.90, aes(x=y.dif.rcp85m.mri.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.90.forest

g.density.y.dif.rcp85m.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp85m.mri.90, aes(x=y.dif.rcp85m.mri.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.90.natural.open

g.density.y.dif.rcp85m.mri.90.human <- ggplot(success_projections.human.rcp85m.mri.90, aes(x=y.dif.rcp85m.mri.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.mri.90.human

difference.density.by.landuse.rcp85m.mri.90 <- ggarrange(g.density.y.dif.rcp85m.mri.90.ag,g.density.y.dif.rcp85m.mri.90.forest,
                                                         g.density.y.dif.rcp85m.mri.90.human,g.density.y.dif.rcp85m.mri.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85m.mri.90


####Difference of temperatures within land uses
success_projections.ag.rcp85m.mri.90$y.dif.Tmax_std_gridmet.rcp85m.mri.90 <- success_projections.ag.rcp85m.mri.90$Tmax_std_gridmet.rcp85m.mri.90 - success_projections.ag.rcp85m.mri.90$Tmax_std_gridmet
success_projections.forest.rcp85m.mri.90$y.dif.Tmax_std_gridmet.rcp85m.mri.90<- success_projections.forest.rcp85m.mri.90$Tmax_std_gridmet.rcp85m.mri.90 - success_projections.forest.rcp85m.mri.90$Tmax_std_gridmet
success_projections.natural.open.rcp85m.mri.90$y.dif.Tmax_std_gridmet.rcp85m.mri.90 <- success_projections.natural.open.rcp85m.mri.90$Tmax_std_gridmet.rcp85m.mri.90 - success_projections.natural.open.rcp85m.mri.90$Tmax_std_gridmet
success_projections.human.rcp85m.mri.90$y.dif.Tmax_std_gridmet.rcp85m.mri.90 <- success_projections.human.rcp85m.mri.90$Tmax_std_gridmet.rcp85m.mri.90 - success_projections.human.rcp85m.mri.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85m.mri.90.ag <- ggplot(success_projections.ag.rcp85m.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.90.ag

g.Tmax.y.dif.rcp85m.mri.90.forest <- ggplot(success_projections.forest.rcp85m.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.90.forest

g.Tmax.y.dif.rcp85m.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp85m.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.90.natural.open

g.Tmax.y.dif.rcp85m.mri.90.human <- ggplot(success_projections.human.rcp85m.mri.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.mri.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.mri.90.human

difference.Tmax.by.landuse.rcp85m.mri.90 <- ggarrange(g.Tmax.y.dif.rcp85m.mri.90.ag,g.Tmax.y.dif.rcp85m.mri.90.forest,
                                                      g.Tmax.y.dif.rcp85m.mri.90.human,g.Tmax.y.dif.rcp85m.mri.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85m.mri.90



####Difference of precipitation within land uses
success_projections.ag.rcp85m.mri.90$y.dif.pcp_std_gridmet.rcp85m.mri.90 <- success_projections.ag.rcp85m.mri.90$pcpbefore_raw_gridmet.rcp85m.mri.90 - success_projections.ag.rcp85m.mri.90$pcpbefore_raw_gridmet
success_projections.forest.rcp85m.mri.90$y.dif.pcp_std_gridmet.rcp85m.mri.90<- success_projections.forest.rcp85m.mri.90$pcpbefore_raw_gridmet.rcp85m.mri.90 - success_projections.forest.rcp85m.mri.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85m.mri.90$y.dif.pcp_std_gridmet.rcp85m.mri.90 <- success_projections.natural.open.rcp85m.mri.90$pcpbefore_raw_gridmet.rcp85m.mri.90 - success_projections.natural.open.rcp85m.mri.90$pcpbefore_raw_gridmet
success_projections.human.rcp85m.mri.90$y.dif.pcp_std_gridmet.rcp85m.mri.90 <- success_projections.human.rcp85m.mri.90$pcpbefore_raw_gridmet.rcp85m.mri.90 - success_projections.human.rcp85m.mri.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85m.mri.90.ag <- ggplot(success_projections.ag.rcp85m.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.90.ag

g.pcp.y.dif.rcp85m.mri.90.forest <- ggplot(success_projections.forest.rcp85m.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.90.forest

g.pcp.y.dif.rcp85m.mri.90.natural.open <- ggplot(success_projections.natural.open.rcp85m.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.90.natural.open

g.pcp.y.dif.rcp85m.mri.90.human <- ggplot(success_projections.human.rcp85m.mri.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.mri.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.mri.90.human

difference.pcp.by.landuse.rcp85m.mri.90 <- ggarrange(g.pcp.y.dif.rcp85m.mri.90.ag,g.pcp.y.dif.rcp85m.mri.90.forest,
                                                     g.pcp.y.dif.rcp85m.mri.90.human,g.pcp.y.dif.rcp85m.mri.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85m.mri.90














####CANESM
#RCP 45e mean canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45e.canesm.mean <- 0
success_projections$y.fut.rcp45e.canesm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.canesm.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.canesm.mean  + 
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.canesm.mean )

success_projections$y.now.rcp45e.canesm.mean<- 0
success_projections$y.now.rcp45e.canesm.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                            NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                            substrate*success_projections$substrate_binary +
                                                            success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45e.canesm.mean <- success_projections$y.fut.rcp45e.canesm.mean - success_projections$y.now.rcp45e.canesm.mean


###Scatter plot with x = now y = future
g.now.rcp45e.canesm.mean <- ggplot(success_projections, aes(x=y.now.rcp45e.canesm.mean, y = y.fut.rcp45e.canesm.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 mean canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45e.canesm.mean


###Density plot of the difference future - now
g.density.y.dif.rcp45e.canesm.mean <- ggplot(success_projections, aes(x=y.dif.rcp45e.canesm.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 mean canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45e.canesm.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45e.canesm.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45e.canesm.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45e.canesm.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45e.canesm.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45e.canesm.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45e.canesm.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45e.canesm.mean.ag <- ggplot(success_projections.ag.rcp45e.canesm.mean , aes(x=y.dif.rcp45e.canesm.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.mean.ag

g.density.y.dif.rcp45e.canesm.mean.forest <- ggplot(success_projections.forest.rcp45e.canesm.mean, aes(x=y.dif.rcp45e.canesm.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.mean.forest

g.density.y.dif.rcp45e.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp45e.canesm.mean, aes(x=y.dif.rcp45e.canesm.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.mean.natural.open

g.density.y.dif.rcp45e.canesm.mean.human <- ggplot(success_projections.human.rcp45e.canesm.mean, aes(x=y.dif.rcp45e.canesm.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.mean.human

difference.density.by.landuse.rcp45e.canesm.mean <- ggarrange(g.density.y.dif.rcp45e.canesm.mean.ag,g.density.y.dif.rcp45e.canesm.mean.forest,
                                                              g.density.y.dif.rcp45e.canesm.mean.human,g.density.y.dif.rcp45e.canesm.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45e.canesm.mean


####Difference of temperatures within land uses
success_projections.ag.rcp45e.canesm.mean$y.dif.Tmax_std_gridmet.rcp45e.canesm.mean <- success_projections.ag.rcp45e.canesm.mean$Tmax_std_gridmet.rcp45e.canesm.mean - success_projections.ag.rcp45e.canesm.mean$Tmax_std_gridmet
success_projections.forest.rcp45e.canesm.mean$y.dif.Tmax_std_gridmet.rcp45e.canesm.mean<- success_projections.forest.rcp45e.canesm.mean$Tmax_std_gridmet.rcp45e.canesm.mean - success_projections.forest.rcp45e.canesm.mean$Tmax_std_gridmet
success_projections.natural.open.rcp45e.canesm.mean$y.dif.Tmax_std_gridmet.rcp45e.canesm.mean <- success_projections.natural.open.rcp45e.canesm.mean$Tmax_std_gridmet.rcp45e.canesm.mean - success_projections.natural.open.rcp45e.canesm.mean$Tmax_std_gridmet
success_projections.human.rcp45e.canesm.mean$y.dif.Tmax_std_gridmet.rcp45e.canesm.mean <- success_projections.human.rcp45e.canesm.mean$Tmax_std_gridmet.rcp45e.canesm.mean - success_projections.human.rcp45e.canesm.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45e.canesm.mean.ag <- ggplot(success_projections.ag.rcp45e.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.mean.ag

g.Tmax.y.dif.rcp45e.canesm.mean.forest <- ggplot(success_projections.forest.rcp45e.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.mean.forest

g.Tmax.y.dif.rcp45e.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp45e.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.mean.natural.open

g.Tmax.y.dif.rcp45e.canesm.mean.human <- ggplot(success_projections.human.rcp45e.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.mean.human

difference.Tmax.by.landuse.rcp45e.canesm.mean <- ggarrange(g.Tmax.y.dif.rcp45e.canesm.mean.ag,g.Tmax.y.dif.rcp45e.canesm.mean.forest,
                                                           g.Tmax.y.dif.rcp45e.canesm.mean.human,g.Tmax.y.dif.rcp45e.canesm.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45e.canesm.mean



####Difference of precipitation within land uses
success_projections.ag.rcp45e.canesm.mean$y.dif.pcp_std_gridmet.rcp45e.canesm.mean <- success_projections.ag.rcp45e.canesm.mean$pcpbefore_raw_gridmet.rcp45e.canesm.mean - success_projections.ag.rcp45e.canesm.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp45e.canesm.mean$y.dif.pcp_std_gridmet.rcp45e.canesm.mean<- success_projections.forest.rcp45e.canesm.mean$pcpbefore_raw_gridmet.rcp45e.canesm.mean - success_projections.forest.rcp45e.canesm.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45e.canesm.mean$y.dif.pcp_std_gridmet.rcp45e.canesm.mean <- success_projections.natural.open.rcp45e.canesm.mean$pcpbefore_raw_gridmet.rcp45e.canesm.mean - success_projections.natural.open.rcp45e.canesm.mean$pcpbefore_raw_gridmet
success_projections.human.rcp45e.canesm.mean$y.dif.pcp_std_gridmet.rcp45e.canesm.mean <- success_projections.human.rcp45e.canesm.mean$pcpbefore_raw_gridmet.rcp45e.canesm.mean - success_projections.human.rcp45e.canesm.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45e.canesm.mean.ag <- ggplot(success_projections.ag.rcp45e.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.mean.ag

g.pcp.y.dif.rcp45e.canesm.mean.forest <- ggplot(success_projections.forest.rcp45e.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.mean.forest

g.pcp.y.dif.rcp45e.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp45e.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.mean.natural.open

g.pcp.y.dif.rcp45e.canesm.mean.human <- ggplot(success_projections.human.rcp45e.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.mean.human

difference.pcp.by.landuse.rcp45e.canesm.mean <- ggarrange(g.pcp.y.dif.rcp45e.canesm.mean.ag,g.pcp.y.dif.rcp45e.canesm.mean.forest,
                                                          g.pcp.y.dif.rcp45e.canesm.mean.human,g.pcp.y.dif.rcp45e.canesm.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45e.canesm.mean

#RCP 45e 10% canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45e.canesm.10 <- 0
success_projections$y.fut.rcp45e.canesm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.canesm.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.canesm.10  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.canesm.10 )

success_projections$y.now.rcp45e.canesm.10<- 0
success_projections$y.now.rcp45e.canesm.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45e.canesm.10 <- success_projections$y.fut.rcp45e.canesm.10 - success_projections$y.now.rcp45e.canesm.10


###Scatter plot with x = now y = future
g.now.rcp45e.canesm.10 <- ggplot(success_projections, aes(x=y.now.rcp45e.canesm.10, y = y.fut.rcp45e.canesm.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 10% canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45e.canesm.10


###Density plot of the difference future - now
g.density.y.dif.rcp45e.canesm.10 <- ggplot(success_projections, aes(x=y.dif.rcp45e.canesm.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 10% canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45e.canesm.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45e.canesm.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45e.canesm.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45e.canesm.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45e.canesm.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45e.canesm.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45e.canesm.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45e.canesm.10.ag <- ggplot(success_projections.ag.rcp45e.canesm.10 , aes(x=y.dif.rcp45e.canesm.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.10.ag

g.density.y.dif.rcp45e.canesm.10.forest <- ggplot(success_projections.forest.rcp45e.canesm.10, aes(x=y.dif.rcp45e.canesm.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.10.forest

g.density.y.dif.rcp45e.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp45e.canesm.10, aes(x=y.dif.rcp45e.canesm.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.10.natural.open

g.density.y.dif.rcp45e.canesm.10.human <- ggplot(success_projections.human.rcp45e.canesm.10, aes(x=y.dif.rcp45e.canesm.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.10.human

difference.density.by.landuse.rcp45e.canesm.10 <- ggarrange(g.density.y.dif.rcp45e.canesm.10.ag,g.density.y.dif.rcp45e.canesm.10.forest,
                                                            g.density.y.dif.rcp45e.canesm.10.human,g.density.y.dif.rcp45e.canesm.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45e.canesm.10


####Difference of temperatures within land uses
success_projections.ag.rcp45e.canesm.10$y.dif.Tmax_std_gridmet.rcp45e.canesm.10 <- success_projections.ag.rcp45e.canesm.10$Tmax_std_gridmet.rcp45e.canesm.10 - success_projections.ag.rcp45e.canesm.10$Tmax_std_gridmet
success_projections.forest.rcp45e.canesm.10$y.dif.Tmax_std_gridmet.rcp45e.canesm.10<- success_projections.forest.rcp45e.canesm.10$Tmax_std_gridmet.rcp45e.canesm.10 - success_projections.forest.rcp45e.canesm.10$Tmax_std_gridmet
success_projections.natural.open.rcp45e.canesm.10$y.dif.Tmax_std_gridmet.rcp45e.canesm.10 <- success_projections.natural.open.rcp45e.canesm.10$Tmax_std_gridmet.rcp45e.canesm.10 - success_projections.natural.open.rcp45e.canesm.10$Tmax_std_gridmet
success_projections.human.rcp45e.canesm.10$y.dif.Tmax_std_gridmet.rcp45e.canesm.10 <- success_projections.human.rcp45e.canesm.10$Tmax_std_gridmet.rcp45e.canesm.10 - success_projections.human.rcp45e.canesm.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45e.canesm.10.ag <- ggplot(success_projections.ag.rcp45e.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.10.ag

g.Tmax.y.dif.rcp45e.canesm.10.forest <- ggplot(success_projections.forest.rcp45e.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.10.forest

g.Tmax.y.dif.rcp45e.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp45e.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.10.natural.open

g.Tmax.y.dif.rcp45e.canesm.10.human <- ggplot(success_projections.human.rcp45e.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.10.human

difference.Tmax.by.landuse.rcp45e.canesm.10 <- ggarrange(g.Tmax.y.dif.rcp45e.canesm.10.ag,g.Tmax.y.dif.rcp45e.canesm.10.forest,
                                                         g.Tmax.y.dif.rcp45e.canesm.10.human,g.Tmax.y.dif.rcp45e.canesm.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45e.canesm.10



####Difference of precipitation within land uses
success_projections.ag.rcp45e.canesm.10$y.dif.pcp_std_gridmet.rcp45e.canesm.10 <- success_projections.ag.rcp45e.canesm.10$pcpbefore_raw_gridmet.rcp45e.canesm.10 - success_projections.ag.rcp45e.canesm.10$pcpbefore_raw_gridmet
success_projections.forest.rcp45e.canesm.10$y.dif.pcp_std_gridmet.rcp45e.canesm.10<- success_projections.forest.rcp45e.canesm.10$pcpbefore_raw_gridmet.rcp45e.canesm.10 - success_projections.forest.rcp45e.canesm.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45e.canesm.10$y.dif.pcp_std_gridmet.rcp45e.canesm.10 <- success_projections.natural.open.rcp45e.canesm.10$pcpbefore_raw_gridmet.rcp45e.canesm.10 - success_projections.natural.open.rcp45e.canesm.10$pcpbefore_raw_gridmet
success_projections.human.rcp45e.canesm.10$y.dif.pcp_std_gridmet.rcp45e.canesm.10 <- success_projections.human.rcp45e.canesm.10$pcpbefore_raw_gridmet.rcp45e.canesm.10 - success_projections.human.rcp45e.canesm.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45e.canesm.10.ag <- ggplot(success_projections.ag.rcp45e.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.10.ag

g.pcp.y.dif.rcp45e.canesm.10.forest <- ggplot(success_projections.forest.rcp45e.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.10.forest

g.pcp.y.dif.rcp45e.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp45e.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.10.natural.open

g.pcp.y.dif.rcp45e.canesm.10.human <- ggplot(success_projections.human.rcp45e.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.10.human

difference.pcp.by.landuse.rcp45e.canesm.10 <- ggarrange(g.pcp.y.dif.rcp45e.canesm.10.ag,g.pcp.y.dif.rcp45e.canesm.10.forest,
                                                        g.pcp.y.dif.rcp45e.canesm.10.human,g.pcp.y.dif.rcp45e.canesm.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45e.canesm.10

#RCP 45e 90% canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45e.canesm.90 <- 0
success_projections$y.fut.rcp45e.canesm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45e.canesm.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45e.canesm.90  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45e.canesm.90 )

success_projections$y.now.rcp45e.canesm.90<- 0
success_projections$y.now.rcp45e.canesm.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45e.canesm.90 <- success_projections$y.fut.rcp45e.canesm.90 - success_projections$y.now.rcp45e.canesm.90


###Scatter plot with x = now y = future
g.now.rcp45e.canesm.90 <- ggplot(success_projections, aes(x=y.now.rcp45e.canesm.90, y = y.fut.rcp45e.canesm.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 90% canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45e.canesm.90


###Density plot of the difference future - now
g.density.y.dif.rcp45e.canesm.90 <- ggplot(success_projections, aes(x=y.dif.rcp45e.canesm.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 90% canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45e.canesm.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45e.canesm.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45e.canesm.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45e.canesm.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45e.canesm.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45e.canesm.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45e.canesm.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45e.canesm.90.ag <- ggplot(success_projections.ag.rcp45e.canesm.90 , aes(x=y.dif.rcp45e.canesm.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.90.ag

g.density.y.dif.rcp45e.canesm.90.forest <- ggplot(success_projections.forest.rcp45e.canesm.90, aes(x=y.dif.rcp45e.canesm.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.90.forest

g.density.y.dif.rcp45e.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp45e.canesm.90, aes(x=y.dif.rcp45e.canesm.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.90.natural.open

g.density.y.dif.rcp45e.canesm.90.human <- ggplot(success_projections.human.rcp45e.canesm.90, aes(x=y.dif.rcp45e.canesm.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45e.canesm.90.human

difference.density.by.landuse.rcp45e.canesm.90 <- ggarrange(g.density.y.dif.rcp45e.canesm.90.ag,g.density.y.dif.rcp45e.canesm.90.forest,
                                                            g.density.y.dif.rcp45e.canesm.90.human,g.density.y.dif.rcp45e.canesm.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45e.canesm.90


####Difference of temperatures within land uses
success_projections.ag.rcp45e.canesm.90$y.dif.Tmax_std_gridmet.rcp45e.canesm.90 <- success_projections.ag.rcp45e.canesm.90$Tmax_std_gridmet.rcp45e.canesm.90 - success_projections.ag.rcp45e.canesm.90$Tmax_std_gridmet
success_projections.forest.rcp45e.canesm.90$y.dif.Tmax_std_gridmet.rcp45e.canesm.90<- success_projections.forest.rcp45e.canesm.90$Tmax_std_gridmet.rcp45e.canesm.90 - success_projections.forest.rcp45e.canesm.90$Tmax_std_gridmet
success_projections.natural.open.rcp45e.canesm.90$y.dif.Tmax_std_gridmet.rcp45e.canesm.90 <- success_projections.natural.open.rcp45e.canesm.90$Tmax_std_gridmet.rcp45e.canesm.90 - success_projections.natural.open.rcp45e.canesm.90$Tmax_std_gridmet
success_projections.human.rcp45e.canesm.90$y.dif.Tmax_std_gridmet.rcp45e.canesm.90 <- success_projections.human.rcp45e.canesm.90$Tmax_std_gridmet.rcp45e.canesm.90 - success_projections.human.rcp45e.canesm.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45e.canesm.90.ag <- ggplot(success_projections.ag.rcp45e.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.90.ag

g.Tmax.y.dif.rcp45e.canesm.90.forest <- ggplot(success_projections.forest.rcp45e.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.90.forest

g.Tmax.y.dif.rcp45e.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp45e.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.90.natural.open

g.Tmax.y.dif.rcp45e.canesm.90.human <- ggplot(success_projections.human.rcp45e.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp45e.canesm.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45e.canesm.90.human

difference.Tmax.by.landuse.rcp45e.canesm.90 <- ggarrange(g.Tmax.y.dif.rcp45e.canesm.90.ag,g.Tmax.y.dif.rcp45e.canesm.90.forest,
                                                         g.Tmax.y.dif.rcp45e.canesm.90.human,g.Tmax.y.dif.rcp45e.canesm.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45e.canesm.90



####Difference of precipitation within land uses
success_projections.ag.rcp45e.canesm.90$y.dif.pcp_std_gridmet.rcp45e.canesm.90 <- success_projections.ag.rcp45e.canesm.90$pcpbefore_raw_gridmet.rcp45e.canesm.90 - success_projections.ag.rcp45e.canesm.90$pcpbefore_raw_gridmet
success_projections.forest.rcp45e.canesm.90$y.dif.pcp_std_gridmet.rcp45e.canesm.90<- success_projections.forest.rcp45e.canesm.90$pcpbefore_raw_gridmet.rcp45e.canesm.90 - success_projections.forest.rcp45e.canesm.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45e.canesm.90$y.dif.pcp_std_gridmet.rcp45e.canesm.90 <- success_projections.natural.open.rcp45e.canesm.90$pcpbefore_raw_gridmet.rcp45e.canesm.90 - success_projections.natural.open.rcp45e.canesm.90$pcpbefore_raw_gridmet
success_projections.human.rcp45e.canesm.90$y.dif.pcp_std_gridmet.rcp45e.canesm.90 <- success_projections.human.rcp45e.canesm.90$pcpbefore_raw_gridmet.rcp45e.canesm.90 - success_projections.human.rcp45e.canesm.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45e.canesm.90.ag <- ggplot(success_projections.ag.rcp45e.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.90.ag

g.pcp.y.dif.rcp45e.canesm.90.forest <- ggplot(success_projections.forest.rcp45e.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.90.forest

g.pcp.y.dif.rcp45e.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp45e.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.90.natural.open

g.pcp.y.dif.rcp45e.canesm.90.human <- ggplot(success_projections.human.rcp45e.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp45e.canesm.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45e.canesm.90.human

difference.pcp.by.landuse.rcp45e.canesm.90 <- ggarrange(g.pcp.y.dif.rcp45e.canesm.90.ag,g.pcp.y.dif.rcp45e.canesm.90.forest,
                                                        g.pcp.y.dif.rcp45e.canesm.90.human,g.pcp.y.dif.rcp45e.canesm.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45e.canesm.90


#RCP 45m mean canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45m.canesm.mean <- 0
success_projections$y.fut.rcp45m.canesm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.canesm.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.canesm.mean  + 
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.canesm.mean )

success_projections$y.now.rcp45m.canesm.mean<- 0
success_projections$y.now.rcp45m.canesm.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                            NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                            substrate*success_projections$substrate_binary +
                                                            success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45m.canesm.mean <- success_projections$y.fut.rcp45m.canesm.mean - success_projections$y.now.rcp45m.canesm.mean


###Scatter plot with x = now y = future
g.now.rcp45m.canesm.mean <- ggplot(success_projections, aes(x=y.now.rcp45m.canesm.mean, y = y.fut.rcp45m.canesm.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 mean canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45m.canesm.mean


###Density plot of the difference future - now
g.density.y.dif.rcp45m.canesm.mean <- ggplot(success_projections, aes(x=y.dif.rcp45m.canesm.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 mean canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45m.canesm.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45m.canesm.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45m.canesm.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45m.canesm.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45m.canesm.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45m.canesm.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45m.canesm.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45m.canesm.mean.ag <- ggplot(success_projections.ag.rcp45m.canesm.mean , aes(x=y.dif.rcp45m.canesm.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.mean.ag

g.density.y.dif.rcp45m.canesm.mean.forest <- ggplot(success_projections.forest.rcp45m.canesm.mean, aes(x=y.dif.rcp45m.canesm.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.mean.forest

g.density.y.dif.rcp45m.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp45m.canesm.mean, aes(x=y.dif.rcp45m.canesm.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.mean.natural.open

g.density.y.dif.rcp45m.canesm.mean.human <- ggplot(success_projections.human.rcp45m.canesm.mean, aes(x=y.dif.rcp45m.canesm.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.mean.human

difference.density.by.landuse.rcp45m.canesm.mean <- ggarrange(g.density.y.dif.rcp45m.canesm.mean.ag,g.density.y.dif.rcp45m.canesm.mean.forest,
                                                              g.density.y.dif.rcp45m.canesm.mean.human,g.density.y.dif.rcp45m.canesm.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45m.canesm.mean


####Difference of temperatures within land uses
success_projections.ag.rcp45m.canesm.mean$y.dif.Tmax_std_gridmet.rcp45m.canesm.mean <- success_projections.ag.rcp45m.canesm.mean$Tmax_std_gridmet.rcp45m.canesm.mean - success_projections.ag.rcp45m.canesm.mean$Tmax_std_gridmet
success_projections.forest.rcp45m.canesm.mean$y.dif.Tmax_std_gridmet.rcp45m.canesm.mean<- success_projections.forest.rcp45m.canesm.mean$Tmax_std_gridmet.rcp45m.canesm.mean - success_projections.forest.rcp45m.canesm.mean$Tmax_std_gridmet
success_projections.natural.open.rcp45m.canesm.mean$y.dif.Tmax_std_gridmet.rcp45m.canesm.mean <- success_projections.natural.open.rcp45m.canesm.mean$Tmax_std_gridmet.rcp45m.canesm.mean - success_projections.natural.open.rcp45m.canesm.mean$Tmax_std_gridmet
success_projections.human.rcp45m.canesm.mean$y.dif.Tmax_std_gridmet.rcp45m.canesm.mean <- success_projections.human.rcp45m.canesm.mean$Tmax_std_gridmet.rcp45m.canesm.mean - success_projections.human.rcp45m.canesm.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45m.canesm.mean.ag <- ggplot(success_projections.ag.rcp45m.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.mean.ag

g.Tmax.y.dif.rcp45m.canesm.mean.forest <- ggplot(success_projections.forest.rcp45m.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.mean.forest

g.Tmax.y.dif.rcp45m.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp45m.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.mean.natural.open

g.Tmax.y.dif.rcp45m.canesm.mean.human <- ggplot(success_projections.human.rcp45m.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.mean.human

difference.Tmax.by.landuse.rcp45m.canesm.mean <- ggarrange(g.Tmax.y.dif.rcp45m.canesm.mean.ag,g.Tmax.y.dif.rcp45m.canesm.mean.forest,
                                                           g.Tmax.y.dif.rcp45m.canesm.mean.human,g.Tmax.y.dif.rcp45m.canesm.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45m.canesm.mean



####Difference of precipitation within land uses
success_projections.ag.rcp45m.canesm.mean$y.dif.pcp_std_gridmet.rcp45m.canesm.mean <- success_projections.ag.rcp45m.canesm.mean$pcpbefore_raw_gridmet.rcp45m.canesm.mean - success_projections.ag.rcp45m.canesm.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp45m.canesm.mean$y.dif.pcp_std_gridmet.rcp45m.canesm.mean<- success_projections.forest.rcp45m.canesm.mean$pcpbefore_raw_gridmet.rcp45m.canesm.mean - success_projections.forest.rcp45m.canesm.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45m.canesm.mean$y.dif.pcp_std_gridmet.rcp45m.canesm.mean <- success_projections.natural.open.rcp45m.canesm.mean$pcpbefore_raw_gridmet.rcp45m.canesm.mean - success_projections.natural.open.rcp45m.canesm.mean$pcpbefore_raw_gridmet
success_projections.human.rcp45m.canesm.mean$y.dif.pcp_std_gridmet.rcp45m.canesm.mean <- success_projections.human.rcp45m.canesm.mean$pcpbefore_raw_gridmet.rcp45m.canesm.mean - success_projections.human.rcp45m.canesm.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45m.canesm.mean.ag <- ggplot(success_projections.ag.rcp45m.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.mean.ag

g.pcp.y.dif.rcp45m.canesm.mean.forest <- ggplot(success_projections.forest.rcp45m.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.mean.forest

g.pcp.y.dif.rcp45m.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp45m.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.mean.natural.open

g.pcp.y.dif.rcp45m.canesm.mean.human <- ggplot(success_projections.human.rcp45m.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.mean.human

difference.pcp.by.landuse.rcp45m.canesm.mean <- ggarrange(g.pcp.y.dif.rcp45m.canesm.mean.ag,g.pcp.y.dif.rcp45m.canesm.mean.forest,
                                                          g.pcp.y.dif.rcp45m.canesm.mean.human,g.pcp.y.dif.rcp45m.canesm.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45m.canesm.mean

#RCP 45m 10% canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45m.canesm.10 <- 0
success_projections$y.fut.rcp45m.canesm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.canesm.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.canesm.10  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.canesm.10 )

success_projections$y.now.rcp45m.canesm.10<- 0
success_projections$y.now.rcp45m.canesm.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45m.canesm.10 <- success_projections$y.fut.rcp45m.canesm.10 - success_projections$y.now.rcp45m.canesm.10


###Scatter plot with x = now y = future
g.now.rcp45m.canesm.10 <- ggplot(success_projections, aes(x=y.now.rcp45m.canesm.10, y = y.fut.rcp45m.canesm.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 10% canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45m.canesm.10


###Density plot of the difference future - now
g.density.y.dif.rcp45m.canesm.10 <- ggplot(success_projections, aes(x=y.dif.rcp45m.canesm.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 10% canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45m.canesm.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45m.canesm.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45m.canesm.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45m.canesm.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45m.canesm.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45m.canesm.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45m.canesm.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45m.canesm.10.ag <- ggplot(success_projections.ag.rcp45m.canesm.10 , aes(x=y.dif.rcp45m.canesm.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.10.ag

g.density.y.dif.rcp45m.canesm.10.forest <- ggplot(success_projections.forest.rcp45m.canesm.10, aes(x=y.dif.rcp45m.canesm.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.10.forest

g.density.y.dif.rcp45m.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp45m.canesm.10, aes(x=y.dif.rcp45m.canesm.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.10.natural.open

g.density.y.dif.rcp45m.canesm.10.human <- ggplot(success_projections.human.rcp45m.canesm.10, aes(x=y.dif.rcp45m.canesm.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.10.human

difference.density.by.landuse.rcp45m.canesm.10 <- ggarrange(g.density.y.dif.rcp45m.canesm.10.ag,g.density.y.dif.rcp45m.canesm.10.forest,
                                                            g.density.y.dif.rcp45m.canesm.10.human,g.density.y.dif.rcp45m.canesm.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45m.canesm.10


####Difference of temperatures within land uses
success_projections.ag.rcp45m.canesm.10$y.dif.Tmax_std_gridmet.rcp45m.canesm.10 <- success_projections.ag.rcp45m.canesm.10$Tmax_std_gridmet.rcp45m.canesm.10 - success_projections.ag.rcp45m.canesm.10$Tmax_std_gridmet
success_projections.forest.rcp45m.canesm.10$y.dif.Tmax_std_gridmet.rcp45m.canesm.10<- success_projections.forest.rcp45m.canesm.10$Tmax_std_gridmet.rcp45m.canesm.10 - success_projections.forest.rcp45m.canesm.10$Tmax_std_gridmet
success_projections.natural.open.rcp45m.canesm.10$y.dif.Tmax_std_gridmet.rcp45m.canesm.10 <- success_projections.natural.open.rcp45m.canesm.10$Tmax_std_gridmet.rcp45m.canesm.10 - success_projections.natural.open.rcp45m.canesm.10$Tmax_std_gridmet
success_projections.human.rcp45m.canesm.10$y.dif.Tmax_std_gridmet.rcp45m.canesm.10 <- success_projections.human.rcp45m.canesm.10$Tmax_std_gridmet.rcp45m.canesm.10 - success_projections.human.rcp45m.canesm.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45m.canesm.10.ag <- ggplot(success_projections.ag.rcp45m.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.10.ag

g.Tmax.y.dif.rcp45m.canesm.10.forest <- ggplot(success_projections.forest.rcp45m.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.10.forest

g.Tmax.y.dif.rcp45m.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp45m.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.10.natural.open

g.Tmax.y.dif.rcp45m.canesm.10.human <- ggplot(success_projections.human.rcp45m.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.10.human

difference.Tmax.by.landuse.rcp45m.canesm.10 <- ggarrange(g.Tmax.y.dif.rcp45m.canesm.10.ag,g.Tmax.y.dif.rcp45m.canesm.10.forest,
                                                         g.Tmax.y.dif.rcp45m.canesm.10.human,g.Tmax.y.dif.rcp45m.canesm.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45m.canesm.10



####Difference of precipitation within land uses
success_projections.ag.rcp45m.canesm.10$y.dif.pcp_std_gridmet.rcp45m.canesm.10 <- success_projections.ag.rcp45m.canesm.10$pcpbefore_raw_gridmet.rcp45m.canesm.10 - success_projections.ag.rcp45m.canesm.10$pcpbefore_raw_gridmet
success_projections.forest.rcp45m.canesm.10$y.dif.pcp_std_gridmet.rcp45m.canesm.10<- success_projections.forest.rcp45m.canesm.10$pcpbefore_raw_gridmet.rcp45m.canesm.10 - success_projections.forest.rcp45m.canesm.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45m.canesm.10$y.dif.pcp_std_gridmet.rcp45m.canesm.10 <- success_projections.natural.open.rcp45m.canesm.10$pcpbefore_raw_gridmet.rcp45m.canesm.10 - success_projections.natural.open.rcp45m.canesm.10$pcpbefore_raw_gridmet
success_projections.human.rcp45m.canesm.10$y.dif.pcp_std_gridmet.rcp45m.canesm.10 <- success_projections.human.rcp45m.canesm.10$pcpbefore_raw_gridmet.rcp45m.canesm.10 - success_projections.human.rcp45m.canesm.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45m.canesm.10.ag <- ggplot(success_projections.ag.rcp45m.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.10.ag

g.pcp.y.dif.rcp45m.canesm.10.forest <- ggplot(success_projections.forest.rcp45m.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.10.forest

g.pcp.y.dif.rcp45m.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp45m.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.10.natural.open

g.pcp.y.dif.rcp45m.canesm.10.human <- ggplot(success_projections.human.rcp45m.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.10.human

difference.pcp.by.landuse.rcp45m.canesm.10 <- ggarrange(g.pcp.y.dif.rcp45m.canesm.10.ag,g.pcp.y.dif.rcp45m.canesm.10.forest,
                                                        g.pcp.y.dif.rcp45m.canesm.10.human,g.pcp.y.dif.rcp45m.canesm.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45m.canesm.10

#RCP 45m 90% canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp45m.canesm.90 <- 0
success_projections$y.fut.rcp45m.canesm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp45m.canesm.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp45m.canesm.90  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp45m.canesm.90 )

success_projections$y.now.rcp45m.canesm.90<- 0
success_projections$y.now.rcp45m.canesm.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp45m.canesm.90 <- success_projections$y.fut.rcp45m.canesm.90 - success_projections$y.now.rcp45m.canesm.90


###Scatter plot with x = now y = future
g.now.rcp45m.canesm.90 <- ggplot(success_projections, aes(x=y.now.rcp45m.canesm.90, y = y.fut.rcp45m.canesm.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP45 90% canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp45m.canesm.90


###Density plot of the difference future - now
g.density.y.dif.rcp45m.canesm.90 <- ggplot(success_projections, aes(x=y.dif.rcp45m.canesm.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP45 90% canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp45m.canesm.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp45m.canesm.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp45m.canesm.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp45m.canesm.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp45m.canesm.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp45m.canesm.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp45m.canesm.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp45m.canesm.90.ag <- ggplot(success_projections.ag.rcp45m.canesm.90 , aes(x=y.dif.rcp45m.canesm.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.90.ag

g.density.y.dif.rcp45m.canesm.90.forest <- ggplot(success_projections.forest.rcp45m.canesm.90, aes(x=y.dif.rcp45m.canesm.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.90.forest

g.density.y.dif.rcp45m.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp45m.canesm.90, aes(x=y.dif.rcp45m.canesm.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.90.natural.open

g.density.y.dif.rcp45m.canesm.90.human <- ggplot(success_projections.human.rcp45m.canesm.90, aes(x=y.dif.rcp45m.canesm.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp45m.canesm.90.human

difference.density.by.landuse.rcp45m.canesm.90 <- ggarrange(g.density.y.dif.rcp45m.canesm.90.ag,g.density.y.dif.rcp45m.canesm.90.forest,
                                                            g.density.y.dif.rcp45m.canesm.90.human,g.density.y.dif.rcp45m.canesm.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp45m.canesm.90


####Difference of temperatures within land uses
success_projections.ag.rcp45m.canesm.90$y.dif.Tmax_std_gridmet.rcp45m.canesm.90 <- success_projections.ag.rcp45m.canesm.90$Tmax_std_gridmet.rcp45m.canesm.90 - success_projections.ag.rcp45m.canesm.90$Tmax_std_gridmet
success_projections.forest.rcp45m.canesm.90$y.dif.Tmax_std_gridmet.rcp45m.canesm.90<- success_projections.forest.rcp45m.canesm.90$Tmax_std_gridmet.rcp45m.canesm.90 - success_projections.forest.rcp45m.canesm.90$Tmax_std_gridmet
success_projections.natural.open.rcp45m.canesm.90$y.dif.Tmax_std_gridmet.rcp45m.canesm.90 <- success_projections.natural.open.rcp45m.canesm.90$Tmax_std_gridmet.rcp45m.canesm.90 - success_projections.natural.open.rcp45m.canesm.90$Tmax_std_gridmet
success_projections.human.rcp45m.canesm.90$y.dif.Tmax_std_gridmet.rcp45m.canesm.90 <- success_projections.human.rcp45m.canesm.90$Tmax_std_gridmet.rcp45m.canesm.90 - success_projections.human.rcp45m.canesm.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp45m.canesm.90.ag <- ggplot(success_projections.ag.rcp45m.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.90.ag

g.Tmax.y.dif.rcp45m.canesm.90.forest <- ggplot(success_projections.forest.rcp45m.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.90.forest

g.Tmax.y.dif.rcp45m.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp45m.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.90.natural.open

g.Tmax.y.dif.rcp45m.canesm.90.human <- ggplot(success_projections.human.rcp45m.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp45m.canesm.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp45m.canesm.90.human

difference.Tmax.by.landuse.rcp45m.canesm.90 <- ggarrange(g.Tmax.y.dif.rcp45m.canesm.90.ag,g.Tmax.y.dif.rcp45m.canesm.90.forest,
                                                         g.Tmax.y.dif.rcp45m.canesm.90.human,g.Tmax.y.dif.rcp45m.canesm.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp45m.canesm.90



####Difference of precipitation within land uses
success_projections.ag.rcp45m.canesm.90$y.dif.pcp_std_gridmet.rcp45m.canesm.90 <- success_projections.ag.rcp45m.canesm.90$pcpbefore_raw_gridmet.rcp45m.canesm.90 - success_projections.ag.rcp45m.canesm.90$pcpbefore_raw_gridmet
success_projections.forest.rcp45m.canesm.90$y.dif.pcp_std_gridmet.rcp45m.canesm.90<- success_projections.forest.rcp45m.canesm.90$pcpbefore_raw_gridmet.rcp45m.canesm.90 - success_projections.forest.rcp45m.canesm.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp45m.canesm.90$y.dif.pcp_std_gridmet.rcp45m.canesm.90 <- success_projections.natural.open.rcp45m.canesm.90$pcpbefore_raw_gridmet.rcp45m.canesm.90 - success_projections.natural.open.rcp45m.canesm.90$pcpbefore_raw_gridmet
success_projections.human.rcp45m.canesm.90$y.dif.pcp_std_gridmet.rcp45m.canesm.90 <- success_projections.human.rcp45m.canesm.90$pcpbefore_raw_gridmet.rcp45m.canesm.90 - success_projections.human.rcp45m.canesm.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp45m.canesm.90.ag <- ggplot(success_projections.ag.rcp45m.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.90.ag

g.pcp.y.dif.rcp45m.canesm.90.forest <- ggplot(success_projections.forest.rcp45m.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.90.forest

g.pcp.y.dif.rcp45m.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp45m.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.90.natural.open

g.pcp.y.dif.rcp45m.canesm.90.human <- ggplot(success_projections.human.rcp45m.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp45m.canesm.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp45m.canesm.90.human

difference.pcp.by.landuse.rcp45m.canesm.90 <- ggarrange(g.pcp.y.dif.rcp45m.canesm.90.ag,g.pcp.y.dif.rcp45m.canesm.90.forest,
                                                        g.pcp.y.dif.rcp45m.canesm.90.human,g.pcp.y.dif.rcp45m.canesm.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp45m.canesm.90


#RCP 85e mean canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85e.canesm.mean <- 0
success_projections$y.fut.rcp85e.canesm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.canesm.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.canesm.mean  + 
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.canesm.mean )

success_projections$y.now.rcp85e.canesm.mean<- 0
success_projections$y.now.rcp85e.canesm.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                            NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                            substrate*success_projections$substrate_binary +
                                                            success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85e.canesm.mean <- success_projections$y.fut.rcp85e.canesm.mean - success_projections$y.now.rcp85e.canesm.mean


###Scatter plot with x = now y = future
g.now.rcp85e.canesm.mean <- ggplot(success_projections, aes(x=y.now.rcp85e.canesm.mean, y = y.fut.rcp85e.canesm.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 mean canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85e.canesm.mean


###Density plot of the difference future - now
g.density.y.dif.rcp85e.canesm.mean <- ggplot(success_projections, aes(x=y.dif.rcp85e.canesm.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 mean canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85e.canesm.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85e.canesm.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85e.canesm.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85e.canesm.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85e.canesm.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85e.canesm.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85e.canesm.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85e.canesm.mean.ag <- ggplot(success_projections.ag.rcp85e.canesm.mean , aes(x=y.dif.rcp85e.canesm.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.mean.ag

g.density.y.dif.rcp85e.canesm.mean.forest <- ggplot(success_projections.forest.rcp85e.canesm.mean, aes(x=y.dif.rcp85e.canesm.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.mean.forest

g.density.y.dif.rcp85e.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp85e.canesm.mean, aes(x=y.dif.rcp85e.canesm.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.mean.natural.open

g.density.y.dif.rcp85e.canesm.mean.human <- ggplot(success_projections.human.rcp85e.canesm.mean, aes(x=y.dif.rcp85e.canesm.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.mean.human

difference.density.by.landuse.rcp85e.canesm.mean <- ggarrange(g.density.y.dif.rcp85e.canesm.mean.ag,g.density.y.dif.rcp85e.canesm.mean.forest,
                                                              g.density.y.dif.rcp85e.canesm.mean.human,g.density.y.dif.rcp85e.canesm.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85e.canesm.mean


####Difference of temperatures within land uses
success_projections.ag.rcp85e.canesm.mean$y.dif.Tmax_std_gridmet.rcp85e.canesm.mean <- success_projections.ag.rcp85e.canesm.mean$Tmax_std_gridmet.rcp85e.canesm.mean - success_projections.ag.rcp85e.canesm.mean$Tmax_std_gridmet
success_projections.forest.rcp85e.canesm.mean$y.dif.Tmax_std_gridmet.rcp85e.canesm.mean<- success_projections.forest.rcp85e.canesm.mean$Tmax_std_gridmet.rcp85e.canesm.mean - success_projections.forest.rcp85e.canesm.mean$Tmax_std_gridmet
success_projections.natural.open.rcp85e.canesm.mean$y.dif.Tmax_std_gridmet.rcp85e.canesm.mean <- success_projections.natural.open.rcp85e.canesm.mean$Tmax_std_gridmet.rcp85e.canesm.mean - success_projections.natural.open.rcp85e.canesm.mean$Tmax_std_gridmet
success_projections.human.rcp85e.canesm.mean$y.dif.Tmax_std_gridmet.rcp85e.canesm.mean <- success_projections.human.rcp85e.canesm.mean$Tmax_std_gridmet.rcp85e.canesm.mean - success_projections.human.rcp85e.canesm.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85e.canesm.mean.ag <- ggplot(success_projections.ag.rcp85e.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.mean.ag

g.Tmax.y.dif.rcp85e.canesm.mean.forest <- ggplot(success_projections.forest.rcp85e.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.mean.forest

g.Tmax.y.dif.rcp85e.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp85e.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.mean.natural.open

g.Tmax.y.dif.rcp85e.canesm.mean.human <- ggplot(success_projections.human.rcp85e.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.mean.human

difference.Tmax.by.landuse.rcp85e.canesm.mean <- ggarrange(g.Tmax.y.dif.rcp85e.canesm.mean.ag,g.Tmax.y.dif.rcp85e.canesm.mean.forest,
                                                           g.Tmax.y.dif.rcp85e.canesm.mean.human,g.Tmax.y.dif.rcp85e.canesm.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85e.canesm.mean



####Difference of precipitation within land uses
success_projections.ag.rcp85e.canesm.mean$y.dif.pcp_std_gridmet.rcp85e.canesm.mean <- success_projections.ag.rcp85e.canesm.mean$pcpbefore_raw_gridmet.rcp85e.canesm.mean - success_projections.ag.rcp85e.canesm.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp85e.canesm.mean$y.dif.pcp_std_gridmet.rcp85e.canesm.mean<- success_projections.forest.rcp85e.canesm.mean$pcpbefore_raw_gridmet.rcp85e.canesm.mean - success_projections.forest.rcp85e.canesm.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85e.canesm.mean$y.dif.pcp_std_gridmet.rcp85e.canesm.mean <- success_projections.natural.open.rcp85e.canesm.mean$pcpbefore_raw_gridmet.rcp85e.canesm.mean - success_projections.natural.open.rcp85e.canesm.mean$pcpbefore_raw_gridmet
success_projections.human.rcp85e.canesm.mean$y.dif.pcp_std_gridmet.rcp85e.canesm.mean <- success_projections.human.rcp85e.canesm.mean$pcpbefore_raw_gridmet.rcp85e.canesm.mean - success_projections.human.rcp85e.canesm.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85e.canesm.mean.ag <- ggplot(success_projections.ag.rcp85e.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.mean.ag

g.pcp.y.dif.rcp85e.canesm.mean.forest <- ggplot(success_projections.forest.rcp85e.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.mean.forest

g.pcp.y.dif.rcp85e.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp85e.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.mean.natural.open

g.pcp.y.dif.rcp85e.canesm.mean.human <- ggplot(success_projections.human.rcp85e.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.mean.human

difference.pcp.by.landuse.rcp85e.canesm.mean <- ggarrange(g.pcp.y.dif.rcp85e.canesm.mean.ag,g.pcp.y.dif.rcp85e.canesm.mean.forest,
                                                          g.pcp.y.dif.rcp85e.canesm.mean.human,g.pcp.y.dif.rcp85e.canesm.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85e.canesm.mean

#RCP 85e 10% canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85e.canesm.10 <- 0
success_projections$y.fut.rcp85e.canesm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.canesm.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.canesm.10  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.canesm.10 )

success_projections$y.now.rcp85e.canesm.10<- 0
success_projections$y.now.rcp85e.canesm.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85e.canesm.10 <- success_projections$y.fut.rcp85e.canesm.10 - success_projections$y.now.rcp85e.canesm.10


###Scatter plot with x = now y = future
g.now.rcp85e.canesm.10 <- ggplot(success_projections, aes(x=y.now.rcp85e.canesm.10, y = y.fut.rcp85e.canesm.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 10% canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85e.canesm.10


###Density plot of the difference future - now
g.density.y.dif.rcp85e.canesm.10 <- ggplot(success_projections, aes(x=y.dif.rcp85e.canesm.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 10% canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85e.canesm.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85e.canesm.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85e.canesm.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85e.canesm.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85e.canesm.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85e.canesm.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85e.canesm.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85e.canesm.10.ag <- ggplot(success_projections.ag.rcp85e.canesm.10 , aes(x=y.dif.rcp85e.canesm.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.10.ag

g.density.y.dif.rcp85e.canesm.10.forest <- ggplot(success_projections.forest.rcp85e.canesm.10, aes(x=y.dif.rcp85e.canesm.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.10.forest

g.density.y.dif.rcp85e.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp85e.canesm.10, aes(x=y.dif.rcp85e.canesm.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.10.natural.open

g.density.y.dif.rcp85e.canesm.10.human <- ggplot(success_projections.human.rcp85e.canesm.10, aes(x=y.dif.rcp85e.canesm.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.10.human

difference.density.by.landuse.rcp85e.canesm.10 <- ggarrange(g.density.y.dif.rcp85e.canesm.10.ag,g.density.y.dif.rcp85e.canesm.10.forest,
                                                            g.density.y.dif.rcp85e.canesm.10.human,g.density.y.dif.rcp85e.canesm.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85e.canesm.10


####Difference of temperatures within land uses
success_projections.ag.rcp85e.canesm.10$y.dif.Tmax_std_gridmet.rcp85e.canesm.10 <- success_projections.ag.rcp85e.canesm.10$Tmax_std_gridmet.rcp85e.canesm.10 - success_projections.ag.rcp85e.canesm.10$Tmax_std_gridmet
success_projections.forest.rcp85e.canesm.10$y.dif.Tmax_std_gridmet.rcp85e.canesm.10<- success_projections.forest.rcp85e.canesm.10$Tmax_std_gridmet.rcp85e.canesm.10 - success_projections.forest.rcp85e.canesm.10$Tmax_std_gridmet
success_projections.natural.open.rcp85e.canesm.10$y.dif.Tmax_std_gridmet.rcp85e.canesm.10 <- success_projections.natural.open.rcp85e.canesm.10$Tmax_std_gridmet.rcp85e.canesm.10 - success_projections.natural.open.rcp85e.canesm.10$Tmax_std_gridmet
success_projections.human.rcp85e.canesm.10$y.dif.Tmax_std_gridmet.rcp85e.canesm.10 <- success_projections.human.rcp85e.canesm.10$Tmax_std_gridmet.rcp85e.canesm.10 - success_projections.human.rcp85e.canesm.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85e.canesm.10.ag <- ggplot(success_projections.ag.rcp85e.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.10.ag

g.Tmax.y.dif.rcp85e.canesm.10.forest <- ggplot(success_projections.forest.rcp85e.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.10.forest

g.Tmax.y.dif.rcp85e.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp85e.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.10.natural.open

g.Tmax.y.dif.rcp85e.canesm.10.human <- ggplot(success_projections.human.rcp85e.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.10.human

difference.Tmax.by.landuse.rcp85e.canesm.10 <- ggarrange(g.Tmax.y.dif.rcp85e.canesm.10.ag,g.Tmax.y.dif.rcp85e.canesm.10.forest,
                                                         g.Tmax.y.dif.rcp85e.canesm.10.human,g.Tmax.y.dif.rcp85e.canesm.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85e.canesm.10



####Difference of precipitation within land uses
success_projections.ag.rcp85e.canesm.10$y.dif.pcp_std_gridmet.rcp85e.canesm.10 <- success_projections.ag.rcp85e.canesm.10$pcpbefore_raw_gridmet.rcp85e.canesm.10 - success_projections.ag.rcp85e.canesm.10$pcpbefore_raw_gridmet
success_projections.forest.rcp85e.canesm.10$y.dif.pcp_std_gridmet.rcp85e.canesm.10<- success_projections.forest.rcp85e.canesm.10$pcpbefore_raw_gridmet.rcp85e.canesm.10 - success_projections.forest.rcp85e.canesm.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85e.canesm.10$y.dif.pcp_std_gridmet.rcp85e.canesm.10 <- success_projections.natural.open.rcp85e.canesm.10$pcpbefore_raw_gridmet.rcp85e.canesm.10 - success_projections.natural.open.rcp85e.canesm.10$pcpbefore_raw_gridmet
success_projections.human.rcp85e.canesm.10$y.dif.pcp_std_gridmet.rcp85e.canesm.10 <- success_projections.human.rcp85e.canesm.10$pcpbefore_raw_gridmet.rcp85e.canesm.10 - success_projections.human.rcp85e.canesm.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85e.canesm.10.ag <- ggplot(success_projections.ag.rcp85e.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.10.ag

g.pcp.y.dif.rcp85e.canesm.10.forest <- ggplot(success_projections.forest.rcp85e.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.10.forest

g.pcp.y.dif.rcp85e.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp85e.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.10.natural.open

g.pcp.y.dif.rcp85e.canesm.10.human <- ggplot(success_projections.human.rcp85e.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.10.human

difference.pcp.by.landuse.rcp85e.canesm.10 <- ggarrange(g.pcp.y.dif.rcp85e.canesm.10.ag,g.pcp.y.dif.rcp85e.canesm.10.forest,
                                                        g.pcp.y.dif.rcp85e.canesm.10.human,g.pcp.y.dif.rcp85e.canesm.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85e.canesm.10

#RCP 85e 90% canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85e.canesm.90 <- 0
success_projections$y.fut.rcp85e.canesm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85e.canesm.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85e.canesm.90  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85e.canesm.90 )

success_projections$y.now.rcp85e.canesm.90<- 0
success_projections$y.now.rcp85e.canesm.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85e.canesm.90 <- success_projections$y.fut.rcp85e.canesm.90 - success_projections$y.now.rcp85e.canesm.90


###Scatter plot with x = now y = future
g.now.rcp85e.canesm.90 <- ggplot(success_projections, aes(x=y.now.rcp85e.canesm.90, y = y.fut.rcp85e.canesm.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 90% canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85e.canesm.90


###Density plot of the difference future - now
g.density.y.dif.rcp85e.canesm.90 <- ggplot(success_projections, aes(x=y.dif.rcp85e.canesm.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 90% canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85e.canesm.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85e.canesm.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85e.canesm.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85e.canesm.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85e.canesm.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85e.canesm.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85e.canesm.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85e.canesm.90.ag <- ggplot(success_projections.ag.rcp85e.canesm.90 , aes(x=y.dif.rcp85e.canesm.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.90.ag

g.density.y.dif.rcp85e.canesm.90.forest <- ggplot(success_projections.forest.rcp85e.canesm.90, aes(x=y.dif.rcp85e.canesm.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.90.forest

g.density.y.dif.rcp85e.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp85e.canesm.90, aes(x=y.dif.rcp85e.canesm.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.90.natural.open

g.density.y.dif.rcp85e.canesm.90.human <- ggplot(success_projections.human.rcp85e.canesm.90, aes(x=y.dif.rcp85e.canesm.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85e.canesm.90.human

difference.density.by.landuse.rcp85e.canesm.90 <- ggarrange(g.density.y.dif.rcp85e.canesm.90.ag,g.density.y.dif.rcp85e.canesm.90.forest,
                                                            g.density.y.dif.rcp85e.canesm.90.human,g.density.y.dif.rcp85e.canesm.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85e.canesm.90


####Difference of temperatures within land uses
success_projections.ag.rcp85e.canesm.90$y.dif.Tmax_std_gridmet.rcp85e.canesm.90 <- success_projections.ag.rcp85e.canesm.90$Tmax_std_gridmet.rcp85e.canesm.90 - success_projections.ag.rcp85e.canesm.90$Tmax_std_gridmet
success_projections.forest.rcp85e.canesm.90$y.dif.Tmax_std_gridmet.rcp85e.canesm.90<- success_projections.forest.rcp85e.canesm.90$Tmax_std_gridmet.rcp85e.canesm.90 - success_projections.forest.rcp85e.canesm.90$Tmax_std_gridmet
success_projections.natural.open.rcp85e.canesm.90$y.dif.Tmax_std_gridmet.rcp85e.canesm.90 <- success_projections.natural.open.rcp85e.canesm.90$Tmax_std_gridmet.rcp85e.canesm.90 - success_projections.natural.open.rcp85e.canesm.90$Tmax_std_gridmet
success_projections.human.rcp85e.canesm.90$y.dif.Tmax_std_gridmet.rcp85e.canesm.90 <- success_projections.human.rcp85e.canesm.90$Tmax_std_gridmet.rcp85e.canesm.90 - success_projections.human.rcp85e.canesm.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85e.canesm.90.ag <- ggplot(success_projections.ag.rcp85e.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.90.ag

g.Tmax.y.dif.rcp85e.canesm.90.forest <- ggplot(success_projections.forest.rcp85e.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.90.forest

g.Tmax.y.dif.rcp85e.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp85e.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.90.natural.open

g.Tmax.y.dif.rcp85e.canesm.90.human <- ggplot(success_projections.human.rcp85e.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp85e.canesm.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85e.canesm.90.human

difference.Tmax.by.landuse.rcp85e.canesm.90 <- ggarrange(g.Tmax.y.dif.rcp85e.canesm.90.ag,g.Tmax.y.dif.rcp85e.canesm.90.forest,
                                                         g.Tmax.y.dif.rcp85e.canesm.90.human,g.Tmax.y.dif.rcp85e.canesm.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85e.canesm.90



####Difference of precipitation within land uses
success_projections.ag.rcp85e.canesm.90$y.dif.pcp_std_gridmet.rcp85e.canesm.90 <- success_projections.ag.rcp85e.canesm.90$pcpbefore_raw_gridmet.rcp85e.canesm.90 - success_projections.ag.rcp85e.canesm.90$pcpbefore_raw_gridmet
success_projections.forest.rcp85e.canesm.90$y.dif.pcp_std_gridmet.rcp85e.canesm.90<- success_projections.forest.rcp85e.canesm.90$pcpbefore_raw_gridmet.rcp85e.canesm.90 - success_projections.forest.rcp85e.canesm.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85e.canesm.90$y.dif.pcp_std_gridmet.rcp85e.canesm.90 <- success_projections.natural.open.rcp85e.canesm.90$pcpbefore_raw_gridmet.rcp85e.canesm.90 - success_projections.natural.open.rcp85e.canesm.90$pcpbefore_raw_gridmet
success_projections.human.rcp85e.canesm.90$y.dif.pcp_std_gridmet.rcp85e.canesm.90 <- success_projections.human.rcp85e.canesm.90$pcpbefore_raw_gridmet.rcp85e.canesm.90 - success_projections.human.rcp85e.canesm.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85e.canesm.90.ag <- ggplot(success_projections.ag.rcp85e.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.90.ag

g.pcp.y.dif.rcp85e.canesm.90.forest <- ggplot(success_projections.forest.rcp85e.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.90.forest

g.pcp.y.dif.rcp85e.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp85e.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.90.natural.open

g.pcp.y.dif.rcp85e.canesm.90.human <- ggplot(success_projections.human.rcp85e.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp85e.canesm.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85e.canesm.90.human

difference.pcp.by.landuse.rcp85e.canesm.90 <- ggarrange(g.pcp.y.dif.rcp85e.canesm.90.ag,g.pcp.y.dif.rcp85e.canesm.90.forest,
                                                        g.pcp.y.dif.rcp85e.canesm.90.human,g.pcp.y.dif.rcp85e.canesm.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85e.canesm.90


#RCP 85m mean canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85m.canesm.mean <- 0
success_projections$y.fut.rcp85m.canesm.mean <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.canesm.mean   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.canesm.mean  + 
                                                             NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                             substrate*success_projections$substrate_binary +
                                                             success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.canesm.mean )

success_projections$y.now.rcp85m.canesm.mean<- 0
success_projections$y.now.rcp85m.canesm.mean <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                            NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                            substrate*success_projections$substrate_binary +
                                                            success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85m.canesm.mean <- success_projections$y.fut.rcp85m.canesm.mean - success_projections$y.now.rcp85m.canesm.mean


###Scatter plot with x = now y = future
g.now.rcp85m.canesm.mean <- ggplot(success_projections, aes(x=y.now.rcp85m.canesm.mean, y = y.fut.rcp85m.canesm.mean))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 mean canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85m.canesm.mean


###Density plot of the difference future - now
g.density.y.dif.rcp85m.canesm.mean <- ggplot(success_projections, aes(x=y.dif.rcp85m.canesm.mean ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 mean canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.mean


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85m.canesm.mean <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85m.canesm.mean))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85m.canesm.mean 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85m.canesm.mean <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85m.canesm.mean <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85m.canesm.mean <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85m.canesm.mean <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85m.canesm.mean.ag <- ggplot(success_projections.ag.rcp85m.canesm.mean , aes(x=y.dif.rcp85m.canesm.mean ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.mean.ag

g.density.y.dif.rcp85m.canesm.mean.forest <- ggplot(success_projections.forest.rcp85m.canesm.mean, aes(x=y.dif.rcp85m.canesm.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.mean.forest

g.density.y.dif.rcp85m.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp85m.canesm.mean, aes(x=y.dif.rcp85m.canesm.mean ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.mean.natural.open

g.density.y.dif.rcp85m.canesm.mean.human <- ggplot(success_projections.human.rcp85m.canesm.mean, aes(x=y.dif.rcp85m.canesm.mean ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.mean.human

difference.density.by.landuse.rcp85m.canesm.mean <- ggarrange(g.density.y.dif.rcp85m.canesm.mean.ag,g.density.y.dif.rcp85m.canesm.mean.forest,
                                                              g.density.y.dif.rcp85m.canesm.mean.human,g.density.y.dif.rcp85m.canesm.mean.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85m.canesm.mean


####Difference of temperatures within land uses
success_projections.ag.rcp85m.canesm.mean$y.dif.Tmax_std_gridmet.rcp85m.canesm.mean <- success_projections.ag.rcp85m.canesm.mean$Tmax_std_gridmet.rcp85m.canesm.mean - success_projections.ag.rcp85m.canesm.mean$Tmax_std_gridmet
success_projections.forest.rcp85m.canesm.mean$y.dif.Tmax_std_gridmet.rcp85m.canesm.mean<- success_projections.forest.rcp85m.canesm.mean$Tmax_std_gridmet.rcp85m.canesm.mean - success_projections.forest.rcp85m.canesm.mean$Tmax_std_gridmet
success_projections.natural.open.rcp85m.canesm.mean$y.dif.Tmax_std_gridmet.rcp85m.canesm.mean <- success_projections.natural.open.rcp85m.canesm.mean$Tmax_std_gridmet.rcp85m.canesm.mean - success_projections.natural.open.rcp85m.canesm.mean$Tmax_std_gridmet
success_projections.human.rcp85m.canesm.mean$y.dif.Tmax_std_gridmet.rcp85m.canesm.mean <- success_projections.human.rcp85m.canesm.mean$Tmax_std_gridmet.rcp85m.canesm.mean - success_projections.human.rcp85m.canesm.mean$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85m.canesm.mean.ag <- ggplot(success_projections.ag.rcp85m.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.mean.ag

g.Tmax.y.dif.rcp85m.canesm.mean.forest <- ggplot(success_projections.forest.rcp85m.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.mean.forest

g.Tmax.y.dif.rcp85m.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp85m.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.mean.natural.open

g.Tmax.y.dif.rcp85m.canesm.mean.human <- ggplot(success_projections.human.rcp85m.canesm.mean, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.mean.human

difference.Tmax.by.landuse.rcp85m.canesm.mean <- ggarrange(g.Tmax.y.dif.rcp85m.canesm.mean.ag,g.Tmax.y.dif.rcp85m.canesm.mean.forest,
                                                           g.Tmax.y.dif.rcp85m.canesm.mean.human,g.Tmax.y.dif.rcp85m.canesm.mean.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85m.canesm.mean



####Difference of precipitation within land uses
success_projections.ag.rcp85m.canesm.mean$y.dif.pcp_std_gridmet.rcp85m.canesm.mean <- success_projections.ag.rcp85m.canesm.mean$pcpbefore_raw_gridmet.rcp85m.canesm.mean - success_projections.ag.rcp85m.canesm.mean$pcpbefore_raw_gridmet
success_projections.forest.rcp85m.canesm.mean$y.dif.pcp_std_gridmet.rcp85m.canesm.mean<- success_projections.forest.rcp85m.canesm.mean$pcpbefore_raw_gridmet.rcp85m.canesm.mean - success_projections.forest.rcp85m.canesm.mean$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85m.canesm.mean$y.dif.pcp_std_gridmet.rcp85m.canesm.mean <- success_projections.natural.open.rcp85m.canesm.mean$pcpbefore_raw_gridmet.rcp85m.canesm.mean - success_projections.natural.open.rcp85m.canesm.mean$pcpbefore_raw_gridmet
success_projections.human.rcp85m.canesm.mean$y.dif.pcp_std_gridmet.rcp85m.canesm.mean <- success_projections.human.rcp85m.canesm.mean$pcpbefore_raw_gridmet.rcp85m.canesm.mean - success_projections.human.rcp85m.canesm.mean$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85m.canesm.mean.ag <- ggplot(success_projections.ag.rcp85m.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.mean ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.mean.ag

g.pcp.y.dif.rcp85m.canesm.mean.forest <- ggplot(success_projections.forest.rcp85m.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.mean ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.mean.forest

g.pcp.y.dif.rcp85m.canesm.mean.natural.open <- ggplot(success_projections.natural.open.rcp85m.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.mean))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.mean.natural.open

g.pcp.y.dif.rcp85m.canesm.mean.human <- ggplot(success_projections.human.rcp85m.canesm.mean, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.mean))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.mean.human

difference.pcp.by.landuse.rcp85m.canesm.mean <- ggarrange(g.pcp.y.dif.rcp85m.canesm.mean.ag,g.pcp.y.dif.rcp85m.canesm.mean.forest,
                                                          g.pcp.y.dif.rcp85m.canesm.mean.human,g.pcp.y.dif.rcp85m.canesm.mean.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85m.canesm.mean

#RCP 85e 10% canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85m.canesm.10 <- 0
success_projections$y.fut.rcp85m.canesm.10 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.canesm.10   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.canesm.10  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.canesm.10 )

success_projections$y.now.rcp85m.canesm.10<- 0
success_projections$y.now.rcp85m.canesm.10 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85m.canesm.10 <- success_projections$y.fut.rcp85m.canesm.10 - success_projections$y.now.rcp85m.canesm.10


###Scatter plot with x = now y = future
g.now.rcp85m.canesm.10 <- ggplot(success_projections, aes(x=y.now.rcp85m.canesm.10, y = y.fut.rcp85m.canesm.10))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 10% canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85m.canesm.10


###Density plot of the difference future - now
g.density.y.dif.rcp85m.canesm.10 <- ggplot(success_projections, aes(x=y.dif.rcp85m.canesm.10 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 10% canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.10


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85m.canesm.10 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85m.canesm.10))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85m.canesm.10 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85m.canesm.10 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85m.canesm.10 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85m.canesm.10 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85m.canesm.10 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85m.canesm.10.ag <- ggplot(success_projections.ag.rcp85m.canesm.10 , aes(x=y.dif.rcp85m.canesm.10 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.10.ag

g.density.y.dif.rcp85m.canesm.10.forest <- ggplot(success_projections.forest.rcp85m.canesm.10, aes(x=y.dif.rcp85m.canesm.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.10.forest

g.density.y.dif.rcp85m.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp85m.canesm.10, aes(x=y.dif.rcp85m.canesm.10 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.10.natural.open

g.density.y.dif.rcp85m.canesm.10.human <- ggplot(success_projections.human.rcp85m.canesm.10, aes(x=y.dif.rcp85m.canesm.10 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.10.human

difference.density.by.landuse.rcp85m.canesm.10 <- ggarrange(g.density.y.dif.rcp85m.canesm.10.ag,g.density.y.dif.rcp85m.canesm.10.forest,
                                                            g.density.y.dif.rcp85m.canesm.10.human,g.density.y.dif.rcp85m.canesm.10.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85m.canesm.10


####Difference of temperatures within land uses
success_projections.ag.rcp85m.canesm.10$y.dif.Tmax_std_gridmet.rcp85m.canesm.10 <- success_projections.ag.rcp85m.canesm.10$Tmax_std_gridmet.rcp85m.canesm.10 - success_projections.ag.rcp85m.canesm.10$Tmax_std_gridmet
success_projections.forest.rcp85m.canesm.10$y.dif.Tmax_std_gridmet.rcp85m.canesm.10<- success_projections.forest.rcp85m.canesm.10$Tmax_std_gridmet.rcp85m.canesm.10 - success_projections.forest.rcp85m.canesm.10$Tmax_std_gridmet
success_projections.natural.open.rcp85m.canesm.10$y.dif.Tmax_std_gridmet.rcp85m.canesm.10 <- success_projections.natural.open.rcp85m.canesm.10$Tmax_std_gridmet.rcp85m.canesm.10 - success_projections.natural.open.rcp85m.canesm.10$Tmax_std_gridmet
success_projections.human.rcp85m.canesm.10$y.dif.Tmax_std_gridmet.rcp85m.canesm.10 <- success_projections.human.rcp85m.canesm.10$Tmax_std_gridmet.rcp85m.canesm.10 - success_projections.human.rcp85m.canesm.10$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85m.canesm.10.ag <- ggplot(success_projections.ag.rcp85m.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.10.ag

g.Tmax.y.dif.rcp85m.canesm.10.forest <- ggplot(success_projections.forest.rcp85m.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.10.forest

g.Tmax.y.dif.rcp85m.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp85m.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.10.natural.open

g.Tmax.y.dif.rcp85m.canesm.10.human <- ggplot(success_projections.human.rcp85m.canesm.10, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.10.human

difference.Tmax.by.landuse.rcp85m.canesm.10 <- ggarrange(g.Tmax.y.dif.rcp85m.canesm.10.ag,g.Tmax.y.dif.rcp85m.canesm.10.forest,
                                                         g.Tmax.y.dif.rcp85m.canesm.10.human,g.Tmax.y.dif.rcp85m.canesm.10.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85m.canesm.10



####Difference of precipitation within land uses
success_projections.ag.rcp85m.canesm.10$y.dif.pcp_std_gridmet.rcp85m.canesm.10 <- success_projections.ag.rcp85m.canesm.10$pcpbefore_raw_gridmet.rcp85m.canesm.10 - success_projections.ag.rcp85m.canesm.10$pcpbefore_raw_gridmet
success_projections.forest.rcp85m.canesm.10$y.dif.pcp_std_gridmet.rcp85m.canesm.10<- success_projections.forest.rcp85m.canesm.10$pcpbefore_raw_gridmet.rcp85m.canesm.10 - success_projections.forest.rcp85m.canesm.10$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85m.canesm.10$y.dif.pcp_std_gridmet.rcp85m.canesm.10 <- success_projections.natural.open.rcp85m.canesm.10$pcpbefore_raw_gridmet.rcp85m.canesm.10 - success_projections.natural.open.rcp85m.canesm.10$pcpbefore_raw_gridmet
success_projections.human.rcp85m.canesm.10$y.dif.pcp_std_gridmet.rcp85m.canesm.10 <- success_projections.human.rcp85m.canesm.10$pcpbefore_raw_gridmet.rcp85m.canesm.10 - success_projections.human.rcp85m.canesm.10$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85m.canesm.10.ag <- ggplot(success_projections.ag.rcp85m.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.10 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.10.ag

g.pcp.y.dif.rcp85m.canesm.10.forest <- ggplot(success_projections.forest.rcp85m.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.10 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.10.forest

g.pcp.y.dif.rcp85m.canesm.10.natural.open <- ggplot(success_projections.natural.open.rcp85m.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.10))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.10.natural.open

g.pcp.y.dif.rcp85m.canesm.10.human <- ggplot(success_projections.human.rcp85m.canesm.10, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.10))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.10.human

difference.pcp.by.landuse.rcp85m.canesm.10 <- ggarrange(g.pcp.y.dif.rcp85m.canesm.10.ag,g.pcp.y.dif.rcp85m.canesm.10.forest,
                                                        g.pcp.y.dif.rcp85m.canesm.10.human,g.pcp.y.dif.rcp85m.canesm.10.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85m.canesm.10

#RCP 85e 90% canesm-----------------------------------------------------------
###Create success_projections
success_projections$y.fut.rcp85m.canesm.90 <- 0
success_projections$y.fut.rcp85m.canesm.90 <-  inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet.rcp85m.canesm.90   + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet.rcp85m.canesm.90  + 
                                                           NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                           substrate*success_projections$substrate_binary +
                                                           success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet.rcp85m.canesm.90 )

success_projections$y.now.rcp85m.canesm.90<- 0
success_projections$y.now.rcp85m.canesm.90 <- inv.logit(intercept + Tmax_std_gridmet*success_projections$Tmax_std_gridmet  + PcpBefore_raw_gridmet*success_projections$pcpbefore_raw_gridmet + 
                                                          NLCD_p_forest*success_projections$NLCD_p_forest + NLCD_p_human*success_projections$NLCD_p_human  + NLCD_p_ag*success_projections$NLCD_p_ag  +
                                                          substrate*success_projections$substrate_binary +
                                                          success_projections$LU + success_projections$int*success_projections$Tmax_std_gridmet)


success_projections$y.dif.rcp85m.canesm.90 <- success_projections$y.fut.rcp85m.canesm.90 - success_projections$y.now.rcp85m.canesm.90


###Scatter plot with x = now y = future
g.now.rcp85m.canesm.90 <- ggplot(success_projections, aes(x=y.now.rcp85m.canesm.90, y = y.fut.rcp85m.canesm.90))+xlim(min=0.6,max=0.85)+ylim(min=0.6,max=0.85)+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.3, na.rm = TRUE)+
  geom_abline(intercept = 0, slope = 1,color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("At least one success: RCP85 90% canesm")+ ylab("Future")+ xlab("Now")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.now.rcp85m.canesm.90


###Density plot of the difference future - now
g.density.y.dif.rcp85m.canesm.90 <- ggplot(success_projections, aes(x=y.dif.rcp85m.canesm.90 ))+ my.theme + 
  geom_density(fill ="yellow2", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Success projections difference RCP85 90% canesm ")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.90


###Scatter plot with x=Tnestperiod_meanmax_gridmet, y=difference in nesting success
g.meanmax.nestpd.rcp85m.canesm.90 <- ggplot(success_projections, aes(x=tnestpd_meanmax_gridmet, y=y.dif.rcp85m.canesm.90))+my.theme+
  geom_point( aes(color=NewLU1),alpha=0.2, na.rm = TRUE)+
  geom_hline(aes(yintercept =0),color="black", linetype = "dashed") +
  geom_vline(aes(xintercept =0),color="black", linetype = "dashed") +
  geom_smooth(method="lm",se=F)+
  ggtitle("success_projections Vs. MeanMax Nesting Period")+ ylab("Future - Now")+ xlab("tnestpd_meanmax_gridmet")+
  guides(color = guide_legend(override.aes = list(alpha = 1) ) )

g.meanmax.nestpd.rcp85m.canesm.90 

####Density plots with landuses
###Subsetting by Land Use
success_projections.ag.rcp85m.canesm.90 <- subset(success_projections, NewLU1 =="Ag")
success_projections.forest.rcp85m.canesm.90 <- subset(success_projections, NewLU1 =="Forest")
success_projections.natural.open.rcp85m.canesm.90 <- subset(success_projections, NewLU1 =="Natural_open")
success_projections.human.rcp85m.canesm.90 <- subset(success_projections, NewLU1 =="Human")

##Plots difference in success_projections
g.density.y.dif.rcp85m.canesm.90.ag <- ggplot(success_projections.ag.rcp85m.canesm.90 , aes(x=y.dif.rcp85m.canesm.90 ))+ my.theme + ylim(min=0,max=100) + xlim(min=-0.06,max=0.06) +
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.90.ag

g.density.y.dif.rcp85m.canesm.90.forest <- ggplot(success_projections.forest.rcp85m.canesm.90, aes(x=y.dif.rcp85m.canesm.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.90.forest

g.density.y.dif.rcp85m.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp85m.canesm.90, aes(x=y.dif.rcp85m.canesm.90 ))+ my.theme + ylim(min=0,max=100)+xlim(min=-0.06,max=0.06) +
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.90.natural.open

g.density.y.dif.rcp85m.canesm.90.human <- ggplot(success_projections.human.rcp85m.canesm.90, aes(x=y.dif.rcp85m.canesm.90 ))+ my.theme + ylim(min=0,max=100)+ xlim(min=-0.06,max=0.06) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("success_projections difference Human")+ ylab("Density")+ xlab("Future - Now")

g.density.y.dif.rcp85m.canesm.90.human

difference.density.by.landuse.rcp85m.canesm.90 <- ggarrange(g.density.y.dif.rcp85m.canesm.90.ag,g.density.y.dif.rcp85m.canesm.90.forest,
                                                            g.density.y.dif.rcp85m.canesm.90.human,g.density.y.dif.rcp85m.canesm.90.natural.open,nrow=2,ncol=2)

difference.density.by.landuse.rcp85m.canesm.90


####Difference of temperatures within land uses
success_projections.ag.rcp85m.canesm.90$y.dif.Tmax_std_gridmet.rcp85m.canesm.90 <- success_projections.ag.rcp85m.canesm.90$Tmax_std_gridmet.rcp85m.canesm.90 - success_projections.ag.rcp85m.canesm.90$Tmax_std_gridmet
success_projections.forest.rcp85m.canesm.90$y.dif.Tmax_std_gridmet.rcp85m.canesm.90<- success_projections.forest.rcp85m.canesm.90$Tmax_std_gridmet.rcp85m.canesm.90 - success_projections.forest.rcp85m.canesm.90$Tmax_std_gridmet
success_projections.natural.open.rcp85m.canesm.90$y.dif.Tmax_std_gridmet.rcp85m.canesm.90 <- success_projections.natural.open.rcp85m.canesm.90$Tmax_std_gridmet.rcp85m.canesm.90 - success_projections.natural.open.rcp85m.canesm.90$Tmax_std_gridmet
success_projections.human.rcp85m.canesm.90$y.dif.Tmax_std_gridmet.rcp85m.canesm.90 <- success_projections.human.rcp85m.canesm.90$Tmax_std_gridmet.rcp85m.canesm.90 - success_projections.human.rcp85m.canesm.90$Tmax_std_gridmet


##Plots differnce in temperature by landuse
g.Tmax.y.dif.rcp85m.canesm.90.ag <- ggplot(success_projections.ag.rcp85m.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.90.ag

g.Tmax.y.dif.rcp85m.canesm.90.forest <- ggplot(success_projections.forest.rcp85m.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.90.forest

g.Tmax.y.dif.rcp85m.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp85m.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.90.natural.open

g.Tmax.y.dif.rcp85m.canesm.90.human <- ggplot(success_projections.human.rcp85m.canesm.90, aes(x=y.dif.Tmax_std_gridmet.rcp85m.canesm.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Temperature difference Human")+ ylab("Density")+ xlab("Future - Now")

g.Tmax.y.dif.rcp85m.canesm.90.human

difference.Tmax.by.landuse.rcp85m.canesm.90 <- ggarrange(g.Tmax.y.dif.rcp85m.canesm.90.ag,g.Tmax.y.dif.rcp85m.canesm.90.forest,
                                                         g.Tmax.y.dif.rcp85m.canesm.90.human,g.Tmax.y.dif.rcp85m.canesm.90.natural.open,nrow=2,ncol=2)

difference.Tmax.by.landuse.rcp85m.canesm.90



####Difference of precipitation within land uses
success_projections.ag.rcp85m.canesm.90$y.dif.pcp_std_gridmet.rcp85m.canesm.90 <- success_projections.ag.rcp85m.canesm.90$pcpbefore_raw_gridmet.rcp85m.canesm.90 - success_projections.ag.rcp85m.canesm.90$pcpbefore_raw_gridmet
success_projections.forest.rcp85m.canesm.90$y.dif.pcp_std_gridmet.rcp85m.canesm.90<- success_projections.forest.rcp85m.canesm.90$pcpbefore_raw_gridmet.rcp85m.canesm.90 - success_projections.forest.rcp85m.canesm.90$pcpbefore_raw_gridmet
success_projections.natural.open.rcp85m.canesm.90$y.dif.pcp_std_gridmet.rcp85m.canesm.90 <- success_projections.natural.open.rcp85m.canesm.90$pcpbefore_raw_gridmet.rcp85m.canesm.90 - success_projections.natural.open.rcp85m.canesm.90$pcpbefore_raw_gridmet
success_projections.human.rcp85m.canesm.90$y.dif.pcp_std_gridmet.rcp85m.canesm.90 <- success_projections.human.rcp85m.canesm.90$pcpbefore_raw_gridmet.rcp85m.canesm.90 - success_projections.human.rcp85m.canesm.90$pcpbefore_raw_gridmet


##Plots differnce in precipitation by landuse
g.pcp.y.dif.rcp85m.canesm.90.ag <- ggplot(success_projections.ag.rcp85m.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.90 ))+ my.theme + 
  geom_density(fill ="coral1", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Agriculture")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.90.ag

g.pcp.y.dif.rcp85m.canesm.90.forest <- ggplot(success_projections.forest.rcp85m.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.90 ))+ my.theme + 
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Forest")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.90.forest

g.pcp.y.dif.rcp85m.canesm.90.natural.open <- ggplot(success_projections.natural.open.rcp85m.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.90))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Natural Open")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.90.natural.open

g.pcp.y.dif.rcp85m.canesm.90.human <- ggplot(success_projections.human.rcp85m.canesm.90, aes(x=y.dif.pcp_std_gridmet.rcp85m.canesm.90))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Precipitation difference Human")+ ylab("Density")+ xlab("Future - Now")

g.pcp.y.dif.rcp85m.canesm.90.human

difference.pcp.by.landuse.rcp85m.canesm.90 <- ggarrange(g.pcp.y.dif.rcp85m.canesm.90.ag,g.pcp.y.dif.rcp85m.canesm.90.forest,
                                                        g.pcp.y.dif.rcp85m.canesm.90.human,g.pcp.y.dif.rcp85m.canesm.90.natural.open,nrow=2,ncol=2)

difference.pcp.by.landuse.rcp85m.canesm.90










