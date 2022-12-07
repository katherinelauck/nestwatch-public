library(lme4)
library(tidyverse)

setwd("~/Documents/nestwatch/")

dd=readRDS("Data/active/success-cleaned.rds")
load('Data/active/raw-collapsed.RData')
cc=read.csv("Data/active/canopycover_8-12-22.csv",header=TRUE)

# Subset down raw data to just the nest attempts that we analyze
raw=nest[match(dd$attempt, nest $attempt),]

# Add canopy cover to dd 
cc=cc[match(dd$UnCoor,cc$UnCoor),] # reorder and align the cc dataset

dd$cc_carto=cc$conus2011_carto # choose closer year
dd$cc_carto[which(as.numeric(as.character(dd$year))>2013)]= cc$conus2016_carto[which(as.numeric(as.character(dd$year))>2013)]
dd$cc_carto = dd$cc_carto/100

dd$cc_anal=cc$conus2011_analy # choose closer year
dd$cc_anal[which(as.numeric(as.character(dd$year))>2013)]= cc$conus2016_analy[which(as.numeric(as.character(dd$year))>2013)]
dd$cc_anal= dd$cc_anal/100

########################################################
# Q1: What comprises the different land use categories?#
########################################################
HabitatCodes=c("NW-for", "NW-ag", "NW-orch-vin", "NW-xmas", "NW-beach", "NW-burn", "NW-chap", "NW-clrcut", "NW-des", "NW-fw", "NW-grass", "NW-sw", "NW-campus", "NW-cem", "NW-cmpgrd", "NW-com-ind", "NW-golf", "NW-human", "NW-park", "NW-pit", "NW-pwrln", "NW-road", "NW-ry")

HabitatNames=c("Woodland/forest", "Agricultural area", "Orchard/vineyard", "Christmas tree farm", "Beach/tundra", "Recently burned area", "Chapparal/shrubland", "Recently clear cut area", "Desert scrub", "Fresh water", "Natural grassland/prairie", "Salt water", "School/campus/church/hospital", "Cemetery", "Campground", "Industrial/commercial area", "Golf course", "Human modified", "Public park/green space", "Landfill/gravel pit/strip mine", "Power/Utility corridor", "Roadside", "Residential area")

HabitatNameMatch=as.data.frame(cbind(HabitatCodes,HabitatNames))

dd$HabitatClass=raw$habitat1
dd$Count=1

HabitatCounts=aggregate(dd$Count,list(dd$HabitatClass,dd$NewLU1),sum)
CategoryCounts=aggregate(dd$Count,list(dd$NewLU1),sum)

HabitatTable=as.data.frame(cbind(HabitatNameMatch[match(HabitatNameMatch$HabitatCodes, HabitatCounts[,1]),2], HabitatCounts[,2:3]))
colnames(HabitatTable)=c("Habitat","Category","NumberOfAttempts")

HabitatTable$FractionOfTotalAttempts=round(HabitatTable[,3]/sum(HabitatTable[,3]),3)
HabitatTable$FractionOfCategoryAttempts=round(HabitatTable[,3] /CategoryCounts[match(HabitatTable[,2], CategoryCounts[,1]),2],3)

HabitatTable$Category=as.character(HabitatTable$Category)
HabitatTable$Category[which(HabitatTable$Category=="Ag")]="Agriculture"
HabitatTable$Category[which(HabitatTable$Category=="Human")]="Developed"
write.csv(HabitatTable,"~/Desktop/HabitatTable.csv")


##############################################################
# Q2: What is the composition of the surrounding urban area? #
##############################################################
dd$NLCD_developed_open_500m= raw$NLCD_developed_open_500m
dd$NLCD_developed_low_500m = raw$NLCD_developed_low_500m
dd$NLCD_developed_medium_500m = raw$NLCD_developed_medium_500m
dd$NLCD_developed_high_500m = raw$NLCD_developed_high_500m

SurroundingDeveloped=as.data.frame(aggregate(list(dd $NLCD_developed_open_500m,dd $NLCD_developed_low_500m, dd $NLCD_developed_medium_500m, dd $NLCD_developed_high_500m),list(dd $NewLU1),mean))
colnames(SurroundingDeveloped)=c("Category","NLCD_developed_open_500m","NLCD_developed_low_500m","NLCD_developed_medium_500m","NLCD_developed_high_500m")

# Conclusion: 'developed' nests are very rarely in city centers. On average, the fraciton of high, medium, low, and open developed areas was 1%, 5%, 10%, and 20% of the landscape within 500m of the nest site. 

##################################################################
# Q3: Do temperature anamolies vary between land use categories? #
##################################################################

# This part runs models with and without land use predicting temperature anamolies. It takes a while to run
M1=lmer(Tmax_std_gridmet~NewLU1 + (1| Region/UnCoor)+(1|year),data=dd)
M0=lmer(Tmax_std_gridmet~ + (1| Region/UnCoor)+(1|year),data=dd)
anova(M1,M0) # Delta AIC is 1 between models; Chisq= 5.43 and P= 0.15. In short, no evidence for differences in temperature anamolies between land use types

par(mar=c(5,5,1,1))
boxplot(Tmax_std_gridmet~NewLU1,data=dd,xlab="",ylab="Maximum temperature anamoly",frame.plot=TRUE,axes=FALSE,cex.lab=2)
axis(1,at=c(1,2,3,4),labels=c("Forest","Agriculture","Natural Open","Developed"),cex.axis=1.25)
axis(2,at=c(-5,0,5,10),cex.axis=1.25)

##############################################################
# Q4: Are temperature anamolies more southernly distributed? #
##############################################################
LatLon=separate(dd,UnCoor,sep="_",into=c("X","Y"))
dd$X=as.numeric(LatLon$X)
dd$Y=as.numeric(LatLon$Y)

# This part runs models with and without land use predicting temperature anamolies. It takes a while to run
M1=lmer(Tmax_std_gridmet~Y + (1| Region/UnCoor)+(1|year),data=dd)
M0=lmer(Tmax_std_gridmet~ + (1| Region/UnCoor)+(1|year),data=dd)
anova(M1,M0) # Delta AIC is5, Chisq is 6.34, P is 0.01
summary(M1) # estimate of Y is 0.01. Suggests that hot temperature anamolies (if anything) are maybe a tiny more likely at higher (not) lower latitudes. Goes against the reviewer's suggestion. But the effect size is miniscule (see graph below)

par(mar=c(5,5,1,1))
plot(dd$Tmax_std_gridmet~dd$Y,pch=19,col="gray",ylab="Maximum temperature anamoly", cex.lab=2,xlab="Latitude",cex=.7)
xs=seq(min(dd$Y),max(dd$Y),1)
ys=fixef(M1)[1]+xs*fixef(M1)[2]
lines(xs,ys,lwd=2,col="red")

####################################################
# Q5: Compare elevations among land-use categories #
####################################################
dd$Elevation=raw$Amatulli_Elevation

M1_aov=aov(log(dd$Elevation+1)  ~ dd$NewLU1) # Need to log transform for normality 
summary(M1_aov) # anova is significant 
TukeyHSD(M1_aov) # all significantly different from each other

par(mar=c(5,5,1,1))
boxplot(Elevation ~NewLU1,data=dd,xlab="",ylab="Elevation (m)",frame.plot=TRUE,axes=FALSE,cex.lab=2)
axis(1,at=c(1,2,3,4),labels=c("Forest","Agriculture","Natural Open","Developed"),cex.axis=1.25)
axis(2,at=c(0,1000,2000,3000,4000),cex.axis=1.25)

# Median elevation is highest in forest, then natural open, then agriculture and finally human (see median and means below)
aggregate(dd$Elevation,list(dd$NewLU1),median)
aggregate(dd$Elevation,list(dd$NewLU1),mean)

####################################################
# Q6: Compare latitudes among land-use categories #
####################################################
M1_aov=aov(dd$Y  ~ dd$NewLU1) # Need to log transform for normality 
summary(M1_aov) # anova is significant 
TukeyHSD(M1_aov) # all significantly different from each other

par(mar=c(5,5,1,1))
boxplot(Y ~NewLU1,data=dd,xlab="",ylab="Latitude (decimal degrees)",frame.plot=TRUE,axes=FALSE,cex.lab=2)
axis(1,at=c(1,2,3,4),labels=c("Forest","Agriculture","Natural Open","Developed"),cex.axis=1.25)
axis(2,at=c(25,30,35,40,45),cex.axis=1.25)

# Natural open is more northern, then agriculture, then human and finally forest. No difference between ag and natural open. But the differences are *really* small (i.e., less than .5 degrees for median and 1 degree for mean)
aggregate(dd$Y,list(dd$NewLU1),mean)
aggregate(dd$Y,list(dd$NewLU1),median)

##################################################
# Q7: Correlate laydate with temperature anamoly #
##################################################
dd$laydate_doy=as.numeric(strftime(as.Date(dd$laydate),format="%j"))

Forest=dd[which(dd$NewLU1=="Forest"),]
M1_forest=lmer(Tmax_std_gridmet~ laydate_doy + (1| Region/UnCoor)+(1|year),data=Forest)
M0_forest=lmer(Tmax_std_gridmet~ + (1| Region/UnCoor)+(1|year),data=Forest)
anova(M1_forest, M0_forest)

Open=dd[which(dd$NewLU1=="Natural_open"),]
M1_open=lmer(Tmax_std_gridmet~ laydate_doy + (1| Region/UnCoor)+(1|year),data=Open)
M0_open=lmer(Tmax_std_gridmet~ + (1| Region/UnCoor)+(1|year),data= Open)
anova(M1_open, M0_open)

Ag=dd[which(dd$NewLU1=="Ag"),]
M1_ag=lmer(Tmax_std_gridmet~ laydate_doy + (1| Region/UnCoor)+(1|year),data= Ag)
M0_ag=lmer(Tmax_std_gridmet~ + (1| Region/UnCoor)+(1|year),data= Ag)
anova(M1_ag, M0_ag)

Human=dd[which(dd$NewLU1=="Human"),]
M1_human=lmer(Tmax_std_gridmet~ laydate_doy + (1| Region/UnCoor)+(1|year),data=Human)
M0_human=lmer(Tmax_std_gridmet~ + (1| Region/UnCoor)+(1|year),data= Human)
anova(M1_human, M0_human)

layout(t(c(1,2,3,4)),widths=c(1.2,1,1,1.05))

par(mar=c(5,5,2,0))
plot(Forest$Tmax_std_gridmet~ Forest$laydate_doy,pch=19,col="gray",ylab="Maximum temperature anamoly", xlab="Lay Date (day of year)",cex.lab=1.5,cex.main=1.5,main="Forest",cex=.7,ylim=c(-5,11),xlim=c(-0,365),frame.plot=TRUE, axes=FALSE)
xs=seq(min(Forest$laydate_doy),max(Forest$laydate_doy),1)
ys=fixef(M1_forest)[1]+xs*fixef(M1_forest)[2]
lines(xs,ys,lwd=2,col="red")
axis(1,at=c(0,100,200,300),cex.axis=1.2)
axis(2,at=c(-5,0,5,10),cex.axis=1.2)

par(mar=c(5,1,2,0))
plot(Ag$Tmax_std_gridmet~ Ag $laydate_doy,pch=19,col="gray",ylab="", xlab="Lay Date (day of year)",cex.lab=1.5,cex.main=1.5,main="Agriculture",cex=.7,ylim=c(-5,11),xlim=c(-0,365),frame.plot=TRUE, axes=FALSE)
xs=seq(min(Ag $laydate_doy),max(Ag $laydate_doy),1)
ys=fixef(M1_ag)[1]+xs*fixef(M1_ag)[2]
lines(xs,ys,lwd=2,col="red")
axis(1,at=c(0,100,200,300),cex.axis=1.2)

par(mar=c(5,1,2,0))
plot(Open$Tmax_std_gridmet~ Open $laydate_doy,pch=19,col="gray",ylab="", xlab="Lay Date (day of year)",cex.lab=1.5,cex.main=1.5,main="Natural open",cex=.7,ylim=c(-5,11),xlim=c(-0,365),frame.plot=TRUE, axes=FALSE)
xs=seq(min(Open $laydate_doy),max(Open $laydate_doy),1)
ys=fixef(M1_open)[1]+xs*fixef(M1_open)[2]
lines(xs,ys,lwd=2,col="red")
axis(1,at=c(0,100,200,300),cex.axis=1.2)

par(mar=c(5,1,2,1))
plot(Human$Tmax_std_gridmet~ Human $laydate_doy,pch=19,col="gray",ylab="", xlab="Lay Date (day of year)",cex.lab=1.5,cex.main=1.5,main="Developed",cex=.7,ylim=c(-5,11),xlim=c(-0,365),frame.plot=TRUE, axes=FALSE)
xs=seq(min(Human $laydate_doy),max(Human $laydate_doy),1)
ys=fixef(M1_human)[1]+xs*fixef(M1_human)[2]
lines(xs,ys,lwd=2,col="red")
axis(1,at=c(0,100,200,300),cex.axis=1.2)

# Overall, in every land use, higher temperature anamolies tend to occur for earlier laydates. But this is a super minor effect. 

####################################################################
# Q8: Calculate sample sizes by nest types and conservation scores #
####################################################################

# Histogram of conservation scores
par(mar=c(5,5,1,1))
plot(0,0,pch=19,col="white",xlim=c(4,16),ylim=c(0,75000),xlab="Conservation Score",ylab="# of Nest Attempts",main="",cex.lab=1.5,frame.plot=TRUE,axes=FALSE)
hist(dd$ConservationScore,add=TRUE)
axis(1,at=c(4,6,8,10,12,14,16))
axis(2,at=c(0,15000,30000,45000,60000,75000))

# Values for birds in nest boxes

aggregate(dd$Count,list(dd$substrate_binary),sum) # 9888 natural nests and 142975 in nest boxes
aggregate(dd$Count,list(dd$cavity_binary),sum) # 8646 exposed nests and 144217 in cavities

############################################################
# Q9: Do raw temperature values vary among land use types? #
############################################################
dd$TmaxRaw=raw$Tmax_raw

# This part runs models with and without land use predicting temperature anamolies. It takes a while to run
library(emmeans)

M1=lmer(TmaxRaw ~NewLU1 +laydate_scaled+ (1| Region/UnCoor)+(1|year),data=dd)
M0=lmer(TmaxRaw ~laydate_scaled + (1| Region/UnCoor)+(1|year),data=dd)
anova(M1,M0) # Model without NewLU1 does not converge 

summary(M1)

M1_aov=aov(TmaxRaw ~NewLU1,data=dd) # shows there is variation between all land use types in raw temperatures
TukeyHSD(M1_aov)

aggregate(dd$TmaxRaw,list(dd$NewLU1),mean)
aggregate(dd$TmaxRaw,list(dd$NewLU1),median)

par(mar=c(5,5,1,1))
boxplot(TmaxRaw ~NewLU1,data=dd,xlab="",ylab="Maximum temperature (raw)",frame.plot=TRUE,axes=FALSE,cex.lab=2,ylim=c(0,50))
axis(1,at=c(1,2,3,4),labels=c("Forest","Agriculture","Natural Open","Developed"),cex.axis=1.25)
axis(2,at=c(0,10,20,30,40,50),cex.axis=1.25)

plot(dd$TmaxRaw,dd$Tmax_std_gridmet)

############################################################
# Q9: Does canopy cover values vary among land use types? #
############################################################
# Cartographic first
par(mar=c(5,5,1,1))
boxplot(cc_carto ~NewLU1,data=dd,xlab="",ylab="Canopy Cover (Cartographic)",frame.plot=TRUE,axes=FALSE,cex.lab=2,ylim=c(0,1))
axis(1,at=c(1,2,3,4),labels=c("Forest","Agriculture","Natural Open","Developed"),cex.axis=1.25)
axis(2,at=c(0,.2,.4,.6,.8,1),cex.axis=1.25)

aggregate(dd$cc_carto,list(dd$NewLU1),mean)
aggregate(dd$cc_carto,list(dd$NewLU1),median)

M1=lmer(cc_carto ~NewLU1 + (1| Region/UnCoor)+(1|year),data=dd)
M0=lmer(cc_carto ~ + (1| Region/UnCoor)+(1|year),data=dd)
anova(M1,M0) # Model without NewLU1 does not converge An
M1_aov=aov(cc_carto ~NewLU1,data=dd)
TukeyHSD(M1_aov)


# Analytical second

par(mar=c(5,5,1,1))
boxplot(cc_anal ~NewLU1,data=dd,xlab="",ylab="Canopy Cover (Analytical)",frame.plot=TRUE,axes=FALSE,cex.lab=2,ylim=c(0,1))
axis(1,at=c(1,2,3,4),labels=c("Forest","Agriculture","Natural Open","Developed"),cex.axis=1.25)
axis(2,at=c(0,.2,.4,.6,.8,1),cex.axis=1.25)

aggregate(dd$cc_anal,list(dd$NewLU1),mean)
aggregate(dd$cc_anal,list(dd$NewLU1),median)

M1=lmer(cc_anal ~NewLU1 + (1| Region/UnCoor)+(1|year),data=dd)
M0=lmer(cc_anal ~ + (1| Region/UnCoor)+(1|year),data=dd)
anova(M1,M0) # Model without NewLU1 does not converge An
M1_aov=aov(cc_anal ~NewLU1,data=dd)
TukeyHSD(M1_aov)


############################################################
# Q10: Compare nlcd and local land use #
############################################################
nlcd=read.csv("Data/active/data_PixelValue_NLCD.csv")

nlcd$reclass="na"
nlcd$reclass[which(nlcd$NLCD_ClassName%in%c("decidious_forest","evergreen_forest","mixed_forest"))]="nlcd_forest"
nlcd$reclass[which(nlcd$NLCD_ClassName%in%c("barren_land","emergent_herbaceous_wetlands","grassland","woody_wetlands","pasture","shrub","open_water"))]="nlcd_natural_open"
nlcd$reclass[which(nlcd$NLCD_ClassName%in%c("developed_high","developed_low","developed_medium","developed_open"))]="nlcd_developed"
nlcd$reclass[which(nlcd$NLCD_ClassName%in%c("cultivated_crop"))]="nlcd_ag"

# Check to see how much the reclass NLCD and local land use match up for each land use

unique(nlcd$NewLU1)

length(which(nlcd$reclass=="nlcd_forest"&nlcd$NewLU1=="Forest"))/length(which(nlcd$NewLU1=="Forest"))
length(which(nlcd$reclass=="nlcd_ag"&nlcd$NewLU1=="Ag"))/length(which(nlcd$NewLU1=="Ag"))
length(which(nlcd$reclass=="nlcd_natural_open"&nlcd$NewLU1=="Natural_open"))/length(which(nlcd$NewLU1=="Natural_open"))
length(which(nlcd$reclass=="nlcd_developed"&nlcd$NewLU1=="Human"))/length(which(nlcd$NewLU1=="Human"))

############################################################
# Q11: Make table of sample size by species etc #
############################################################
spp=sort(unique(dd$species))
sampleSIZE=array(dim=c(length(spp),6),0)
colnames(sampleSIZE)=c("Species/Group","Conservation Score","Forest","Agriculture","Natural Open","Developed")

sampleSIZE[,1]=as.character(spp)
sampleSIZE[,2]=dd$ConservationScore[match(spp,dd$species)]

ag1=aggregate(dd$count, list(dd$species,dd$NewLU1),sum)

Forest=ag1[which(ag1[,2]=="Forest"),]
sampleSIZE[match(Forest[,1],spp),3]=Forest[,3]

Ag=ag1[which(ag1[,2]=="Ag"),]
sampleSIZE[match(Ag[,1],spp),4]=Ag[,3]

Nat_Open=ag1[which(ag1[,2]=="Natural_open"),]
sampleSIZE[match(Nat_Open[,1],spp),5]=Nat_Open[,3]

Human=ag1[which(ag1[,2]=="Human"),]
sampleSIZE[match(Human[,1],spp),6]=Human[,3]

ag2=aggregate(dd$count, list(dd$NewLU1,dd$cavity_binary),sum)
ag3=aggregate(dd$count, list(dd$NewLU1,dd$substrate_binary),sum)

sampleSIZE=rbind(
      c("Cavity Nests","NA",ag2[which(ag2[,2]=="1"),3]),
      c("Non-cavity Nests","NA",ag2[which(ag2[,2]=="0"),3]),
      c("Nestbox Nests","NA",ag3[which(ag3[,2]=="1"),3]),
      c("Non-nestbox Nests","NA",ag3[which(ag3[,2]=="0"),3]),
      sampleSIZE)

write.csv(sampleSIZE,"Figures/samplesizeTABLE.csv")
