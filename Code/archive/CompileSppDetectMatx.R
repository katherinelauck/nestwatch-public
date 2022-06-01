load('~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet_meanNestpdTemp_gridmet.RData')
nobbs=read.csv("~/Google Drive/NestwatchProject/Data/NotInBBS.csv",header=TRUE)
processed2=processed[which(is.na(processed$NLCD_barren_land_1km)==FALSE),] # Exclude bad sites
processed3=processed2[which(is.na(processed2$at_least_one_success)==FALSE),] # Exclude those without success
processed4=processed3[which(is.na(processed3$laydate)==FALSE),] # Exclude those without laydates
processed5=processed4[which(processed4$habitat1!=""),] # Exclude those without local landuse

processed5$NewLU1<-NA
processed5$NewLU1[processed5$habitat1 %in% c("NW-ag","NW-xmas","NW-orch-vin")]<-"Ag"
processed5$NewLU1[processed5$habitat1 %in% c("NW-airport","NW-campus","NW-cem","NW-cmpgrd","NW-com-ind","NW-golf","NW-human","NW-park","NW-pit","NW-pwrln","NW-road","NW-ry")]<-"Human"
processed5$NewLU1[processed5$habitat1 %in% c("NW-for")]<-"Forest"
processed5$NewLU1[processed5$habitat1 %in% c("NW-burn","NW-chap","NW-des","NW-grass","NW-fw","NW-sw","NW-beach","NW-clrcut")]<-"Natural_open"

processed6=processed5[which(is.na(processed5$NewLU1)==FALSE),] # Exclude those without proper local land use
unique(processed6$habitat1)

spp=sort(unique(processed6$species))
fout=array(dim=c(0,7))
colnames(fout)=c("Species","NumAttempts","NumSites","Attempts_Ag","Attempts_Forest","Attempts_Human","Attempts_Natural_open")
for ( i in 1:length(spp)){
  oneSPP=processed6[which(processed6$species==spp[i]),]
  tot_attempts=length(unique(oneSPP$attempt))
  tot_sites=length(unique(oneSPP$UnCoor))
  types=table(oneSPP$NewLU1)
  
  attempt_lu=c(0,0,0,0)
  attempt_lu[match(names(types),sort(unique(processed6$NewLU1)))]=types
  
  out=c(spp[i],tot_attempts,tot_sites,attempt_lu)
  fout=rbind(fout,out)
  }
fout=as.data.frame(fout)

fout$NotBBS=0
fout$NotBBS[na.omit(match(nobbs[,3],fout[,1]))]=1

write.csv(fout,"~/Desktop/fout.csv")


################################
# Look at laydate distributions#
################################
# Filter to the included species
include=read.csv("~/Google Drive/NestwatchProject/Data/IncludeSpecies.csv",header=TRUE)
processed7=processed6[which(match(processed6$species,include$Species)>0),]

doys=as.numeric(strftime(processed7$laydate, format = "%j"))
hist(doys)

as.numeric(strftime("2020-08-15", format = "%j"))
length(which(doys>228))
as.numeric(strftime("2020-02-15", format = "%j"))
length(which(doys<46))

WeirdDates1=processed7[which(doys<61 |doys>214 ),] # March and August
WeirdDates2=processed7[which(doys<46 |doys>228 ),] # Mid Feb and Mid August
table(WeirdDates$species)
table(WeirdDates2$species)

# Map plot
plot(processed7$lon,processed7$lat,col="lightgray",cex=.4,pch=19,axes=FALSE,frame.plot=TRUE,xlab="",ylab="")
legend(-120,70,c("All Dates","< March 1 OR >Aug 1","< Feb 15 OR >Aug 15"),fill=c("lightgray","red","blue"),bty="n")
points(WeirdDates$lon,WeirdDates$lat,col="red",pch=19,cex=.4)
points(WeirdDates2$lon,WeirdDates2$lat,col="blue",pch=19,cex=.4)


# Hist plot
layout(c(1,2),heights=c(.5,1))
par(mar=c(0,0,0,0))
plot(0,0,pch=19,col="white",axes=FALSE,xlab="",ylab="",main="")
legend(-1,1,c("All Dates: N= 153010","< March 1 OR >Aug 1: N= 849","< Feb 15 OR >Aug 15: N =209"),fill=c("lightgray","red","blue"),bty="n")

par(mar=c(5,5,0,1))
hist(doys,xlab="days of year",ylab="Frequency",main="")
abline(v=61,col="red",lwd=2)
abline(v=214,col="red",lwd=2)
abline(v=228,col="blue",lwd=2)
abline(v=46,col="blue",lwd=2)


