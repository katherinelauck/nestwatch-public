rm(list=ls())
library(tidyverse)
library(daymetr)
library(parallel)
load('~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevation.RData')

# Establish daymet extraction function
DayCentOneSite=function(lat, lon, dates, days_prior, days_after){
  doys=as.numeric(strftime(dates, format = "%j"))
  years= format(dates,"%Y")
 
  DAYMET=download_daymet(site = "SITE", lat = lat, lon = lon, start = 1989, end = 2019, path = tempdir(), internal = TRUE, silent = FALSE, force = FALSE, simplify = FALSE)
  DAYMET=DAYMET$data
  DAYMET$tave..deg.c.=(DAYMET$tmin..deg.c.+DAYMET$tmax..deg.c.)/2

  
  WeatherData=array(dim=c(0,12))
  colnames(WeatherData)=c("noanomoly_mean_precip" ,"noanomoly_max_tmax" ,"noanomoly_min_tmin" ,"noanomoly_mean_tmean","mean_anamoly_mean_precip" ,"mean_anamoly_max_tmax" ,"mean_anamoly_min_tmin" ,"mean_anamoly_mean_tmean","z_anamoly_mean_precip" ,"z_anamoly_max_tmax" ,"z_anamoly_mean_tmin" ,"z_anamoly_mean_tmean")
  
  for (j in 1:length(dates)){
    min_days=doys[j]-days_prior
    max_days=doys[j]+days_after
  
    if(min_days>0 & max_days<366){
      Summarized= DAYMET%>%
                  filter(yday>=min_days)%>%
                  filter(yday<=max_days)%>%
                  group_by(year)%>%
                  summarize(mean_precip=mean(prcp..mm.day.),max_tmax=max(tmax..deg.c.),min_tmin=min(tmin..deg.c.),mean_tmean=mean(tave..deg.c.))
    }

    if(min_days<1 & max_days<366){
      DAYMET_altered=DAYMET
      DAYMET_altered$year[which(DAYMET_altered$yday>=(min_days+365))]=DAYMET_altered$year[which(DAYMET_altered$yday>=(min_days+365))]+1
      Summarized= DAYMET_altered%>%
       filter(year<2020 & year>1989)%>%
        filter(yday<=max_days|yday>=(min_days+365))%>%
        group_by(year)%>%
        summarize(mean_precip=mean(prcp..mm.day.),max_tmax=max(tmax..deg.c.),min_tmin=min(tmin..deg.c.),mean_tmean=mean(tave..deg.c.))
          
      }

    if(min_days>0 & max_days>365){
      DAYMET_altered=DAYMET
      DAYMET_altered$year[which(DAYMET_altered$yday<=(max_days-365))]=DAYMET_altered$year[DAYMET_altered$yday<=(max_days-365)]-1
      Summarized= DAYMET_altered%>%
        filter(year<2020 & year>1989)%>%
        filter(yday>=min_days|yday<=(max_days-365))%>%
        group_by(year)%>%
        summarize(mean_precip=mean(prcp..mm.day.),max_tmax=max(tmax..deg.c.),min_tmin=min(tmin..deg.c.),mean_tmean=mean(tave..deg.c.))
      Summarized[30,2:4]=NA
      }

   	raw= as.numeric(Summarized[,2:5][Summarized$year==years[j],])
    mean= as.numeric(raw-apply(na.omit(Summarized[,2:5]),2,mean))      
    z=as.numeric(mean/apply(na.omit(Summarized[,2:5]),2,sd))              
  
    WeatherData=rbind(WeatherData,c(raw,mean,z))
  }
  WeatherData
}


errorCATCH_daymet=function(i,days_prior,days_after){
	oneSITE=dd_attempts[which(dd_attempts$UnCoor ==locations[i]),]  
  weather= try (DayCentOneSite(lat=oneSITE$lat[1],lon=oneSITE$lon[1], dates=as.Date(oneSITE$laydate,"%Y-%m-%d"),days_prior = days_prior,days_after = days_after)) 
  if(class(weather)[1]=="try-error"){
    weather=array(dim=c(dim(oneSITE)[1],8),"NA")
    cbind(locations[i],as.character(oneSITE$attempt),weather)
  } else{
  	cbind(locations[i],as.character(oneSITE$attempt),weather)
  	} 
}

# Exclude data without lay dates
dd_laydate= processed[which(is.na(processed$laydate)=="FALSE"),]

# Exclude data with lay dates in 2020
dd_attempts =dd_laydate[which(dd_laydate$Year!="2020"),]

# Identify unique locations
locations=sort(unique(dd_attempts$UnCoor))


###################
# Run the function#
###################
Batch=1
seq=(round(length(locations)/6)*Batch-(round(length(locations)/6)-1)):(round(length(locations)/6)*Batch)
max(seq)

if(Batch==6){
	seq=(round(length(locations)/6)*Batch-(round(length(locations)/6)-1)):length(locations)	
}

DayMetData_Sub =lapply(seq, function(i) errorCATCH_daymet(i, days_prior=365,days_after=0))


DayMetData=array(dim=c(0,14))
colnames(DayMetData)=c("UnID","AttemptID","noanomoly_mean_precip" ,"noanomoly_max_tmax" ,"noanomoly_min_tmin" ,"noanomoly_mean_tmean","mean_anamoly_mean_precip" ,"mean_anamoly_max_tmax" ,"mean_anamoly_min_tmin" ,"mean_anamoly_mean_tmean","z_anamoly_mean_precip" ,"z_anamoly_max_tmax" ,"z_anamoly_mean_tmin" ,"z_anamoly_mean_tmean")
for (i in 1:length(DayMetData_Sub)){
  oneDAYMET=DayMetData_Sub[[i]]
  colnames(oneDAYMET)=colnames(DayMetData)
	DayMetData=rbind(DayMetData ,oneDAYMET)
}

fname=paste("~/Google Drive/NestwatchProject/Data/","DayMetData_",Batch,"_365prioranamoly.csv",sep="")
write.csv(DayMetData,fname)


######################
# Compile DayMET Data#
######################
batch_1=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_1_45prioranamoly.csv",header=TRUE)[,2:11]
batch_2=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_2_45prioranamoly.csv",header=TRUE)[,2:11]
batch_3=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_3_45prioranamoly.csv",header=TRUE)[,2:11]
batch_4=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_4_45prioranamoly.csv",header=TRUE)[,2:11]
batch_5=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_5_45prioranamoly.csv",header=TRUE)[,2:11]
batch_6=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_6_45prioranamoly.csv",header=TRUE)[,2:11]
DayMetData_45=rbind(batch_1,batch_2,batch_3,batch_4,batch_5,batch_6)

batch_1=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_1_365prioranamoly.csv",header=TRUE)[,2:11]
batch_2=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_2_365prioranamoly.csv",header=TRUE)[,2:11]
batch_3=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_3_365prioranamoly.csv",header=TRUE)[,2:11]
batch_4=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_4_365prioranamoly.csv",header=TRUE)[,2:11]
batch_5=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_5_365prioranamoly.csv",header=TRUE)[,2:11]
batch_6=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_6_365prioranamoly.csv",header=TRUE)[,2:11]

DayMetData_365=rbind(batch_1,batch_2,batch_3,batch_4,batch_5,batch_6)
dim(DayMetData_365)

################################
# Identify sites not calculated#
################################
colnames(DayMetData_45)[3:10]=paste(colnames(DayMetData_45)[3:10],"Next45",sep="_")
DayMetData_45$NotCalculated_Next45_Anamoly=0
DayMetData_45$NotCalculated_Next45_Anamoly[which(is.na(DayMetData_45$mean_anamoly_mean_precip_Next45)==TRUE)]=1

colnames(DayMetData_365)[3:10]=paste(colnames(DayMetData_365)[3:10],"Prior365",sep="_")
DayMetData_365$NotCalculated_Prior365_Anamoly=0
DayMetData_365$NotCalculated_Prior365_Anamoly[which(is.na(DayMetData_365$mean_anamoly_mean_precip_Prior365)==TRUE)]=1


######################
# Compile Everything #
######################
DayMetData_Anamoly=cbind(DayMetData_45,DayMetData_365[3:11])
save(DayMetData_Anamoly,file="~/Google Drive/NestwatchProject/Data/DayMetData_Anamoly.RData")

rm(list=ls())
load("~/Google Drive/NestwatchProject/Data/DayMetData_Anamoly.RData")
colnames(DayMetData_Anamoly)
