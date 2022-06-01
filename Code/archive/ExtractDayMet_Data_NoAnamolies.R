rm(list=ls())
library(daymetr)
library(parallel)
load('~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevation.RData')

# Establish daymet extraction function
DayCentOneSite=function(lat, lon, dates, days_prior, days_after){
  year_min=min(format(dates,"%Y"))
  year_max=max(format(dates,"%Y"))
  doys=as.numeric(strftime(dates, format = "%j"))
  if(length(which(doys<days_prior))>0){
    year_min=as.numeric(year_min)-1
  }
  if(length(which((doys+days_after)>365))>0){
    if(length(which(format(dates,"%Y")[which((doys+days_after)>365)]>=year_max))>0){
      year_max =as.numeric(year_max)+1      
    }
  }


DAYMET=download_daymet(site = "SITE", lat = lat, lon = lon, start = year_min, end = year_max, path = tempdir(), internal = TRUE, silent = FALSE, force = FALSE, simplify = FALSE)
  DAYMET=DAYMET$data
  
  WeatherData=array(dim=c(0,4))
  colnames(WeatherData)=c("precip_mmday","tmax_degc","tmin_degc","tave_degc")
  for (j in 1:length(dates)){
    row_num=which(DAYMET$year==format(dates,"%Y")[j] & DAYMET$yday==doys[j])
    min_row=row_num-days_prior
    max_row=row_num+days_after
    data_sub= DAYMET[min_row: max_row,c("prcp..mm.day.","tmax..deg.c.","tmin..deg.c.")]
	data_sub $tave=apply(data_sub[,2:3],1,mean)
    WeatherData=rbind(WeatherData,apply(data_sub,2,mean)) 
  }
  WeatherData
}

errorCATCH_daymet=function(i,days_prior,days_after){
	oneSITE=dd_attempts[which(dd_attempts$UnCoor ==locations[i]),]  
  weather= try (DayCentOneSite(lat=oneSITE$lat[1],lon=oneSITE$lon[1], dates=as.Date(oneSITE$laydate,"%Y-%m-%d"),days_prior = days_prior,days_after = days_after)) 
  if(class(weather)[1]=="try-error"){
    weather=array(dim=c(dim(oneSITE)[1],4),"NA")
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
if(Batch==6){
	seq=(round(length(locations)/6)*Batch-(round(length(locations)/6)-1)):length(locations)	
}
DayMetData_Sub =lapply(seq, function(i) errorCATCH_daymet(i, days_prior=0,days_after=45))


DayMetData=array(dim=c(0,6))
colnames(DayMetData)=c("UnID","AttemptID","precip_mmday","tmax_degc","tmin_degc","tave_degc")
for (i in 1:length(DayMetData_Sub)){
	DayMetData=rbind(DayMetData ,DayMetData_Sub[[i]])
}

fname=paste("~/Google Drive/NestwatchProject/Data/","DayMetData_",Batch,"_45.csv",sep="")
write.csv(DayMetData,fname)


######################
# Compile DayMET Data#
######################

batch_1=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_1_45.csv",header=TRUE)[,2:7]
batch_2=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_2_45.csv",header=TRUE)[,2:7]
batch_3=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_3_45.csv",header=TRUE)[,2:7]
batch_4=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_4_45.csv",header=TRUE)[,2:7]
batch_5=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_5_45.csv",header=TRUE)[,2:7]
batch_6=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_6_45.csv",header=TRUE)[,2:7]

DayMetData_45=rbind(batch_1,batch_2,batch_3,batch_4,batch_5,batch_6)

batch_1=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_1_365.csv",header=TRUE)[,2:7]
batch_2=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_2_365.csv",header=TRUE)[,2:7]
batch_3=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_3_365.csv",header=TRUE)[,2:7]
batch_4=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_4_365.csv",header=TRUE)[,2:7]
batch_5=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_5_365.csv",header=TRUE)[,2:7]
batch_6=read.csv("~/Google Drive/NestwatchProject/Data/DayMetData_6_365.csv",header=TRUE)[,2:7]

DayMetData_365=rbind(batch_1,batch_2,batch_3,batch_4,batch_5,batch_6)
head(DayMetData_45)

################################
# Identify sites not calculated#
################################
colnames(DayMetData_45)[3:6]=paste(colnames(DayMetData_45)[3:6],"Next45_no_anamoly",sep="_")
DayMetData_45$NotCalculated_Next45_no_anamoly=0
DayMetData_45$NotCalculated_Next45_no_anamoly[which(is.na(DayMetData_45$precip_mmday_Next45_no_anamoly)==TRUE)]=1

colnames(DayMetData_365)[3:6]=paste(colnames(DayMetData_365)[3:6],"Prior365_no_anamoly",sep="_")
DayMetData_365$NotCalculated_Prior365_no_anamoly=0
DayMetData_365$NotCalculated_Prior365_no_anamoly[which(is.na(DayMetData_365$precip_mmday_Prior365_no_anamoly)==TRUE)]=1

DayMetData_NoAnamoly=cbind(DayMetData_45,DayMetData_365[3:7])

save(DayMetData_NoAnamoly,file="~/Google Drive/NestwatchProject/Data/DayMetData_NoAnamoly.RData")

