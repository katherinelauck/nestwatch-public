rm(list=ls())
library(raster)
library(lme4)
library(scales)
library(geosphere)

############
# Functions#
############
climGenerator=function(months,climatetype,climate_directory,method,locations){
  setwd(climate_directory)
  climate_files=list.files(climate_directory)
  climate_files=climate_files[grep(climatetype,climate_files)]
  
  nsites=dim(locations@data)[1]
  climates=array(dim=c(nsites,0))
  
  for (i in 1:length(months)){
    clim_month_raster=raster(climate_files[grep(months[i],climate_files)])
    climates=cbind(climates,extract(clim_month_raster,locations))
  }
  
  if(method=="mean"){
    climates_summarized=apply(climates,1,mean)
  }
  if(method=="sum"){
    climates_summarized=apply(climates,1,sum)
  }
  if(method=="max"){
    climates_summarized=apply(climates,1,max)
  }
  if(method=="min"){
    climates_summarized=apply(climates,1,min)
  }
  climates_summarized
}

################
# Read in files#
################
dd=read.csv("~/Google Drive/NestwatchProject/Data/NestwatchData.csv",header=TRUE)

####################################################
# Alter data to make a unique dataset of site years#
####################################################
dd$YEAR=substr(dd$OBS_DT,6,9) # Extract the year of the observation. If it is blank, extract from laydate, hatchdate, or fledgedate
dd$YEAR[which(dd$YEAR=="")]=substr(dd$FIRST_LAY_DT,6,9)[which(dd$YEAR=="")]
dd$YEAR[which(dd$YEAR=="")]=substr(dd$HATCH_DT,6,9)[which(dd$YEAR=="")]
dd$YEAR[which(dd$YEAR=="")]=substr(dd$FLEDGE_DT,6,9)[which(dd$YEAR=="")]

dd$LatLon=paste(dd$LATITUDE,dd$LONGITUDE,sep="_") # Make unique site code
dd$LatLonYear=paste(dd$LATITUDE,dd$LONGITUDE,dd$YEAR,sep="_") # Make unique site year code

LatLon=dd[match(unique(dd$LatLon),dd$LatLon),] # Build dataset of unique site years
LatLon=LatLon[,match(c("LatLon","LATITUDE","LONGITUDE"),colnames(LatLon))]

LatLonYear=dd[match(unique(dd$LatLonYear),dd$LatLonYear),] # Build dataset of unique site years
LatLonYear=LatLonYear[,match(c("LatLonYear","LATITUDE","LONGITUDE","YEAR"),colnames(LatLonYear))]

###############################################################
# Extract the climate data (long term averages from WorldClim)#
###############################################################
months=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12")
climate_directory="~/Google Drive/NestwatchProject/Data/Worldclim/"
locations=SpatialPointsDataFrame(coords= cbind(LatLon$LONGITUDE,LatLon$LATITUDE),data=LatLon,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Need to iteratively toggle points that happen to fall outside of land climate rasters
Climate=climGenerator(months,climatetype="pre",climate_directory,method="mean",locations) # Calculate climate for all sites
NoSites=LatLon[which(is.na(Climate)==TRUE),] # identify sites that cant have climate calculated

NoSites$LATITUDE=NoSites$LATITUDE+.25 # Try moving the sites .25 degrees north
locations_update=SpatialPointsDataFrame(coords= cbind(NoSites$LONGITUDE,NoSites$LATITUDE),data=NoSites,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
NoSites_Climate=climGenerator(months,climatetype="pre",climate_directory,method="mean",locations_update) # recalculate climate 
length(which(is.na(NoSites_Climate)==TRUE)) # 512 sites moved .25 degrees north

NoSites[which(is.na(NoSites_Climate)==TRUE),]$LONGITUDE=NoSites[which(is.na(NoSites_Climate)==TRUE),]$LONGITUDE-.25 # Try moving remaining sites .25 degrees west
locations_update=SpatialPointsDataFrame(coords= cbind(NoSites$LONGITUDE,NoSites$LATITUDE),data=NoSites,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
NoSites_Climate=climGenerator(months,climatetype="pre",climate_directory,method="mean",locations_update) # recalculate climate 
length(which(is.na(NoSites_Climate)==TRUE)) # 48 sites moved .25 degrees west

NoSites[which(is.na(NoSites_Climate)==TRUE),]$LONGITUDE=NoSites[which(is.na(NoSites_Climate)==TRUE),]$LONGITUDE-.25 # Try moving remaining sites .25 degrees more west
locations_update=SpatialPointsDataFrame(coords= cbind(NoSites$LONGITUDE,NoSites$LATITUDE),data=NoSites,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
NoSites_Climate=climGenerator(months,climatetype="pre",climate_directory,method="mean",locations_update) # recalculate climate 
length(which(is.na(NoSites_Climate)==TRUE)) # 29 sites moved .5 degrees west

# Replace with the updated lat and lons
LatLon$Update_Lat=LatLon$LATITUDE
LatLon$Update_Lon=LatLon$LONGITUDE

LatLon[match(NoSites$LatLon,LatLon$LatLon),]$Update_Lat=NoSites$LATITUDE
LatLon[match(NoSites$LatLon,LatLon$LatLon),]$Update_Lon=NoSites$LONGITUDE

# Now rerun climate for all the values we care about. 
locations=SpatialPointsDataFrame(coords= cbind(LatLon$Update_Lon,LatLon$Update_Lat),data=LatLon,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
months=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12")

LatLon$TotalAnnualPrecip=climGenerator(months,climatetype="pre",climate_directory,method="sum",locations) 
LatLon$AveAnnualTemp=climGenerator(months,climatetype="tavg",climate_directory,method="mean",locations) 
LatLon$MaxAnnualTemp=climGenerator(months,climatetype="tmax",climate_directory,method="max",locations) 
LatLon$MinAnnualTemp=climGenerator(months,climatetype="tmin",climate_directory,method="min",locations) 

# Choose breeding months to be march through august 
months=c("_03","_04","_05","_06","_07","_08")
LatLon$TotalBreedPrecip=climGenerator(months,climatetype="pre",climate_directory,method="sum",locations) 
LatLon$AveBreedTemp=climGenerator(months,climatetype="tavg",climate_directory,method="mean",locations) 
LatLon$MaxBreedTemp=climGenerator(months,climatetype="tmax",climate_directory,method="max",locations) 
LatLon$MinBreedTemp=climGenerator(months,climatetype="tmin",climate_directory,method="min",locations) 

# Now recombine with the original dataset
LatLon[which(is.na(LatLon$MinAnnualTemp)==TRUE),c(4,5)]=NA #Change sites that couldnt be toggled back to NA. 

Climate_NestWatch=cbind(dd,LatLon[match(dd$LatLon,LatLon$LatLon),4:13])
saveRDS(Climate_NestWatch,"~/Google Drive/NestwatchProject/Data/Climate_NestWatch.RDS")

