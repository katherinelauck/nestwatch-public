#----------------------------------------------------------------------------------------------------------------------------------

#--------------------------- Pull gridMET climate data for KarpLab NestWatch project - v5.0 - 11/20/20 --------------------

#----------------------------------------------------------------------------------------------------------------------------------
# New in v5: includes Tmin and Pcp 45 days after laydate

#library(rworldmap)
#library(LSD)
library(raster)
library(RNetCDF)
library(remotes)
library(sf)
library(abind)

dir="/Users/billanderegg/Desktop/Misc docs/KarpLab_NestWatch/"
testsites <- read.csv(paste(dir,"Nestwatch_Site_Dates.csv", sep=""))
testsites1 <- testsites[unique(testsites[,3]),]


# Summary: Attempt at climateR worked for some sites and not others - maybe projection issue?
remotes::install_github("mikejohnson51/AOI") # suggested!
remotes::install_github("mikejohnson51/climateR")
library(AOI)
library(climateR)

# (sites = read.csv(paste(dir,"Nestwatch_Site_Dates.csv", sep=""), header=T)[1:10,] %>% 
#  st_as_sf(coords = c("lon", "lat"), crs = 4326))
(sites <- testsites1[1:100,] %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326))
sites_stack = getGridMET(AOI = sites, 
                           param ="tmax", 
                           startDate = "2015-01-01", endDate = "2019-12-31")                           
sites_wide = extract_sites(sites_stack, sites, "UnCoor")    
sites_wide$tmax[1:5, 1:100]



#---------------------- Brute force method of downloading gridMET netCDFs
# First, all "pr" and "tmmx" files from https://www.northwestknowledge.net/metdata/data/

# Example variable and year
infile <- open.nc(paste(dir,"pr_2000.nc", sep=""))
info = file.inq.nc(infile)
print(info)
for (i in 0:(info$ndims-1)) print(paste(dim.inq.nc(infile,i)))
for (i in 0:(info$nvars-1)) print(paste(var.inq.nc(infile,i)))
ppt = var.get.nc(infile,"precipitation_amount")
dim(ppt)
lat1 = var.get.nc(infile,"lat")
dim(lat1)
lat1
lon1 = var.get.nc(infile,"lon")
dim(lon1)
lon1

#Sys.setenv('R_MAX_VSIZE'=32000000000)

#--------------- Now, pull in all climate data and bind it into a matrix of 

# Create a grid-cell ID column: 11295 unique UnCoor values
gridID <- array(dim=c(225311,5))
for (i in 1:225311){
	gridID[i,1] <- testsites1[i,3]
	gridID[i,2] <- which.min(abs(testsites[i,5]-lat1))
	gridID[i,3] <- which.min(abs(testsites[i,4]-lon1))
	gridID[i,4] <- testsites[i,4]
	gridID[i,5] <- testsites[i,5]
}
dim(unique(gridID[,2:3]))		# 8551 unique grid cells


# Iterate through sites, then extract time-series for that grid cell
tmax.all <- array(dim=c(0,8551))
gridID1 <- unique(gridID[,2:3])
for (i in 1979:2020){
	infile <- open.nc(paste(dir,"tmmx_", i, ".nc", sep=""))
	climate1 <- var.get.nc(infile,"air_temperature")
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmax.all <- rbind(tmax.all, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
print(Sys.time())

tmin.all <- array(dim=c(0,8551))
gridID1 <- unique(gridID[,2:3])
for (i in 1979:2020){
	infile <- open.nc(paste(dir,"tmmn_", i, ".nc", sep=""))
	climate1 <- var.get.nc(infile,"air_temperature")
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmin.all <- rbind(tmin.all, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
print(Sys.time())

pcp.all <- array(dim=c(0,8551))
for (i in 1979:2020){
	infile <- open.nc(paste(dir,"pr_", i, ".nc", sep=""))
	climate1 <- var.get.nc(infile,"precipitation_amount")
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	pcp.all <- rbind(pcp.all, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
print(Sys.time())

write.csv(tmax.all, "tmax.all.csv")
write.csv(pcp.all, "pcp.all.csv")

#------------ Now go through each lay-date and extract the metrics
# Rows will be all 225,311 nest attempts; 
# Columns: Pcp 365 days before attempt, mean-anomaly pcp (1980-2000) for same window, z-anomaly
# Same for Tmax 45 days after attempt

climate.date <- array(dim=c(0,2))	#Yr1 is 1979, leapyears 1980,4,8,92,96,2000,04,08,12,16,20
for (i in 1:42){
	if(i %in% c(2,6,10,14,18,22,26,30,34,38,42)) yrval<-array(dim=c(366,2)) else yrval<-array(dim=c(365,2))
	yrval[,1] <- i+1978
	yrval[,2] <- seq(1,dim(yrval)[1],by=1)
	climate.date <- rbind(climate.date,yrval)
}

LayClimateTemp <- function(climatedata, laydate, days_before, days_after){
	lay.doy <- as.numeric(strftime(laydate, format="%j"))
	lay.yr <- as.numeric(strftime(laydate, format="%Y"))
	temp.row <- which(lay.doy==climate.date[,2] & lay.yr==climate.date[,1])
	laytemp <- max(climatedata[(temp.row-days_before):(temp.row+days_after)])
	startrows <- which(lay.doy==climate.date[,2] & climate.date[,1]%in%c(1980:2000))
	mean.temp <- array(dim=c(length(startrows), 1))
	for(i in 1:length(startrows)){
		mean.temp[i,1] <- max(climatedata[(startrows[i]+days_before):(startrows[i]+days_after)])
	}
	mean_anom <- laytemp-mean(mean.temp, na.rm=T)
	z_anom <- mean_anom/sd(mean.temp, na.rm=T)
	return(c(laytemp, mean_anom, z_anom))
}
LayClimateTemp(tmax.all[,1], testsites[1,6], 0, 45)

LayClimateTempMin <- function(climatedata, laydate, days_before, days_after){
	lay.doy <- as.numeric(strftime(laydate, format="%j"))
	lay.yr <- as.numeric(strftime(laydate, format="%Y"))
	temp.row <- which(lay.doy==climate.date[,2] & lay.yr==climate.date[,1])
	laytemp <- min(climatedata[(temp.row-days_before):(temp.row+days_after)])
	startrows <- which(lay.doy==climate.date[,2] & climate.date[,1]%in%c(1980:2000))
	mean.temp <- array(dim=c(length(startrows), 1))
	for(i in 1:length(startrows)){
		mean.temp[i,1] <- min(climatedata[(startrows[i]+days_before):(startrows[i]+days_after)])
	}
	mean_anom <- laytemp-mean(mean.temp, na.rm=T)
	z_anom <- mean_anom/sd(mean.temp, na.rm=T)
	return(c(laytemp, mean_anom, z_anom))
}
LayClimateTempMin(tmin.all[,1], testsites[1,6], 0, 45)

LayClimatePcp <- function(climatedata, laydate, days_before, days_after){
	lay.doy <- as.numeric(strftime(laydate, format="%j"))
	lay.yr <- as.numeric(strftime(laydate, format="%Y"))
	pcp.row <- which(lay.doy==climate.date[,2] & lay.yr==climate.date[,1])
	laypcp <- sum(climatedata[(pcp.row-days_before):(pcp.row+days_after)])
	startrows <- which(lay.doy==climate.date[,2] & climate.date[,1]%in%c(1980:2000))
	mean.pcp <- array(dim=c(length(startrows), 1))
	for(i in 1:length(startrows)){
		mean.pcp[i,1] <- sum(climatedata[(startrows[i]-days_before):(startrows[i]+days_after)])
	}
	mean_anom <- laypcp-mean(mean.pcp, na.rm=T)
	z_anom <- mean_anom/sd(mean.pcp, na.rm=T)
	return(c(laypcp, mean_anom, z_anom))
}
LayClimatePcp(pcp.all[,1], testsites[1,6], 365, 0)


#-------------------------- Now, loop over all lay-attempts and extract climate
# dir="/Users/BillsComputer/Desktop/Projects-Literature/Misc/DannyLabNestWatchCC/"
# tmax.all <- read.csv(paste(dir,"tmax.all.csv", sep=""))
# pcp.all <- read.csv(paste(dir,"pcp.all.csv", sep=""))

# Tmax after and Precip before
climate.master <- array(dim=c(255311,7))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	climate.master[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatedata1 <- tmax.all[,cellID]
	climatedata2 <- pcp.all[,cellID]
	climate.master[i,2:4] <- LayClimateTemp(climatedata1, testsites[i,6], 0, 45)
	climate.master[i,5:7] <- LayClimatePcp(climatedata2, testsites[i,6], 365, 0)
}
print(Sys.time())
# only NAs after row 225,312

sum(flag[,1], na.rm=T)
range(flag[,2], na.rm=T)
hist(flag[,2], breaks=50)
abline(v=0.072)

write.csv(climate.master, paste(dir,"gridMEToutput_v1_tmax-prcpbefore.csv", sep=""))


# Tmin after and Precip after
climate.master2 <- array(dim=c(255311,7))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	climate.master2[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatedata1 <- tmin.all[,cellID]
	climatedata2 <- pcp.all[,cellID]
	climate.master2[i,2:4] <- LayClimateTempMin(climatedata1, testsites[i,6], 0, 45)
	climate.master2[i,5:7] <- LayClimatePcp(climatedata2, testsites[i,6], 0, 45)
}
print(Sys.time())
# only NAs after row 225,312

sum(flag[,1], na.rm=T)
range(flag[,2], na.rm=T)

write.csv(climate.master2, paste(dir,"gridMEToutput_v1_tmin-prcpafter.csv", sep=""))


