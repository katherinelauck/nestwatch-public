#----------------------------------------------------------------------------------------------------------------------------------

#------------------------- Pull gridMET & CMIP6 climate data for KarpLab NestWatch project - v7.0 - 02/21/21 --------------------

#----------------------------------------------------------------------------------------------------------------------------------
# New in v7: includes CMIP6 download script

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



#---------------------- Brute force method of downloading gridMET netCDFs
# First, all "pr" and "tmmx" files from https://www.northwestknowledge.net/metdata/data/

# Example variable and year
infile <- open.nc(paste(dir,"pr_2000.nc", sep=""))
info = file.inq.nc(infile)
print(info)
for (i in 0:(info$ndims-1)) print(paste(dim.inq.nc(infile,i)))
for (i in 0:(info$nvars-1)) print(paste(var.inq.nc(infile,i)))
ppta = var.get.nc(infile,"precipitation_amount")
dim(ppta)
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

# Has correct units using unpack=TRUE in "var.get.nc()"
tmax.all1 <- array(dim=c(0,8551))
gridID1 <- unique(gridID[,2:3])
for (i in 1979:2020){
	infile <- open.nc(paste(dir,"tmmx_", i, ".nc", sep=""))
	climate1 <- var.get.nc(infile,"air_temperature", unpack=TRUE)
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmax.all1 <- rbind(tmax.all1, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}

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

tmin.all1 <- array(dim=c(0,8551))
gridID1 <- unique(gridID[,2:3])
for (i in 1979:2020){
	infile <- open.nc(paste(dir,"tmmn_", i, ".nc", sep=""))
	climate1 <- var.get.nc(infile,"air_temperature", unpack=TRUE)
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmin.all1 <- rbind(tmin.all1, climate2)
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





#------------------------ Try MACA CMIP5 OpenDAP version

#--------- RCIP4.5: Precip
# GFDL - ESM2M
infile <- open.nc("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_pr_GFDL-ESM2M_r1i1p1_rcp45_2006_2099_CONUS_daily.nc?lat[0:1:584],lon[0:1:1385],time[0:1:34332],precipitation[131:1:265][0:1:584][0:1:1385]")
info = file.inq.nc(infile)			# Subsetting is [Time][Lat][Lon]
print(info)
for (i in 0:(info$ndims-1)) print(paste(dim.inq.nc(infile,i)))
for (i in 0:(info$nvars-1)) print(paste(var.inq.nc(infile,i)))
ppt = var.get.nc(infile,"precipitation")
dim(ppt)							# Dimentions are Lat/Lon/Time
lat2 = var.get.nc(infile,"lat")
dim(lat2)
lat2
lon2 = var.get.nc(infile,"lon")
dim(lon2)
lon2
time1 = var.get.nc(infile,"time")
dim(time1)
time1[1:100]			# 38716 is Jan 1, 2006; Jan 1 2040 is +12,418


lon3 <- ((lon2+180)%%360)-180

# Now works

link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_pr_GFDL-ESM2M_r1i1p1_rcp45_2006_2099_CONUS_daily.nc?precipitation[" 
pcp.gfdl45mid <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 12417+floor(365.25*i)-364
	dayceiling <- 12417+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"precipitation")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	pcp.gfdl45mid <- rbind(pcp.gfdl45mid, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}

# This function differs from the historical one by taking in 2 year chunks of future climate data
LayClimatePcpFut <- function(climatedatafut, climatedatahist, laydate, days_before, days_after){
	lay.doy <- as.numeric(strftime(laydate, format="%j"))
	lay.yr <- as.numeric(strftime(laydate, format="%Y"))
	pcp.row <- which(lay.doy==climate.date[,2] & lay.yr==climate.date[,1])
	pcp.rowfut <- lay.doy+365
	laypcp <- sum(climatedatafut[(pcp.rowfut-days_before):(pcp.rowfut+days_after)])
	startrows <- which(lay.doy==climate.date[,2] & climate.date[,1]%in%c(1980:2000))
	mean.pcp <- array(dim=c(length(startrows), 1))
	for(i in 1:length(startrows)){
		mean.pcp[i,1] <- sum(climatedatahist[(startrows[i]-days_before):(startrows[i]+days_after)])
	}
	mean_anom <- laypcp-mean(mean.pcp, na.rm=T)
	z_anom <- mean_anom/sd(mean.pcp, na.rm=T)
	return(c(laypcp, mean_anom, z_anom))
}
LayClimatePcpFut(pcp.gfdl45mid[1:730,1], pcp.all[,1], testsites[1,6], 365, 0)

pr.rcp45m.gfdl <- array(dim=c(225311,19))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	pr.rcp45m.gfdl[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatehold <- array(dim=c(19,6))
	for (j in 1:19){
		climatedata1 <- pcp.all[,cellID]
		climatedata2 <- pcp.gfdl45mid[(365*j+365-729):(365*j+365),cellID]
		climatehold[j,1:3] <- LayClimatePcpFut(climatedata2, climatedata1, testsites[i,6], 0, 45)
		climatehold[j,4:6] <- LayClimatePcpFut(climatedata2, climatedata1, testsites[i,6], 365, 0)
	}
	pr.rcp45m.gfdl[i,2:7] <- colMeans(climatehold, na.rm=T)
	pr.rcp45m.gfdl[i,8:13] <- apply(climatehold,2,quantile, probs = c(0.10),  na.rm = TRUE)
	pr.rcp45m.gfdl[i,14:19] <- apply(climatehold,2,quantile, probs = c(0.90),  na.rm = TRUE)
}
print(Sys.time())

pr.rcp45m.gfdl[1:10,]

write.csv(pr.rcp45m.gfdl , paste(dir,"MACA_v1_pcp_rcp45m_gfdl.csv", sep=""))

rm(pcp.gfdl45mid)
rm(pr.rcp45m.gfdl)

# End of century
pcp.gfdl45end <- array(dim=c(0,8551))
for (i in 20:20){
	dayfloor <- 27027+floor(365.25*i)-364
	dayceiling <- 27027 +floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"precipitation")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	pcp.gfdl45end <- rbind(pcp.gfdl45end, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}

dim(pcp.gfdl45end)

pr.rcp45e.gfdl <- array(dim=c(225311,19))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	pr.rcp45e.gfdl[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatehold <- array(dim=c(19,6))
	for (j in 1:19){
		climatedata1 <- pcp.all[,cellID]
		climatedata2 <- pcp.gfdl45end[(365*j+365-729):(365*j+365),cellID]
		climatehold[j,1:3] <- LayClimatePcpFut(climatedata2, climatedata1, testsites[i,6], 0, 45)
		climatehold[j,4:6] <- LayClimatePcpFut(climatedata2, climatedata1, testsites[i,6], 365, 0)
	}
	pr.rcp45e.gfdl[i,2:7] <- colMeans(climatehold, na.rm=T)
	pr.rcp45e.gfdl[i,8:13] <- apply(climatehold,2,quantile, probs = c(0.10),  na.rm = TRUE)
	pr.rcp45e.gfdl[i,14:19] <- apply(climatehold,2,quantile, probs = c(0.90),  na.rm = TRUE)
}
print(Sys.time())

write.csv(pr.rcp45e.gfdl , paste(dir,"MACA_v1_pcp_rcp45e_gfdl.csv", sep=""))

rm(pcp.gfdl45end)
rm(pr.rcp45e.gfdl)

# Now Tmin/Tmax
LayClimateTempMaxFut <- function(climatedatafut, climatedatahist, laydate, days_before, days_after){
	lay.doy <- as.numeric(strftime(laydate, format="%j"))
	lay.yr <- as.numeric(strftime(laydate, format="%Y"))
	temp.row <- which(lay.doy==climate.date[,2] & lay.yr==climate.date[,1])
	temp.rowfut <- lay.doy+365
	laytemp <- max(climatedatafut[(temp.rowfut-days_before):(temp.rowfut+days_after)])
	startrows <- which(lay.doy==climate.date[,2] & climate.date[,1]%in%c(1980:2000))
	mean.temp <- array(dim=c(length(startrows), 1))
	for(i in 1:length(startrows)){
		mean.temp[i,1] <- max(climatedatahist[(startrows[i]-days_before):(startrows[i]+days_after)])
	}
	mean_anom <- laytemp-mean(mean.temp, na.rm=T)
	z_anom <- mean_anom/sd(mean.temp, na.rm=T)
	return(c(laytemp, mean_anom, z_anom))
}

LayClimateTempMinFut <- function(climatedatafut, climatedatahist, laydate, days_before, days_after){
	lay.doy <- as.numeric(strftime(laydate, format="%j"))
	lay.yr <- as.numeric(strftime(laydate, format="%Y"))
	temp.row <- which(lay.doy==climate.date[,2] & lay.yr==climate.date[,1])
	temp.rowfut <- lay.doy+365
	laytemp <- min(climatedatafut[(temp.rowfut-days_before):(temp.rowfut+days_after)])
	startrows <- which(lay.doy==climate.date[,2] & climate.date[,1]%in%c(1980:2000))
	mean.temp <- array(dim=c(length(startrows), 1))
	for(i in 1:length(startrows)){
		mean.temp[i,1] <- min(climatedatahist[(startrows[i]-days_before):(startrows[i]+days_after)])
	}
	mean_anom <- laytemp-mean(mean.temp, na.rm=T)
	z_anom <- mean_anom/sd(mean.temp, na.rm=T)
	return(c(laytemp, mean_anom, z_anom))
}
LayClimateTempMinFut(tmin.gfdl45mid[,1], tmin.all1[,1], testsites[1,6], 0, 45)


# Mid-Century
link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_tasmin_GFDL-ESM2M_r1i1p1_rcp45_2006_2099_CONUS_daily.nc?air_temperature[" 
tmin.gfdl45mid <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 12417+floor(365.25*i)-364
	dayceiling <- 12417+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"air_temperature")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmin.gfdl45mid <- rbind(tmin.gfdl45mid, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
tmin.gfdl45mid <- tmin.gfdl45mid/10

link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2006_2099_CONUS_daily.nc?air_temperature[" 
tmax.gfdl45mid <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 12417+floor(365.25*i)-364
	dayceiling <- 12417+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"air_temperature")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmax.gfdl45mid <- rbind(tmax.gfdl45mid, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
tmax.gfdl45mid <- tmax.gfdl45mid/10

temp.rcp45m.gfdl <- array(dim=c(225311,19))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	temp.rcp45m.gfdl[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatehold <- array(dim=c(19,6))
	for (j in 1:19){
		climatedata1a <- tmin.all1[,cellID]
		climatedata1b <- tmax.all1[,cellID]
		climatedata2a <- tmin.gfdl45mid[(365*j+365-729):(365*j+365),cellID]
		climatedata2b <- tmax.gfdl45mid[(365*j+365-729):(365*j+365),cellID]
		climatehold[j,1:3] <- LayClimateTempMinFut(climatedata2a, climatedata1a, testsites[i,6], 0, 45)
		climatehold[j,4:6] <- LayClimateTempMaxFut(climatedata2b, climatedata1b, testsites[i,6], 0, 45)
	}
	temp.rcp45m.gfdl[i,2:7] <- colMeans(climatehold, na.rm=T)
	temp.rcp45m.gfdl[i,8:13] <- apply(climatehold,2,quantile, probs = c(0.10),  na.rm = TRUE)
	temp.rcp45m.gfdl[i,14:19] <- apply(climatehold,2,quantile, probs = c(0.90),  na.rm = TRUE)
}
print(Sys.time())

temp.rcp45m.gfdl[1:10,]

#write.csv(temp.rcp45m.gfdl , paste(dir,"MACA_v1_temp_rcp45m_gfdl.csv", sep=""))
rm(temp.rcp45m.gfdl)

# End of century
link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_tasmin_GFDL-ESM2M_r1i1p1_rcp45_2006_2099_CONUS_daily.nc?air_temperature[" 
tmin.gfdl45end <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 27027+floor(365.25*i)-364
	dayceiling <- 27027+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"air_temperature")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmin.gfdl45end <- rbind(tmin.gfdl45end, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
tmin.gfdl45end <- tmin.gfdl45end/10

link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp45_2006_2099_CONUS_daily.nc?air_temperature[" 
tmax.gfdl45end <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 27027+floor(365.25*i)-364
	dayceiling <- 27027+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"air_temperature")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmax.gfdl45end <- rbind(tmax.gfdl45end, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
tmax.gfdl45end <- tmax.gfdl45end/10

print(Sys.time())

temp.rcp45e.gfdl <- array(dim=c(225311,19))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	temp.rcp45e.gfdl[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatehold <- array(dim=c(19,6))
	for (j in 1:19){
		climatedata1a <- tmin.all1[,cellID]
		climatedata1b <- tmax.all1[,cellID]
		climatedata2a <- tmin.gfdl45end[(365*j+365-729):(365*j+365),cellID]
		climatedata2b <- tmax.gfdl45end[(365*j+365-729):(365*j+365),cellID]
		climatehold[j,1:3] <- LayClimateTempMinFut(climatedata2a, climatedata1a, testsites[i,6], 0, 45)
		climatehold[j,4:6] <- LayClimateTempMaxFut(climatedata2b, climatedata1b, testsites[i,6], 0, 45)
	}
	temp.rcp45e.gfdl[i,2:7] <- colMeans(climatehold, na.rm=T)
	temp.rcp45e.gfdl[i,8:13] <- apply(climatehold,2,quantile, probs = c(0.10),  na.rm = TRUE)
	temp.rcp45e.gfdl[i,14:19] <- apply(climatehold,2,quantile, probs = c(0.90),  na.rm = TRUE)
}
print(Sys.time())

temp.rcp45e.gfdl[1:10,]

#write.csv(temp.rcp45e.gfdl , paste(dir,"MACA_v1_temp_rcp45e_gfdl.csv", sep=""))


#------------- GFDL RCP8.5
link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_pr_GFDL-ESM2M_r1i1p1_rcp85_2006_2099_CONUS_daily.nc?precipitation[" 
pcp.gfdl85mid <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 12417+floor(365.25*i)-364
	dayceiling <- 12417+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"precipitation")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	pcp.gfdl85mid <- rbind(pcp.gfdl85mid, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}

# This function differs from the historical one by taking in 2 year chunks of future climate data
LayClimatePcpFut <- function(climatedatafut, climatedatahist, laydate, days_before, days_after){
	lay.doy <- as.numeric(strftime(laydate, format="%j"))
	lay.yr <- as.numeric(strftime(laydate, format="%Y"))
	pcp.row <- which(lay.doy==climate.date[,2] & lay.yr==climate.date[,1])
	pcp.rowfut <- lay.doy+365
	laypcp <- sum(climatedatafut[(pcp.rowfut-days_before):(pcp.rowfut+days_after)])
	startrows <- which(lay.doy==climate.date[,2] & climate.date[,1]%in%c(1980:2000))
	mean.pcp <- array(dim=c(length(startrows), 1))
	for(i in 1:length(startrows)){
		mean.pcp[i,1] <- sum(climatedatahist[(startrows[i]-days_before):(startrows[i]+days_after)])
	}
	mean_anom <- laypcp-mean(mean.pcp, na.rm=T)
	z_anom <- mean_anom/sd(mean.pcp, na.rm=T)
	return(c(laypcp, mean_anom, z_anom))
}
LayClimatePcpFut(pcp.gfdl85mid[1:730,1], pcp.all[,1], testsites[1,6], 365, 0)

pr.rcp85m.gfdl <- array(dim=c(225311,19))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	pr.rcp85m.gfdl[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatehold <- array(dim=c(19,6))
	for (j in 1:19){
		climatedata1 <- pcp.all[,cellID]
		climatedata2 <- pcp.gfdl85mid[(365*j+365-729):(365*j+365),cellID]
		climatehold[j,1:3] <- LayClimatePcpFut(climatedata2, climatedata1, testsites[i,6], 0, 45)
		climatehold[j,4:6] <- LayClimatePcpFut(climatedata2, climatedata1, testsites[i,6], 365, 0)
	}
	pr.rcp85m.gfdl[i,2:7] <- colMeans(climatehold, na.rm=T)
	pr.rcp85m.gfdl[i,8:13] <- apply(climatehold,2,quantile, probs = c(0.10),  na.rm = TRUE)
	pr.rcp85m.gfdl[i,14:19] <- apply(climatehold,2,quantile, probs = c(0.90),  na.rm = TRUE)
}
print(Sys.time())

pr.rcp85m.gfdl[1:10,]

write.csv(pr.rcp85m.gfdl , paste(dir,"MACA_v1_pcp_rcp85m_gfdl.csv", sep=""))

rm(pcp.gfdl85mid)
rm(pr.rcp85m.gfdl)

# End of century
pcp.gfdl85end <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 27027+floor(365.25*i)-364
	dayceiling <- 27027 +floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"precipitation")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	pcp.gfdl85end <- rbind(pcp.gfdl85end, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}

dim(pcp.gfdl85end)

pr.rcp85e.gfdl <- array(dim=c(225311,19))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	pr.rcp85e.gfdl[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatehold <- array(dim=c(19,6))
	for (j in 1:19){
		climatedata1 <- pcp.all[,cellID]
		climatedata2 <- pcp.gfdl85end[(365*j+365-729):(365*j+365),cellID]
		climatehold[j,1:3] <- LayClimatePcpFut(climatedata2, climatedata1, testsites[i,6], 0, 45)
		climatehold[j,4:6] <- LayClimatePcpFut(climatedata2, climatedata1, testsites[i,6], 365, 0)
	}
	pr.rcp85e.gfdl[i,2:7] <- colMeans(climatehold, na.rm=T)
	pr.rcp85e.gfdl[i,8:13] <- apply(climatehold,2,quantile, probs = c(0.10),  na.rm = TRUE)
	pr.rcp85e.gfdl[i,14:19] <- apply(climatehold,2,quantile, probs = c(0.90),  na.rm = TRUE)
}
print(Sys.time())

pr.rcp85e.gfdl[1:10,]

write.csv(pr.rcp85e.gfdl , paste(dir,"MACA_v1_pcp_rcp85e_gfdl.csv", sep=""))

rm(pcp.gfdl85end)
rm(pr.rcp85e.gfdl)

# Now Tmin/Tmax
LayClimateTempMaxFut <- function(climatedatafut, climatedatahist, laydate, days_before, days_after){
	lay.doy <- as.numeric(strftime(laydate, format="%j"))
	lay.yr <- as.numeric(strftime(laydate, format="%Y"))
	temp.row <- which(lay.doy==climate.date[,2] & lay.yr==climate.date[,1])
	temp.rowfut <- lay.doy+365
	laytemp <- max(climatedatafut[(temp.rowfut-days_before):(temp.rowfut+days_after)])
	startrows <- which(lay.doy==climate.date[,2] & climate.date[,1]%in%c(1980:2000))
	mean.temp <- array(dim=c(length(startrows), 1))
	for(i in 1:length(startrows)){
		mean.temp[i,1] <- max(climatedatahist[(startrows[i]-days_before):(startrows[i]+days_after)])
	}
	mean_anom <- laytemp-mean(mean.temp, na.rm=T)
	z_anom <- mean_anom/sd(mean.temp, na.rm=T)
	return(c(laytemp, mean_anom, z_anom))
}

LayClimateTempMinFut <- function(climatedatafut, climatedatahist, laydate, days_before, days_after){
	lay.doy <- as.numeric(strftime(laydate, format="%j"))
	lay.yr <- as.numeric(strftime(laydate, format="%Y"))
	temp.row <- which(lay.doy==climate.date[,2] & lay.yr==climate.date[,1])
	temp.rowfut <- lay.doy+365
	laytemp <- min(climatedatafut[(temp.rowfut-days_before):(temp.rowfut+days_after)])
	startrows <- which(lay.doy==climate.date[,2] & climate.date[,1]%in%c(1980:2000))
	mean.temp <- array(dim=c(length(startrows), 1))
	for(i in 1:length(startrows)){
		mean.temp[i,1] <- min(climatedatahist[(startrows[i]-days_before):(startrows[i]+days_after)])
	}
	mean_anom <- laytemp-mean(mean.temp, na.rm=T)
	z_anom <- mean_anom/sd(mean.temp, na.rm=T)
	return(c(laytemp, mean_anom, z_anom))
}
LayClimateTempMinFut(tmin.gfdl85mid[,1], tmin.all1[,1], testsites[1,6], 0, 45)


# Mid-Century
link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_tasmin_GFDL-ESM2M_r1i1p1_rcp85_2006_2099_CONUS_daily.nc?air_temperature[" 
tmin.gfdl85mid <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 12417+floor(365.25*i)-364
	dayceiling <- 12417+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"air_temperature")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmin.gfdl85mid <- rbind(tmin.gfdl85mid, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
tmin.gfdl85mid <- tmin.gfdl85mid/10

link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp85_2006_2099_CONUS_daily.nc?air_temperature[" 
tmax.gfdl85mid <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 12417+floor(365.25*i)-364
	dayceiling <- 12417+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"air_temperature")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmax.gfdl85mid <- rbind(tmax.gfdl85mid, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
tmax.gfdl85mid <- tmax.gfdl85mid/10

temp.rcp85m.gfdl <- array(dim=c(225311,19))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	temp.rcp85m.gfdl[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatehold <- array(dim=c(19,6))
	for (j in 1:19){
		climatedata1a <- tmin.all1[,cellID]
		climatedata1b <- tmax.all1[,cellID]
		climatedata2a <- tmin.gfdl85mid[(365*j+365-729):(365*j+365),cellID]
		climatedata2b <- tmax.gfdl85mid[(365*j+365-729):(365*j+365),cellID]
		climatehold[j,1:3] <- LayClimateTempMinFut(climatedata2a, climatedata1a, testsites[i,6], 0, 45)
		climatehold[j,4:6] <- LayClimateTempMaxFut(climatedata2b, climatedata1b, testsites[i,6], 0, 45)
	}
	temp.rcp85m.gfdl[i,2:7] <- colMeans(climatehold, na.rm=T)
	temp.rcp85m.gfdl[i,8:13] <- apply(climatehold,2,quantile, probs = c(0.10),  na.rm = TRUE)
	temp.rcp85m.gfdl[i,14:19] <- apply(climatehold,2,quantile, probs = c(0.90),  na.rm = TRUE)
}
print(Sys.time())

temp.rcp85m.gfdl[1:10,]

write.csv(temp.rcp85m.gfdl , paste(dir,"MACA_v1_temp_rcp85m_gfdl.csv", sep=""))
rm(temp.rcp85m.gfdl)

# End of century
link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_tasmin_GFDL-ESM2M_r1i1p1_rcp85_2006_2099_CONUS_daily.nc?air_temperature[" 
tmin.gfdl85end <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 27027+floor(365.25*i)-364
	dayceiling <- 27027+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"air_temperature")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmin.gfdl85end <- rbind(tmin.gfdl85end, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
tmin.gfdl85end <- tmin.gfdl85end/10

link1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_tasmax_GFDL-ESM2M_r1i1p1_rcp85_2006_2099_CONUS_daily.nc?air_temperature[" 
tmax.gfdl85end <- array(dim=c(0,8551))
for (i in 1:20){
	dayfloor <- 27027+floor(365.25*i)-364
	dayceiling <- 27027+floor(365.25*i)
	infile <- open.nc(paste(link1, dayfloor, ":1:", dayceiling, "][0:1:584][0:1:1385]", sep=""))
	climate1a <- 10*var.get.nc(infile,"air_temperature")
	climate1 <- climate1a[,rev(seq_len(ncol(climate1a))),]			# Flip MACA climate to same orientation as gridMET
	climate2 <- array(dim=c(dim(climate1)[3], 8551))
	for (j in 1:8551){
	climate2[,j] <- climate1[gridID1[j,2], gridID1[j,1],]
	}
	tmax.gfdl85end <- rbind(tmax.gfdl85end, climate2)
	rm(infile)
	rm(climate1)
	rm(climate2)
}
tmax.gfdl85end <- tmax.gfdl85end/10

print(Sys.time())

temp.rcp85e.gfdl <- array(dim=c(225311,19))
flag <- array(dim=c(225311,2))
for (i in 1:225311){
	temp.rcp85e.gfdl[i,1] <- testsites[i,3]
	w1 <- gridID[which(testsites[i,4]==gridID[,4] & testsites[i,5]==gridID[,5]),]
	if(length(w1)<6) cell<-w1[2:3] else cell<-w1[1,2:3]
	dist <- which.min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	if(length(w1)==0) cell<-gridID[dist,2:3]
	if(length(w1)==0) flag[i,1]<-1
	if(length(w1)==0) flag[i,2]<-min(abs(testsites[i,4]-gridID[,4])+abs(testsites[i,5]-gridID[,5]))
	cellID <- which(gridID1[,1]==cell[1] & gridID1[,2]==cell[2])
	climatehold <- array(dim=c(19,6))
	for (j in 1:19){
		climatedata1a <- tmin.all1[,cellID]
		climatedata1b <- tmax.all1[,cellID]
		climatedata2a <- tmin.gfdl85end[(365*j+365-729):(365*j+365),cellID]
		climatedata2b <- tmax.gfdl85end[(365*j+365-729):(365*j+365),cellID]
		climatehold[j,1:3] <- LayClimateTempMinFut(climatedata2a, climatedata1a, testsites[i,6], 0, 45)
		climatehold[j,4:6] <- LayClimateTempMaxFut(climatedata2b, climatedata1b, testsites[i,6], 0, 45)
	}
	temp.rcp85e.gfdl[i,2:7] <- colMeans(climatehold, na.rm=T)
	temp.rcp85e.gfdl[i,8:13] <- apply(climatehold,2,quantile, probs = c(0.10),  na.rm = TRUE)
	temp.rcp85e.gfdl[i,14:19] <- apply(climatehold,2,quantile, probs = c(0.90),  na.rm = TRUE)
}
print(Sys.time())

temp.rcp85e.gfdl[1:10,]

write.csv(temp.rcp85e.gfdl , paste(dir,"MACA_v1_temp_rcp85e_gfdl.csv", sep=""))




