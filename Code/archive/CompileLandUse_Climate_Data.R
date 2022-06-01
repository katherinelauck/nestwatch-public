rm(list=ls())
load("~/Google Drive/NestwatchProject/Data/nestwatch_one_row_per_attempt.rdata")
climLANDuse=readRDS("~/Google Drive/NestwatchProject/Data/Climate_Modis_LandUse_Nestwatch.rds")
dd=processed

# Add Modis and WorldClim Data to Attempt Data
dd=cbind(dd,climLANDuse[match(dd$attempt,climLANDuse$ATTEMPT_ID),47:70])

# Redefine column names to make clear where they emerged from
colnames(dd)[18:25]=paste("WorldClim",colnames(dd)[18:25],sep="_")
colnames(dd)[26:41]=paste("Modis",colnames(dd)[26:41],sep="_")

# Add in elevation 
library(raster)
elev=raster("~/Google Drive/NestwatchProject/Data/Elevation/elevation_1KMmd_GMTEDmd.tif")
locations=SpatialPointsDataFrame(coords= cbind(dd$lon,dd$lat),data=dd,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
Amatulli_Elevation=extract(elev,locations)
dd=cbind(dd,Amatulli_Elevation)
saveRDS(dd,file="~/Google Drive/NestwatchProject/Data/nestwatch_one_row_per_attempt_modis_worldclim_elevation.RDS")
