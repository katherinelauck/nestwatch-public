rm(list=ls())

library(raster)
library(tidyverse)
library(rgdal)

#personal computer
setwd("~/Documents/Projects/NestWatch"); getwd()
nestwatch <- read.csv('Data/NestwatchData.csv', header=T)
lc <- raster('GIS/NLCD/NLCD_2008_Land_Cover_L48_20190424.img')


#from lab computer
#lc <- raster('~/Documents/ElissaOlimpi/NLCD/NLCD_2008_Land_Cover_L48_20190424.img')
#setwd("~/Google Drive/NestwatchProject"); getwd()
#nestwatch <- read.csv('Data/NestwatchData.csv', header=T)


#choose unique nest locations
pt <- nestwatch[,1:3]
#Remove duplicates based on LOC_ID columns
pt <- pt[!duplicated(pt$LOC_ID), ]

#assign CRS to nest locations
pt2 <- pt
coordinates(pt2)  <-  c("LONGITUDE",  "LATITUDE")
#WGS84 (EPSG: 4326): CRS used by Google Earth, 1 of 3 most commonly used in US for unprojected data
proj4string(pt2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

crs(pt2)
crs(lc)
#same +datum=WGS84, different proj
#what is proj=aea? Albers Equal Area

#reproject coords to match NLCD
pt3 <- spTransform(pt2, CRS(as.character(crs(lc))))

#plot NLCD and coords
plot(lc)
points(pt3, pch=16, col="red", cex=.75)




#####################  landscape extraction ########################

#get full list of lc classes for all sites
#extract 500m, 1km buffer land cover
radList <- c(500, 1000, 2000)
nRads <- length(radList)

lcListBig <-c(11,12,21:24,31,41:43,51,52,71:74,81,82,90,95,96)
nLc <- length(lcListBig)
nNAs <- NULL
tLen <- NULL
siteL <- pt3@data$LOC_ID
xList <- pt[,"LONGITUDE"]
yList <- pt[,"LATITUDE"]

nSites <- length(siteL)

# loop through radii
bigTab <- matrix(data=, nrow=nSites, ncol=0)
for(i in 1:length(radList)){
  # extract from buffers around points
  bufferRad <- radList[i]
  extractList <- raster::extract(lc, pt3, buffer=radList[i],
                         method="simple")
  # count occurances per lc class
  counti <- vector()
  for(j in 1:nSites){
    for(k in 1:nLc){
      lcCt <- sum(extractList[[j]] == lcListBig[k])
      counti <- c(counti, lcCt)
    }
  }
  # make lc count matrices
  lcTab <- (matrix(data=counti, ncol=nLc, nrow=nSites, byrow=TRUE))
  rownames(lcTab) <- siteL
  lcClassList <- lapply(lcListBig, function(x) paste("LC", x, "_",bufferRad, "m", sep=""))
  colnames(lcTab) <- lcClassList
  # merge matrices
  bigTab <- cbind(bigTab, lcTab)
}


# make data frame and export buffers
lcBuffs <- as.data.frame(bigTab)
lcBuffs$LOC_ID <- siteL
lcBuffs$X <- xList
lcBuffs$Y <- yList
lcBuffs$Total_500m <- rowSums(lcBuffs[,1:nLc])
lcBuffs$Total_1km <- rowSums(lcBuffs[,(nLc+1):(nLc*2)])
lcBuffs$Total_2km <- rowSums(lcBuffs[,(nLc*2+1):(nLc*3)])

csvName1 <- "Saved/NLCD2008_500m1km2km.csv"
write.csv(lcBuffs, csvName1, row.names=F)
          
#data check
lcBuffs %>%
  filter(Total_2km==0 | is.na(Total_2km)==T) %>%
  select(LOC_ID,X,Y) -> lcBuffs2

lcBuffs3 <- lcBuffs2
coordinates(lcBuffs3)  <-  c("X",  "Y")
#WGS84 (EPSG: 4326): CRS used by Google Earth, 1 of 3 most commonly used in US for unprojected data
proj4string(lcBuffs3)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

crs(lcBuffs3)
crs(lc)
#same +datum=WGS84, different proj
#what is proj=aea? Albers Equal Area

#reproject coords to match NLCD
lcBuffs4 <- spTransform(lcBuffs3, CRS(as.character(crs(lc))))

#plot NLCD and coords
plot(lc)
points(lcBuffs4, pch=16, col="red", cex=.75)

