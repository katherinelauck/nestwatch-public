rm(list=ls())

library(raster)
library(tidyverse)
library(rgdal)

#personal computer
#setwd("~/Documents/Projects/NestWatch"); getwd()
#pt <- read.csv('Saved/Points_WithNLCD2011.csv', header=T)
#lc <- raster('GIS/NLCD/NLCD_2011_Land_Cover_L48_20190424.img')

#from lab computer
lc <- raster('~/Documents/ElissaOlimpi/NLCD/NLCD_2011_Land_Cover_L48_20190424.img')
pt <- read.csv('~/Google Drive/NestwatchProject/Saved/Points_WithNLCD2011.csv', header=T)
setwd("~/Google Drive/NestwatchProject"); getwd()

#limit data for testing code
#pt <- pt[1:25,]

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
radList <- c(500, 1000)
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

lcBuffs %>%
  mutate(Seminatural_500m = LC41_500m + LC42_500m + LC43_500m +
           LC51_500m + LC52_500m + 
           LC71_500m + LC72_500m + LC81_500m + 
           LC90_500m + LC95_500m) %>%
  mutate(Developed_500m = LC21_500m + LC22_500m + LC23_500m + LC24_500m) %>%
  mutate(Semi_500m = Seminatural_500m/Total_500m,
         Dev_500m = Developed_500m/Total_500m,
         Ag_500m = LC82_500m/Total_500m) %>%
  
  mutate(Seminatural_1km = LC41_1000m + LC42_1000m + LC43_1000m +
           LC51_1000m + LC52_1000m + 
           LC71_1000m + LC72_1000m + LC81_1000m + 
           LC90_1000m + LC95_1000m) %>%
  mutate(Developed_1km = LC21_1000m + LC22_1000m + LC23_1000m + LC24_1000m) %>%
  mutate(Semi_1km = Seminatural_1km/Total_1km,
         Dev_1km = Developed_1km/Total_1km,
         Ag_1km = LC82_1000m/Total_1km) %>%
  
  dplyr::select(-Seminatural_500m, -Developed_500m,
                -Seminatural_1km, -Developed_1km) -> lcBuffs



csvName1 <- "Saved/NLCD2011_500m1km.csv"
write.csv(lcBuffs, csvName1, row.names=F)


# look for NAs
sumNA <- sum(is.na(lcBuffs))
print(paste("The number of NA cells in this buffer datset is now ", sumNA, ".", sep=""))

