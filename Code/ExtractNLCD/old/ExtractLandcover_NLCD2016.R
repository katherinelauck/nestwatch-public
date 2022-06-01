#https://stackoverflow.com/questions/15824853/large-img-file-processing-in-r-gis
#https://mbjoseph.github.io/posts/2018-12-27-categorical-spatial-data-extraction-around-buffered-points-in-r/

rm(list=ls())

library(raster)
library(tidyverse)
library(rgdal)

#personal computer
#setwd("~/Documents/Projects/NestWatch"); getwd()
#nestwatch <- read.csv('Data/NestwatchData.csv', header=T)
#lc <- raster('~GIS/NLCD/NLCD_2016_Land_Cover_L48_20190424.img')

#from lab computer
lc <- raster('~/Documents/ElissaOlimpi/NLCD/NLCD_2016_Land_Cover_L48_20190424.img')
nestwatch <- read.csv('~/Google Drive/NestwatchProject/Data/NestwatchData.csv', header=T)
setwd("~/Google Drive/NestwatchProject"); getwd()

#choose unique nest locations
pt <- nestwatch[,1:3]
#Remove duplicates based on LOC_ID columns
pt <- pt[!duplicated(pt$LOC_ID), ]

#limit data for testing code
#pt <- pt[1:10,]

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

# clean up categories???
#1: open water (11)
#2: perennial ice/snow (12)
#3: developed (21,22,23,24)
#4: barren land (31)
#5: forest (41,42,43)
#6: shrub/scrub (51,52)
#7: grassland/herbaceous (71,72)
#8: lichen/moss (73,74)
#9: pasture/hay (81)
#10: crops (82)
#11: wetland (90,95)

#m <- c(-Inf,0,NA,
#       11,11,1,
#       12,12,2,
#       21,24,3,
#       31,31,4,
#       41,43,5,
#       51,52,6,
#       71,72,7,
#       73,74,8,
#       81,81,9,
#       82,82,10,
#       90,95,11,
#       96,Inf,NA)
#rclmat <- matrix(m, ncol=3, byrow=TRUE)
#lc2 <- reclassify(NLCD, rclmat, right=NA)



#####################  landscape extraction ########################
# get full list of lc classes for all sites
radList <- as.vector(seq(from=50,to=2000, by=50))
nRads <- length(radList)
#extract 2km buffer land cover
extractListBig <- raster::extract(lc, pt3, buffer=radList[nRads],
                          method="simple")
#remove points that do not have extraction data
#extractListBig <- extractListBig[!is.na(extractListBig)]
#which(!is.na(extractListBig)==T) #keep these rows
#how many points should be removed?
which(is.na(extractListBig)==T)

pt <- pt[which(!is.na(extractListBig)==T),]
pt2 <- pt
coordinates(pt2)  <-  c("LONGITUDE",  "LATITUDE")
proj4string(pt2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
#reproject coords to match NLCD
pt3 <- spTransform(pt2, CRS(as.character(crs(lc))))

#add a data check here to verify that points and NLCD are same CRS
crs(pt3)
crs(lc)

#now that out of range points have been removed, try extraction again
extractListBig <- raster::extract(lc, pt3, buffer=radList[nRads],
                                  method="simple")

# count NAs
lcListBig <-c(11,12,21:24,31,41:43,51,52,71:74,81,82,90,95,96)
nLc <- length(lcListBig)
nNAs <- NULL
tLen <- NULL
siteL <- pt3@data$LOC_ID
xList <- pt[,"LONGITUDE"]
yList <- pt[,"LATITUDE"]

nSites <- length(siteL)

for(i in 1:nSites){
  leni <- length(extractListBig[i][[1]])
  nNAi <- sum(is.na(extractListBig[i][[1]]))
  nNAs <- c(nNAs, nNAi)
  tLen <- c(tLen, leni)
}
propNAs <- nNAs / tLen

naPropCrit <- 0.05
reclass_flag <- sum(propNAs > naPropCrit); reclass_flag
#can add reclass function to this loop to reclassify NAs
if(reclass_flag < 1){
  print("Acceptable number of NAs in data")
} else {
  print("!!! There are too many NAs to proceed with analysis !!!")
}


# loop through radii
bigTab <- matrix(nrow=nSites, ncol=0)

for(i in 1:length(radList)){
  # extract from buffers around points
  bufferRad <- radList[i]
  extractList <- raster::extract(lc, pt3, buffer=radList[i],
                         method="simple")
  # count occurrences per lc class
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
  lcClassList <- lapply(lcListBig, function(x) paste("LC", x, "R",bufferRad,sep=""))
  colnames(lcTab) <- lcClassList
  # merge matrices
  bigTab <- cbind(bigTab, lcTab)
}



# switch from buffers to doughnuts
bigTabRings <- bigTab
nCols <- nRads*nLc
colInd <- (nLc+1):nCols
for(i in 1:nSites){
  for(j in colInd){
    q1 <- bigTab[i,j]
    q2 <- bigTab[i,j-nLc]
    bigTabRings[i,j] <- q1 - q2
  }
}



# convert matrix entries from cells to square meters
lcRes <- res(lc)[1]
bigTab <- bigTab * lcRes * lcRes
bigTabRings <- bigTabRings * lcRes * lcRes



# make data frame and export buffers
lcBuffs <- as.data.frame(bigTab)
lcBuffs$Site <- siteL
lcBuffs$X <- xList
lcBuffs$Y <- yList
csvName1 <- "Saved/NLCD2016_buffers.csv"
write.csv(lcBuffs, csvName1, row.names=FALSE)

# make data frame and export rings
lcRings <- as.data.frame(bigTabRings)
lcRings$Site <- siteL
lcRings$X <- xList
lcRings$Y <- yList
csvName2 <- "Saved/NLCD2016_rings.csv"
write.csv(lcRings, csvName2, row.names=FALSE)

# look for NAs
sumNA <- sum(is.na(lcBuffs))
print(paste("The number of NA cells in this buffer datset is now ", sumNA, ".", sep=""))

