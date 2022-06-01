
rm(list=ls())

library(raster)
library(tidyverse)
library(rgdal)

#personal computer
#setwd("~/Documents/Projects/NestWatch"); getwd()
#nestwatch <- read.csv('Data/NestwatchData.csv', header=T)
#lc <- raster('GIS/NLCD/NLCD_2001_Land_Cover_AK_20200213.img')

#from lab computer
lc <- raster('~/Documents/ElissaOlimpi/NLCD/NLCD_2001_Land_Cover_AK_20200213.img')
nestwatch <- read.csv('~/Google Drive/NestwatchProject/Data/NestwatchData.csv', header=T)
setwd("~/Google Drive/NestwatchProject"); getwd()

#choose unique nest locations
pt <- nestwatch[,1:3]
#Remove duplicates based on LOC_ID columns
pt <- pt[!duplicated(pt$LOC_ID), ]

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
#extract 2km buffer land cover
extractListBig <- raster::extract(lc, pt3, buffer=2000,
                                  method="simple")

#how many points do not have extraction data?
length(which(is.na(extractListBig)==T))

#remove points without extractions data
pt <- pt[which(!is.na(extractListBig)==T),]
#save points with extraction data
write.csv(pt, "Saved/Points_WithNLCD2001_AK.csv", row.names=F)
pt2 <- pt
coordinates(pt2)  <-  c("LONGITUDE",  "LATITUDE")
proj4string(pt2)  <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
#reproject coords to match NLCD
pt3 <- spTransform(pt2, CRS(as.character(crs(lc))))

#add a data check here to verify that points and NLCD are same CRS
crs(pt3)
crs(lc)


#remove points that do not have extraction data
extractListBig <- extractListBig[!is.na(extractListBig)]


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


# count occurrences per lc class
counti <- vector()
for(j in 1:nSites){
  for(k in 1:nLc){
    lcCt <- sum(extractListBig[[j]] == lcListBig[k])
    counti <- c(counti, lcCt)
  }
}
# make lc count matrices
lcTab <- (matrix(data=counti, ncol=nLc, nrow=nSites, byrow=TRUE))
rownames(lcTab) <- siteL
lcClassList <- lapply(lcListBig, function(x) paste("LC", x, "_2km",sep=""))
colnames(lcTab) <- lcClassList

# make data frame and export buffers
lcBuffs <- as.data.frame(lcTab)
lcBuffs$LOC_ID <- siteL
lcBuffs$X <- xList
lcBuffs$Y <- yList
lcBuffs$Total <- rowSums(lcBuffs[,1:nLc])
lcBuffs %>%
  mutate(Seminatural = LC41_2km + LC42_2km + LC43_2km +
           LC51_2km + LC52_2km + 
           LC71_2km + LC72_2km + LC81_2km + 
           LC90_2km + LC95_2km) %>%
  mutate(Developed = LC21_2km + LC22_2km + LC23_2km + LC24_2km) %>%
  mutate(Semi_2km = Seminatural/Total,
         Dev_2km = Developed/Total,
         Ag_2km = LC82_2km/Total) %>%
  dplyr::select(-Seminatural, -Developed) -> lcBuffs

csvName1 <- "Saved/NLCD2001_AK_2km.csv"
write.csv(lcBuffs, csvName1, row.names=F)


# look for NAs
sumNA <- sum(is.na(lcBuffs))
print(paste("The number of NA cells in this buffer datset is now ", sumNA, ".", sep=""))
