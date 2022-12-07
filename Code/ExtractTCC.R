rm(list=ls())

library(terra)
library(tidyverse)

#### read in and reformat nestwatch data ####

setwd("//r2d2.cnre.vt.edu/R2D2/eolimpi/Documents/NestWatch"); getwd()

#df <- readRDS('Data/active/success-cleaned.rds') %>%
df <- readRDS('Data/success-cleaned.rds') %>%
  ungroup() %>% as.data.frame() %>%
  select(attempt, UnCoor, NewLU1) %>%
  mutate(X = as.numeric(str_extract(UnCoor, "[^_]+")),
         Y = as.numeric(sub(".*_", "", UnCoor)))
str(df)

#choose unique nest locations
pt <- df[,c(2,4,5)]
#Remove duplicates based on unique coordinate
pt <- pt[!duplicated(pt$UnCoor), ]

#assign CRS
#@#WGS84 (EPSG: 4326): CRS used by Google Earth, 1 of 3 most commonly used in US for unprojected data
crdref <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
#make spatial points (SpatVect) with defined CRS
lonlat <- cbind(pt$X, pt$Y)
pt2 <- vect(lonlat, crs=crdref)
#data check
class(pt2)
pt2

#### read in TCC data ####

#data available here: https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/index.php
#used 'analytical' TCC where masks are applied to 

#2016 cartographic
conus2016_carto <- rast('GIS/TCC/usfs_carto_CONUS_2016/usfs_2016_treecanopy_cartographic_12-14-2018.img')
#2016 analytical
conus2016_analy <- rast('GIS/TCC/analytical_CONUS_2016/usfs_2016_CONUS_canopy_analytical_12-14-2018_u8.img')
#2011 cartographic
conus2011_carto <- rast('GIS/TCC/usfs_carto_CONUS_2011/usfs_2011_treecanopy_cartographic_12-14-2018.img')
#2011 analytical
conus2011_analy <- rast('GIS/TCC/analytical_CONUS_2011/usfs_2011_CONUS_canopy_analytical_12-14-2018_u8.img')

##### extract point canopy cover #####  
#set up environment
siteL <- pt$UnCoor
nSites <- length(siteL)

#reproject points to match TCC raster
pt3 <- project(pt2,conus2016_carto)
#are all rasters same projection? yes
conus2016_carto
conus2016_analy
conus2011_carto
conus2011_analy

#are pt (dataframe) and pt3 (spatial point) same dims?
nrow(pt)
length(pt3)

pt$conus2016_carto <- extract(conus2016_carto, pt3, method='simple', ID=F) %>%
  select(-1) %>% pull(ID)
#pt$AK2016_carto <- extract(AK2016_carto, pt3, method='simple', ID=F) %>%
#  select(-1) %>% pull(ID)
pt$conus2016_analy <- extract(conus2016_analy, pt3, method='simple', ID=F) %>%
  select(-1) %>% pull(ID)
#pt$AK2016_analy <- extract(AK2016_analy, pt3, method='simple', ID=F) %>%
#  select(-1) %>% pull(ID)

pt$conus2011_carto <- extract(conus2011_carto, pt3, method='simple', ID=F) %>%
  select(-1) %>% pull(ID)
#pt$AK2011_carto <- extract(AK2011_carto, pt3, method='simple', ID=F) %>%
#  select(-1) %>% pull(ID)
pt$conus2011_analy <- extract(conus2011_analy, pt3, method='simple', ID=F) %>%
  select(-1) %>% pull(ID)
#pt$AK2011_analy <- extract(AK2011_analy, pt3, method='simple', ID=F) %>%
#  select(-1) %>% pull(ID)

#data check
head(pt)

write.csv(pt, 'Data/canopycover_8-12-22.csv')
