rm(list=ls())
library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(raster)
library(rnaturalearth)
library(MODIS)
library(velox)
library(viridis)
library(geosphere)

############
# Functions#
############


################
# Read in files#
################
dd=readRDS("~/Google Drive/NestwatchProject/Data/Climate_NestWatch.RDS")

####################################################
# Alter data to make a unique dataset of site years#
####################################################
dd$YEAR=substr(dd$OBS_DT,6,9) # Extract the year of the observation. If it is blank, extract from laydate, hatchdate, or fledgedate
dd$YEAR[which(dd$YEAR=="")]=substr(dd$FIRST_LAY_DT,6,9)[which(dd$YEAR=="")]
dd$YEAR[which(dd$YEAR=="")]=substr(dd$HATCH_DT,6,9)[which(dd$YEAR=="")]
dd$YEAR[which(dd$YEAR=="")]=substr(dd$FLEDGE_DT,6,9)[which(dd$YEAR=="")]
dd[which(dd$YEAR=="2104"),]$YEAR="2014"

dd$LatLon=paste(dd$LATITUDE,dd$LONGITUDE,sep="_") # Make unique site code
dd$LatLonYear=paste(dd$LATITUDE,dd$LONGITUDE,dd$YEAR,sep="_") # Make unique site year code

LatLon=dd[match(unique(dd$LatLon),dd$LatLon),] # Build dataset of unique site years
LatLon=LatLon[,match(c("LatLon","LATITUDE","LONGITUDE"),colnames(LatLon))]

LatLonYear=dd[match(unique(dd$LatLonYear),dd$LatLonYear),] # Build dataset of unique site years
LatLonYear=LatLonYear[,match(c("LatLonYear","LATITUDE","LONGITUDE","YEAR"),colnames(LatLonYear))]

LatLonYear$YEAR_Update= LatLonYear$YEAR
LatLonYear$YEAR_Update[which(as.numeric(LatLonYear$YEAR_Update)>2018)]=2018 # Years past 2018 set to 2018
LatLonYear$YEAR_Update[which(as.numeric(LatLonYear$YEAR_Update)<2001)]=2001 # Years before 2001 set to 2001
LatLonYear$YEAR_Update[which(is.na(as.numeric(LatLonYear$YEAR_Update)==TRUE))]=round(mean(na.omit(as.numeric(LatLonYear$YEAR)))) # Sites without a year are set to the average year (2013)


####################
# Extract the data #
####################
	select<-dplyr::select
	projection <- raster::projection
	distinct<-dplyr::distinct
	# Need boundaries of continental US
	
	print("Formatting modis data....")
	if(!exists("US_Data")) {
	US_Data <- ne_download(scale = 50, category = "cultural",
	                              type = "countries",
	                              returnclass = "sf")%>%
	                               filter(GU_A3 %in% c("USA","CAN")) %>%                   
								  st_set_precision(1e6) %>%
								  summarize()%>%
								    st_transform(crs = paste("+proj=sinu +lon_0=0 +x_0=0 +y_0=0",
	                           "+a=6371007.181 +b=6371007.181 +units=m +no_defs"))
	}

	neighborhood_radius= 696
	landcover <- list.files("~/Google Drive/NestwatchProject/Data/modis", "^modis_mcd12q1_umd", full.names = TRUE)[1] %>% 
	  stack()

	  landcover <- names(landcover) %>% 
	  str_extract("(?<=modis_mcd12q1_umd_)[0-9]{4}") %>% 
	  paste0("y", .) %>% 
	  setNames(landcover, .)
	
	max_lc_year <- names(landcover) %>% 
	  str_extract("[0-9]{4}") %>% 
	  as.integer() %>% 
	  max()
	  
	  head(LatLonYear)

	  bird_buff <- LatLonYear %>% 
	  distinct(year = YEAR_Update,
	           LatLonYear, LATITUDE, LONGITUDE) %>% 
	  # for 2019 use 2018 landcover data
	  mutate(year_lc = if_else(as.integer(year) > max_lc_year, 
	                           as.character(max_lc_year), year),
	         year_lc = paste0("y", year_lc)) %>% 
	  # convert to spatial features
	  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% 
	  # transform to modis projection
	  st_transform(crs = projection(landcover)) %>% 
	  # buffer to create neighborhood around each point
	  st_buffer(dist = neighborhood_radius) %>% 
	  # nest by year
	  nest(data = c(year, LatLonYear, geometry))


	# function to extract landcover data for all checklists in a given year
	calculate_pland <- function(yr, regions, lc) {
	  # create a lookup table to get locality_id from row number
	  locs <- st_set_geometry(regions, NULL) %>% 
	    mutate(id = row_number())
	  
	  # extract using velox
	  lc_vlx <- velox(landcover[[yr]])
	  lc_vlx$extract(regions, df = TRUE) %>% 
	    # velox doesn't properly name columns, fix that
	    set_names(c("id", "landcover")) %>% 
	    # join to lookup table to get locality_id
	    inner_join(locs, ., by = "id") %>% 
	    select(-id)
	}
	
	
	lc_extract <- bird_buff %>% 
	  mutate(pland = map2(year_lc, data, calculate_pland, lc = landcover)) %>% 
	  select(pland) %>% 
	  unnest(cols = pland)

		
	pland <- lc_extract %>% 
	  # count landcovers
	  count(LatLonYear, year, landcover) %>% 
	  # calculate proporiton
	  group_by(LatLonYear, year) %>% 
	  mutate(pland = n / sum(n)) %>% 
	  ungroup() %>% 
	  select(-n) %>% 
	  # remove NAs after tallying so pland is relative to total number of cells
	  filter(!is.na(landcover))
	  
  pland <- pland %>% 
	  mutate(landcover = paste0("pland_", str_pad(landcover, 2, pad = "0"))) %>% 
	  pivot_wider(names_from = landcover, values_from = pland, values_fill = list(pland = 0))

ordPLAND=pland[match(dd$LatLonYear,pland$LatLonYear),]
colnames(ordPLAND)[3:18]=c("P_Water","P_Savannah","P_Urban","P_PermanentWetland","P_Grassland","P_OpenShrubland","P_Cropland","P_NonVegetated","P_CroplandNaturalMosaic","P_WoodySavannah","P_DeciduousBroadleafForest","P_EvergreenBroadleafForest","P_ClosedShrubland","P_MixedForest","P_EvergreenNeedleLeafForest","P_DeciduousNeedleleafForest")
dd=cbind(dd,ordPLAND[,3:18])

saveRDS(dd,'~/Google Drive/NestwatchProject/Data/Climate_Modis_LandUse_Nestwatch.RDS')
