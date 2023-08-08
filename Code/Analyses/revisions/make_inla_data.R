#!/usr/bin/env Rscript

##### Implement INLA with nestwatch data to check for sensitivity to spatial autocorrelation
##### Author: Katherine Lauck
##### Last updated: 20230221

library(tidyverse)
library(terra)

d <- read_rds("Data/active/success-cleaned.rds") %>% ungroup()

phen <- c("Region", "species","UnCoor","year","lon", "lat") # Base columns with spatial information we'll need

resp <- "at_least_one_success" # Response variable

covar <- c("Tmax_std_gridmet", # Temp anomaly
           "NewLU1", # Local land use (Ag, Forest, Natural_open, Human)
           "Tmax_std_gridmet_sq", # Temp anomaly squared
           "pcpbefore_raw_gridmet", # precipitation
           "NLCD_p_forest", # Landscape forest % cover
           "NLCD_p_human", # landscape human % cover
           "NLCD_p_ag", # landscape ag % cover
           "substrate_binary", # binary indication of whether in nestbox or not
           "laydate_scaled") # Julian date laydate

TestHosts <- na.omit(d[, c(phen, resp, covar)]) # Getting rid of NA's, picking adults
# We are using the [] to subset and only extract specific columns

TestHosts$UnCoor <- as.factor(TestHosts$UnCoor)

Locations <- data.frame(lon = TestHosts$lon,lat = TestHosts$lat)
loc <- vect(Locations,geom = c("lon","lat"),crs = "EPSG:4326")
proj_loc <- project(loc,"ESRI:102010")
newLocations <- as.data.frame(proj_loc,geom = "XY") %>% as.matrix()

new <- TestHosts %>% mutate(X = newLocations[,1],Y = newLocations[,2])

write_rds(new,'results/revisions/inla_data.rds')

