# How to investigate collapsing data and dealing with missing values
# Authors: Alison Ke and Katherine Lauck
# Last updated: 2/5/2021
# 
# To those writing code to append land use: scroll to end of script to find section. There, source appropriate land use data and copy/paste append code.
# 
# Commented code can be used for validation or exploration, but none of it is necessary to build the master dataset.
# 
# Structure:
#   1. Dependencies
#   2. lubridate dates
#   3. Explore data (all commented)
#   4. Update species names
#   5. Subset for testing
#   6. Collapse attempts (under construction)
#   7. Append land use data (under construction)
rm(list=ls())
#########################
##### 1. Dependencies
#########################
library(tidyverse)
library(lubridate)
library(rio)
library(foreach)
library(magrittr)
library(furrr)
library(stringr)
library(raster)
library(lme4)
library(scales)
library(geosphere)
library(sf)
library(gridExtra)
library(rnaturalearth)
#library(MODIS)
#library(velox)
library(viridis)


#setwd("~/Google Drive/NestwatchProject")
raw <- import('Data/archive/NestwatchData.csv')

str(raw)

#########################
##### 2. Transform FIRST_LAY_DT, HATCH_DT, FLEDGE_DT, and OBS_DT so that they can be handled numerically by R.
#########################
raw %<>%
  mutate_at(vars(FIRST_LAY_DT,HATCH_DT,FLEDGE_DT,OBS_DT),
            parse_date_time, orders = 'dbY:HMS') %>%
  mutate_at(vars(FIRST_LAY_DT,HATCH_DT,FLEDGE_DT,OBS_DT), as_date)
str(raw)

# ########################
# ##### 3. Explore data
# ########################
# Highlight some weird outcome and egg codes that we don't yet understand
# unique(filter(raw,OUTCOME_CODE_LIST == 'i')$EGGS_HOST_ATLEAST)
# raw %>%
#   filter(OUTCOME_CODE_LIST == 'i') %>%
#   filter(EGGS_HOST_ATLEAST > 0)

# ## search for mismatched years
# mismatched_years <- raw[which(year(raw$FIRST_LAY_DT) != year(raw$OBS_DT) |
#                                year(raw$FLEDGE_DT) != year(raw$OBS_DT) |
#                                year(raw$HATCH_DT) != year(raw$OBS_DT) |
#                                 year(raw$FIRST_LAY_DT) != year(raw$HATCH_DT) |
#                                 year(raw$FIRST_LAY_DT) != year(raw$FLEDGE_DT) |
#                                 year(raw$HATCH_DT) != year(raw$FLEDGE_DT)),]
# obs_dt_more_than_drived_dates<- mismatched_years[which(year(mismatched_years$OBS_DT) >
#                                               year(mismatched_years$FIRST_LAY_DT) |
#                                               year(mismatched_years$OBS_DT) >
#                                               year(mismatched_years$HATCH_DT) |
#                                               year(mismatched_years$OBS_DT) >
#                                               year(mismatched_years$FLEDGE_DT)),]
# 
# save(mismatched_years,file = './Data/year_mismatch.rdata')
# 
# proj_period_mismatch <- mismatched_years[which(as.numeric(str_extract(mismatched_years$PROJ_PERIOD_ID, pattern = '\\d{4}')) != year(mismatched_years$OBS_DT)),]
# 
# all_proj_period_mismatch <- raw[which(as.numeric(str_extract(raw$PROJ_PERIOD_ID, pattern = '\\d{4}')) != year(raw$OBS_DT)),]

# # explore which attempts are missing outcomes. all single - row records are missing outcomes - so these are likely nests with not enough info to assign outcome, or weren't really 'officially' considered nesting attempts
# filter(raw,ATTEMPT_ID == unique(filter(raw,OUTCOME_CODE_LIST == '')$ATTEMPT_ID)[1900])
# missing_length <- filter(raw,OUTCOME_CODE_LIST == '') %>%
#   group_by(ATTEMPT_ID) %>% # split into a list by attempt number
#   group_map(function(x,...){nrow(x)})
# which((missing_length > 1)==TRUE)
# 
# # which attempts are f3? u? common threads?
# # Notes: f3 outcomes don't always lose all of their young at once - sometimes only a few eggs at once (likely snake predation), sometimes all young dead.
# filter(raw,ATTEMPT_ID == unique(filter(raw,OUTCOME_CODE_LIST == 'f3')$ATTEMPT_ID)[10])
# filter(raw,ATTEMPT_ID == unique(filter(raw,OUTCOME_CODE_LIST == 'u')$ATTEMPT_ID)[16])
# u_length <- filter(raw,OUTCOME_CODE_LIST == 'u') %>%
#   group_by(ATTEMPT_ID) %>% # split into a list by attempt number
#   group_map(function(x,...){nrow(x)})
# which((u_length > 1)==TRUE)
# 
# nrow(filter(raw,ATTEMPT_ID == unique(filter(raw,OUTCOME_CODE_LIST == 'u1')$ATTEMPT_ID)[125]))
# u1_length <- filter(raw,OUTCOME_CODE_LIST == 'u1') %>%
#   group_by(ATTEMPT_ID) %>% # split into a list by attempt number
#   group_map(function(x,...){nrow(x[,1])})
# which((u1_length == 1)==TRUE)

########################
##### 4. Species name key
########################
# load('./Data/nestwatch_one_row_per_attempt_modis_worldclim_elevation.Rdata')
# 
# processed <- nestwatch_one_row_per_attempt_modis_worldclim_elevation

# pull spuh spp list
ebird <- import("Data/archive/BirdCodes.xlsx")

# translate species codes in raw
sp.key <- raw %>%
  pull(SPECIES_CODE) %>%
  bind_cols(.,ebird$`English name`[match(raw$SPECIES_CODE,ebird$`eBird species code 2019`)]) %>%
  distinct()
sp.key <- sp.key[order(sp.key$...1),]

spuh <- ebird %>%
  filter(category == 'spuh') %>%
  pull(`eBird species code 2019`)
slash <- ebird %>%
  filter(category == 'slash') %>%
  pull(`eBird species code 2019`)
hybrid <- ebird %>%
  filter(category == 'hybrid') %>%
  pull(`eBird species code 2019`)

# update_sp <- raw %>%
#   filter(!SPECIES_CODE %in% spuh,
#          !SPECIES_CODE %in% slash,
#          !SPECIES_CODE %in% hybrid) %>%
#   pull(SPECIES_CODE) %>%
#   bind_cols(nestwatch_code = .,ebird_name = ebird$`English name`[match(.,ebird$`eBird species code 2019`)]) %>%
#   distinct() %>%
#   arrange(nestwatch_code)# %>% # remove spp codes that are for spuh
#   #mutate(SPECIES_CODE = str_replace_all(SPECIES_CODE,pattern = '\\d+','')) %>% # remove digits
#   #filter(SPECIES_CODE != 'x' & SPECIES_CODE != 'y') # remove strange spp codes w/ x & y
# 
# write_csv(update_sp,"Data/sp-key.csv")

# Filter out spuh, slash, hybrid species

raw <- raw %>%
  filter(!SPECIES_CODE %in% spuh,
         !SPECIES_CODE %in% slash,
         !SPECIES_CODE %in% hybrid)

##### Update sp name in raw based on standardized common names
sp.key <- read_csv("Data/archive/sp-key-commonname.csv")
raw$SPECIES_CODE <- sp.key$ebird_name[match(raw$SPECIES_CODE,sp.key$nestwatch_code)]

sp.remove <- c("Black Baza", "Scarlet Macaw", "Warbling White-eye")

update_sp <- raw %>%
  filter(!SPECIES_CODE %in% sp.remove)

#write_csv(raw,"Data/nestwatch_update-sp.csv")

# Return names that are removed or replaced - could check against list of SPECIES_CODE in raw that matches removal criteria
removed_or_replaced <- unique(anti_join(tibble(raw$SPECIES_CODE), tibble(update_sp$SPECIES_CODE), by = c(`raw$SPECIES_CODE` = 'update_sp$SPECIES_CODE')))

########################
##### 5. Subset for bug testing 
########################
# 
# tester attempt; to be used to demonstrate collapse function as well

# attempt <- filter(update_sp,ATTEMPT_ID == "A1002842")
# attempt2 <- filter(update_sp,ATTEMPT_ID == "A1000066")
# attempt3 <- filter(update_sp,ATTEMPT_ID == "A1000069")
# attempt <- filter(update_sp,ATTEMPT_ID == update_sp$ATTEMPT_ID[1200])
attempt <- filter(update_sp,ATTEMPT_ID == "A1000676")
# update_sp$ATTEMPT_ID[which(is.na(update_sp$FIRST_LAY_DT) & update_sp$OUTCOME_CODE_LIST == "s")]
# na.date <- update_sp[which(is.na(update_sp$OBS.DT) & is.na(update_sp$FIRST_LAY_DATE)),]
# na.obs.date <- update_sp[which(is.na(update_sp$OBS.DT)),]

## subset data for bug testing
test <- update_sp[5000:10000,]
# unique(test$ATTEMPT_ID)[61]
# attempt <- filter(test,ATTEMPT_ID == "A1001879")

#########################
##### 6. Map collapse function (below) over raw and save processed
#########################

collapse <- function(attempt,...){ # input 
  ## intermediate variables
  
  attempt_id <- attempt$ATTEMPT_ID[1]
  loc_id <- attempt$LOC_ID[1]
  outcome <- attempt$OUTCOME_CODE_LIST[1]
  substrate <- attempt$SUBSTRATE_CODE[1]
  
  ## Update dates
  # Compare year of PROJ_PERIOD_ID, FIRST_LAY_DT, HATCH_DT, and FLEDGE_DT, and OBS_DT.
  # Throw out rows where:
  #   1. More than two years are represented
  #   2. More than one column differs from most common date
  # Paste most common year to disagreeing columns
  
  years <- c(as.numeric(str_extract(attempt$PROJ_PERIOD_ID[1],'\\d{4}')),
             year(attempt$FIRST_LAY_DT[1]),
             year(attempt$HATCH_DT[1]),
             year(attempt$FLEDGE_DT[1]),
             year(attempt$OBS_DT[1]))
  count_year <- tibble(years) %>% count(years)
  
  if (length(unique(years)[!is.na(unique(years))]) == 1) { # if there is only one year present, keep this row
    year <- unique(years)[!is.na(unique(years))]
  } else if (length(unique(years)[!is.na(unique(years))]) > 2) { # if there are more than two different years present, remove this row
    year <- NA
  } else if (!(1 %in% count_year[!is.na(count_year$years),'n'])) { # if the less common year has more than one occurance, remove this row
    year <- NA
  } else if (identical(count_year[!is.na(count_year$years),'n'])) { # if there are two potential years and they are both represented once, remove this row
    year <- NA
  } else if (1 %in% count_year[!is.na(count_year$years),'n']) { # Just to make sure - the less common year only occurs once. Keep this row and paste most common year to non-NA date columns. Leave PROJ_PERIOD_ID alone because it's not used in later calculations.
    
    final_year <- count_year %>%
      filter(n == max(count_year$n[!is.na(count_year$years)])) %>%
      pull(years)
    
    year(attempt$FIRST_LAY_DT) <- final_year
    year(attempt$HATCH_DT) <- final_year
    year(attempt$FLEDGE_DT) <- final_year
    year(attempt$OBS_DT) <- final_year
    
    year <- final_year
    
  } else {
    year <- NA
    } # any other random cases, remove them until we know what the deal is
  
  ## predictors
  
  ### Laydate
  if(!is.na(attempt$FIRST_LAY_DT[1])){ # if laydate is specified, use it
    laydate <- attempt$FIRST_LAY_DT[1]
  } else if(any(!is.na(attempt$OBS_DT))){ # if not, but there are included obs_dt, choose from two options: completed clutch = NA, but sometime in the future could count backwards from hatchdate based on sp (need these data first) - could also do this for laydate where obs_dt are crazy, count backwards from laydate if the obs_dt are reasonable
    x <- attempt %>%
      filter(EGGS_HOST_ATLEAST >= 1)
    if(nrow(x) <= 1) { # need at least two rows to determine if clutch is complete or not
      laydate <- NA
    } else if(identical(attempt$EGGS_HOST_ATLEAST[which.min(attempt$OBS_DT)], max(attempt$EGGS_HOST_ATLEAST,na.rm = TRUE))){ ## if the first observation has the max # of eggs, the clutch is complete and laydate is hard to determine accurately - in future could instead count backwards from hatchdate
      laydate <- NA
    } else if(identical(min(x$OBS_DT),min(attempt$OBS_DT))) { # if the first obs of x and first obs of attempt are the same, and above we have already filtered out max eggs
      laydate <- min(x$OBS_DT,na.rm = TRUE)-(x$EGGS_HOST_ATLEAST[which.min(x$OBS_DT)]-1)
    } else if (min(x$OBS_DT,na.rm = TRUE)-attempt$OBS_DT[which(attempt$OBS_DT == min(x$OBS_DT,na.rm = TRUE))-1] <= 10){ ## if the gap in obs_dt before & after eggs is reasonable, then count backward from first obs of eggs. Problem is, possibly is no pre-eggs observation. Issue here: could possibly have non-max @ first obs but no previous obs => can't find previous obs_dt. Solution: test for non-max eggs @first obs. If first obs = non-max eggs, can directly count backwards without validation. Otherwise, must check to see if gap in obs is reasonably small. Weirdly, it seems that there are 38 attempts with more than one observation per date. Need to find a way to look at those attempts.
      laydate <- min(x$OBS_DT,na.rm = TRUE)-(x$EGGS_HOST_ATLEAST[which.min(x$OBS_DT)]-1)
    } else {laydate <- NA} # otherwise give up - but this in the future could instead count backwards from hatchdate
  } else {laydate <- NA} # otherwise give up
  ### (lay date as listed or eggs first observed? *Eggs first observed minus number of eggs? Hatch date minus lay period per species? Could work back from hatch date if known)
  
  ### Hatch date
  if(!is.na(attempt$HATCH_DT[1])){
    hatchdate <- attempt$HATCH_DT[1]
  } else if(any(!is.na(attempt$YOUNG_HOST_LIVE_ATLEAST)) & !is.na(laydate)){
    x <- attempt %>%
      filter(YOUNG_HOST_LIVE_ATLEAST >= 1)
    hatchdate <- laydate + 14 # alternative: min(x$OBS_DT,na.rm = TRUE)
  } else {hatchdate <- NA}## hatch date (hatch date as indicated or young first observed? Lay date plus 14 days?)
  
  lat <- attempt$LATITUDE[1];lon <- attempt$LONGITUDE[1] ## lat & lon
  hab1 <- attempt$HABITAT_CODE_1[1] ## habitat 1
  hab2 <- attempt$HABITAT_CODE_2[1] ## habitat 2
  ele <- attempt$ELEVATION_M[1] ## elevation
  height <- attempt$HEIGHT_M[1] ## height
  enter_orientation <- attempt$ENTRANCE_ORIENTATION[1] ## orientation
  species <- attempt$SPECIES_CODE[1]## species
  
  ## response variables
  
  ### at least one success = success
  if(is.na(attempt$YOUNG_HOST_FLEDGED_ATLEAST[1])){
    at_least_one_success <- NA
  } else if(attempt$YOUNG_HOST_FLEDGED_ATLEAST[1] >= 1){
    at_least_one_success <- 1
  } else {at_least_one_success <- 0} 
  
  ### percent brood fledged
  if(all(is.na(attempt$EGGS_HOST_ATLEAST))){perc_fledged <- NA
  } else {perc_fledged <- attempt$YOUNG_HOST_FLEDGED_ATLEAST[1]/max(attempt$EGGS_HOST_ATLEAST,na.rm = TRUE)} 
  ### at least one failure = failure
  if(is.na(perc_fledged)) {
    at_least_one_failure <- NA
  } else if(perc_fledged < 1) {
    at_least_one_failure <- 1
  } else {at_least_one_failure <- 0}
  
  ## if failure, predation or not?
  if(attempt$OUTCOME_CODE_LIST[1] == c('f3')) {
    predation <- 1
  } else if(attempt$OUTCOME_CODE_LIST[1] %in% c('f','f1','f2','f4','f5','f6')) {
    predation <- 0
  } else {predation <- NA}
  
  ## max cowbird chicks -- NOTE if using this code - this value is buggy
  if(length(filter(attempt,YOUNG_PARASITE_LIVE_ATLEAST >= 1)) == 0){
    max_cowbird <- NA
  } else {
    max_cowbird <- max(attempt$YOUNG_PARASITE_LIVE_ATLEAST)
  }
  
  ## at least one cowbird chick -- NOTE if using this code - this value is buggy
  if(is.na(max_cowbird)){
    cowbird_lgl <- 0
  } else {
    cowbird_lgl <- 1
  }
  
  ## failed due to parasitism
  if(attempt$OUTCOME_CODE_LIST[1] == c('f4')) {
    fail_cowbird <- 1
  } else if(attempt$OUTCOME_CODE_LIST[1] %in% c('f','f1','f2','f3','f5','f6')) {
    fail_cowbird <- 0
  } else {fail_cowbird <- NA}
  
  ## number fledged
  total_young_fledged <- as.numeric(attempt$YOUNG_HOST_FLEDGED_ATLEAST[1])
  
  # - look at f3 outcome, unknown outcome, and missing outcomes and see how they match up
  # - predation as missing babies/eggs, or the whole nest missing?
  # - reclassify all or reclassify only unknowns or missing values?
  
  output <- tibble(attempt = attempt_id,
                   loc_id = loc_id,
                   species = species,
                   lat = lat,
                   lon = lon,
                   elevation = ele,
                   substrate = substrate,
                   height = height,
                   enter_orientation = enter_orientation,
                   habitat1 = hab1,
                   habitat2 = hab2,
                   outcome = outcome,
                   year = year,
                   laydate = laydate,
                   hatchdate = hatchdate,
                   total_young_fledged = total_young_fledged,
                   perc_fledged = perc_fledged,
                   predation = predation,
                   at_least_one_failure = at_least_one_failure,
                   at_least_one_success = at_least_one_success,
                   max_cowbird = max_cowbird,
                   cowbird_lgl = cowbird_lgl,
                   fail_cowbird = fail_cowbird)# combine predictors and response variables into a vector
  
  return(output)
}

plan(multisession)

test <- filter(update_sp,YOUNG_PARASITE_LIVE_ATLEAST > 0)

proc.test <- test %>%
  group_split(ATTEMPT_ID) %>% # split into a list by attempt number
  future_map_dfr(collapse)  %>% # map over each element of the list
  filter(!is.na(year)) # remove rows with conflicting dates
str(proc.test)
length(unique(test$ATTEMPT_ID))
sum(year(proc.test$laydate) != year(proc.test$hatchdate),na.rm = TRUE)


system.time(
  processed <- update_sp %>%
    group_split(ATTEMPT_ID) %>% # split into a list by attempt number
    future_map_dfr(collapse) %>% # map over each element of the list
    filter(!is.na(year)) # remove rows with conflicting or missing years
  ) 
#save(processed, file='./Data/oneAttemptOneRow.Rdata')
names(processed)

#####################
# 7. Add in WorldClim Data 
#####################
# load('~/Google Drive/NestwatchProject/Data/oneAttemptOneRow.rdata')
# names(processed)
# climGenerator=function(months,climatetype,climate_directory,method,locations){
#   setwd(climate_directory)
#   climate_files=list.files(climate_directory)
#   climate_files=climate_files[grep(climatetype,climate_files)]
#   
#   nsites=dim(locations@data)[1]
#   climates=array(dim=c(nsites,0))
#   
#   for (i in 1:length(months)){
#     clim_month_raster=raster(climate_files[grep(months[i],climate_files)])
#     climates=cbind(climates,extract(clim_month_raster,locations))
#   }
#   
#   if(method=="mean"){
#     climates_summarized=apply(climates,1,mean)
#   }
#   if(method=="sum"){
#     climates_summarized=apply(climates,1,sum)
#   }
#   if(method=="max"){
#     climates_summarized=apply(climates,1,max)
#   }
#   if(method=="min"){
#     climates_summarized=apply(climates,1,min)
#   }
#   climates_summarized
# }
# 
processed$UnCoor=paste(processed$lon, processed$lat,sep="_") # Idenitfy unique coordinates 
# 
# LatLon= processed[match(unique(processed $UnCoor), processed $UnCoor),] # Build dataset of unique sites
# LatLon=LatLon[,match(c("UnCoor","lat","lon"),colnames(LatLon))]
# 
# # Extract the climate data (long term averages from WorldClim)#
# months=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12")
# climate_directory="~/Google Drive/NestwatchProject/Data/Worldclim/"
# locations=SpatialPointsDataFrame(coords= cbind(LatLon$lon,LatLon$lat),data=LatLon,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# 
# # Need to iteratively toggle points that happen to fall outside of land climate rasters
# Climate=climGenerator(months,climatetype="pre",climate_directory,method="mean",locations) # Calculate climate for all sites
# NoSites=LatLon[which(is.na(Climate)==TRUE),] # identify sites that cant have climate calculated
# 
# NoSites$lat=NoSites$lat+.25 # Try moving the sites .25 degrees north
# locations_update=SpatialPointsDataFrame(coords= cbind(NoSites$lon,NoSites$lat),data=NoSites,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# NoSites_Climate=climGenerator(months,climatetype="pre",climate_directory,method="mean",locations_update) # recalculate climate 
# length(which(is.na(NoSites_Climate)==TRUE)) # 510 sites moved .25 degrees north
# 
# NoSites[which(is.na(NoSites_Climate)==TRUE),]$lon=NoSites[which(is.na(NoSites_Climate)==TRUE),]$lon-.25 # Try moving remaining sites .25 degrees west
# locations_update=SpatialPointsDataFrame(coords= cbind(NoSites$lon,NoSites$lat),data=NoSites,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# NoSites_Climate=climGenerator(months,climatetype="pre",climate_directory,method="mean",locations_update) # recalculate climate 
# length(which(is.na(NoSites_Climate)==TRUE)) # 48 sites moved .25 degrees west
# 
# NoSites[which(is.na(NoSites_Climate)==TRUE),]$lon=NoSites[which(is.na(NoSites_Climate)==TRUE),]$lon-.25 # Try moving remaining sites .25 degrees more west
# locations_update=SpatialPointsDataFrame(coords= cbind(NoSites$lon,NoSites$lat),data=NoSites,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# NoSites_Climate=climGenerator(months,climatetype="pre",climate_directory,method="mean",locations_update) # recalculate climate 
# length(which(is.na(NoSites_Climate)==TRUE)) # 29 sites moved .5 degrees west
# 
# # Replace with the updated lat and lons
# LatLon$Update_Lat=LatLon$lat
# LatLon$Update_Lon=LatLon$lon
# 
# LatLon[match(NoSites$UnCoor,LatLon$UnCoor),]$Update_Lat=NoSites$lat
# LatLon[match(NoSites$UnCoor,LatLon$UnCoor),]$Update_Lon=NoSites$lon
# 
# # Now rerun climate for all the values we care about. 
# locations=SpatialPointsDataFrame(coords= cbind(LatLon$Update_Lon,LatLon$Update_Lat),data=LatLon,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
# months=c("_01","_02","_03","_04","_05","_06","_07","_08","_09","_10","_11","_12")
# 
# LatLon$WorldClim_TotalAnnualPrecip=climGenerator(months,climatetype="pre",climate_directory,method="sum",locations) 
# LatLon$WorldClim_AveAnnualTemp=climGenerator(months,climatetype="tavg",climate_directory,method="mean",locations) 
# LatLon$WorldClim_MaxAnnualTemp=climGenerator(months,climatetype="tmax",climate_directory,method="max",locations) 
# LatLon$WorldClim_MinAnnualTemp=climGenerator(months,climatetype="tmin",climate_directory,method="min",locations) 
# 
# # Choose breeding months to be march through august 
# months=c("_03","_04","_05","_06","_07","_08")
# LatLon$WorldClim_TotalBreedPrecip=climGenerator(months,climatetype="pre",climate_directory,method="sum",locations) 
# LatLon$WorldClim_AveBreedTemp=climGenerator(months,climatetype="tavg",climate_directory,method="mean",locations) 
# LatLon$WorldClim_MaxBreedTemp=climGenerator(months,climatetype="tmax",climate_directory,method="max",locations) 
# LatLon$WorldClim_MinBreedTemp=climGenerator(months,climatetype="tmin",climate_directory,method="min",locations) 
# 
# # Now recombine with the original dataset
# LatLon[which(is.na(LatLon$WorldClim_MinAnnualTemp)==TRUE),c(4,5)]=NA #Change sites that couldnt be toggled back to NA. 
# 
# processed =cbind(processed,LatLon[match(processed$UnCoor,LatLon$UnCoor),4:13])
# 
# head(processed)
# #####################
# # 8. Add in Modis Data 
# #####################
# processed$LatLonYear=paste(processed $lat, processed $lon, processed $year,sep="_") # Make unique site year code
# 
# LatLonYear= processed[match(unique(processed $LatLonYear), processed $LatLonYear),] # Build dataset of unique site years
# LatLonYear=LatLonYear[,match(c("LatLonYear","lat","lon","year"),colnames(LatLonYear))]
# colnames(LatLonYear)[4]="Year"
# LatLonYear$Year=as.character(LatLonYear$Year)
# LatLonYear$YEAR_Update= LatLonYear$Year
# LatLonYear$YEAR_Update[which(as.numeric(LatLonYear$YEAR_Update)>2018)]=2018 # Years past 2018 set to 2018
# LatLonYear$YEAR_Update[which(as.numeric(LatLonYear$YEAR_Update)<2001)]=2001 # Years before 2001 set to 2001
# LatLonYear$YEAR_Update[which(is.na(as.numeric(LatLonYear$YEAR_Update)==TRUE))]=round(mean(na.omit(as.numeric(LatLonYear$Year)))) # Sites without a year are 
# 
# #extract the land use data
# 	select<-dplyr::select
# 	projection <- raster::projection
# 	distinct<-dplyr::distinct
# 	# Need boundaries of continental US
# 	
# 	print("Formatting modis data....")
# 	if(!exists("US_Data")) {
# 	US_Data <- ne_download(scale = 50, category = "cultural",
# 	                              type = "countries",
# 	                              returnclass = "sf")%>%
# 	                               filter(GU_A3 %in% c("USA","CAN")) %>%                   
# 								  st_set_precision(1e6) %>%
# 								  summarize()%>%
# 								    st_transform(crs = paste("+proj=sinu +lon_0=0 +x_0=0 +y_0=0",
# 	                           "+a=6371007.181 +b=6371007.181 +units=m +no_defs"))
# 	}
# 
# 	neighborhood_radius= 696
# 	landcover <- list.files("~/Google Drive/NestwatchProject/Data/modis", "^modis_mcd12q1_umd", full.names = TRUE) %>% 
# 	  stack()
# 3*ceiling(max(res(landcover)))/2
# 	  landcover <- names(landcover) %>% 
# 	  str_extract("(?<=modis_mcd12q1_umd_)[0-9]{4}") %>% 
# 	  paste0("y", .) %>% 
# 	  setNames(landcover, .)
# 	
# 	max_lc_year <- names(landcover) %>% 
# 	  str_extract("[0-9]{4}") %>% 
# 	  as.integer() %>% 
# 	  max()
# 	  
# 
# 	  bird_buff <- LatLonYear %>% 
# 	  distinct(year = YEAR_Update,
# 	           LatLonYear, lat, lon) %>% 
# 	  # for 2019 use 2018 landcover data
# 	  mutate(year_lc = if_else(as.integer(year) > max_lc_year, 
# 	                           as.character(max_lc_year), year),
# 	         year_lc = paste0("y", year_lc)) %>% 
# 	  # convert to spatial features
# 	  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
# 	  # transform to modis projection
# 	  st_transform(crs = projection(landcover)) %>% 
# 	  # buffer to create neighborhood around each point
# 	  st_buffer(dist = neighborhood_radius) %>% 
# 	  # nest by year
# 	  nest(data = c(year, LatLonYear, geometry))
# 
# 
# 	# function to extract landcover data for all checklists in a given year
# 	calculate_pland <- function(yr, regions, lc) {
# 	  # create a lookup table to get locality_id from row number
# 	  locs <- st_set_geometry(regions, NULL) %>% 
# 	    mutate(id = row_number())
# 	  
# 	  # extract using velox
# 	  lc_vlx <- velox(landcover[[yr]])
# 	  lc_vlx$extract(regions, df = TRUE) %>% 
# 	    # velox doesn't properly name columns, fix that
# 	    set_names(c("id", "landcover")) %>% 
# 	    # join to lookup table to get locality_id
# 	    inner_join(locs, ., by = "id") %>% 
# 	    select(-id)
# 	}
# 	
# 	
# 	lc_extract <- bird_buff %>% 
# 	  mutate(pland = map2(year_lc, data, calculate_pland, lc = landcover)) %>% 
# 	  select(pland) %>% 
# 	  unnest(cols = pland)
# 
# 		
# 	pland <- lc_extract %>% 
# 	  # count landcovers
# 	  count(LatLonYear, year, landcover) %>% 
# 	  # calculate proporiton
# 	  group_by(LatLonYear, year) %>% 
# 	  mutate(pland = n / sum(n)) %>% 
# 	  ungroup() %>% 
# 	  select(-n) %>% 
# 	  # remove NAs after tallying so pland is relative to total number of cells
# 	  filter(!is.na(landcover))
# 	  
#   pland <- pland %>% 
# 	  mutate(landcover = paste0("pland_", str_pad(landcover, 2, pad = "0"))) %>% 
# 	  pivot_wider(names_from = landcover, values_from = pland, values_fill = list(pland = 0))
# 
# ordPLAND=pland[match(processed$LatLonYear,pland$LatLonYear),]
# colnames(ordPLAND)[3:18]=c("MODIS_P_Water","MODIS_P_Savannah","MODIS_P_Urban","MODIS_P_PermanentWetland","MODIS_P_Grassland","MODIS_P_OpenShrubland","MODIS_P_Cropland","MODIS_P_NonVegetated","MODIS_P_CroplandNaturalMosaic","MODIS_P_WoodySavannah","MODIS_P_DeciduousBroadleafForest","MODIS_P_EvergreenBroadleafForest","MODIS_P_ClosedShrubland","MODIS_P_MixedForest","MODIS_P_EvergreenNeedleLeafForest","MODIS_P_DeciduousNeedleleafForest")
# 
# processed=cbind(processed,ordPLAND[,3:18])

#####################
# 9. Add in Elevation 
#####################
elev=raster("Data/Elevation/elevation_1KMmd_GMTEDmd.tif")
locations=SpatialPointsDataFrame(coords= cbind(processed $lon, processed $lat),data= processed,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
Amatulli_Elevation=extract(elev,locations)
processed =cbind(processed,Amatulli_Elevation)

#save(processed, file='~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevation.RData')

########################
##### 10. Append land use data to collapsed dataset by loc ID or lat/long?
########################

#load('~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevation.RData')
NLCD <- read.csv("Data/NLCD/NLCD_prop.csv")
NLCD$LOC_ID_year=paste(NLCD$LOC_ID,NLCD$year)
NLCD<- NLCD[,c(6:72)]

#Omit sites where the total area is not correct (these are sites on the border with Canada)
NLCD=NLCD[which(NLCD$Total_2km>13850),]
processed$LOC_ID_year <- as.factor(paste(processed$loc_id,processed$year))
processed=cbind(processed,NLCD[match(processed$LOC_ID_year,NLCD$LOC_ID_year),1:66])

#save(processed, file='~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCD.RData')

##############################################
# 11. Add in spatial autocorrelation hexgrids#
##############################################
#load('~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCD.RData')

library(dggridR)
dggs_250 <- dgconstruct(spacing = 250) # generate hexagonal grid with ~  km betweeen cells
dggs_200 <- dgconstruct(spacing = 200) # generate hexagonal grid with ~  km betweeen cells

processed$Region200=as.character(dgGEO_to_SEQNUM(dggs_200, processed$lon, processed$lat)$seqnum)
processed$Region250=as.character(dgGEO_to_SEQNUM(dggs_250, processed$lon, processed$lat)$seqnum)

#save(processed, file='~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCD.RData')

#####################
# 12. Add in DayMet #
#####################
# load('~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCD.RData')
# load("~/Google Drive/NestwatchProject/Data/DayMetData.RData")
# OrdDayMet=DayMetData[match(processed$attempt,DayMetData$AttemptID),]
# processed=cbind(processed,OrdDayMet[,3:28])
# 
# save(processed, file='~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet.RData')

###############################################
##Add Rahel Sollman´s spatial autocorrelation##
###############################################

# load('~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet.RData')
# 
# sollman_spaut_failure <- read.csv('~/Google Drive/NestwatchProject/Data/sollman_spaut_failure.csv')
# sollman_spaut_success <- read.csv('~/Google Drive/NestwatchProject/Data/sollman_spaut_success.csv')
# 
# sollman_spaut_failure <- sollman_spaut_failure[,-c(1,3,4)]
# names(sollman_spaut_failure)[2] <- c("sollman_spaut_failure")
# 
# sollman_spaut_success <- sollman_spaut_success[,-c(1,3,4)]
# names(sollman_spaut_success)[2] <- c("sollman_spaut_success")
# 
# processed$sollman_spaut_failure =sollman_spaut_failure [match(processed$UnCoor, sollman_spaut_failure$UnCoor),2]
# processed$sollman_spaut_success = sollman_spaut_success [match(processed$UnCoor, sollman_spaut_success $UnCoor),2]
# 
# save(processed, file='~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet.RData')

###############################################
##Add extra DayMet columns##
###############################################
# 
# load("~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet.RData")
# 
# nest <- processed %>% 
#   mutate(mean_nestpd_temp = noanomoly_max_tmax_Next45-mean_anamoly_max_tmax_Next45) %>%
#   group_by(species) %>%
#   mutate(z_nestpd_temp = (mean_nestpd_temp-mean(mean_nestpd_temp,na.rm = TRUE))/sd(mean_nestpd_temp,na.rm = TRUE)) %>%
#   ungroup()
# 
# save(nest,file = "~/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet_meanNestpdTemp.RData")

###############################################
##Add gridMET columns##
###############################################

gridmetmax <- read_csv("Data/gridMEToutput_v1_tmaxafter-prcpbefore.csv") %>%
  dplyr::select(attempt,Tmax_raw:PcpBefore_std) %>%
  right_join(y = processed,by=c("attempt"="attempt")) %>%
  mutate(Tmax_raw = (Tmax_raw*.1 + 220)-273.15) %>%
  mutate(Tmax_anom = (Tmax_anom*.1 + 220)-273.15) %>%
  mutate(Tmeanmax = Tmax_raw-Tmax_anom) %>%
  mutate(PcpBefore_raw = PcpBefore_raw/10) %>%
  group_by(species) %>%
  mutate(Tstdmax_sp = (Tmeanmax-mean(Tmeanmax,na.rm = TRUE))/sd(Tmeanmax,na.rm = TRUE)) %>%
  ungroup()

gridmetmin <- read_csv("Data/gridMEToutput_v1_tminafter-prcpafter.csv") %>%
  dplyr::select(attempt,Tmin_raw:Pcp45dAfter_std) %>%
  right_join(y = gridmetmax,by=c("attempt"="attempt")) %>%
  mutate(Tmin_raw = (Tmin_raw*.1 + 220)-273.15) %>%
  mutate(Tmin_anom = (Tmin_anom*.1 + 220)-273.15) %>%
  mutate(Tmeanmin = Tmin_raw-Tmin_anom) %>%
  mutate(Pcp45dAfter_raw = Pcp45dAfter_raw/10) %>%
  group_by(species) %>%
  mutate(Tstdmin_sp = (Tmeanmin-mean(Tmeanmin,na.rm = TRUE))/sd(Tmeanmin,na.rm = TRUE)) %>%
  ungroup()

nest <- gridmetmin

save(nest,file='Data/raw-collapsed.RData')
