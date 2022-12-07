# How to investigate collapsing data and dealing with missing values
# Authors: Alison Ke and Katherine Lauck
# Last updated: 9/30/2022
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

########################
##### 3. Species name key
########################

# pull spuh spp list
ebird <- import("Data/archive/BirdCodes.xlsx")

# create vectors of groups of species for removal from raw data
spuh <- ebird %>% 
  filter(category == 'spuh') %>% # codes for birds not identifiable to species
  pull(`eBird species code 2019`)
slash <- ebird %>%
  filter(category == 'slash') %>% # codes for pairs of non-separable species
  pull(`eBird species code 2019`)
hybrid <- ebird %>%
  filter(category == 'hybrid') %>% # codes for identifiable hybrids
  pull(`eBird species code 2019`)

# remove spuh, slash, hybrid species from database
raw <- raw %>% 
  filter(!SPECIES_CODE %in% spuh,
         !SPECIES_CODE %in% slash,
         !SPECIES_CODE %in% hybrid)

##### Update sp name in raw based on standardized common names
sp.key <- read_csv("Data/archive/sp-key-commonname.csv")
raw$SPECIES_CODE <- sp.key$ebird_name[match(raw$SPECIES_CODE,sp.key$nestwatch_code)]

# Remove records of impossible nests
sp.remove <- c("Black Baza", "Scarlet Macaw", "Warbling White-eye")
update_sp <- raw %>%
  filter(!SPECIES_CODE %in% sp.remove)

#########################
##### 4. Map collapse function (below) over raw and save processed
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
  #   1. More than two years are represented OR
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
    } else if(identical(attempt$EGGS_HOST_ATLEAST[which.min(attempt$OBS_DT)], max(attempt$EGGS_HOST_ATLEAST,na.rm = TRUE))){ ## if the first observation has the max # of eggs, the clutch is complete and laydate is hard to determine accurately
      laydate <- NA
    } else if(identical(min(x$OBS_DT),min(attempt$OBS_DT))) { # if the first obs of eggs and first obs of attempt are the same, and above we have already filtered out max eggs
      laydate <- min(x$OBS_DT,na.rm = TRUE)-(x$EGGS_HOST_ATLEAST[which.min(x$OBS_DT)]-1) # count backwards number days equal to first observation number of eggs. Assumes one egg laid per day
    } else if (min(x$OBS_DT,na.rm = TRUE)-attempt$OBS_DT[which(attempt$OBS_DT == min(x$OBS_DT,na.rm = TRUE))-1] <= 10){ ## if the gap in obs_dt before & after eggs is reasonable (<= 10 days), then count backward from first obs of eggs.
      laydate <- min(x$OBS_DT,na.rm = TRUE)-(x$EGGS_HOST_ATLEAST[which.min(x$OBS_DT)]-1)
    } else {laydate <- NA} # otherwise give up
  } else {laydate <- NA} # otherwise give up
  
  ### Hatch date
  if(!is.na(attempt$HATCH_DT[1])){ # if there is a hatch date, use it
    hatchdate <- attempt$HATCH_DT[1]
  } else if(any(!is.na(attempt$YOUNG_HOST_LIVE_ATLEAST)) & !is.na(laydate)){ # if there are young, assume they hatched after 14 days. Not necessarily correct. If you intend to use hatch date in analysis, need to find a better way to estimate
    x <- attempt %>%
      filter(YOUNG_HOST_LIVE_ATLEAST >= 1)
    hatchdate <- laydate + 14 # alternative: min(x$OBS_DT,na.rm = TRUE)
  } else {hatchdate <- NA} # Else give up
  
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
  
  ## max cowbird chicks
  if(max(attempt$YOUNG_PARASITE_LIVE_ATLEAST,na.rm = TRUE) > 0){
    max_cowbird <- max(attempt$YOUNG_PARASITE_LIVE_ATLEAST,na.rm = TRUE)
  } else {
    max_cowbird <- 0
  }
  
  ## at least one cowbird chick
  if(max_cowbird > 0){
    cowbird_lgl <- 1
  } else {
    cowbird_lgl <- 0
  }
  
  ## failed due to parasitism
  if(attempt$OUTCOME_CODE_LIST[1] == c('f4')) {
    fail_cowbird <- 1
  } else if(attempt$OUTCOME_CODE_LIST[1] %in% c('f','f1','f2','f3','f5','f6')) {
    fail_cowbird <- 0
  } else {fail_cowbird <- NA}
  
  ## number fledged
  total_young_fledged <- as.numeric(attempt$YOUNG_HOST_FLEDGED_ATLEAST[1])
  
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

system.time(
  processed <- update_sp %>%
    group_split(ATTEMPT_ID) %>% # split into a list by attempt number
    future_map_dfr(collapse) %>% # map over each element of the list
    filter(!is.na(year)) # remove rows with conflicting or missing years
) 

names(processed)

#####################
# 5. Add in Elevation 
#####################

processed$UnCoor=paste(processed$lon, processed$lat,sep="_") # Identify unique coordinates for later spatial autocorrelation corrections

elev=raster("Data/archive/Elevation/elevation_1KMmd_GMTEDmd.tif")
locations=SpatialPointsDataFrame(coords= cbind(processed $lon, processed $lat),data= processed,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
Amatulli_Elevation=extract(elev,locations)
processed =cbind(processed,Amatulli_Elevation)

########################
##### 6. Append landscape context data to collapsed dataset by loc ID
########################

NLCD <- read.csv("Data/archive/NLCD/NLCD_prop.csv")
NLCD$LOC_ID_year=paste(NLCD$LOC_ID,NLCD$year)
NLCD<- NLCD[,c(6:72)]

#Omit sites where the total area is not correct (these are sites on the border with Canada)
NLCD=NLCD[which(NLCD$Total_2km>13850),] # 13850 is the number of 30m^2 pixels in 2km radius, such that 99% of area in circle is defined. This line drops all sites that don't have 99% coverage of NLCD data within 2km radius circle.
processed$LOC_ID_year <- as.factor(paste(processed$loc_id,processed$year))
processed=cbind(processed,NLCD[match(processed$LOC_ID_year,NLCD$LOC_ID_year),1:66])

##############################################
# 7. Add in spatial autocorrelation hexgrids#
##############################################

library(dggridR)
dggs_350 <- dgconstruct(spacing = 350)
dggs_300 <- dgconstruct(spacing = 300)
dggs_250 <- dgconstruct(spacing = 250) # generate hexagonal grid with ~  km betweeen cells
dggs_200 <- dgconstruct(spacing = 200) # generate hexagonal grid with ~  km betweeen cells
dggs_150 <- dgconstruct(spacing = 150)
dggs_100 <- dgconstruct(spacing = 100)
dggs_50 <- dgconstruct(spacing = 50)

processed$Region350=as.character(dgGEO_to_SEQNUM(dggs_350, processed$lon, processed$lat)$seqnum)
processed$Region300=as.character(dgGEO_to_SEQNUM(dggs_300, processed$lon, processed$lat)$seqnum)
processed$Region200=as.character(dgGEO_to_SEQNUM(dggs_200, processed$lon, processed$lat)$seqnum)
processed$Region250=as.character(dgGEO_to_SEQNUM(dggs_250, processed$lon, processed$lat)$seqnum)
processed$Region150=as.character(dgGEO_to_SEQNUM(dggs_150, processed$lon, processed$lat)$seqnum)
processed$Region100=as.character(dgGEO_to_SEQNUM(dggs_100, processed$lon, processed$lat)$seqnum)
processed$Region50=as.character(dgGEO_to_SEQNUM(dggs_50, processed$lon, processed$lat)$seqnum)

###############################################
## 8. Add gridMET columns##
###############################################

gridmetmax <- read_csv("Data/archive/gridMEToutput_v1_tmaxafter-prcpbefore.csv") %>%
  dplyr::select(attempt,Tmax_raw:PcpBefore_std) %>%
  right_join(y = processed,by=c("attempt"="attempt")) %>%
  mutate(Tmax_raw = (Tmax_raw*.1 + 220)-273.15) %>%
  mutate(Tmax_anom = (Tmax_anom*.1 + 220)-273.15) %>%
  mutate(Tmeanmax = Tmax_raw-Tmax_anom) %>%
  mutate(PcpBefore_raw = PcpBefore_raw/10) %>%
  group_by(species) %>%
  mutate(Tstdmax_sp = (Tmeanmax-mean(Tmeanmax,na.rm = TRUE))/sd(Tmeanmax,na.rm = TRUE)) %>%
  ungroup()

gridmetmin <- read_csv("Data/archive/gridMEToutput_v1_tminafter-prcpafter.csv") %>%
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

###############################################
## 8. Add Sollman predictor##
###############################################

sollman <- read_csv("Data/archive/sollman_spaut_success.csv") %>% dplyr::select(UnCoor,prop_success)

nest <- nest %>% left_join(y = sollman,by = "UnCoor")

is.na(nest$prop_success) %>% sum()


save(nest,file='Data/archive/raw-collapsed.RData')
