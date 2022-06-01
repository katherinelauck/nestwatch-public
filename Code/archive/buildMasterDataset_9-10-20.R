# How to investigate collapsing data and dealing with missing values
# Authors: Alison Ke and Katherine Lauck
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

setwd("~/Google Drive/NestwatchProject")
raw <- import('Data/NestwatchData.csv')

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
# Plan after a quick look at Danny's email + Robin's answers: remove names starting with x and y when they have multiple numbers after them, then remove all remaining numbers. Some names to look closely at (that I don't know how to handle): rxyfli, whip-p1

# Once land use data has been finalized, add line to source those changes if building dataset from scratch

# load('./Data/nestwatch_one_row_per_attempt_modis_worldclim_elevation.Rdata')
# 
# processed <- nestwatch_one_row_per_attempt_modis_worldclim_elevation

# pull spuh spp list
ebird <- import("./Data/BirdCodes.xlsx")
spuh <- ebird %>%
  filter(category == 'spuh') %>%
  pull(`eBird species code 2019`)

update_sp <- raw %>%
  filter(!SPECIES_CODE %in% spuh) %>% # remove spp codes that are for spuh
  mutate(SPECIES_CODE = str_replace_all(SPECIES_CODE,pattern = '\\d+','')) %>% # remove digits
  filter(SPECIES_CODE != 'x' & SPECIES_CODE != 'y') # remove strange spp codes w/ x & y

# ## For exploration purposes: return codes from eBird to translate codes that all have numbers in the eBird key
# sp.code <- pull(ebird,`eBird species code 2019`)
# colnames(sp.code) <- 'species'
# possible_removal <- unique(anti_join(tibble(processed$species),tibble(sp.code),by = c('processed$species' = 'sp.code')))
# colnames(possible_removal) <- 'species'
# 
# map(pull(possible_removal,var = 'species'),grep,x = sp.code,value = TRUE)

# Update easowl
update_sp$SPECIES_CODE[which(update_sp$SPECIES_CODE == 'easowl')] <- 'eassco'

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
    rm_row <- FALSE
  } else if (length(unique(years)[!is.na(unique(years))]) > 2) { # if there are more than two different years present, remove this row
    rm_row <- TRUE
  } else if (!(1 %in% count_year[!is.na(count_year$years),'n'])) { # if the less common year has more than one occurance, remove this row
    rm_row <- TRUE
  } else if (identical(count_year[!is.na(count_year$years),'n'])) { # if there are two potential years and they are both represented once, remove this row
    rm_row <- TRUE
  } else if (1 %in% count_year[!is.na(count_year$years),'n']) { # Just to make sure - the less common year only occurs once. Keep this row and paste most common year to non-NA date columns. Leave PROJ_PERIOD_ID alone because it's not used in later calculations.
    rm_row <- FALSE
    
    final_year <- count_year %>%
      filter(n == max(count_year$n[!is.na(count_year$years)])) %>%
      pull(years)
    
    year(attempt$FIRST_LAY_DT) <- final_year
    year(attempt$HATCH_DT) <- final_year
    year(attempt$FLEDGE_DT) <- final_year
    year(attempt$OBS_DT) <- final_year
    
  } else {rm_row <- TRUE} # any other random cases, remove them until we know what the deal is
  
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
  ### (lay date as listed or eggs first observed? *Eggs first observed minus number of eggs? Hatch date minus lay period per species? Add complication - if clutch already completed, should be NA because we don't really know, also could work back from hatch date if known)
  
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
  
  ## max cowbird chicks
  if(length(filter(attempt,YOUNG_PARASITE_LIVE_ATLEAST >= 1)) == 0){
    max_cowbird <- NA
  } else {
    max_cowbird <- max(attempt$YOUNG_PARASITE_LIVE_ATLEAST)
  }
  
  ## at least one cowbird chick
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
  
  output <- tibble(rm_row = rm_row,
                   attempt = attempt_id,
                   loc_id = loc_id,
                   species = species,
                   lat = lat,
                   lon = lon,
                   elevation = ele,
                   height = height,
                   enter_orientation = enter_orientation,
                   habitat1 = hab1,
                   habitat2 = hab2,
                   outcome = outcome,
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

plan(multiprocess)

# proc.test <- test %>%
#   group_split(ATTEMPT_ID) %>% # split into a list by attempt number
#   future_map_dfr(collapse)  %>% # map over each element of the list
#   filter(rm_row == FALSE) %>% # remove rows with conflicting dates
#   select(-rm_row) # remove rm_row column# map over each element of the list
# str(proc.test)
# length(unique(test$ATTEMPT_ID))
# sum(year(proc.test$laydate) != year(proc.test$hatchdate),na.rm = TRUE)

processed <- update_sp %>%
  group_split(ATTEMPT_ID) %>% # split into a list by attempt number
  future_map_dfr(collapse) %>% # map over each element of the list
  filter(rm_row == FALSE) %>% # remove rows with conflicting dates
  select(-rm_row) # remove rm_row column
save(processed, file='./Data/oneAttemptOneRow.rdata')

########################
##### 7. Append land use data to collapsed dataset by loc ID or lat/long?
########################
# Under construction

# save(master, file='./Data/master.rdata')