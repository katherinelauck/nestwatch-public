###### Fix species' names
###### Author: Katherine Lauck
###### Last updated: 18 November 2020

# dependencies
library(tidyverse)

raw <- import('Data/NestwatchData.csv')

# ##### Generate key for raw dataset
# ebird <- import("./Data/BirdCodes.xlsx")
# 
# # translate species codes in raw
# sp.key <- raw %>% 
#   pull(SPECIES_CODE) %>%
#   bind_cols(.,ebird$`English name`[match(raw$SPECIES_CODE,ebird$`eBird species code 2019`)]) %>%
#   distinct()
# sp.key <- sp.key[order(sp.key$...1),]
# 
# spuh <- ebird %>%
#   filter(category == 'spuh') %>%
#   pull(`eBird species code 2019`)
# slash <- ebird %>%
#   filter(category == 'slash') %>%
#   pull(`eBird species code 2019`)
# hybrid <- ebird %>%
#   filter(category == 'hybrid') %>%
#   pull(`eBird species code 2019`)
# 
# update_sp <- raw %>%
#   filter(!SPECIES_CODE %in% spuh,
#          !SPECIES_CODE %in% slash,
#          !SPECIES_CODE %in% hybrid) %>%
#   pull(SPECIES_CODE) %>%
#   bind_cols(nestwatch_code = .,ebird_name = ebird$`English name`[match(.,ebird$`eBird species code 2019`)]) %>%
#   distinct() %>%
#   arrange(nestwatch_code)# %>% # remove spp codes that are for spuh
# #mutate(SPECIES_CODE = str_replace_all(SPECIES_CODE,pattern = '\\d+','')) %>% # remove digits
# #filter(SPECIES_CODE != 'x' & SPECIES_CODE != 'y') # remove strange spp codes w/ x & y
# 
# write_csv(update_sp,"Data/sp-key.csv")

##### Update sp name in raw based on standardized common names
sp.key <- read_csv("Data/sp-key-commonname.csv")
raw$SPECIES_CODE <- sp.key$ebird_name[match(raw$SPECIES_CODE,sp.key$nestwatch_code)]

sp.remove <- c("Black Baza", "Scarlet Macaw", "Warbling White-eye")

sp.update <- raw %>%
  filter(!SPECIES_CODE %in% sp.remove)

write_csv(sp.update,"Data/nestwatch_sp-update.csv")

