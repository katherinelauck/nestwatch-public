# Prepare data for analyses. This script's input is "Data/archive/raw-collapsed.RData" and it outputs properly scaled data clean of NAs, with one database for success-based models and one for failure-based models.
# Author: Katherine S Lauck
# Last updated: 10 Oct 2022

# Dependencies

library(GGally)
library(afex)
library("lme4")
library("lubridate")
library(tidyverse)
library(fuzzyjoin)

load("Data/active/raw-collapsed.RData")

#### Factorize spatial autocorrelation variables

nest$UnCoor<-factor(nest$UnCoor)
nest$Region<-factor(nest$Region200)
nest$Region150 <- factor(nest$Region150)
nest$Region100 <- factor(nest$Region100)
nest$Region50 <- factor(nest$Region50)

#### Create land use factor
#NLCD categories: one that is forest vs not
#one that is human, forest, non-forest
#LU categories:agriculture, forest, open natural habitats, urban

nest$NLCD_p_forest<-nest$NLCD_decidious_forest_2km+nest$NLCD_evergreen_forest_2km+nest$NLCD_mixed_forest_2km
nest$NLCD_p_human<-nest$NLCD_developed_open_2km+nest$NLCD_developed_low_2km+nest$NLCD_developed_high_2km+nest$NLCD_developed_medium_2km
nest$NLCD_p_ag<-nest$NLCD_cultivated_crops_2km+nest$NLCD_pasture_2km

nest$NLCD_p_forest_500m<-nest$NLCD_decidious_forest_500m+nest$NLCD_evergreen_forest_500m+nest$NLCD_mixed_forest_500m
nest$NLCD_p_human_500m<-nest$NLCD_developed_open_500m+nest$NLCD_developed_low_500m+nest$NLCD_developed_high_500m+nest$NLCD_developed_medium_500m
nest$NLCD_p_ag_500m<-nest$NLCD_cultivated_crops_500m+nest$NLCD_pasture_500m

nest$NLCD_p_forest_1km<-nest$NLCD_decidious_forest_1km+nest$NLCD_evergreen_forest_1km+nest$NLCD_mixed_forest_1km
nest$NLCD_p_human_1km<-nest$NLCD_developed_open_1km+nest$NLCD_developed_low_1km+nest$NLCD_developed_high_1km+nest$NLCD_developed_medium_1km
nest$NLCD_p_ag_1km<-nest$NLCD_cultivated_crops_1km+nest$NLCD_pasture_1km

nest$NewLU1<-NA
nest$NewLU1[nest$habitat1 %in% c("NW-ag","NW-xmas","NW-orch-vin")]<-"Ag"
nest$NewLU1[nest$habitat1 %in% c("NW-airport","NW-campus","NW-cem","NW-cmpgrd","NW-com-ind","NW-golf","NW-human","NW-park","NW-pit","NW-pwrln","NW-road","NW-ry")]<-"Human"
nest$NewLU1[nest$habitat1 %in% c("NW-for")]<-"Forest"
nest$NewLU1[nest$habitat1 %in% c("NW-burn","NW-chap","NW-des","NW-grass","NW-fw","NW-sw","NW-beach","NW-clrcut")]<-"Natural_open"

nest$NewLU1<-factor(nest$NewLU1,levels = c("Forest","Ag","Natural_open","Human")) # relevel factor

#### 
nest$substrate_binary[which(nest$substrate == "nesbox")] <- 1
nest$substrate_binary[which(is.na(nest$substrate_binary))] <- 0
nest$cavity_binary[which(nest$substrate %in% c("nesbox","ltrcav","ssdtcv"))] <- 1
nest$cavity_binary[which(is.na(nest$cavity_binary))] <- 0
nest$substrate<-factor(nest$substrate)

summary(nest$NewLU1)

#### Create avg annual temp variable

Tavgannual <- read_csv("Data/archive/AnnualTave_WorldClim.csv") # 96 points moved .05 degrees, 4 points moved .5 degrees (latitude)

### Add to dataset

nest <- left_join(nest,dplyr::select(.data = Tavgannual, c(UnCoor, AnnualTave_WorldClim)), by = c("UnCoor"))

### Scale by AK's dataset

sheard <- read_csv("Data/archive/sheard.csv")

### Create scientific name to common name key using BirdCodes.csv

# filter to only the birds in the nest data, then match Tree name from sheard to scientific name in birdcodes
names <- read_csv("Data/archive/BirdCodes.csv") %>%
  filter(`English name` %in% nest$species) %>%
  left_join(dplyr::select(.data = sheard,`Tree name`),by = c("scientific name" = "Tree name"),keep = TRUE)

# All birds in nest have a corresponding common name in birdcodes. Thank goodness. But, most of the scientific names in birdcodes match the Tree name in sheard, and 36 do not. Some of these match the IUCN name. So, we will replace the Tree name with the IUCN name. This will help with 30 of the 36 species.

sheard$`Tree name`[which(sheard$`IUCN name` %in% filter(names,is.na(`Tree name`))$`scientific name`)] <- filter(sheard,`IUCN name` %in% filter(names,is.na(`Tree name`))$`scientific name`)$`IUCN name`

# Check again. How many are left

names <- read_csv("Data/archive/BirdCodes.csv") %>%
  filter(`English name` %in% nest$species) %>%
  left_join(dplyr::select(.data = sheard,`Tree name`),by = c("scientific name" = "Tree name"),keep = TRUE)

# Six species do not have a matching IUCN name or Tree name in the sheard dataset. I will find out manually what the scientific names of these species are and replace the Tree name in birdcodes, then use Tree name as the key to join sheard to names and then names to nest

names$`Tree name`[which(is.na(names$`Tree name`))] <- 
  c("Larus philadelphia","Picoides villosus","Picoides albolarvatus","Troglodytes troglodytes","Spizella arborea","Dendroica petechia")

# Have we done it?

filter(names,is.na(`Tree name`))$`English name`

temp <- left_join(names,dplyr::select(.data = sheard,c(`Tree name`,AnnualTemp,TempRange)), by = c("Tree name"="Tree name"))

### Use key to left join AnnualTemp and TempRange to nest

nest <- left_join(nest, dplyr::select(.data = temp,c(`English name`,AnnualTemp,TempRange)),by = c("species" = "English name"))

### Scale AnnualTave_WorldClim by subtracting AnnualTemp and dividing by TempRange

nest <- mutate(nest, tmean_rel2sp_z = (AnnualTave_WorldClim - AnnualTemp)/TempRange) %>%
  mutate(tmean_rel2sp_anom = AnnualTave_WorldClim - AnnualTemp)

#### Create mean max & min temp over mean nest period
# For some reason 3/4 gridmet files had 30k rows of NAs at end. This code removes them
gm_max_v1 <- read_csv("Data/archive/gridMEToutput_v1_tmaxafter-prcpbefore.csv")[-c(225312:255311),] 
gm_min_v1 <- read_csv("Data/archive/gridMEToutput_v1_tminafter-prcpafter.csv")[-c(225312:255311),] 
gm_max_v2 <- read_csv("Data/archive/gridMEToutput_v2-fixedlaydate_tmaxafter-prcpbefore.csv")
gm_min_v2 <- read_csv("Data/archive/gridMEToutput_v2-fixedlaydate_tminafter-prcpafter.csv")[-c(225312:255311),] 

gm_max_v2 <- bind_cols(gm_max_v2,gm_max_v1$attempt) %>%
  dplyr::select(c(V2, V3, ...9)) %>%
  rename(Tmax_raw = V2,Tmax_anom = V3,attempt = ...9) %>%
  bind_cols(., dplyr::select(.data = gm_min_v2, c(V2,V3))) %>%
  rename(Tmin_raw = V2, Tmin_anom = V3) %>%
  mutate(tmeanmax_avgnestpd_gridmet = -(Tmax_anom - Tmax_raw)) %>%
  mutate(tmeanmin_avgnestpd_gridmet = -(Tmin_anom - Tmin_raw))

### Add to dataset

nest <- left_join(nest,dplyr::select(.data = gm_max_v2,c(tmeanmax_avgnestpd_gridmet,tmeanmin_avgnestpd_gridmet,attempt)),by = "attempt")

#### Scale variables
nest$laydate_scaled <- c(scale(yday(nest$laydate)))

# rename variables for consistency with model names
nest$Tmax_std_gridmet <- nest$Tmax_std
nest$Tmin_std_gridmet <- nest$Tmin_std
nest$pcpbefore_std_gridmet <- nest$PcpBefore_std
nest$pcpafter_std_gridmet <- nest$Pcp45dAfter_std
nest$tnestpd_stdmaxsp_gridmet <- nest$Tstdmax_sp
nest$tnestpd_stdminsp_gridmet <- nest$Tstdmin_sp


#### Create and add column with BBS-derived population trend

bbs<-read.csv("Data/archive/BBS data.csv")
codes<-read_csv("Data/archive/BirdCodes.csv")
all<-unique(nest$species)

codes$codes.simp<-gsub("\\d+","",codes$ebird)

code.df<-data.frame(codes.simp=codes$codes.simp,english=codes$`English name`)

code.merge<-merge(nest,codes,by.x="species",by.y="ebird", all.x=T, all.y=F)

cnkey<-read.csv("Data/archive/sp-key-commonname.csv")
cnkey$nospace<-gsub(" ","",cnkey$ebird_name)
bbs$nospace<-gsub(" ","",bbs$Species.Name)

code.merge$nospace<-gsub(" ","",code.merge$species)

bbs.merge<-merge(code.merge,bbs,"nospace", all.x = TRUE)

bbs.merge$Trend.scaled<-c(scale(bbs.merge$Trend))

nest <- nest %>%
  left_join(dplyr::select(bbs.merge, c(attempt,Trend,Trend.scaled)), by = "attempt")

### add conservation scores
conserv <- read_csv("Data/archive/NABCI_ConservationScores.csv")

nest <- nest %>%
  left_join(dplyr::select(conserv, c(CommonName,ConservationScore)), by = c("species" = "CommonName"))

nest$ConservationScore.scaled <- c(scale(nest$ConservationScore))

sort(names(nest))

#### Species and year as factor

nest$year <- as.factor(nest$year)

nest <- mutate(nest,Tmax_std_gridmet_sq = Tmax_std_gridmet * Tmax_std_gridmet)

nest$lat_sq <- nest$lat * nest$lat
nest$lon_sq <- nest$lon * nest$lon

#### Select columns in any models and ones we might want to build in the future

nest <- nest %>% 
  dplyr::select("attempt", "species","Trend","Trend.scaled", "ConservationScore", "ConservationScore.scaled","substrate_binary", "cavity_binary","laydate","laydate_scaled","year","at_least_one_success","at_least_one_failure","NewLU1","Region350","Region300","Region250","Region","Region150","Region100","Region50","UnCoor","lat","lon","lat_sq","lon_sq","Tmax_std_gridmet","Tmax_std_gridmet_sq","Tmin_std_gridmet","Tmeanmax","tnestpd_stdmaxsp_gridmet","Tmeanmin","tnestpd_stdminsp_gridmet","AnnualTave_WorldClim","tmeanmax_avgnestpd_gridmet","tmeanmin_avgnestpd_gridmet","tmean_rel2sp_z","tmean_rel2sp_anom","NLCD_p_ag","NLCD_p_forest","NLCD_p_human","NLCD_p_ag_500m","NLCD_p_forest_500m","NLCD_p_human_500m","NLCD_p_ag_1km","NLCD_p_forest_1km","NLCD_p_human_1km","PcpBefore_raw","pcpbefore_std_gridmet","Pcp45dAfter_raw","pcpafter_std_gridmet","max_cowbird","fail_cowbird","cowbird_lgl","predation","outcome","prop_success")

ele_key <- read_csv("Data/active/elevation.csv") %>% select(UnCoor,elevation)
nest <- left_join(x = nest,y = ele_key,by = "UnCoor")

apply(nest,2,function(x){sum(is.na(x))})

#### Drop NAs

full <- nest %>% 
  drop_na(!c(Trend,Trend.scaled,at_least_one_failure,at_least_one_success,ConservationScore, ConservationScore.scaled,cowbird_lgl,fail_cowbird,max_cowbird,predation,outcome,AnnualTave_WorldClim,tnestpd_stdmaxsp_gridmet,tnestpd_stdminsp_gridmet,tmeanmax_avgnestpd_gridmet,tmeanmin_avgnestpd_gridmet,tmean_rel2sp_z,tmean_rel2sp_anom))
full$species <- as.character(full$species)

#### Drop NAs from success, drop failure

success <- full %>%
  drop_na(at_least_one_success) %>%
  dplyr::select(!at_least_one_failure)

#### Filter spp for success

## Tabulate how many attempts per land use per species

spp=sort(unique(success$species))
fout=array(dim=c(0,7))
colnames(fout)=c("Species","NumAttempts","NumSites","Attempts_Forest","Attempts_Ag","Attempts_Natural_open","Attempts_Human")
for ( i in 1:length(spp)){
  oneSPP=success[which(success$species==spp[i]),]
  tot_attempts=length(unique(oneSPP$attempt))
  tot_sites=length(unique(oneSPP$UnCoor))
  types=table(oneSPP$NewLU1)
  
  attempt_lu=c(0,0,0,0)
  attempt_lu[match(names(types),sort(unique(success$NewLU1)))]=types
  
  out=c(spp[i],tot_attempts,tot_sites,attempt_lu)
  fout=rbind(fout,out)
}
fout=data.frame(fout)

## Make a list of the species that nested at least five times in at least two land uses

sp.keep <- fout %>%
  #tibble() %>%
  mutate(ag_bin = if_else(as.numeric(Attempts_Ag) < 5, 0, 1),
         forest_bin = if_else(as.numeric(Attempts_Forest) < 5, 0, 1),
         human_bin = if_else(as.numeric(Attempts_Human) < 5, 0, 1),
         natural_open_bin = if_else(as.numeric(Attempts_Natural_open) < 5, 0, 1)) %>%
  mutate(include = if_else(ag_bin + forest_bin + human_bin + natural_open_bin > 1, 1, 0)) %>%
  filter(include == 1)

success <- filter(success,species %in% sp.keep$Species)

#### Exclude species with fewer than 100 nesting attempts out of the species that Danny wanted to include

success <- success %>% group_by(species) %>% 
  mutate(greaterThan100 = if_else(n()<100,0,1))
success$species <- as.factor(success$species)

#### Scale & square appropriate columns
success$pcpbefore_raw_gridmet <- c(scale(success$PcpBefore_raw))
success$pcpbefore_raw_gridmet_sq <- success$pcpbefore_raw_gridmet * success$pcpbefore_raw_gridmet
success$pcpafter_raw_gridmet <- c(scale(success$Pcp45dAfter_raw))
success$pcpafter_raw_gridmet_sq <- success$pcpafter_raw_gridmet * success$pcpafter_raw_gridmet
success$pcpafter_std_gridmet_sq <- success$pcpafter_std_gridmet * success$pcpafter_std_gridmet
success$tnestpd_meanmin_gridmet <- c(scale(success$Tmeanmin))
success$tnestpd_meanmax_gridmet <- c(scale(success$Tmeanmax))
success$NLCD_p_ag <- c(scale(success$NLCD_p_ag))
success$NLCD_p_forest <- c(scale(success$NLCD_p_forest))
success$NLCD_p_human <- c(scale(success$NLCD_p_human))
success$NLCD_p_ag_500m <- c(scale(success$NLCD_p_ag_500m))
success$NLCD_p_forest_500m <- c(scale(success$NLCD_p_forest_500m))
success$NLCD_p_human_500m <- c(scale(success$NLCD_p_human_500m))
success$NLCD_p_ag_1km <- c(scale(success$NLCD_p_ag_1km))
success$NLCD_p_forest_1km <- c(scale(success$NLCD_p_forest_1km))
success$NLCD_p_human_1km <- c(scale(success$NLCD_p_human_1km))
success$tmeanmax_avgnestpd_gridmet_scaled <- c(scale(success$tmeanmax_avgnestpd_gridmet))
success$tmeanmin_avgnestpd_gridmet_scaled <- c(scale(success$tmeanmin_avgnestpd_gridmet))
success$tmean_rel2sp_anom <- c(scale(success$tmean_rel2sp_anom))
success$elevation = c(scale(success$elevation))
success$sollman = c(scale(success$prop_success))
success$lat <- c(scale(success$lat))
success$lon <- c(scale(success$lon))
success$lat_sq <- c(scale(success$lat_sq))
success$lon_sq <- c(scale(success$lon_sq))

#### Write data

write_rds(success, "Data/active/success-cleaned.rds")
