##Load libraries
library(reshape)
library(reshape2)
library(tidyr)
library(plyr)

###Read NLCD data
NLCD2001 <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2001_500m1km2km.csv")
NLCD2004 <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2004_500m1km2km.csv")
NLCD2006 <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2006_500m1km2km.csv")
NLCD2008 <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2008_500m1km2km.csv")
NLCD2011 <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2011_500m1km2km.csv")
NLCD2013 <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2013_500m1km2km.csv")
NLCD2016 <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2016_500m1km2km.csv")

##Alaska
NLCD2001_AK <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2001_AK_500m1km2km.csv")
NLCD2011_AK<- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2011_AK_500m1km2km.csv")
NLCD2016_AK <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2016_AK_500m1km2km.csv")

###2001
###Rename the variables in the  dataframe according to the clasiification in NLCD. This will make easier to interpretate the variables
names(NLCD2001) [1:63] <- c("NLCD_open_water_500m_2001","NLCD_ice_500m_2001","NLCD_developed_open_500m_2001","NLCD_developed_low_500m_2001","NLCD_developed_medium_500m_2001",
                                "NLCD_developed_high_500m_2001","NLCD_barren_land_500m_2001","NLCD_decidious_forest_500m_2001","NLCD_evergreen_forest_500m_2001","NLCD_mixed_forest_500m_2001",
                                "NLCD_dwarf_shrub_500m_2001","NLCD_shrub_500m_2001","NLCD_grassland_500m_2001","NLCD_sedge_500m_2001","NLCD_lichens_500m_2001","NLCD_moss_500m_2001","NLCD_pasture_500m_2001",
                                "NLCD_cultivated_crops_500m_2001","NLCD_woody_wetland_500m_2001","NLCD_emergent_herbaceous_wetlands_500m_2001","NLCD_unkown_500m_2001","NLCD_open_water_1km_2001","NLCD_ice_1km_2001","NLCD_developed_open_1km_2001","NLCD_developed_low_1km_2001","NLCD_developed_medium_1km_2001",
                                "NLCD_developed_high_1km_2001","NLCD_barren_land_1km_2001","NLCD_decidious_forest_1km_2001","NLCD_evergreen_forest_1km_2001","NLCD_mixed_forest_1km_2001",
                                "NLCD_dwarf_shrub_1km_2001","NLCD_shrub_1km_2001","NLCD_grassland_1km_2001","NLCD_sedge_1km_2001","NLCD_lichens_1km_2001","NLCD_moss_1km_2001","NLCD_pasture_1km_2001",
                                "NLCD_cultivated_crops_1km_2001","NLCD_woody_wetland_1km_2001","NLCD_emergent_herbaceous_wetlands_1km_2001","NLCD_unkown_1km_2001","NLCD_open_water_2km_2001","NLCD_ice_2km_2001","NLCD_developed_open_2km_2001","NLCD_developed_low_2km_2001","NLCD_developed_medium_2km_2001",
                                "NLCD_developed_high_2km_2001","NLCD_barren_land_2km_2001","NLCD_decidious_forest_2km_2001","NLCD_evergreen_forest_2km_2001","NLCD_mixed_forest_2km_2001",
                                "NLCD_dwarf_shrub_2km_2001","NLCD_shrub_2km_2001","NLCD_grassland_2km_2001","NLCD_sedge_2km_2001","NLCD_lichens_2km_2001","NLCD_moss_2km_2001","NLCD_pasture_2km_2001",
                                "NLCD_cultivated_crops_2km_2001","NLCD_woody_wetland_2km_2001","NLCD_emergent_herbaceous_wetlands_2km_2001","NLCD_unkown_2km_2001" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2001_prop <- NLCD2001[-c(1:nrow(NLCD2001)),]


###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2001)){
  a <- NLCD2001[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2001_prop <- rbind(NLCD2001_prop,a)
  print(i)
}

###Save the data frame with propotions
write.csv(NLCD2001_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2001_prop.csv")

###Alaska
###Rename the variables in the  dataframe according to the clasiification in NLCD. This will make easier to interpretate the variables
ak.2001 <- is.na(NLCD2001_AK$LC11_500m)
na.ak.2001 <- which(ak.2001==T)
NLCD2001_AK <- NLCD2001_AK[-na.ak.2001,]


names(NLCD2001_AK) [1:63] <- c("NLCD_open_water_500m_2001","NLCD_ice_500m_2001","NLCD_developed_open_500m_2001","NLCD_developed_low_500m_2001","NLCD_developed_medium_500m_2001",
                                   "NLCD_developed_high_500m_2001","NLCD_barren_land_500m_2001","NLCD_decidious_forest_500m_2001","NLCD_evergreen_forest_500m_2001","NLCD_mixed_forest_500m_2001",
                                   "NLCD_dwarf_shrub_500m_2001","NLCD_shrub_500m_2001","NLCD_grassland_500m_2001","NLCD_sedge_500m_2001","NLCD_lichens_500m_2001","NLCD_moss_500m_2001","NLCD_pasture_500m_2001",
                                   "NLCD_cultivated_crops_500m_2001","NLCD_woody_wetland_500m_2001","NLCD_emergent_herbaceous_wetlands_500m_2001","NLCD_unkown_500m_2001","NLCD_open_water_1km_2001","NLCD_ice_1km_2001","NLCD_developed_open_1km_2001","NLCD_developed_low_1km_2001","NLCD_developed_medium_1km_2001",
                                   "NLCD_developed_high_1km_2001","NLCD_barren_land_1km_2001","NLCD_decidious_forest_1km_2001","NLCD_evergreen_forest_1km_2001","NLCD_mixed_forest_1km_2001",
                                   "NLCD_dwarf_shrub_1km_2001","NLCD_shrub_1km_2001","NLCD_grassland_1km_2001","NLCD_sedge_1km_2001","NLCD_lichens_1km_2001","NLCD_moss_1km_2001","NLCD_pasture_1km_2001",
                                   "NLCD_cultivated_crops_1km_2001","NLCD_woody_wetland_1km_2001","NLCD_emergent_herbaceous_wetlands_1km_2001","NLCD_unkown_1km_2001","NLCD_open_water_2km_2001","NLCD_ice_2km_2001","NLCD_developed_open_2km_2001","NLCD_developed_low_2km_2001","NLCD_developed_medium_2km_2001",
                                   "NLCD_developed_high_2km_2001","NLCD_barren_land_2km_2001","NLCD_decidious_forest_2km_2001","NLCD_evergreen_forest_2km_2001","NLCD_mixed_forest_2km_2001",
                                   "NLCD_dwarf_shrub_2km_2001","NLCD_shrub_2km_2001","NLCD_grassland_2km_2001","NLCD_sedge_2km_2001","NLCD_lichens_2km_2001","NLCD_moss_2km_2001","NLCD_pasture_2km_2001",
                                   "NLCD_cultivated_crops_2km_2001","NLCD_woody_wetland_2km_2001","NLCD_emergent_herbaceous_wetlands_2km_2001","NLCD_unkown_2km_2001" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2001_AK_prop <- NLCD2001_AK[-c(1:nrow(NLCD2001_AK)),]

###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2001_AK)){
  a <- NLCD2001_AK[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2001_AK_prop <- rbind(NLCD2001_AK_prop,a)
  print(i)
}


###Save the data frame with propotions
write.csv(NLCD2001_AK_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2001_AK_prop.csv")



###2004
names(NLCD2004) [1:63] <- c("NLCD_open_water_500m_2004","NLCD_ice_500m_2004","NLCD_developed_open_500m_2004","NLCD_developed_low_500m_2004","NLCD_developed_medium_500m_2004",
                            "NLCD_developed_high_500m_2004","NLCD_barren_land_500m_2004","NLCD_decidious_forest_500m_2004","NLCD_evergreen_forest_500m_2004","NLCD_mixed_forest_500m_2004",
                            "NLCD_dwarf_shrub_500m_2004","NLCD_shrub_500m_2004","NLCD_grassland_500m_2004","NLCD_sedge_500m_2004","NLCD_lichens_500m_2004","NLCD_moss_500m_2004","NLCD_pasture_500m_2004",
                            "NLCD_cultivated_crops_500m_2004","NLCD_woody_wetland_500m_2004","NLCD_emergent_herbaceous_wetlands_500m_2004","NLCD_unkown_500m_2004","NLCD_open_water_1km_2004","NLCD_ice_1km_2004","NLCD_developed_open_1km_2004","NLCD_developed_low_1km_2004","NLCD_developed_medium_1km_2004",
                            "NLCD_developed_high_1km_2004","NLCD_barren_land_1km_2004","NLCD_decidious_forest_1km_2004","NLCD_evergreen_forest_1km_2004","NLCD_mixed_forest_1km_2004",
                            "NLCD_dwarf_shrub_1km_2004","NLCD_shrub_1km_2004","NLCD_grassland_1km_2004","NLCD_sedge_1km_2004","NLCD_lichens_1km_2004","NLCD_moss_1km_2004","NLCD_pasture_1km_2004",
                            "NLCD_cultivated_crops_1km_2004","NLCD_woody_wetland_1km_2004","NLCD_emergent_herbaceous_wetlands_1km_2004","NLCD_unkown_1km_2004","NLCD_open_water_2km_2004","NLCD_ice_2km_2004","NLCD_developed_open_2km_2004","NLCD_developed_low_2km_2004","NLCD_developed_medium_2km_2004",
                            "NLCD_developed_high_2km_2004","NLCD_barren_land_2km_2004","NLCD_decidious_forest_2km_2004","NLCD_evergreen_forest_2km_2004","NLCD_mixed_forest_2km_2004",
                            "NLCD_dwarf_shrub_2km_2004","NLCD_shrub_2km_2004","NLCD_grassland_2km_2004","NLCD_sedge_2km_2004","NLCD_lichens_2km_2004","NLCD_moss_2km_2004","NLCD_pasture_2km_2004",
                            "NLCD_cultivated_crops_2km_2004","NLCD_woody_wetland_2km_2004","NLCD_emergent_herbaceous_wetlands_2km_2004","NLCD_unkown_2km_2004" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2004_prop <- NLCD2004[-c(1:nrow(NLCD2004)),]


###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2004)){
  a <- NLCD2004[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2004_prop <- rbind(NLCD2004_prop,a)
  print(i)
}

###Save the data frame with propotions
write.csv(NLCD2004_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2004_prop.csv")


###2006
names(NLCD2006) [1:63] <- c("NLCD_open_water_500m_2006","NLCD_ice_500m_2006","NLCD_developed_open_500m_2006","NLCD_developed_low_500m_2006","NLCD_developed_medium_500m_2006",
                            "NLCD_developed_high_500m_2006","NLCD_barren_land_500m_2006","NLCD_decidious_forest_500m_2006","NLCD_evergreen_forest_500m_2006","NLCD_mixed_forest_500m_2006",
                            "NLCD_dwarf_shrub_500m_2006","NLCD_shrub_500m_2006","NLCD_grassland_500m_2006","NLCD_sedge_500m_2006","NLCD_lichens_500m_2006","NLCD_moss_500m_2006","NLCD_pasture_500m_2006",
                            "NLCD_cultivated_crops_500m_2006","NLCD_woody_wetland_500m_2006","NLCD_emergent_herbaceous_wetlands_500m_2006","NLCD_unkown_500m_2006","NLCD_open_water_1km_2006","NLCD_ice_1km_2006","NLCD_developed_open_1km_2006","NLCD_developed_low_1km_2006","NLCD_developed_medium_1km_2006",
                            "NLCD_developed_high_1km_2006","NLCD_barren_land_1km_2006","NLCD_decidious_forest_1km_2006","NLCD_evergreen_forest_1km_2006","NLCD_mixed_forest_1km_2006",
                            "NLCD_dwarf_shrub_1km_2006","NLCD_shrub_1km_2006","NLCD_grassland_1km_2006","NLCD_sedge_1km_2006","NLCD_lichens_1km_2006","NLCD_moss_1km_2006","NLCD_pasture_1km_2006",
                            "NLCD_cultivated_crops_1km_2006","NLCD_woody_wetland_1km_2006","NLCD_emergent_herbaceous_wetlands_1km_2006","NLCD_unkown_1km_2006","NLCD_open_water_2km_2006","NLCD_ice_2km_2006","NLCD_developed_open_2km_2006","NLCD_developed_low_2km_2006","NLCD_developed_medium_2km_2006",
                            "NLCD_developed_high_2km_2006","NLCD_barren_land_2km_2006","NLCD_decidious_forest_2km_2006","NLCD_evergreen_forest_2km_2006","NLCD_mixed_forest_2km_2006",
                            "NLCD_dwarf_shrub_2km_2006","NLCD_shrub_2km_2006","NLCD_grassland_2km_2006","NLCD_sedge_2km_2006","NLCD_lichens_2km_2006","NLCD_moss_2km_2006","NLCD_pasture_2km_2006",
                            "NLCD_cultivated_crops_2km_2006","NLCD_woody_wetland_2km_2006","NLCD_emergent_herbaceous_wetlands_2km_2006","NLCD_unkown_2km_2006" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2006_prop <- NLCD2006[-c(1:nrow(NLCD2006)),]


###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2006)){
  a <- NLCD2006[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2006_prop <- rbind(NLCD2006_prop,a)
  print(i)
}

###Save the data frame with propotions
write.csv(NLCD2006_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2006_prop.csv")


###2008
###Rename the variables in the dataframe according to the clasiification in NLCD. This will make easier to interpretate the variables
names(NLCD2008) [1:63] <- c("NLCD_open_water_500m_2008","NLCD_ice_500m_2008","NLCD_developed_open_500m_2008","NLCD_developed_low_500m_2008","NLCD_developed_medium_500m_2008",
                            "NLCD_developed_high_500m_2008","NLCD_barren_land_500m_2008","NLCD_decidious_forest_500m_2008","NLCD_evergreen_forest_500m_2008","NLCD_mixed_forest_500m_2008",
                            "NLCD_dwarf_shrub_500m_2008","NLCD_shrub_500m_2008","NLCD_grassland_500m_2008","NLCD_sedge_500m_2008","NLCD_lichens_500m_2008","NLCD_moss_500m_2008","NLCD_pasture_500m_2008",
                            "NLCD_cultivated_crops_500m_2008","NLCD_woody_wetland_500m_2008","NLCD_emergent_herbaceous_wetlands_500m_2008","NLCD_unkown_500m_2008","NLCD_open_water_1km_2008","NLCD_ice_1km_2008","NLCD_developed_open_1km_2008","NLCD_developed_low_1km_2008","NLCD_developed_medium_1km_2008",
                            "NLCD_developed_high_1km_2008","NLCD_barren_land_1km_2008","NLCD_decidious_forest_1km_2008","NLCD_evergreen_forest_1km_2008","NLCD_mixed_forest_1km_2008",
                            "NLCD_dwarf_shrub_1km_2008","NLCD_shrub_1km_2008","NLCD_grassland_1km_2008","NLCD_sedge_1km_2008","NLCD_lichens_1km_2008","NLCD_moss_1km_2008","NLCD_pasture_1km_2008",
                            "NLCD_cultivated_crops_1km_2008","NLCD_woody_wetland_1km_2008","NLCD_emergent_herbaceous_wetlands_1km_2008","NLCD_unkown_1km_2008","NLCD_open_water_2km_2008","NLCD_ice_2km_2008","NLCD_developed_open_2km_2008","NLCD_developed_low_2km_2008","NLCD_developed_medium_2km_2008",
                            "NLCD_developed_high_2km_2008","NLCD_barren_land_2km_2008","NLCD_decidious_forest_2km_2008","NLCD_evergreen_forest_2km_2008","NLCD_mixed_forest_2km_2008",
                            "NLCD_dwarf_shrub_2km_2008","NLCD_shrub_2km_2008","NLCD_grassland_2km_2008","NLCD_sedge_2km_2008","NLCD_lichens_2km_2008","NLCD_moss_2km_2008","NLCD_pasture_2km_2008",
                            "NLCD_cultivated_crops_2km_2008","NLCD_woody_wetland_2km_2008","NLCD_emergent_herbaceous_wetlands_2km_2008","NLCD_unkown_2km_2008" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2008_prop <- NLCD2008[-c(1:nrow(NLCD2008)),]


###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2008)){
  a <- NLCD2008[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2008_prop <- rbind(NLCD2008_prop,a)
  print(i)
}

###Save the data frame with propotions
write.csv(NLCD2008_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2008_prop.csv")


###2011
names(NLCD2011) [1:63] <- c("NLCD_open_water_500m_2011","NLCD_ice_500m_2011","NLCD_developed_open_500m_2011","NLCD_developed_low_500m_2011","NLCD_developed_medium_500m_2011",
                            "NLCD_developed_high_500m_2011","NLCD_barren_land_500m_2011","NLCD_decidious_forest_500m_2011","NLCD_evergreen_forest_500m_2011","NLCD_mixed_forest_500m_2011",
                            "NLCD_dwarf_shrub_500m_2011","NLCD_shrub_500m_2011","NLCD_grassland_500m_2011","NLCD_sedge_500m_2011","NLCD_lichens_500m_2011","NLCD_moss_500m_2011","NLCD_pasture_500m_2011",
                            "NLCD_cultivated_crops_500m_2011","NLCD_woody_wetland_500m_2011","NLCD_emergent_herbaceous_wetlands_500m_2011","NLCD_unkown_500m_2011","NLCD_open_water_1km_2011","NLCD_ice_1km_2011","NLCD_developed_open_1km_2011","NLCD_developed_low_1km_2011","NLCD_developed_medium_1km_2011",
                            "NLCD_developed_high_1km_2011","NLCD_barren_land_1km_2011","NLCD_decidious_forest_1km_2011","NLCD_evergreen_forest_1km_2011","NLCD_mixed_forest_1km_2011",
                            "NLCD_dwarf_shrub_1km_2011","NLCD_shrub_1km_2011","NLCD_grassland_1km_2011","NLCD_sedge_1km_2011","NLCD_lichens_1km_2011","NLCD_moss_1km_2011","NLCD_pasture_1km_2011",
                            "NLCD_cultivated_crops_1km_2011","NLCD_woody_wetland_1km_2011","NLCD_emergent_herbaceous_wetlands_1km_2011","NLCD_unkown_1km_2011","NLCD_open_water_2km_2011","NLCD_ice_2km_2011","NLCD_developed_open_2km_2011","NLCD_developed_low_2km_2011","NLCD_developed_medium_2km_2011",
                            "NLCD_developed_high_2km_2011","NLCD_barren_land_2km_2011","NLCD_decidious_forest_2km_2011","NLCD_evergreen_forest_2km_2011","NLCD_mixed_forest_2km_2011",
                            "NLCD_dwarf_shrub_2km_2011","NLCD_shrub_2km_2011","NLCD_grassland_2km_2011","NLCD_sedge_2km_2011","NLCD_lichens_2km_2011","NLCD_moss_2km_2011","NLCD_pasture_2km_2011",
                            "NLCD_cultivated_crops_2km_2011","NLCD_woody_wetland_2km_2011","NLCD_emergent_herbaceous_wetlands_2km_2011","NLCD_unkown_2km_2011" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2011_prop <- NLCD2011[-c(1:nrow(NLCD2011)),]


###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2011)){
  a <- NLCD2011[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2011_prop <- rbind(NLCD2011_prop,a)
  print(i)
}

###Save the data frame with propotions
write.csv(NLCD2011_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2011_prop.csv")


#Alaska
###Rename the variables in the  dataframe according to the clasiification in NLCD. This will make easier to interpretate the variables
ak.2011 <- is.na(NLCD2011_AK$LC11_500m)
na.ak.2011 <- which(ak.2011==T)
NLCD2011_AK <- NLCD2011_AK[-na.ak.2011,]

names(NLCD2011_AK) [1:63] <- c("NLCD_open_water_500m_2011","NLCD_ice_500m_2011","NLCD_developed_open_500m_2011","NLCD_developed_low_500m_2011","NLCD_developed_medium_500m_2011",
                               "NLCD_developed_high_500m_2011","NLCD_barren_land_500m_2011","NLCD_decidious_forest_500m_2011","NLCD_evergreen_forest_500m_2011","NLCD_mixed_forest_500m_2011",
                               "NLCD_dwarf_shrub_500m_2011","NLCD_shrub_500m_2011","NLCD_grassland_500m_2011","NLCD_sedge_500m_2011","NLCD_lichens_500m_2011","NLCD_moss_500m_2011","NLCD_pasture_500m_2011",
                               "NLCD_cultivated_crops_500m_2011","NLCD_woody_wetland_500m_2011","NLCD_emergent_herbaceous_wetlands_500m_2011","NLCD_unkown_500m_2011","NLCD_open_water_1km_2011","NLCD_ice_1km_2011","NLCD_developed_open_1km_2011","NLCD_developed_low_1km_2011","NLCD_developed_medium_1km_2011",
                               "NLCD_developed_high_1km_2011","NLCD_barren_land_1km_2011","NLCD_decidious_forest_1km_2011","NLCD_evergreen_forest_1km_2011","NLCD_mixed_forest_1km_2011",
                               "NLCD_dwarf_shrub_1km_2011","NLCD_shrub_1km_2011","NLCD_grassland_1km_2011","NLCD_sedge_1km_2011","NLCD_lichens_1km_2011","NLCD_moss_1km_2011","NLCD_pasture_1km_2011",
                               "NLCD_cultivated_crops_1km_2011","NLCD_woody_wetland_1km_2011","NLCD_emergent_herbaceous_wetlands_1km_2011","NLCD_unkown_1km_2011","NLCD_open_water_2km_2011","NLCD_ice_2km_2011","NLCD_developed_open_2km_2011","NLCD_developed_low_2km_2011","NLCD_developed_medium_2km_2011",
                               "NLCD_developed_high_2km_2011","NLCD_barren_land_2km_2011","NLCD_decidious_forest_2km_2011","NLCD_evergreen_forest_2km_2011","NLCD_mixed_forest_2km_2011",
                               "NLCD_dwarf_shrub_2km_2011","NLCD_shrub_2km_2011","NLCD_grassland_2km_2011","NLCD_sedge_2km_2011","NLCD_lichens_2km_2011","NLCD_moss_2km_2011","NLCD_pasture_2km_2011",
                               "NLCD_cultivated_crops_2km_2011","NLCD_woody_wetland_2km_2011","NLCD_emergent_herbaceous_wetlands_2km_2011","NLCD_unkown_2km_2011" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2011_AK_prop <- NLCD2011_AK[-c(1:nrow(NLCD2011_AK)),]

###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2011_AK)){
  a <- NLCD2011_AK[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2011_AK_prop <- rbind(NLCD2011_AK_prop,a)
  print(i)
}


###Save the data frame with propotions
write.csv(NLCD2011_AK_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2011_AK_prop.csv")

###2013
names(NLCD2013) [1:63] <- c("NLCD_open_water_500m_2013","NLCD_ice_500m_2013","NLCD_developed_open_500m_2013","NLCD_developed_low_500m_2013","NLCD_developed_medium_500m_2013",
                            "NLCD_developed_high_500m_2013","NLCD_barren_land_500m_2013","NLCD_decidious_forest_500m_2013","NLCD_evergreen_forest_500m_2013","NLCD_mixed_forest_500m_2013",
                            "NLCD_dwarf_shrub_500m_2013","NLCD_shrub_500m_2013","NLCD_grassland_500m_2013","NLCD_sedge_500m_2013","NLCD_lichens_500m_2013","NLCD_moss_500m_2013","NLCD_pasture_500m_2013",
                            "NLCD_cultivated_crops_500m_2013","NLCD_woody_wetland_500m_2013","NLCD_emergent_herbaceous_wetlands_500m_2013","NLCD_unkown_500m_2013","NLCD_open_water_1km_2013","NLCD_ice_1km_2013","NLCD_developed_open_1km_2013","NLCD_developed_low_1km_2013","NLCD_developed_medium_1km_2013",
                            "NLCD_developed_high_1km_2013","NLCD_barren_land_1km_2013","NLCD_decidious_forest_1km_2013","NLCD_evergreen_forest_1km_2013","NLCD_mixed_forest_1km_2013",
                            "NLCD_dwarf_shrub_1km_2013","NLCD_shrub_1km_2013","NLCD_grassland_1km_2013","NLCD_sedge_1km_2013","NLCD_lichens_1km_2013","NLCD_moss_1km_2013","NLCD_pasture_1km_2013",
                            "NLCD_cultivated_crops_1km_2013","NLCD_woody_wetland_1km_2013","NLCD_emergent_herbaceous_wetlands_1km_2013","NLCD_unkown_1km_2013","NLCD_open_water_2km_2013","NLCD_ice_2km_2013","NLCD_developed_open_2km_2013","NLCD_developed_low_2km_2013","NLCD_developed_medium_2km_2013",
                            "NLCD_developed_high_2km_2013","NLCD_barren_land_2km_2013","NLCD_decidious_forest_2km_2013","NLCD_evergreen_forest_2km_2013","NLCD_mixed_forest_2km_2013",
                            "NLCD_dwarf_shrub_2km_2013","NLCD_shrub_2km_2013","NLCD_grassland_2km_2013","NLCD_sedge_2km_2013","NLCD_lichens_2km_2013","NLCD_moss_2km_2013","NLCD_pasture_2km_2013",
                            "NLCD_cultivated_crops_2km_2013","NLCD_woody_wetland_2km_2013","NLCD_emergent_herbaceous_wetlands_2km_2013","NLCD_unkown_2km_2013" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2013_prop <- NLCD2013[-c(1:nrow(NLCD2013)),]


###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2013)){
  a <- NLCD2013[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2013_prop <- rbind(NLCD2013_prop,a)
  print(i)
}

###Save the data frame with propotions
write.csv(NLCD2013_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2013_prop.csv")

###2016
names(NLCD2016) [1:63] <- c("NLCD_open_water_500m_2016","NLCD_ice_500m_2016","NLCD_developed_open_500m_2016","NLCD_developed_low_500m_2016","NLCD_developed_medium_500m_2016",
                            "NLCD_developed_high_500m_2016","NLCD_barren_land_500m_2016","NLCD_decidious_forest_500m_2016","NLCD_evergreen_forest_500m_2016","NLCD_mixed_forest_500m_2016",
                            "NLCD_dwarf_shrub_500m_2016","NLCD_shrub_500m_2016","NLCD_grassland_500m_2016","NLCD_sedge_500m_2016","NLCD_lichens_500m_2016","NLCD_moss_500m_2016","NLCD_pasture_500m_2016",
                            "NLCD_cultivated_crops_500m_2016","NLCD_woody_wetland_500m_2016","NLCD_emergent_herbaceous_wetlands_500m_2016","NLCD_unkown_500m_2016","NLCD_open_water_1km_2016","NLCD_ice_1km_2016","NLCD_developed_open_1km_2016","NLCD_developed_low_1km_2016","NLCD_developed_medium_1km_2016",
                            "NLCD_developed_high_1km_2016","NLCD_barren_land_1km_2016","NLCD_decidious_forest_1km_2016","NLCD_evergreen_forest_1km_2016","NLCD_mixed_forest_1km_2016",
                            "NLCD_dwarf_shrub_1km_2016","NLCD_shrub_1km_2016","NLCD_grassland_1km_2016","NLCD_sedge_1km_2016","NLCD_lichens_1km_2016","NLCD_moss_1km_2016","NLCD_pasture_1km_2016",
                            "NLCD_cultivated_crops_1km_2016","NLCD_woody_wetland_1km_2016","NLCD_emergent_herbaceous_wetlands_1km_2016","NLCD_unkown_1km_2016","NLCD_open_water_2km_2016","NLCD_ice_2km_2016","NLCD_developed_open_2km_2016","NLCD_developed_low_2km_2016","NLCD_developed_medium_2km_2016",
                            "NLCD_developed_high_2km_2016","NLCD_barren_land_2km_2016","NLCD_decidious_forest_2km_2016","NLCD_evergreen_forest_2km_2016","NLCD_mixed_forest_2km_2016",
                            "NLCD_dwarf_shrub_2km_2016","NLCD_shrub_2km_2016","NLCD_grassland_2km_2016","NLCD_sedge_2km_2016","NLCD_lichens_2km_2016","NLCD_moss_2km_2016","NLCD_pasture_2km_2016",
                            "NLCD_cultivated_crops_2km_2016","NLCD_woody_wetland_2km_2016","NLCD_emergent_herbaceous_wetlands_2km_2016","NLCD_unkown_2km_2016" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2016_prop <- NLCD2016[-c(1:nrow(NLCD2016)),]


###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2016)){
  a <- NLCD2016[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2016_prop <- rbind(NLCD2016_prop,a)
  print(i)
}

###Save the data frame with propotions
write.csv(NLCD2016_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2016_prop.csv")


#Alaska
###Rename the variables in the  dataframe according to the clasiification in NLCD. This will make easier to interpretate the variables
ak.2016 <- is.na(NLCD2016_AK$LC11_500m)
na.ak.2016 <- which(ak.2016==T)
NLCD2016_AK <- NLCD2016_AK[-na.ak.2011,]

names(NLCD2016_AK) [1:63] <- c("NLCD_open_water_500m_2016","NLCD_ice_500m_2016","NLCD_developed_open_500m_2016","NLCD_developed_low_500m_2016","NLCD_developed_medium_500m_2016",
                               "NLCD_developed_high_500m_2016","NLCD_barren_land_500m_2016","NLCD_decidious_forest_500m_2016","NLCD_evergreen_forest_500m_2016","NLCD_mixed_forest_500m_2016",
                               "NLCD_dwarf_shrub_500m_2016","NLCD_shrub_500m_2016","NLCD_grassland_500m_2016","NLCD_sedge_500m_2016","NLCD_lichens_500m_2016","NLCD_moss_500m_2016","NLCD_pasture_500m_2016",
                               "NLCD_cultivated_crops_500m_2016","NLCD_woody_wetland_500m_2016","NLCD_emergent_herbaceous_wetlands_500m_2016","NLCD_unkown_500m_2016","NLCD_open_water_1km_2016","NLCD_ice_1km_2016","NLCD_developed_open_1km_2016","NLCD_developed_low_1km_2016","NLCD_developed_medium_1km_2016",
                               "NLCD_developed_high_1km_2016","NLCD_barren_land_1km_2016","NLCD_decidious_forest_1km_2016","NLCD_evergreen_forest_1km_2016","NLCD_mixed_forest_1km_2016",
                               "NLCD_dwarf_shrub_1km_2016","NLCD_shrub_1km_2016","NLCD_grassland_1km_2016","NLCD_sedge_1km_2016","NLCD_lichens_1km_2016","NLCD_moss_1km_2016","NLCD_pasture_1km_2016",
                               "NLCD_cultivated_crops_1km_2016","NLCD_woody_wetland_1km_2016","NLCD_emergent_herbaceous_wetlands_1km_2016","NLCD_unkown_1km_2016","NLCD_open_water_2km_2016","NLCD_ice_2km_2016","NLCD_developed_open_2km_2016","NLCD_developed_low_2km_2016","NLCD_developed_medium_2km_2016",
                               "NLCD_developed_high_2km_2016","NLCD_barren_land_2km_2016","NLCD_decidious_forest_2km_2016","NLCD_evergreen_forest_2km_2016","NLCD_mixed_forest_2km_2016",
                               "NLCD_dwarf_shrub_2km_2016","NLCD_shrub_2km_2016","NLCD_grassland_2km_2016","NLCD_sedge_2km_2016","NLCD_lichens_2km_2016","NLCD_moss_2km_2016","NLCD_pasture_2km_2016",
                               "NLCD_cultivated_crops_2km_2016","NLCD_woody_wetland_2km_2016","NLCD_emergent_herbaceous_wetlands_2km_2016","NLCD_unkown_2km_2016" )

######Create a new variable for the loop that will contain proportions instead of absolute values
NLCD2016_AK_prop <- NLCD2016_AK[-c(1:nrow(NLCD2016_AK)),]

###Create a new data frame for each year with the proportions of each land use
###this can take three hours for each data frame
for(i in 1:nrow(NLCD2016_AK)){
  a <- NLCD2016_AK[i,]
  for(j in 1:63){
    if(j<22) {a[,j] <- a[,j]/a$Total_500m}
    if(j>21 & j<43) {a[,j] <- a[,j]/a$Total_1km}
    if(j>42 & j<64) {a[,j] <- a[,j]/a$Total_2km}
  }
  NLCD2016_AK_prop <- rbind(NLCD2016_AK_prop,a)
  print(i)
}


###Save the data frame with propotions
write.csv(NLCD2016_AK_prop, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2016_AK_prop.csv")



########################################
###Load all databases
NLCD2001_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2001_prop.csv")
NLCD2004_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2004_prop.csv")
NLCD2006_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2006_prop.csv")
NLCD2008_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2008_prop.csv")
NLCD2011_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2011_prop.csv")
NLCD2013_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2013_prop.csv")
NLCD2016_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2016_prop.csv")

#Alaska
NLCD2001_AK_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2001_AK_prop.csv")
NLCD2011_AK_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2011_AK_prop.csv")
NLCD2016_AK_prop <- read.csv("/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD2016_AK_prop.csv")

###Creation of variable UnCoor

####I remove the column I do not want to merge
NLCD2001_prop <- NLCD2001_prop[,-1]
NLCD2004_prop <- NLCD2004_prop[2:65]
NLCD2006_prop <- NLCD2006_prop[2:65]
NLCD2008_prop <- NLCD2008_prop[2:65]
NLCD2011_prop <- NLCD2011_prop[2:65]
NLCD2013_prop <- NLCD2013_prop[2:65]
NLCD2016_prop <- NLCD2016_prop[2:65]

#Alaska
NLCD2001_AK_prop <- NLCD2001_AK_prop[,-1]
NLCD2011_AK_prop <- NLCD2011_AK_prop[2:65]
NLCD2016_AK_prop <- NLCD2016_AK_prop[2:65]

####Merge all dataframes of the proportions into one single data frame. 
NLCD <- merge(NLCD2001_prop,NLCD2004_prop,by="LOC_ID")
NLCD <- merge(NLCD,NLCD2006_prop,by="LOC_ID")
NLCD <- merge(NLCD,NLCD2008_prop,by="LOC_ID")
NLCD <- merge(NLCD,NLCD2011_prop,by="LOC_ID")
NLCD <- merge(NLCD,NLCD2013_prop,by="LOC_ID")
NLCD <- merge(NLCD,NLCD2016_prop,by="LOC_ID")


#Alaska
NLCD_AK <- merge(NLCD2001_AK_prop,NLCD2011_AK_prop,by="LOC_ID")
NLCD_AK <- merge(NLCD_AK,NLCD2016_AK_prop,by="LOC_ID")


####Extract year according to PROJ_PERIOD_ID 
###REad Nestwatch data
data <- readRDS("Data/active/success-cleaned.rds")

data <- processed

data$loc_id <- as.factor(data$loc_id)


loc_year <- data.frame( year.int=data$year,LOC_ID=data$loc_id)
loc_year <- data.frame(unique(loc_year))
loc_year$LOC_ID <- as.factor(as.character(loc_year$LOC_ID))

NLCD <- merge(loc_year,NLCD,by="LOC_ID")

perro <- merge(loc_year,NLCD,by="LOC_ID")
perro$LOC_ID <- as.factor(as.character(perro$LOC_ID))

per.na <- is.na(perro$NLCD_open_water_500m_2001)
na.per <- which(per.na==TRUE)

perro.na <- perro[na.per,]
perro.na$LOC_ID <- as.factor(as.character(perro.na$LOC_ID))

#Alaska
NLCD_AK<- merge(loc_year,NLCD_AK,by="LOC_ID")

###For Open water
NLCD$NLCD_open_water_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_open_water_2km[i]<-NLCD$NLCD_open_water_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_open_water_2km[i]<-(2/3*NLCD$NLCD_open_water_2km_2001[i])+(1/3*NLCD$NLCD_open_water_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_open_water_2km[i]<-(1/3*NLCD$NLCD_open_water_2km_2001[i])+(2/3*NLCD$NLCD_open_water_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_open_water_2km[i]<-NLCD$NLCD_open_water_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_open_water_2km[i]<-(1/2*NLCD$NLCD_open_water_2km_2004[i])+(1/2*NLCD$NLCD_open_water_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_open_water_2km[i]<-NLCD$NLCD_open_water_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_open_water_2km[i]<-(1/2*NLCD$NLCD_open_water_2km_2006[i])+(1/2*NLCD$NLCD_open_water_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_open_water_2km[i]<-NLCD$NLCD_open_water_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_open_water_2km[i]<-(2/3*NLCD$NLCD_open_water_2km_2008[i])+(1/3*NLCD$NLCD_open_water_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_open_water_2km[i]<-(1/3*NLCD$NLCD_open_water_2km_2008[i])+(2/3*NLCD$NLCD_open_water_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_open_water_2km[i]<-NLCD$NLCD_open_water_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_open_water_2km[i]<-(1/2*NLCD$NLCD_open_water_2km_2011[i])+(1/2*NLCD$NLCD_open_water_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_open_water_2km[i]<-NLCD$NLCD_open_water_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_open_water_2km[i]<-(2/3*NLCD$NLCD_open_water_2km_2013[i])+(1/3*NLCD$NLCD_open_water_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_open_water_2km[i]<-(1/3*NLCD$NLCD_open_water_2km_2013[i])+(2/3*NLCD$NLCD_open_water_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_open_water_2km[i]<-NLCD$NLCD_open_water_2km_2016[i]
}

NLCD$NLCD_ice_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_ice_2km[i]<-NLCD$NLCD_ice_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_ice_2km[i]<-(2/3*NLCD$NLCD_ice_2km_2001[i])+(1/3*NLCD$NLCD_ice_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_ice_2km[i]<-(1/3*NLCD$NLCD_ice_2km_2001[i])+(2/3*NLCD$NLCD_ice_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_ice_2km[i]<-NLCD$NLCD_ice_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_ice_2km[i]<-(1/2*NLCD$NLCD_ice_2km_2004[i])+(1/2*NLCD$NLCD_ice_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_ice_2km[i]<-NLCD$NLCD_ice_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_ice_2km[i]<-(1/2*NLCD$NLCD_ice_2km_2006[i])+(1/2*NLCD$NLCD_ice_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_ice_2km[i]<-NLCD$NLCD_ice_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_ice_2km[i]<-(2/3*NLCD$NLCD_ice_2km_2008[i])+(1/3*NLCD$NLCD_ice_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_ice_2km[i]<-(1/3*NLCD$NLCD_ice_2km_2008[i])+(2/3*NLCD$NLCD_ice_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_ice_2km[i]<-NLCD$NLCD_ice_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_ice_2km[i]<-(1/2*NLCD$NLCD_ice_2km_2011[i])+(1/2*NLCD$NLCD_ice_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_ice_2km[i]<-NLCD$NLCD_ice_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_ice_2km[i]<-(2/3*NLCD$NLCD_ice_2km_2013[i])+(1/3*NLCD$NLCD_ice_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_ice_2km[i]<-(1/3*NLCD$NLCD_ice_2km_2013[i])+(2/3*NLCD$NLCD_ice_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_ice_2km[i]<-NLCD$NLCD_ice_2km_2016[i]
}



NLCD$NLCD_developed_open_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_open_2km[i]<-NLCD$NLCD_developed_open_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_open_2km[i]<-(2/3*NLCD$NLCD_developed_open_2km_2001[i])+(1/3*NLCD$NLCD_developed_open_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_open_2km[i]<-(1/3*NLCD$NLCD_developed_open_2km_2001[i])+(2/3*NLCD$NLCD_developed_open_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_open_2km[i]<-NLCD$NLCD_developed_open_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_open_2km[i]<-(1/2*NLCD$NLCD_developed_open_2km_2004[i])+(1/2*NLCD$NLCD_developed_open_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_open_2km[i]<-NLCD$NLCD_developed_open_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_open_2km[i]<-(1/2*NLCD$NLCD_developed_open_2km_2006[i])+(1/2*NLCD$NLCD_developed_open_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_open_2km[i]<-NLCD$NLCD_developed_open_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_open_2km[i]<-(2/3*NLCD$NLCD_developed_open_2km_2008[i])+(1/3*NLCD$NLCD_developed_open_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_open_2km[i]<-(1/3*NLCD$NLCD_developed_open_2km_2008[i])+(2/3*NLCD$NLCD_developed_open_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_open_2km[i]<-NLCD$NLCD_developed_open_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_open_2km[i]<-(1/2*NLCD$NLCD_developed_open_2km_2011[i])+(1/2*NLCD$NLCD_developed_open_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_open_2km[i]<-NLCD$NLCD_developed_open_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_open_2km[i]<-(2/3*NLCD$NLCD_developed_open_2km_2013[i])+(1/3*NLCD$NLCD_developed_open_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_open_2km[i]<-(1/3*NLCD$NLCD_developed_open_2km_2013[i])+(2/3*NLCD$NLCD_developed_open_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_open_2km[i]<-NLCD$NLCD_developed_open_2km_2016[i]
}

NLCD$NLCD_developed_low_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_low_2km[i]<-NLCD$NLCD_developed_low_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_low_2km[i]<-(2/3*NLCD$NLCD_developed_low_2km_2001[i])+(1/3*NLCD$NLCD_developed_low_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_low_2km[i]<-(1/3*NLCD$NLCD_developed_low_2km_2001[i])+(2/3*NLCD$NLCD_developed_low_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_low_2km[i]<-NLCD$NLCD_developed_low_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_low_2km[i]<-(1/2*NLCD$NLCD_developed_low_2km_2004[i])+(1/2*NLCD$NLCD_developed_low_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_low_2km[i]<-NLCD$NLCD_developed_low_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_low_2km[i]<-(1/2*NLCD$NLCD_developed_low_2km_2006[i])+(1/2*NLCD$NLCD_developed_low_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_low_2km[i]<-NLCD$NLCD_developed_low_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_low_2km[i]<-(2/3*NLCD$NLCD_developed_low_2km_2008[i])+(1/3*NLCD$NLCD_developed_low_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_low_2km[i]<-(1/3*NLCD$NLCD_developed_low_2km_2008[i])+(2/3*NLCD$NLCD_developed_low_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_low_2km[i]<-NLCD$NLCD_developed_low_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_low_2km[i]<-(1/2*NLCD$NLCD_developed_low_2km_2011[i])+(1/2*NLCD$NLCD_developed_low_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_low_2km[i]<-NLCD$NLCD_developed_low_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_low_2km[i]<-(2/3*NLCD$NLCD_developed_low_2km_2013[i])+(1/3*NLCD$NLCD_developed_low_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_low_2km[i]<-(1/3*NLCD$NLCD_developed_low_2km_2013[i])+(2/3*NLCD$NLCD_developed_low_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_low_2km[i]<-NLCD$NLCD_developed_low_2km_2016[i]
}

NLCD$NLCD_developed_medium_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_medium_2km[i]<-NLCD$NLCD_developed_medium_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_medium_2km[i]<-(2/3*NLCD$NLCD_developed_medium_2km_2001[i])+(1/3*NLCD$NLCD_developed_medium_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_medium_2km[i]<-(1/3*NLCD$NLCD_developed_medium_2km_2001[i])+(2/3*NLCD$NLCD_developed_medium_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_medium_2km[i]<-NLCD$NLCD_developed_medium_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_medium_2km[i]<-(1/2*NLCD$NLCD_developed_medium_2km_2004[i])+(1/2*NLCD$NLCD_developed_medium_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_medium_2km[i]<-NLCD$NLCD_developed_medium_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_medium_2km[i]<-(1/2*NLCD$NLCD_developed_medium_2km_2006[i])+(1/2*NLCD$NLCD_developed_medium_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_medium_2km[i]<-NLCD$NLCD_developed_medium_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_medium_2km[i]<-(2/3*NLCD$NLCD_developed_medium_2km_2008[i])+(1/3*NLCD$NLCD_developed_medium_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_medium_2km[i]<-(1/3*NLCD$NLCD_developed_medium_2km_2008[i])+(2/3*NLCD$NLCD_developed_medium_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_medium_2km[i]<-NLCD$NLCD_developed_medium_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_medium_2km[i]<-(1/2*NLCD$NLCD_developed_medium_2km_2011[i])+(1/2*NLCD$NLCD_developed_medium_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_medium_2km[i]<-NLCD$NLCD_developed_medium_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_medium_2km[i]<-(2/3*NLCD$NLCD_developed_medium_2km_2013[i])+(1/3*NLCD$NLCD_developed_medium_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_medium_2km[i]<-(1/3*NLCD$NLCD_developed_medium_2km_2013[i])+(2/3*NLCD$NLCD_developed_medium_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_medium_2km[i]<-NLCD$NLCD_developed_medium_2km_2016[i]
}

NLCD$NLCD_developed_high_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_high_2km[i]<-NLCD$NLCD_developed_high_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_high_2km[i]<-(2/3*NLCD$NLCD_developed_high_2km_2001[i])+(1/3*NLCD$NLCD_developed_high_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_high_2km[i]<-(1/3*NLCD$NLCD_developed_high_2km_2001[i])+(2/3*NLCD$NLCD_developed_high_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_high_2km[i]<-NLCD$NLCD_developed_high_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_high_2km[i]<-(1/2*NLCD$NLCD_developed_high_2km_2004[i])+(1/2*NLCD$NLCD_developed_high_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_high_2km[i]<-NLCD$NLCD_developed_high_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_high_2km[i]<-(1/2*NLCD$NLCD_developed_high_2km_2006[i])+(1/2*NLCD$NLCD_developed_high_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_high_2km[i]<-NLCD$NLCD_developed_high_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_high_2km[i]<-(2/3*NLCD$NLCD_developed_high_2km_2008[i])+(1/3*NLCD$NLCD_developed_high_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_high_2km[i]<-(1/3*NLCD$NLCD_developed_high_2km_2008[i])+(2/3*NLCD$NLCD_developed_high_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_high_2km[i]<-NLCD$NLCD_developed_high_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_high_2km[i]<-(1/2*NLCD$NLCD_developed_high_2km_2011[i])+(1/2*NLCD$NLCD_developed_high_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_high_2km[i]<-NLCD$NLCD_developed_high_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_high_2km[i]<-(2/3*NLCD$NLCD_developed_high_2km_2013[i])+(1/3*NLCD$NLCD_developed_high_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_high_2km[i]<-(1/3*NLCD$NLCD_developed_high_2km_2013[i])+(2/3*NLCD$NLCD_developed_high_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_high_2km[i]<-NLCD$NLCD_developed_high_2km_2016[i]
}

NLCD$NLCD_barren_land_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_barren_land_2km[i]<-NLCD$NLCD_barren_land_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_barren_land_2km[i]<-(2/3*NLCD$NLCD_barren_land_2km_2001[i])+(1/3*NLCD$NLCD_barren_land_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_barren_land_2km[i]<-(1/3*NLCD$NLCD_barren_land_2km_2001[i])+(2/3*NLCD$NLCD_barren_land_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_barren_land_2km[i]<-NLCD$NLCD_barren_land_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_barren_land_2km[i]<-(1/2*NLCD$NLCD_barren_land_2km_2004[i])+(1/2*NLCD$NLCD_barren_land_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_barren_land_2km[i]<-NLCD$NLCD_barren_land_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_barren_land_2km[i]<-(1/2*NLCD$NLCD_barren_land_2km_2006[i])+(1/2*NLCD$NLCD_barren_land_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_barren_land_2km[i]<-NLCD$NLCD_barren_land_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_barren_land_2km[i]<-(2/3*NLCD$NLCD_barren_land_2km_2008[i])+(1/3*NLCD$NLCD_barren_land_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_barren_land_2km[i]<-(1/3*NLCD$NLCD_barren_land_2km_2008[i])+(2/3*NLCD$NLCD_barren_land_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_barren_land_2km[i]<-NLCD$NLCD_barren_land_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_barren_land_2km[i]<-(1/2*NLCD$NLCD_barren_land_2km_2011[i])+(1/2*NLCD$NLCD_barren_land_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_barren_land_2km[i]<-NLCD$NLCD_barren_land_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_barren_land_2km[i]<-(2/3*NLCD$NLCD_barren_land_2km_2013[i])+(1/3*NLCD$NLCD_barren_land_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_barren_land_2km[i]<-(1/3*NLCD$NLCD_barren_land_2km_2013[i])+(2/3*NLCD$NLCD_barren_land_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_barren_land_2km[i]<-NLCD$NLCD_barren_land_2km_2016[i]
}


NLCD$NLCD_decidious_forest_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_decidious_forest_2km[i]<-NLCD$NLCD_decidious_forest_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_decidious_forest_2km[i]<-(2/3*NLCD$NLCD_decidious_forest_2km_2001[i])+(1/3*NLCD$NLCD_decidious_forest_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_decidious_forest_2km[i]<-(1/3*NLCD$NLCD_decidious_forest_2km_2001[i])+(2/3*NLCD$NLCD_decidious_forest_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_decidious_forest_2km[i]<-NLCD$NLCD_decidious_forest_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_decidious_forest_2km[i]<-(1/2*NLCD$NLCD_decidious_forest_2km_2004[i])+(1/2*NLCD$NLCD_decidious_forest_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_decidious_forest_2km[i]<-NLCD$NLCD_decidious_forest_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_decidious_forest_2km[i]<-(1/2*NLCD$NLCD_decidious_forest_2km_2006[i])+(1/2*NLCD$NLCD_decidious_forest_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_decidious_forest_2km[i]<-NLCD$NLCD_decidious_forest_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_decidious_forest_2km[i]<-(2/3*NLCD$NLCD_decidious_forest_2km_2008[i])+(1/3*NLCD$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_decidious_forest_2km[i]<-(1/3*NLCD$NLCD_decidious_forest_2km_2008[i])+(2/3*NLCD$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_decidious_forest_2km[i]<-NLCD$NLCD_decidious_forest_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_decidious_forest_2km[i]<-(1/2*NLCD$NLCD_decidious_forest_2km_2011[i])+(1/2*NLCD$NLCD_decidious_forest_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_decidious_forest_2km[i]<-NLCD$NLCD_decidious_forest_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_decidious_forest_2km[i]<-(2/3*NLCD$NLCD_decidious_forest_2km_2013[i])+(1/3*NLCD$NLCD_decidious_forest_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_decidious_forest_2km[i]<-(1/3*NLCD$NLCD_decidious_forest_2km_2013[i])+(2/3*NLCD$NLCD_decidious_forest_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_decidious_forest_2km[i]<-NLCD$NLCD_decidious_forest_2km_2016[i]
}

NLCD$NLCD_evergreen_forest_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_evergreen_forest_2km[i]<-NLCD$NLCD_evergreen_forest_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_evergreen_forest_2km[i]<-(2/3*NLCD$NLCD_evergreen_forest_2km_2001[i])+(1/3*NLCD$NLCD_evergreen_forest_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_evergreen_forest_2km[i]<-(1/3*NLCD$NLCD_evergreen_forest_2km_2001[i])+(2/3*NLCD$NLCD_evergreen_forest_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_evergreen_forest_2km[i]<-NLCD$NLCD_evergreen_forest_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_evergreen_forest_2km[i]<-(1/2*NLCD$NLCD_evergreen_forest_2km_2004[i])+(1/2*NLCD$NLCD_evergreen_forest_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_evergreen_forest_2km[i]<-NLCD$NLCD_evergreen_forest_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_evergreen_forest_2km[i]<-(1/2*NLCD$NLCD_evergreen_forest_2km_2006[i])+(1/2*NLCD$NLCD_evergreen_forest_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_evergreen_forest_2km[i]<-NLCD$NLCD_evergreen_forest_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_evergreen_forest_2km[i]<-(2/3*NLCD$NLCD_evergreen_forest_2km_2008[i])+(1/3*NLCD$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_evergreen_forest_2km[i]<-(1/3*NLCD$NLCD_evergreen_forest_2km_2008[i])+(2/3*NLCD$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_evergreen_forest_2km[i]<-NLCD$NLCD_evergreen_forest_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_evergreen_forest_2km[i]<-(1/2*NLCD$NLCD_evergreen_forest_2km_2011[i])+(1/2*NLCD$NLCD_evergreen_forest_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_evergreen_forest_2km[i]<-NLCD$NLCD_evergreen_forest_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_evergreen_forest_2km[i]<-(2/3*NLCD$NLCD_evergreen_forest_2km_2013[i])+(1/3*NLCD$NLCD_evergreen_forest_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_evergreen_forest_2km[i]<-(1/3*NLCD$NLCD_evergreen_forest_2km_2013[i])+(2/3*NLCD$NLCD_evergreen_forest_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_evergreen_forest_2km[i]<-NLCD$NLCD_evergreen_forest_2km_2016[i]
}

NLCD$NLCD_mixed_forest_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_mixed_forest_2km[i]<-NLCD$NLCD_mixed_forest_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_mixed_forest_2km[i]<-(2/3*NLCD$NLCD_mixed_forest_2km_2001[i])+(1/3*NLCD$NLCD_mixed_forest_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_mixed_forest_2km[i]<-(1/3*NLCD$NLCD_mixed_forest_2km_2001[i])+(2/3*NLCD$NLCD_mixed_forest_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_mixed_forest_2km[i]<-NLCD$NLCD_mixed_forest_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_mixed_forest_2km[i]<-(1/2*NLCD$NLCD_mixed_forest_2km_2004[i])+(1/2*NLCD$NLCD_mixed_forest_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_mixed_forest_2km[i]<-NLCD$NLCD_mixed_forest_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_mixed_forest_2km[i]<-(1/2*NLCD$NLCD_mixed_forest_2km_2006[i])+(1/2*NLCD$NLCD_mixed_forest_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_mixed_forest_2km[i]<-NLCD$NLCD_mixed_forest_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_mixed_forest_2km[i]<-(2/3*NLCD$NLCD_mixed_forest_2km_2008[i])+(1/3*NLCD$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_mixed_forest_2km[i]<-(1/3*NLCD$NLCD_mixed_forest_2km_2008[i])+(2/3*NLCD$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_mixed_forest_2km[i]<-NLCD$NLCD_mixed_forest_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_mixed_forest_2km[i]<-(1/2*NLCD$NLCD_mixed_forest_2km_2011[i])+(1/2*NLCD$NLCD_mixed_forest_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_mixed_forest_2km[i]<-NLCD$NLCD_mixed_forest_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_mixed_forest_2km[i]<-(2/3*NLCD$NLCD_mixed_forest_2km_2013[i])+(1/3*NLCD$NLCD_mixed_forest_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_mixed_forest_2km[i]<-(1/3*NLCD$NLCD_mixed_forest_2km_2013[i])+(2/3*NLCD$NLCD_mixed_forest_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_mixed_forest_2km[i]<-NLCD$NLCD_mixed_forest_2km_2016[i]
}

NLCD$NLCD_dwarf_shrub_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_dwarf_shrub_2km[i]<-NLCD$NLCD_dwarf_shrub_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_dwarf_shrub_2km[i]<-(2/3*NLCD$NLCD_dwarf_shrub_2km_2001[i])+(1/3*NLCD$NLCD_dwarf_shrub_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_dwarf_shrub_2km[i]<-(1/3*NLCD$NLCD_dwarf_shrub_2km_2001[i])+(2/3*NLCD$NLCD_dwarf_shrub_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_dwarf_shrub_2km[i]<-NLCD$NLCD_dwarf_shrub_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_dwarf_shrub_2km[i]<-(1/2*NLCD$NLCD_dwarf_shrub_2km_2004[i])+(1/2*NLCD$NLCD_dwarf_shrub_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_dwarf_shrub_2km[i]<-NLCD$NLCD_dwarf_shrub_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_dwarf_shrub_2km[i]<-(1/2*NLCD$NLCD_dwarf_shrub_2km_2006[i])+(1/2*NLCD$NLCD_dwarf_shrub_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_dwarf_shrub_2km[i]<-NLCD$NLCD_dwarf_shrub_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_dwarf_shrub_2km[i]<-(2/3*NLCD$NLCD_dwarf_shrub_2km_2008[i])+(1/3*NLCD$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_dwarf_shrub_2km[i]<-(1/3*NLCD$NLCD_dwarf_shrub_2km_2008[i])+(2/3*NLCD$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_dwarf_shrub_2km[i]<-NLCD$NLCD_dwarf_shrub_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_dwarf_shrub_2km[i]<-(1/2*NLCD$NLCD_dwarf_shrub_2km_2011[i])+(1/2*NLCD$NLCD_dwarf_shrub_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_dwarf_shrub_2km[i]<-NLCD$NLCD_dwarf_shrub_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_dwarf_shrub_2km[i]<-(2/3*NLCD$NLCD_dwarf_shrub_2km_2013[i])+(1/3*NLCD$NLCD_dwarf_shrub_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_dwarf_shrub_2km[i]<-(1/3*NLCD$NLCD_dwarf_shrub_2km_2013[i])+(2/3*NLCD$NLCD_dwarf_shrub_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_dwarf_shrub_2km[i]<-NLCD$NLCD_dwarf_shrub_2km_2016[i]
}

NLCD$NLCD_shrub_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_shrub_2km[i]<-NLCD$NLCD_shrub_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_shrub_2km[i]<-(2/3*NLCD$NLCD_shrub_2km_2001[i])+(1/3*NLCD$NLCD_shrub_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_shrub_2km[i]<-(1/3*NLCD$NLCD_shrub_2km_2001[i])+(2/3*NLCD$NLCD_shrub_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_shrub_2km[i]<-NLCD$NLCD_shrub_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_shrub_2km[i]<-(1/2*NLCD$NLCD_shrub_2km_2004[i])+(1/2*NLCD$NLCD_shrub_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_shrub_2km[i]<-NLCD$NLCD_shrub_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_shrub_2km[i]<-(1/2*NLCD$NLCD_shrub_2km_2006[i])+(1/2*NLCD$NLCD_shrub_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_shrub_2km[i]<-NLCD$NLCD_shrub_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_shrub_2km[i]<-(2/3*NLCD$NLCD_shrub_2km_2008[i])+(1/3*NLCD$NLCD_shrub_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_shrub_2km[i]<-(1/3*NLCD$NLCD_shrub_2km_2008[i])+(2/3*NLCD$NLCD_shrub_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_shrub_2km[i]<-NLCD$NLCD_shrub_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_shrub_2km[i]<-(1/2*NLCD$NLCD_shrub_2km_2011[i])+(1/2*NLCD$NLCD_shrub_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_shrub_2km[i]<-NLCD$NLCD_shrub_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_shrub_2km[i]<-(2/3*NLCD$NLCD_shrub_2km_2013[i])+(1/3*NLCD$NLCD_shrub_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_shrub_2km[i]<-(1/3*NLCD$NLCD_shrub_2km_2013[i])+(2/3*NLCD$NLCD_shrub_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_shrub_2km[i]<-NLCD$NLCD_shrub_2km_2016[i]
}

NLCD$NLCD_grassland_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_grassland_2km[i]<-NLCD$NLCD_grassland_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_grassland_2km[i]<-(2/3*NLCD$NLCD_grassland_2km_2001[i])+(1/3*NLCD$NLCD_grassland_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_grassland_2km[i]<-(1/3*NLCD$NLCD_grassland_2km_2001[i])+(2/3*NLCD$NLCD_grassland_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_grassland_2km[i]<-NLCD$NLCD_grassland_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_grassland_2km[i]<-(1/2*NLCD$NLCD_grassland_2km_2004[i])+(1/2*NLCD$NLCD_grassland_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_grassland_2km[i]<-NLCD$NLCD_grassland_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_grassland_2km[i]<-(1/2*NLCD$NLCD_grassland_2km_2006[i])+(1/2*NLCD$NLCD_grassland_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_grassland_2km[i]<-NLCD$NLCD_grassland_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_grassland_2km[i]<-(2/3*NLCD$NLCD_grassland_2km_2008[i])+(1/3*NLCD$NLCD_grassland_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_grassland_2km[i]<-(1/3*NLCD$NLCD_grassland_2km_2008[i])+(2/3*NLCD$NLCD_grassland_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_grassland_2km[i]<-NLCD$NLCD_grassland_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_grassland_2km[i]<-(1/2*NLCD$NLCD_grassland_2km_2011[i])+(1/2*NLCD$NLCD_grassland_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_grassland_2km[i]<-NLCD$NLCD_grassland_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_grassland_2km[i]<-(2/3*NLCD$NLCD_grassland_2km_2013[i])+(1/3*NLCD$NLCD_grassland_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_grassland_2km[i]<-(1/3*NLCD$NLCD_grassland_2km_2013[i])+(2/3*NLCD$NLCD_grassland_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_grassland_2km[i]<-NLCD$NLCD_grassland_2km_2016[i]
}

NLCD$NLCD_sedge_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_sedge_2km[i]<-NLCD$NLCD_sedge_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_sedge_2km[i]<-(2/3*NLCD$NLCD_sedge_2km_2001[i])+(1/3*NLCD$NLCD_sedge_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_sedge_2km[i]<-(1/3*NLCD$NLCD_sedge_2km_2001[i])+(2/3*NLCD$NLCD_sedge_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_sedge_2km[i]<-NLCD$NLCD_sedge_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_sedge_2km[i]<-(1/2*NLCD$NLCD_sedge_2km_2004[i])+(1/2*NLCD$NLCD_sedge_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_sedge_2km[i]<-NLCD$NLCD_sedge_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_sedge_2km[i]<-(1/2*NLCD$NLCD_sedge_2km_2006[i])+(1/2*NLCD$NLCD_sedge_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_sedge_2km[i]<-NLCD$NLCD_sedge_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_sedge_2km[i]<-(2/3*NLCD$NLCD_sedge_2km_2008[i])+(1/3*NLCD$NLCD_sedge_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_sedge_2km[i]<-(1/3*NLCD$NLCD_sedge_2km_2008[i])+(2/3*NLCD$NLCD_sedge_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_sedge_2km[i]<-NLCD$NLCD_sedge_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_sedge_2km[i]<-(1/2*NLCD$NLCD_sedge_2km_2011[i])+(1/2*NLCD$NLCD_sedge_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_sedge_2km[i]<-NLCD$NLCD_sedge_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_sedge_2km[i]<-(2/3*NLCD$NLCD_sedge_2km_2013[i])+(1/3*NLCD$NLCD_sedge_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_sedge_2km[i]<-(1/3*NLCD$NLCD_sedge_2km_2013[i])+(2/3*NLCD$NLCD_sedge_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_sedge_2km[i]<-NLCD$NLCD_sedge_2km_2016[i]
}

NLCD$NLCD_lichens_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_lichens_2km[i]<-NLCD$NLCD_lichens_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_lichens_2km[i]<-(2/3*NLCD$NLCD_lichens_2km_2001[i])+(1/3*NLCD$NLCD_lichens_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_lichens_2km[i]<-(1/3*NLCD$NLCD_lichens_2km_2001[i])+(2/3*NLCD$NLCD_lichens_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_lichens_2km[i]<-NLCD$NLCD_lichens_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_lichens_2km[i]<-(1/2*NLCD$NLCD_lichens_2km_2004[i])+(1/2*NLCD$NLCD_lichens_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_lichens_2km[i]<-NLCD$NLCD_lichens_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_lichens_2km[i]<-(1/2*NLCD$NLCD_lichens_2km_2006[i])+(1/2*NLCD$NLCD_lichens_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_lichens_2km[i]<-NLCD$NLCD_lichens_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_lichens_2km[i]<-(2/3*NLCD$NLCD_lichens_2km_2008[i])+(1/3*NLCD$NLCD_lichens_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_lichens_2km[i]<-(1/3*NLCD$NLCD_lichens_2km_2008[i])+(2/3*NLCD$NLCD_lichens_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_lichens_2km[i]<-NLCD$NLCD_lichens_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_lichens_2km[i]<-(1/2*NLCD$NLCD_lichens_2km_2011[i])+(1/2*NLCD$NLCD_lichens_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_lichens_2km[i]<-NLCD$NLCD_lichens_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_lichens_2km[i]<-(2/3*NLCD$NLCD_lichens_2km_2013[i])+(1/3*NLCD$NLCD_lichens_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_lichens_2km[i]<-(1/3*NLCD$NLCD_lichens_2km_2013[i])+(2/3*NLCD$NLCD_lichens_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_lichens_2km[i]<-NLCD$NLCD_lichens_2km_2016[i]
}

NLCD$NLCD_moss_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_moss_2km[i]<-NLCD$NLCD_moss_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_moss_2km[i]<-(2/3*NLCD$NLCD_moss_2km_2001[i])+(1/3*NLCD$NLCD_moss_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_moss_2km[i]<-(1/3*NLCD$NLCD_moss_2km_2001[i])+(2/3*NLCD$NLCD_moss_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_moss_2km[i]<-NLCD$NLCD_moss_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_moss_2km[i]<-(1/2*NLCD$NLCD_moss_2km_2004[i])+(1/2*NLCD$NLCD_moss_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_moss_2km[i]<-NLCD$NLCD_moss_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_moss_2km[i]<-(1/2*NLCD$NLCD_moss_2km_2006[i])+(1/2*NLCD$NLCD_moss_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_moss_2km[i]<-NLCD$NLCD_moss_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_moss_2km[i]<-(2/3*NLCD$NLCD_moss_2km_2008[i])+(1/3*NLCD$NLCD_moss_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_moss_2km[i]<-(1/3*NLCD$NLCD_moss_2km_2008[i])+(2/3*NLCD$NLCD_moss_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_moss_2km[i]<-NLCD$NLCD_moss_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_moss_2km[i]<-(1/2*NLCD$NLCD_moss_2km_2011[i])+(1/2*NLCD$NLCD_moss_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_moss_2km[i]<-NLCD$NLCD_moss_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_moss_2km[i]<-(2/3*NLCD$NLCD_moss_2km_2013[i])+(1/3*NLCD$NLCD_moss_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_moss_2km[i]<-(1/3*NLCD$NLCD_moss_2km_2013[i])+(2/3*NLCD$NLCD_moss_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_moss_2km[i]<-NLCD$NLCD_moss_2km_2016[i]
}

NLCD$NLCD_pasture_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_pasture_2km[i]<-NLCD$NLCD_pasture_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_pasture_2km[i]<-(2/3*NLCD$NLCD_pasture_2km_2001[i])+(1/3*NLCD$NLCD_pasture_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_pasture_2km[i]<-(1/3*NLCD$NLCD_pasture_2km_2001[i])+(2/3*NLCD$NLCD_pasture_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_pasture_2km[i]<-NLCD$NLCD_pasture_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_pasture_2km[i]<-(1/2*NLCD$NLCD_pasture_2km_2004[i])+(1/2*NLCD$NLCD_pasture_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_pasture_2km[i]<-NLCD$NLCD_pasture_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_pasture_2km[i]<-(1/2*NLCD$NLCD_pasture_2km_2006[i])+(1/2*NLCD$NLCD_pasture_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_pasture_2km[i]<-NLCD$NLCD_pasture_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_pasture_2km[i]<-(2/3*NLCD$NLCD_pasture_2km_2008[i])+(1/3*NLCD$NLCD_pasture_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_pasture_2km[i]<-(1/3*NLCD$NLCD_pasture_2km_2008[i])+(2/3*NLCD$NLCD_pasture_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_pasture_2km[i]<-NLCD$NLCD_pasture_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_pasture_2km[i]<-(1/2*NLCD$NLCD_pasture_2km_2011[i])+(1/2*NLCD$NLCD_pasture_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_pasture_2km[i]<-NLCD$NLCD_pasture_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_pasture_2km[i]<-(2/3*NLCD$NLCD_pasture_2km_2013[i])+(1/3*NLCD$NLCD_pasture_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_pasture_2km[i]<-(1/3*NLCD$NLCD_pasture_2km_2013[i])+(2/3*NLCD$NLCD_pasture_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_pasture_2km[i]<-NLCD$NLCD_pasture_2km_2016[i]
}

NLCD$NLCD_cultivated_crops_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_cultivated_crops_2km[i]<-NLCD$NLCD_cultivated_crops_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_cultivated_crops_2km[i]<-(2/3*NLCD$NLCD_cultivated_crops_2km_2001[i])+(1/3*NLCD$NLCD_cultivated_crops_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_cultivated_crops_2km[i]<-(1/3*NLCD$NLCD_cultivated_crops_2km_2001[i])+(2/3*NLCD$NLCD_cultivated_crops_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_cultivated_crops_2km[i]<-NLCD$NLCD_cultivated_crops_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_cultivated_crops_2km[i]<-(1/2*NLCD$NLCD_cultivated_crops_2km_2004[i])+(1/2*NLCD$NLCD_cultivated_crops_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_cultivated_crops_2km[i]<-NLCD$NLCD_cultivated_crops_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_cultivated_crops_2km[i]<-(1/2*NLCD$NLCD_cultivated_crops_2km_2006[i])+(1/2*NLCD$NLCD_cultivated_crops_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_cultivated_crops_2km[i]<-NLCD$NLCD_cultivated_crops_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_cultivated_crops_2km[i]<-(2/3*NLCD$NLCD_cultivated_crops_2km_2008[i])+(1/3*NLCD$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_cultivated_crops_2km[i]<-(1/3*NLCD$NLCD_cultivated_crops_2km_2008[i])+(2/3*NLCD$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_cultivated_crops_2km[i]<-NLCD$NLCD_cultivated_crops_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_cultivated_crops_2km[i]<-(1/2*NLCD$NLCD_cultivated_crops_2km_2011[i])+(1/2*NLCD$NLCD_cultivated_crops_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_cultivated_crops_2km[i]<-NLCD$NLCD_cultivated_crops_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_cultivated_crops_2km[i]<-(2/3*NLCD$NLCD_cultivated_crops_2km_2013[i])+(1/3*NLCD$NLCD_cultivated_crops_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_cultivated_crops_2km[i]<-(1/3*NLCD$NLCD_cultivated_crops_2km_2013[i])+(2/3*NLCD$NLCD_cultivated_crops_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_cultivated_crops_2km[i]<-NLCD$NLCD_cultivated_crops_2km_2016[i]
}

NLCD$NLCD_woody_wetland_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_woody_wetland_2km[i]<-NLCD$NLCD_woody_wetland_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_woody_wetland_2km[i]<-(2/3*NLCD$NLCD_woody_wetland_2km_2001[i])+(1/3*NLCD$NLCD_woody_wetland_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_woody_wetland_2km[i]<-(1/3*NLCD$NLCD_woody_wetland_2km_2001[i])+(2/3*NLCD$NLCD_woody_wetland_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_woody_wetland_2km[i]<-NLCD$NLCD_woody_wetland_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_woody_wetland_2km[i]<-(1/2*NLCD$NLCD_woody_wetland_2km_2004[i])+(1/2*NLCD$NLCD_woody_wetland_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_woody_wetland_2km[i]<-NLCD$NLCD_woody_wetland_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_woody_wetland_2km[i]<-(1/2*NLCD$NLCD_woody_wetland_2km_2006[i])+(1/2*NLCD$NLCD_woody_wetland_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_woody_wetland_2km[i]<-NLCD$NLCD_woody_wetland_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_woody_wetland_2km[i]<-(2/3*NLCD$NLCD_woody_wetland_2km_2008[i])+(1/3*NLCD$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_woody_wetland_2km[i]<-(1/3*NLCD$NLCD_woody_wetland_2km_2008[i])+(2/3*NLCD$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_woody_wetland_2km[i]<-NLCD$NLCD_woody_wetland_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_woody_wetland_2km[i]<-(1/2*NLCD$NLCD_woody_wetland_2km_2011[i])+(1/2*NLCD$NLCD_woody_wetland_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_woody_wetland_2km[i]<-NLCD$NLCD_woody_wetland_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_woody_wetland_2km[i]<-(2/3*NLCD$NLCD_woody_wetland_2km_2013[i])+(1/3*NLCD$NLCD_woody_wetland_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_woody_wetland_2km[i]<-(1/3*NLCD$NLCD_woody_wetland_2km_2013[i])+(2/3*NLCD$NLCD_woody_wetland_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_woody_wetland_2km[i]<-NLCD$NLCD_woody_wetland_2km_2016[i]
}

NLCD$NLCD_emergent_herbaceous_wetlands_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2004[i])+(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2006[i])+(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2008[i])+(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2008[i])+(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2011[i])+(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2013[i])+(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2013[i])+(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_2km_2016[i]
}

NLCD$NLCD_unkown_2km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_unkown_2km[i]<-NLCD$NLCD_unkown_2km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_unkown_2km[i]<-(2/3*NLCD$NLCD_unkown_2km_2001[i])+(1/3*NLCD$NLCD_unkown_2km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_unkown_2km[i]<-(1/3*NLCD$NLCD_unkown_2km_2001[i])+(2/3*NLCD$NLCD_unkown_2km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_unkown_2km[i]<-NLCD$NLCD_unkown_2km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_unkown_2km[i]<-(1/2*NLCD$NLCD_unkown_2km_2004[i])+(1/2*NLCD$NLCD_unkown_2km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_unkown_2km[i]<-NLCD$NLCD_unkown_2km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_unkown_2km[i]<-(1/2*NLCD$NLCD_unkown_2km_2006[i])+(1/2*NLCD$NLCD_unkown_2km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_unkown_2km[i]<-NLCD$NLCD_unkown_2km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_unkown_2km[i]<-(2/3*NLCD$NLCD_unkown_2km_2008[i])+(1/3*NLCD$NLCD_unkown_2km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_unkown_2km[i]<-(1/3*NLCD$NLCD_unkown_2km_2008[i])+(2/3*NLCD$NLCD_unkown_2km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_unkown_2km[i]<-NLCD$NLCD_unkown_2km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_unkown_2km[i]<-(1/2*NLCD$NLCD_unkown_2km_2011[i])+(1/2*NLCD$NLCD_unkown_2km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_unkown_2km[i]<-NLCD$NLCD_unkown_2km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_unkown_2km[i]<-(2/3*NLCD$NLCD_unkown_2km_2013[i])+(1/3*NLCD$NLCD_unkown_2km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_unkown_2km[i]<-(1/3*NLCD$NLCD_unkown_2km_2013[i])+(2/3*NLCD$NLCD_unkown_2km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_unkown_2km[i]<-NLCD$NLCD_unkown_2km_2016[i]
}

###1km
###For Open water
NLCD$NLCD_open_water_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_open_water_1km[i]<-NLCD$NLCD_open_water_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_open_water_1km[i]<-(2/3*NLCD$NLCD_open_water_1km_2001[i])+(1/3*NLCD$NLCD_open_water_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_open_water_1km[i]<-(1/3*NLCD$NLCD_open_water_1km_2001[i])+(2/3*NLCD$NLCD_open_water_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_open_water_1km[i]<-NLCD$NLCD_open_water_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_open_water_1km[i]<-(1/2*NLCD$NLCD_open_water_1km_2004[i])+(1/2*NLCD$NLCD_open_water_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_open_water_1km[i]<-NLCD$NLCD_open_water_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_open_water_1km[i]<-(1/2*NLCD$NLCD_open_water_1km_2006[i])+(1/2*NLCD$NLCD_open_water_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_open_water_1km[i]<-NLCD$NLCD_open_water_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_open_water_1km[i]<-(2/3*NLCD$NLCD_open_water_1km_2008[i])+(1/3*NLCD$NLCD_open_water_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_open_water_1km[i]<-(1/3*NLCD$NLCD_open_water_1km_2008[i])+(2/3*NLCD$NLCD_open_water_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_open_water_1km[i]<-NLCD$NLCD_open_water_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_open_water_1km[i]<-(1/2*NLCD$NLCD_open_water_1km_2011[i])+(1/2*NLCD$NLCD_open_water_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_open_water_1km[i]<-NLCD$NLCD_open_water_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_open_water_1km[i]<-(2/3*NLCD$NLCD_open_water_1km_2013[i])+(1/3*NLCD$NLCD_open_water_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_open_water_1km[i]<-(1/3*NLCD$NLCD_open_water_1km_2013[i])+(2/3*NLCD$NLCD_open_water_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_open_water_1km[i]<-NLCD$NLCD_open_water_1km_2016[i]
}

NLCD$NLCD_ice_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_ice_1km[i]<-NLCD$NLCD_ice_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_ice_1km[i]<-(2/3*NLCD$NLCD_ice_1km_2001[i])+(1/3*NLCD$NLCD_ice_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_ice_1km[i]<-(1/3*NLCD$NLCD_ice_1km_2001[i])+(2/3*NLCD$NLCD_ice_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_ice_1km[i]<-NLCD$NLCD_ice_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_ice_1km[i]<-(1/2*NLCD$NLCD_ice_1km_2004[i])+(1/2*NLCD$NLCD_ice_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_ice_1km[i]<-NLCD$NLCD_ice_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_ice_1km[i]<-(1/2*NLCD$NLCD_ice_1km_2006[i])+(1/2*NLCD$NLCD_ice_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_ice_1km[i]<-NLCD$NLCD_ice_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_ice_1km[i]<-(2/3*NLCD$NLCD_ice_1km_2008[i])+(1/3*NLCD$NLCD_ice_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_ice_1km[i]<-(1/3*NLCD$NLCD_ice_1km_2008[i])+(2/3*NLCD$NLCD_ice_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_ice_1km[i]<-NLCD$NLCD_ice_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_ice_1km[i]<-(1/2*NLCD$NLCD_ice_1km_2011[i])+(1/2*NLCD$NLCD_ice_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_ice_1km[i]<-NLCD$NLCD_ice_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_ice_1km[i]<-(2/3*NLCD$NLCD_ice_1km_2013[i])+(1/3*NLCD$NLCD_ice_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_ice_1km[i]<-(1/3*NLCD$NLCD_ice_1km_2013[i])+(2/3*NLCD$NLCD_ice_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_ice_1km[i]<-NLCD$NLCD_ice_1km_2016[i]
}



NLCD$NLCD_developed_open_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_open_1km[i]<-NLCD$NLCD_developed_open_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_open_1km[i]<-(2/3*NLCD$NLCD_developed_open_1km_2001[i])+(1/3*NLCD$NLCD_developed_open_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_open_1km[i]<-(1/3*NLCD$NLCD_developed_open_1km_2001[i])+(2/3*NLCD$NLCD_developed_open_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_open_1km[i]<-NLCD$NLCD_developed_open_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_open_1km[i]<-(1/2*NLCD$NLCD_developed_open_1km_2004[i])+(1/2*NLCD$NLCD_developed_open_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_open_1km[i]<-NLCD$NLCD_developed_open_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_open_1km[i]<-(1/2*NLCD$NLCD_developed_open_1km_2006[i])+(1/2*NLCD$NLCD_developed_open_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_open_1km[i]<-NLCD$NLCD_developed_open_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_open_1km[i]<-(2/3*NLCD$NLCD_developed_open_1km_2008[i])+(1/3*NLCD$NLCD_developed_open_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_open_1km[i]<-(1/3*NLCD$NLCD_developed_open_1km_2008[i])+(2/3*NLCD$NLCD_developed_open_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_open_1km[i]<-NLCD$NLCD_developed_open_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_open_1km[i]<-(1/2*NLCD$NLCD_developed_open_1km_2011[i])+(1/2*NLCD$NLCD_developed_open_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_open_1km[i]<-NLCD$NLCD_developed_open_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_open_1km[i]<-(2/3*NLCD$NLCD_developed_open_1km_2013[i])+(1/3*NLCD$NLCD_developed_open_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_open_1km[i]<-(1/3*NLCD$NLCD_developed_open_1km_2013[i])+(2/3*NLCD$NLCD_developed_open_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_open_1km[i]<-NLCD$NLCD_developed_open_1km_2016[i]
}

NLCD$NLCD_developed_low_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_low_1km[i]<-NLCD$NLCD_developed_low_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_low_1km[i]<-(2/3*NLCD$NLCD_developed_low_1km_2001[i])+(1/3*NLCD$NLCD_developed_low_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_low_1km[i]<-(1/3*NLCD$NLCD_developed_low_1km_2001[i])+(2/3*NLCD$NLCD_developed_low_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_low_1km[i]<-NLCD$NLCD_developed_low_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_low_1km[i]<-(1/2*NLCD$NLCD_developed_low_1km_2004[i])+(1/2*NLCD$NLCD_developed_low_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_low_1km[i]<-NLCD$NLCD_developed_low_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_low_1km[i]<-(1/2*NLCD$NLCD_developed_low_1km_2006[i])+(1/2*NLCD$NLCD_developed_low_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_low_1km[i]<-NLCD$NLCD_developed_low_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_low_1km[i]<-(2/3*NLCD$NLCD_developed_low_1km_2008[i])+(1/3*NLCD$NLCD_developed_low_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_low_1km[i]<-(1/3*NLCD$NLCD_developed_low_1km_2008[i])+(2/3*NLCD$NLCD_developed_low_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_low_1km[i]<-NLCD$NLCD_developed_low_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_low_1km[i]<-(1/2*NLCD$NLCD_developed_low_1km_2011[i])+(1/2*NLCD$NLCD_developed_low_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_low_1km[i]<-NLCD$NLCD_developed_low_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_low_1km[i]<-(2/3*NLCD$NLCD_developed_low_1km_2013[i])+(1/3*NLCD$NLCD_developed_low_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_low_1km[i]<-(1/3*NLCD$NLCD_developed_low_1km_2013[i])+(2/3*NLCD$NLCD_developed_low_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_low_1km[i]<-NLCD$NLCD_developed_low_1km_2016[i]
}

NLCD$NLCD_developed_medium_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_medium_1km[i]<-NLCD$NLCD_developed_medium_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_medium_1km[i]<-(2/3*NLCD$NLCD_developed_medium_1km_2001[i])+(1/3*NLCD$NLCD_developed_medium_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_medium_1km[i]<-(1/3*NLCD$NLCD_developed_medium_1km_2001[i])+(2/3*NLCD$NLCD_developed_medium_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_medium_1km[i]<-NLCD$NLCD_developed_medium_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_medium_1km[i]<-(1/2*NLCD$NLCD_developed_medium_1km_2004[i])+(1/2*NLCD$NLCD_developed_medium_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_medium_1km[i]<-NLCD$NLCD_developed_medium_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_medium_1km[i]<-(1/2*NLCD$NLCD_developed_medium_1km_2006[i])+(1/2*NLCD$NLCD_developed_medium_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_medium_1km[i]<-NLCD$NLCD_developed_medium_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_medium_1km[i]<-(2/3*NLCD$NLCD_developed_medium_1km_2008[i])+(1/3*NLCD$NLCD_developed_medium_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_medium_1km[i]<-(1/3*NLCD$NLCD_developed_medium_1km_2008[i])+(2/3*NLCD$NLCD_developed_medium_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_medium_1km[i]<-NLCD$NLCD_developed_medium_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_medium_1km[i]<-(1/2*NLCD$NLCD_developed_medium_1km_2011[i])+(1/2*NLCD$NLCD_developed_medium_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_medium_1km[i]<-NLCD$NLCD_developed_medium_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_medium_1km[i]<-(2/3*NLCD$NLCD_developed_medium_1km_2013[i])+(1/3*NLCD$NLCD_developed_medium_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_medium_1km[i]<-(1/3*NLCD$NLCD_developed_medium_1km_2013[i])+(2/3*NLCD$NLCD_developed_medium_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_medium_1km[i]<-NLCD$NLCD_developed_medium_1km_2016[i]
}

NLCD$NLCD_developed_high_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_high_1km[i]<-NLCD$NLCD_developed_high_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_high_1km[i]<-(2/3*NLCD$NLCD_developed_high_1km_2001[i])+(1/3*NLCD$NLCD_developed_high_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_high_1km[i]<-(1/3*NLCD$NLCD_developed_high_1km_2001[i])+(2/3*NLCD$NLCD_developed_high_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_high_1km[i]<-NLCD$NLCD_developed_high_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_high_1km[i]<-(1/2*NLCD$NLCD_developed_high_1km_2004[i])+(1/2*NLCD$NLCD_developed_high_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_high_1km[i]<-NLCD$NLCD_developed_high_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_high_1km[i]<-(1/2*NLCD$NLCD_developed_high_1km_2006[i])+(1/2*NLCD$NLCD_developed_high_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_high_1km[i]<-NLCD$NLCD_developed_high_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_high_1km[i]<-(2/3*NLCD$NLCD_developed_high_1km_2008[i])+(1/3*NLCD$NLCD_developed_high_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_high_1km[i]<-(1/3*NLCD$NLCD_developed_high_1km_2008[i])+(2/3*NLCD$NLCD_developed_high_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_high_1km[i]<-NLCD$NLCD_developed_high_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_high_1km[i]<-(1/2*NLCD$NLCD_developed_high_1km_2011[i])+(1/2*NLCD$NLCD_developed_high_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_high_1km[i]<-NLCD$NLCD_developed_high_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_high_1km[i]<-(2/3*NLCD$NLCD_developed_high_1km_2013[i])+(1/3*NLCD$NLCD_developed_high_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_high_1km[i]<-(1/3*NLCD$NLCD_developed_high_1km_2013[i])+(2/3*NLCD$NLCD_developed_high_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_high_1km[i]<-NLCD$NLCD_developed_high_1km_2016[i]
}

NLCD$NLCD_barren_land_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_barren_land_1km[i]<-NLCD$NLCD_barren_land_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_barren_land_1km[i]<-(2/3*NLCD$NLCD_barren_land_1km_2001[i])+(1/3*NLCD$NLCD_barren_land_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_barren_land_1km[i]<-(1/3*NLCD$NLCD_barren_land_1km_2001[i])+(2/3*NLCD$NLCD_barren_land_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_barren_land_1km[i]<-NLCD$NLCD_barren_land_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_barren_land_1km[i]<-(1/2*NLCD$NLCD_barren_land_1km_2004[i])+(1/2*NLCD$NLCD_barren_land_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_barren_land_1km[i]<-NLCD$NLCD_barren_land_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_barren_land_1km[i]<-(1/2*NLCD$NLCD_barren_land_1km_2006[i])+(1/2*NLCD$NLCD_barren_land_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_barren_land_1km[i]<-NLCD$NLCD_barren_land_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_barren_land_1km[i]<-(2/3*NLCD$NLCD_barren_land_1km_2008[i])+(1/3*NLCD$NLCD_barren_land_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_barren_land_1km[i]<-(1/3*NLCD$NLCD_barren_land_1km_2008[i])+(2/3*NLCD$NLCD_barren_land_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_barren_land_1km[i]<-NLCD$NLCD_barren_land_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_barren_land_1km[i]<-(1/2*NLCD$NLCD_barren_land_1km_2011[i])+(1/2*NLCD$NLCD_barren_land_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_barren_land_1km[i]<-NLCD$NLCD_barren_land_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_barren_land_1km[i]<-(2/3*NLCD$NLCD_barren_land_1km_2013[i])+(1/3*NLCD$NLCD_barren_land_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_barren_land_1km[i]<-(1/3*NLCD$NLCD_barren_land_1km_2013[i])+(2/3*NLCD$NLCD_barren_land_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_barren_land_1km[i]<-NLCD$NLCD_barren_land_1km_2016[i]
}


NLCD$NLCD_decidious_forest_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_decidious_forest_1km[i]<-NLCD$NLCD_decidious_forest_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_decidious_forest_1km[i]<-(2/3*NLCD$NLCD_decidious_forest_1km_2001[i])+(1/3*NLCD$NLCD_decidious_forest_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_decidious_forest_1km[i]<-(1/3*NLCD$NLCD_decidious_forest_1km_2001[i])+(2/3*NLCD$NLCD_decidious_forest_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_decidious_forest_1km[i]<-NLCD$NLCD_decidious_forest_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_decidious_forest_1km[i]<-(1/2*NLCD$NLCD_decidious_forest_1km_2004[i])+(1/2*NLCD$NLCD_decidious_forest_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_decidious_forest_1km[i]<-NLCD$NLCD_decidious_forest_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_decidious_forest_1km[i]<-(1/2*NLCD$NLCD_decidious_forest_1km_2006[i])+(1/2*NLCD$NLCD_decidious_forest_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_decidious_forest_1km[i]<-NLCD$NLCD_decidious_forest_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_decidious_forest_1km[i]<-(2/3*NLCD$NLCD_decidious_forest_1km_2008[i])+(1/3*NLCD$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_decidious_forest_1km[i]<-(1/3*NLCD$NLCD_decidious_forest_1km_2008[i])+(2/3*NLCD$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_decidious_forest_1km[i]<-NLCD$NLCD_decidious_forest_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_decidious_forest_1km[i]<-(1/2*NLCD$NLCD_decidious_forest_1km_2011[i])+(1/2*NLCD$NLCD_decidious_forest_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_decidious_forest_1km[i]<-NLCD$NLCD_decidious_forest_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_decidious_forest_1km[i]<-(2/3*NLCD$NLCD_decidious_forest_1km_2013[i])+(1/3*NLCD$NLCD_decidious_forest_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_decidious_forest_1km[i]<-(1/3*NLCD$NLCD_decidious_forest_1km_2013[i])+(2/3*NLCD$NLCD_decidious_forest_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_decidious_forest_1km[i]<-NLCD$NLCD_decidious_forest_1km_2016[i]
}

NLCD$NLCD_evergreen_forest_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_evergreen_forest_1km[i]<-NLCD$NLCD_evergreen_forest_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_evergreen_forest_1km[i]<-(2/3*NLCD$NLCD_evergreen_forest_1km_2001[i])+(1/3*NLCD$NLCD_evergreen_forest_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_evergreen_forest_1km[i]<-(1/3*NLCD$NLCD_evergreen_forest_1km_2001[i])+(2/3*NLCD$NLCD_evergreen_forest_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_evergreen_forest_1km[i]<-NLCD$NLCD_evergreen_forest_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_evergreen_forest_1km[i]<-(1/2*NLCD$NLCD_evergreen_forest_1km_2004[i])+(1/2*NLCD$NLCD_evergreen_forest_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_evergreen_forest_1km[i]<-NLCD$NLCD_evergreen_forest_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_evergreen_forest_1km[i]<-(1/2*NLCD$NLCD_evergreen_forest_1km_2006[i])+(1/2*NLCD$NLCD_evergreen_forest_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_evergreen_forest_1km[i]<-NLCD$NLCD_evergreen_forest_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_evergreen_forest_1km[i]<-(2/3*NLCD$NLCD_evergreen_forest_1km_2008[i])+(1/3*NLCD$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_evergreen_forest_1km[i]<-(1/3*NLCD$NLCD_evergreen_forest_1km_2008[i])+(2/3*NLCD$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_evergreen_forest_1km[i]<-NLCD$NLCD_evergreen_forest_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_evergreen_forest_1km[i]<-(1/2*NLCD$NLCD_evergreen_forest_1km_2011[i])+(1/2*NLCD$NLCD_evergreen_forest_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_evergreen_forest_1km[i]<-NLCD$NLCD_evergreen_forest_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_evergreen_forest_1km[i]<-(2/3*NLCD$NLCD_evergreen_forest_1km_2013[i])+(1/3*NLCD$NLCD_evergreen_forest_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_evergreen_forest_1km[i]<-(1/3*NLCD$NLCD_evergreen_forest_1km_2013[i])+(2/3*NLCD$NLCD_evergreen_forest_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_evergreen_forest_1km[i]<-NLCD$NLCD_evergreen_forest_1km_2016[i]
}

NLCD$NLCD_mixed_forest_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_mixed_forest_1km[i]<-NLCD$NLCD_mixed_forest_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_mixed_forest_1km[i]<-(2/3*NLCD$NLCD_mixed_forest_1km_2001[i])+(1/3*NLCD$NLCD_mixed_forest_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_mixed_forest_1km[i]<-(1/3*NLCD$NLCD_mixed_forest_1km_2001[i])+(2/3*NLCD$NLCD_mixed_forest_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_mixed_forest_1km[i]<-NLCD$NLCD_mixed_forest_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_mixed_forest_1km[i]<-(1/2*NLCD$NLCD_mixed_forest_1km_2004[i])+(1/2*NLCD$NLCD_mixed_forest_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_mixed_forest_1km[i]<-NLCD$NLCD_mixed_forest_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_mixed_forest_1km[i]<-(1/2*NLCD$NLCD_mixed_forest_1km_2006[i])+(1/2*NLCD$NLCD_mixed_forest_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_mixed_forest_1km[i]<-NLCD$NLCD_mixed_forest_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_mixed_forest_1km[i]<-(2/3*NLCD$NLCD_mixed_forest_1km_2008[i])+(1/3*NLCD$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_mixed_forest_1km[i]<-(1/3*NLCD$NLCD_mixed_forest_1km_2008[i])+(2/3*NLCD$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_mixed_forest_1km[i]<-NLCD$NLCD_mixed_forest_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_mixed_forest_1km[i]<-(1/2*NLCD$NLCD_mixed_forest_1km_2011[i])+(1/2*NLCD$NLCD_mixed_forest_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_mixed_forest_1km[i]<-NLCD$NLCD_mixed_forest_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_mixed_forest_1km[i]<-(2/3*NLCD$NLCD_mixed_forest_1km_2013[i])+(1/3*NLCD$NLCD_mixed_forest_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_mixed_forest_1km[i]<-(1/3*NLCD$NLCD_mixed_forest_1km_2013[i])+(2/3*NLCD$NLCD_mixed_forest_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_mixed_forest_1km[i]<-NLCD$NLCD_mixed_forest_1km_2016[i]
}

NLCD$NLCD_dwarf_shrub_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_dwarf_shrub_1km[i]<-NLCD$NLCD_dwarf_shrub_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_dwarf_shrub_1km[i]<-(2/3*NLCD$NLCD_dwarf_shrub_1km_2001[i])+(1/3*NLCD$NLCD_dwarf_shrub_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_dwarf_shrub_1km[i]<-(1/3*NLCD$NLCD_dwarf_shrub_1km_2001[i])+(2/3*NLCD$NLCD_dwarf_shrub_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_dwarf_shrub_1km[i]<-NLCD$NLCD_dwarf_shrub_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_dwarf_shrub_1km[i]<-(1/2*NLCD$NLCD_dwarf_shrub_1km_2004[i])+(1/2*NLCD$NLCD_dwarf_shrub_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_dwarf_shrub_1km[i]<-NLCD$NLCD_dwarf_shrub_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_dwarf_shrub_1km[i]<-(1/2*NLCD$NLCD_dwarf_shrub_1km_2006[i])+(1/2*NLCD$NLCD_dwarf_shrub_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_dwarf_shrub_1km[i]<-NLCD$NLCD_dwarf_shrub_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_dwarf_shrub_1km[i]<-(2/3*NLCD$NLCD_dwarf_shrub_1km_2008[i])+(1/3*NLCD$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_dwarf_shrub_1km[i]<-(1/3*NLCD$NLCD_dwarf_shrub_1km_2008[i])+(2/3*NLCD$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_dwarf_shrub_1km[i]<-NLCD$NLCD_dwarf_shrub_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_dwarf_shrub_1km[i]<-(1/2*NLCD$NLCD_dwarf_shrub_1km_2011[i])+(1/2*NLCD$NLCD_dwarf_shrub_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_dwarf_shrub_1km[i]<-NLCD$NLCD_dwarf_shrub_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_dwarf_shrub_1km[i]<-(2/3*NLCD$NLCD_dwarf_shrub_1km_2013[i])+(1/3*NLCD$NLCD_dwarf_shrub_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_dwarf_shrub_1km[i]<-(1/3*NLCD$NLCD_dwarf_shrub_1km_2013[i])+(2/3*NLCD$NLCD_dwarf_shrub_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_dwarf_shrub_1km[i]<-NLCD$NLCD_dwarf_shrub_1km_2016[i]
}

NLCD$NLCD_shrub_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_shrub_1km[i]<-NLCD$NLCD_shrub_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_shrub_1km[i]<-(2/3*NLCD$NLCD_shrub_1km_2001[i])+(1/3*NLCD$NLCD_shrub_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_shrub_1km[i]<-(1/3*NLCD$NLCD_shrub_1km_2001[i])+(2/3*NLCD$NLCD_shrub_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_shrub_1km[i]<-NLCD$NLCD_shrub_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_shrub_1km[i]<-(1/2*NLCD$NLCD_shrub_1km_2004[i])+(1/2*NLCD$NLCD_shrub_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_shrub_1km[i]<-NLCD$NLCD_shrub_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_shrub_1km[i]<-(1/2*NLCD$NLCD_shrub_1km_2006[i])+(1/2*NLCD$NLCD_shrub_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_shrub_1km[i]<-NLCD$NLCD_shrub_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_shrub_1km[i]<-(2/3*NLCD$NLCD_shrub_1km_2008[i])+(1/3*NLCD$NLCD_shrub_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_shrub_1km[i]<-(1/3*NLCD$NLCD_shrub_1km_2008[i])+(2/3*NLCD$NLCD_shrub_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_shrub_1km[i]<-NLCD$NLCD_shrub_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_shrub_1km[i]<-(1/2*NLCD$NLCD_shrub_1km_2011[i])+(1/2*NLCD$NLCD_shrub_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_shrub_1km[i]<-NLCD$NLCD_shrub_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_shrub_1km[i]<-(2/3*NLCD$NLCD_shrub_1km_2013[i])+(1/3*NLCD$NLCD_shrub_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_shrub_1km[i]<-(1/3*NLCD$NLCD_shrub_1km_2013[i])+(2/3*NLCD$NLCD_shrub_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_shrub_1km[i]<-NLCD$NLCD_shrub_1km_2016[i]
}

NLCD$NLCD_grassland_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_grassland_1km[i]<-NLCD$NLCD_grassland_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_grassland_1km[i]<-(2/3*NLCD$NLCD_grassland_1km_2001[i])+(1/3*NLCD$NLCD_grassland_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_grassland_1km[i]<-(1/3*NLCD$NLCD_grassland_1km_2001[i])+(2/3*NLCD$NLCD_grassland_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_grassland_1km[i]<-NLCD$NLCD_grassland_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_grassland_1km[i]<-(1/2*NLCD$NLCD_grassland_1km_2004[i])+(1/2*NLCD$NLCD_grassland_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_grassland_1km[i]<-NLCD$NLCD_grassland_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_grassland_1km[i]<-(1/2*NLCD$NLCD_grassland_1km_2006[i])+(1/2*NLCD$NLCD_grassland_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_grassland_1km[i]<-NLCD$NLCD_grassland_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_grassland_1km[i]<-(2/3*NLCD$NLCD_grassland_1km_2008[i])+(1/3*NLCD$NLCD_grassland_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_grassland_1km[i]<-(1/3*NLCD$NLCD_grassland_1km_2008[i])+(2/3*NLCD$NLCD_grassland_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_grassland_1km[i]<-NLCD$NLCD_grassland_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_grassland_1km[i]<-(1/2*NLCD$NLCD_grassland_1km_2011[i])+(1/2*NLCD$NLCD_grassland_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_grassland_1km[i]<-NLCD$NLCD_grassland_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_grassland_1km[i]<-(2/3*NLCD$NLCD_grassland_1km_2013[i])+(1/3*NLCD$NLCD_grassland_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_grassland_1km[i]<-(1/3*NLCD$NLCD_grassland_1km_2013[i])+(2/3*NLCD$NLCD_grassland_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_grassland_1km[i]<-NLCD$NLCD_grassland_1km_2016[i]
}

NLCD$NLCD_sedge_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_sedge_1km[i]<-NLCD$NLCD_sedge_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_sedge_1km[i]<-(2/3*NLCD$NLCD_sedge_1km_2001[i])+(1/3*NLCD$NLCD_sedge_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_sedge_1km[i]<-(1/3*NLCD$NLCD_sedge_1km_2001[i])+(2/3*NLCD$NLCD_sedge_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_sedge_1km[i]<-NLCD$NLCD_sedge_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_sedge_1km[i]<-(1/2*NLCD$NLCD_sedge_1km_2004[i])+(1/2*NLCD$NLCD_sedge_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_sedge_1km[i]<-NLCD$NLCD_sedge_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_sedge_1km[i]<-(1/2*NLCD$NLCD_sedge_1km_2006[i])+(1/2*NLCD$NLCD_sedge_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_sedge_1km[i]<-NLCD$NLCD_sedge_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_sedge_1km[i]<-(2/3*NLCD$NLCD_sedge_1km_2008[i])+(1/3*NLCD$NLCD_sedge_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_sedge_1km[i]<-(1/3*NLCD$NLCD_sedge_1km_2008[i])+(2/3*NLCD$NLCD_sedge_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_sedge_1km[i]<-NLCD$NLCD_sedge_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_sedge_1km[i]<-(1/2*NLCD$NLCD_sedge_1km_2011[i])+(1/2*NLCD$NLCD_sedge_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_sedge_1km[i]<-NLCD$NLCD_sedge_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_sedge_1km[i]<-(2/3*NLCD$NLCD_sedge_1km_2013[i])+(1/3*NLCD$NLCD_sedge_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_sedge_1km[i]<-(1/3*NLCD$NLCD_sedge_1km_2013[i])+(2/3*NLCD$NLCD_sedge_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_sedge_1km[i]<-NLCD$NLCD_sedge_1km_2016[i]
}

NLCD$NLCD_lichens_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_lichens_1km[i]<-NLCD$NLCD_lichens_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_lichens_1km[i]<-(2/3*NLCD$NLCD_lichens_1km_2001[i])+(1/3*NLCD$NLCD_lichens_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_lichens_1km[i]<-(1/3*NLCD$NLCD_lichens_1km_2001[i])+(2/3*NLCD$NLCD_lichens_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_lichens_1km[i]<-NLCD$NLCD_lichens_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_lichens_1km[i]<-(1/2*NLCD$NLCD_lichens_1km_2004[i])+(1/2*NLCD$NLCD_lichens_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_lichens_1km[i]<-NLCD$NLCD_lichens_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_lichens_1km[i]<-(1/2*NLCD$NLCD_lichens_1km_2006[i])+(1/2*NLCD$NLCD_lichens_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_lichens_1km[i]<-NLCD$NLCD_lichens_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_lichens_1km[i]<-(2/3*NLCD$NLCD_lichens_1km_2008[i])+(1/3*NLCD$NLCD_lichens_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_lichens_1km[i]<-(1/3*NLCD$NLCD_lichens_1km_2008[i])+(2/3*NLCD$NLCD_lichens_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_lichens_1km[i]<-NLCD$NLCD_lichens_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_lichens_1km[i]<-(1/2*NLCD$NLCD_lichens_1km_2011[i])+(1/2*NLCD$NLCD_lichens_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_lichens_1km[i]<-NLCD$NLCD_lichens_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_lichens_1km[i]<-(2/3*NLCD$NLCD_lichens_1km_2013[i])+(1/3*NLCD$NLCD_lichens_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_lichens_1km[i]<-(1/3*NLCD$NLCD_lichens_1km_2013[i])+(2/3*NLCD$NLCD_lichens_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_lichens_1km[i]<-NLCD$NLCD_lichens_1km_2016[i]
}

NLCD$NLCD_moss_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_moss_1km[i]<-NLCD$NLCD_moss_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_moss_1km[i]<-(2/3*NLCD$NLCD_moss_1km_2001[i])+(1/3*NLCD$NLCD_moss_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_moss_1km[i]<-(1/3*NLCD$NLCD_moss_1km_2001[i])+(2/3*NLCD$NLCD_moss_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_moss_1km[i]<-NLCD$NLCD_moss_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_moss_1km[i]<-(1/2*NLCD$NLCD_moss_1km_2004[i])+(1/2*NLCD$NLCD_moss_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_moss_1km[i]<-NLCD$NLCD_moss_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_moss_1km[i]<-(1/2*NLCD$NLCD_moss_1km_2006[i])+(1/2*NLCD$NLCD_moss_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_moss_1km[i]<-NLCD$NLCD_moss_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_moss_1km[i]<-(2/3*NLCD$NLCD_moss_1km_2008[i])+(1/3*NLCD$NLCD_moss_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_moss_1km[i]<-(1/3*NLCD$NLCD_moss_1km_2008[i])+(2/3*NLCD$NLCD_moss_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_moss_1km[i]<-NLCD$NLCD_moss_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_moss_1km[i]<-(1/2*NLCD$NLCD_moss_1km_2011[i])+(1/2*NLCD$NLCD_moss_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_moss_1km[i]<-NLCD$NLCD_moss_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_moss_1km[i]<-(2/3*NLCD$NLCD_moss_1km_2013[i])+(1/3*NLCD$NLCD_moss_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_moss_1km[i]<-(1/3*NLCD$NLCD_moss_1km_2013[i])+(2/3*NLCD$NLCD_moss_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_moss_1km[i]<-NLCD$NLCD_moss_1km_2016[i]
}

NLCD$NLCD_pasture_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_pasture_1km[i]<-NLCD$NLCD_pasture_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_pasture_1km[i]<-(2/3*NLCD$NLCD_pasture_1km_2001[i])+(1/3*NLCD$NLCD_pasture_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_pasture_1km[i]<-(1/3*NLCD$NLCD_pasture_1km_2001[i])+(2/3*NLCD$NLCD_pasture_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_pasture_1km[i]<-NLCD$NLCD_pasture_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_pasture_1km[i]<-(1/2*NLCD$NLCD_pasture_1km_2004[i])+(1/2*NLCD$NLCD_pasture_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_pasture_1km[i]<-NLCD$NLCD_pasture_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_pasture_1km[i]<-(1/2*NLCD$NLCD_pasture_1km_2006[i])+(1/2*NLCD$NLCD_pasture_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_pasture_1km[i]<-NLCD$NLCD_pasture_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_pasture_1km[i]<-(2/3*NLCD$NLCD_pasture_1km_2008[i])+(1/3*NLCD$NLCD_pasture_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_pasture_1km[i]<-(1/3*NLCD$NLCD_pasture_1km_2008[i])+(2/3*NLCD$NLCD_pasture_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_pasture_1km[i]<-NLCD$NLCD_pasture_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_pasture_1km[i]<-(1/2*NLCD$NLCD_pasture_1km_2011[i])+(1/2*NLCD$NLCD_pasture_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_pasture_1km[i]<-NLCD$NLCD_pasture_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_pasture_1km[i]<-(2/3*NLCD$NLCD_pasture_1km_2013[i])+(1/3*NLCD$NLCD_pasture_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_pasture_1km[i]<-(1/3*NLCD$NLCD_pasture_1km_2013[i])+(2/3*NLCD$NLCD_pasture_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_pasture_1km[i]<-NLCD$NLCD_pasture_1km_2016[i]
}

NLCD$NLCD_cultivated_crops_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_cultivated_crops_1km[i]<-NLCD$NLCD_cultivated_crops_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_cultivated_crops_1km[i]<-(2/3*NLCD$NLCD_cultivated_crops_1km_2001[i])+(1/3*NLCD$NLCD_cultivated_crops_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_cultivated_crops_1km[i]<-(1/3*NLCD$NLCD_cultivated_crops_1km_2001[i])+(2/3*NLCD$NLCD_cultivated_crops_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_cultivated_crops_1km[i]<-NLCD$NLCD_cultivated_crops_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_cultivated_crops_1km[i]<-(1/2*NLCD$NLCD_cultivated_crops_1km_2004[i])+(1/2*NLCD$NLCD_cultivated_crops_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_cultivated_crops_1km[i]<-NLCD$NLCD_cultivated_crops_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_cultivated_crops_1km[i]<-(1/2*NLCD$NLCD_cultivated_crops_1km_2006[i])+(1/2*NLCD$NLCD_cultivated_crops_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_cultivated_crops_1km[i]<-NLCD$NLCD_cultivated_crops_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_cultivated_crops_1km[i]<-(2/3*NLCD$NLCD_cultivated_crops_1km_2008[i])+(1/3*NLCD$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_cultivated_crops_1km[i]<-(1/3*NLCD$NLCD_cultivated_crops_1km_2008[i])+(2/3*NLCD$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_cultivated_crops_1km[i]<-NLCD$NLCD_cultivated_crops_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_cultivated_crops_1km[i]<-(1/2*NLCD$NLCD_cultivated_crops_1km_2011[i])+(1/2*NLCD$NLCD_cultivated_crops_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_cultivated_crops_1km[i]<-NLCD$NLCD_cultivated_crops_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_cultivated_crops_1km[i]<-(2/3*NLCD$NLCD_cultivated_crops_1km_2013[i])+(1/3*NLCD$NLCD_cultivated_crops_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_cultivated_crops_1km[i]<-(1/3*NLCD$NLCD_cultivated_crops_1km_2013[i])+(2/3*NLCD$NLCD_cultivated_crops_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_cultivated_crops_1km[i]<-NLCD$NLCD_cultivated_crops_1km_2016[i]
}

NLCD$NLCD_woody_wetland_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_woody_wetland_1km[i]<-NLCD$NLCD_woody_wetland_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_woody_wetland_1km[i]<-(2/3*NLCD$NLCD_woody_wetland_1km_2001[i])+(1/3*NLCD$NLCD_woody_wetland_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_woody_wetland_1km[i]<-(1/3*NLCD$NLCD_woody_wetland_1km_2001[i])+(2/3*NLCD$NLCD_woody_wetland_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_woody_wetland_1km[i]<-NLCD$NLCD_woody_wetland_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_woody_wetland_1km[i]<-(1/2*NLCD$NLCD_woody_wetland_1km_2004[i])+(1/2*NLCD$NLCD_woody_wetland_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_woody_wetland_1km[i]<-NLCD$NLCD_woody_wetland_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_woody_wetland_1km[i]<-(1/2*NLCD$NLCD_woody_wetland_1km_2006[i])+(1/2*NLCD$NLCD_woody_wetland_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_woody_wetland_1km[i]<-NLCD$NLCD_woody_wetland_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_woody_wetland_1km[i]<-(2/3*NLCD$NLCD_woody_wetland_1km_2008[i])+(1/3*NLCD$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_woody_wetland_1km[i]<-(1/3*NLCD$NLCD_woody_wetland_1km_2008[i])+(2/3*NLCD$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_woody_wetland_1km[i]<-NLCD$NLCD_woody_wetland_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_woody_wetland_1km[i]<-(1/2*NLCD$NLCD_woody_wetland_1km_2011[i])+(1/2*NLCD$NLCD_woody_wetland_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_woody_wetland_1km[i]<-NLCD$NLCD_woody_wetland_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_woody_wetland_1km[i]<-(2/3*NLCD$NLCD_woody_wetland_1km_2013[i])+(1/3*NLCD$NLCD_woody_wetland_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_woody_wetland_1km[i]<-(1/3*NLCD$NLCD_woody_wetland_1km_2013[i])+(2/3*NLCD$NLCD_woody_wetland_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_woody_wetland_1km[i]<-NLCD$NLCD_woody_wetland_1km_2016[i]
}

NLCD$NLCD_emergent_herbaceous_wetlands_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2004[i])+(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2006[i])+(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2008[i])+(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2008[i])+(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2011[i])+(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2013[i])+(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2013[i])+(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_1km_2016[i]
}

NLCD$NLCD_unkown_1km<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_unkown_1km[i]<-NLCD$NLCD_unkown_1km_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_unkown_1km[i]<-(2/3*NLCD$NLCD_unkown_1km_2001[i])+(1/3*NLCD$NLCD_unkown_1km_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_unkown_1km[i]<-(1/3*NLCD$NLCD_unkown_1km_2001[i])+(2/3*NLCD$NLCD_unkown_1km_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_unkown_1km[i]<-NLCD$NLCD_unkown_1km_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_unkown_1km[i]<-(1/2*NLCD$NLCD_unkown_1km_2004[i])+(1/2*NLCD$NLCD_unkown_1km_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_unkown_1km[i]<-NLCD$NLCD_unkown_1km_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_unkown_1km[i]<-(1/2*NLCD$NLCD_unkown_1km_2006[i])+(1/2*NLCD$NLCD_unkown_1km_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_unkown_1km[i]<-NLCD$NLCD_unkown_1km_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_unkown_1km[i]<-(2/3*NLCD$NLCD_unkown_1km_2008[i])+(1/3*NLCD$NLCD_unkown_1km_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_unkown_1km[i]<-(1/3*NLCD$NLCD_unkown_1km_2008[i])+(2/3*NLCD$NLCD_unkown_1km_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_unkown_1km[i]<-NLCD$NLCD_unkown_1km_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_unkown_1km[i]<-(1/2*NLCD$NLCD_unkown_1km_2011[i])+(1/2*NLCD$NLCD_unkown_1km_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_unkown_1km[i]<-NLCD$NLCD_unkown_1km_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_unkown_1km[i]<-(2/3*NLCD$NLCD_unkown_1km_2013[i])+(1/3*NLCD$NLCD_unkown_1km_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_unkown_1km[i]<-(1/3*NLCD$NLCD_unkown_1km_2013[i])+(2/3*NLCD$NLCD_unkown_1km_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_unkown_1km[i]<-NLCD$NLCD_unkown_1km_2016[i]
}


##500m
###For Open water
NLCD$NLCD_open_water_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_open_water_500m[i]<-NLCD$NLCD_open_water_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_open_water_500m[i]<-(2/3*NLCD$NLCD_open_water_500m_2001[i])+(1/3*NLCD$NLCD_open_water_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_open_water_500m[i]<-(1/3*NLCD$NLCD_open_water_500m_2001[i])+(2/3*NLCD$NLCD_open_water_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_open_water_500m[i]<-NLCD$NLCD_open_water_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_open_water_500m[i]<-(1/2*NLCD$NLCD_open_water_500m_2004[i])+(1/2*NLCD$NLCD_open_water_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_open_water_500m[i]<-NLCD$NLCD_open_water_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_open_water_500m[i]<-(1/2*NLCD$NLCD_open_water_500m_2006[i])+(1/2*NLCD$NLCD_open_water_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_open_water_500m[i]<-NLCD$NLCD_open_water_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_open_water_500m[i]<-(2/3*NLCD$NLCD_open_water_500m_2008[i])+(1/3*NLCD$NLCD_open_water_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_open_water_500m[i]<-(1/3*NLCD$NLCD_open_water_500m_2008[i])+(2/3*NLCD$NLCD_open_water_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_open_water_500m[i]<-NLCD$NLCD_open_water_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_open_water_500m[i]<-(1/2*NLCD$NLCD_open_water_500m_2011[i])+(1/2*NLCD$NLCD_open_water_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_open_water_500m[i]<-NLCD$NLCD_open_water_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_open_water_500m[i]<-(2/3*NLCD$NLCD_open_water_500m_2013[i])+(1/3*NLCD$NLCD_open_water_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_open_water_500m[i]<-(1/3*NLCD$NLCD_open_water_500m_2013[i])+(2/3*NLCD$NLCD_open_water_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_open_water_500m[i]<-NLCD$NLCD_open_water_500m_2016[i]
}

NLCD$NLCD_ice_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_ice_500m[i]<-NLCD$NLCD_ice_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_ice_500m[i]<-(2/3*NLCD$NLCD_ice_500m_2001[i])+(1/3*NLCD$NLCD_ice_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_ice_500m[i]<-(1/3*NLCD$NLCD_ice_500m_2001[i])+(2/3*NLCD$NLCD_ice_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_ice_500m[i]<-NLCD$NLCD_ice_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_ice_500m[i]<-(1/2*NLCD$NLCD_ice_500m_2004[i])+(1/2*NLCD$NLCD_ice_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_ice_500m[i]<-NLCD$NLCD_ice_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_ice_500m[i]<-(1/2*NLCD$NLCD_ice_500m_2006[i])+(1/2*NLCD$NLCD_ice_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_ice_500m[i]<-NLCD$NLCD_ice_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_ice_500m[i]<-(2/3*NLCD$NLCD_ice_500m_2008[i])+(1/3*NLCD$NLCD_ice_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_ice_500m[i]<-(1/3*NLCD$NLCD_ice_500m_2008[i])+(2/3*NLCD$NLCD_ice_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_ice_500m[i]<-NLCD$NLCD_ice_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_ice_500m[i]<-(1/2*NLCD$NLCD_ice_500m_2011[i])+(1/2*NLCD$NLCD_ice_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_ice_500m[i]<-NLCD$NLCD_ice_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_ice_500m[i]<-(2/3*NLCD$NLCD_ice_500m_2013[i])+(1/3*NLCD$NLCD_ice_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_ice_500m[i]<-(1/3*NLCD$NLCD_ice_500m_2013[i])+(2/3*NLCD$NLCD_ice_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_ice_500m[i]<-NLCD$NLCD_ice_500m_2016[i]
}



NLCD$NLCD_developed_open_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_open_500m[i]<-NLCD$NLCD_developed_open_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_open_500m[i]<-(2/3*NLCD$NLCD_developed_open_500m_2001[i])+(1/3*NLCD$NLCD_developed_open_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_open_500m[i]<-(1/3*NLCD$NLCD_developed_open_500m_2001[i])+(2/3*NLCD$NLCD_developed_open_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_open_500m[i]<-NLCD$NLCD_developed_open_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_open_500m[i]<-(1/2*NLCD$NLCD_developed_open_500m_2004[i])+(1/2*NLCD$NLCD_developed_open_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_open_500m[i]<-NLCD$NLCD_developed_open_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_open_500m[i]<-(1/2*NLCD$NLCD_developed_open_500m_2006[i])+(1/2*NLCD$NLCD_developed_open_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_open_500m[i]<-NLCD$NLCD_developed_open_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_open_500m[i]<-(2/3*NLCD$NLCD_developed_open_500m_2008[i])+(1/3*NLCD$NLCD_developed_open_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_open_500m[i]<-(1/3*NLCD$NLCD_developed_open_500m_2008[i])+(2/3*NLCD$NLCD_developed_open_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_open_500m[i]<-NLCD$NLCD_developed_open_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_open_500m[i]<-(1/2*NLCD$NLCD_developed_open_500m_2011[i])+(1/2*NLCD$NLCD_developed_open_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_open_500m[i]<-NLCD$NLCD_developed_open_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_open_500m[i]<-(2/3*NLCD$NLCD_developed_open_500m_2013[i])+(1/3*NLCD$NLCD_developed_open_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_open_500m[i]<-(1/3*NLCD$NLCD_developed_open_500m_2013[i])+(2/3*NLCD$NLCD_developed_open_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_open_500m[i]<-NLCD$NLCD_developed_open_500m_2016[i]
}

NLCD$NLCD_developed_low_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_low_500m[i]<-NLCD$NLCD_developed_low_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_low_500m[i]<-(2/3*NLCD$NLCD_developed_low_500m_2001[i])+(1/3*NLCD$NLCD_developed_low_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_low_500m[i]<-(1/3*NLCD$NLCD_developed_low_500m_2001[i])+(2/3*NLCD$NLCD_developed_low_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_low_500m[i]<-NLCD$NLCD_developed_low_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_low_500m[i]<-(1/2*NLCD$NLCD_developed_low_500m_2004[i])+(1/2*NLCD$NLCD_developed_low_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_low_500m[i]<-NLCD$NLCD_developed_low_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_low_500m[i]<-(1/2*NLCD$NLCD_developed_low_500m_2006[i])+(1/2*NLCD$NLCD_developed_low_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_low_500m[i]<-NLCD$NLCD_developed_low_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_low_500m[i]<-(2/3*NLCD$NLCD_developed_low_500m_2008[i])+(1/3*NLCD$NLCD_developed_low_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_low_500m[i]<-(1/3*NLCD$NLCD_developed_low_500m_2008[i])+(2/3*NLCD$NLCD_developed_low_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_low_500m[i]<-NLCD$NLCD_developed_low_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_low_500m[i]<-(1/2*NLCD$NLCD_developed_low_500m_2011[i])+(1/2*NLCD$NLCD_developed_low_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_low_500m[i]<-NLCD$NLCD_developed_low_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_low_500m[i]<-(2/3*NLCD$NLCD_developed_low_500m_2013[i])+(1/3*NLCD$NLCD_developed_low_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_low_500m[i]<-(1/3*NLCD$NLCD_developed_low_500m_2013[i])+(2/3*NLCD$NLCD_developed_low_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_low_500m[i]<-NLCD$NLCD_developed_low_500m_2016[i]
}

NLCD$NLCD_developed_medium_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_medium_500m[i]<-NLCD$NLCD_developed_medium_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_medium_500m[i]<-(2/3*NLCD$NLCD_developed_medium_500m_2001[i])+(1/3*NLCD$NLCD_developed_medium_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_medium_500m[i]<-(1/3*NLCD$NLCD_developed_medium_500m_2001[i])+(2/3*NLCD$NLCD_developed_medium_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_medium_500m[i]<-NLCD$NLCD_developed_medium_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_medium_500m[i]<-(1/2*NLCD$NLCD_developed_medium_500m_2004[i])+(1/2*NLCD$NLCD_developed_medium_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_medium_500m[i]<-NLCD$NLCD_developed_medium_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_medium_500m[i]<-(1/2*NLCD$NLCD_developed_medium_500m_2006[i])+(1/2*NLCD$NLCD_developed_medium_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_medium_500m[i]<-NLCD$NLCD_developed_medium_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_medium_500m[i]<-(2/3*NLCD$NLCD_developed_medium_500m_2008[i])+(1/3*NLCD$NLCD_developed_medium_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_medium_500m[i]<-(1/3*NLCD$NLCD_developed_medium_500m_2008[i])+(2/3*NLCD$NLCD_developed_medium_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_medium_500m[i]<-NLCD$NLCD_developed_medium_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_medium_500m[i]<-(1/2*NLCD$NLCD_developed_medium_500m_2011[i])+(1/2*NLCD$NLCD_developed_medium_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_medium_500m[i]<-NLCD$NLCD_developed_medium_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_medium_500m[i]<-(2/3*NLCD$NLCD_developed_medium_500m_2013[i])+(1/3*NLCD$NLCD_developed_medium_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_medium_500m[i]<-(1/3*NLCD$NLCD_developed_medium_500m_2013[i])+(2/3*NLCD$NLCD_developed_medium_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_medium_500m[i]<-NLCD$NLCD_developed_medium_500m_2016[i]
}

NLCD$NLCD_developed_high_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_developed_high_500m[i]<-NLCD$NLCD_developed_high_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_developed_high_500m[i]<-(2/3*NLCD$NLCD_developed_high_500m_2001[i])+(1/3*NLCD$NLCD_developed_high_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_developed_high_500m[i]<-(1/3*NLCD$NLCD_developed_high_500m_2001[i])+(2/3*NLCD$NLCD_developed_high_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_developed_high_500m[i]<-NLCD$NLCD_developed_high_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_developed_high_500m[i]<-(1/2*NLCD$NLCD_developed_high_500m_2004[i])+(1/2*NLCD$NLCD_developed_high_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_developed_high_500m[i]<-NLCD$NLCD_developed_high_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_developed_high_500m[i]<-(1/2*NLCD$NLCD_developed_high_500m_2006[i])+(1/2*NLCD$NLCD_developed_high_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_developed_high_500m[i]<-NLCD$NLCD_developed_high_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_developed_high_500m[i]<-(2/3*NLCD$NLCD_developed_high_500m_2008[i])+(1/3*NLCD$NLCD_developed_high_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_developed_high_500m[i]<-(1/3*NLCD$NLCD_developed_high_500m_2008[i])+(2/3*NLCD$NLCD_developed_high_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_developed_high_500m[i]<-NLCD$NLCD_developed_high_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_developed_high_500m[i]<-(1/2*NLCD$NLCD_developed_high_500m_2011[i])+(1/2*NLCD$NLCD_developed_high_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_developed_high_500m[i]<-NLCD$NLCD_developed_high_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_developed_high_500m[i]<-(2/3*NLCD$NLCD_developed_high_500m_2013[i])+(1/3*NLCD$NLCD_developed_high_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_developed_high_500m[i]<-(1/3*NLCD$NLCD_developed_high_500m_2013[i])+(2/3*NLCD$NLCD_developed_high_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_developed_high_500m[i]<-NLCD$NLCD_developed_high_500m_2016[i]
}

NLCD$NLCD_barren_land_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_barren_land_500m[i]<-NLCD$NLCD_barren_land_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_barren_land_500m[i]<-(2/3*NLCD$NLCD_barren_land_500m_2001[i])+(1/3*NLCD$NLCD_barren_land_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_barren_land_500m[i]<-(1/3*NLCD$NLCD_barren_land_500m_2001[i])+(2/3*NLCD$NLCD_barren_land_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_barren_land_500m[i]<-NLCD$NLCD_barren_land_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_barren_land_500m[i]<-(1/2*NLCD$NLCD_barren_land_500m_2004[i])+(1/2*NLCD$NLCD_barren_land_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_barren_land_500m[i]<-NLCD$NLCD_barren_land_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_barren_land_500m[i]<-(1/2*NLCD$NLCD_barren_land_500m_2006[i])+(1/2*NLCD$NLCD_barren_land_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_barren_land_500m[i]<-NLCD$NLCD_barren_land_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_barren_land_500m[i]<-(2/3*NLCD$NLCD_barren_land_500m_2008[i])+(1/3*NLCD$NLCD_barren_land_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_barren_land_500m[i]<-(1/3*NLCD$NLCD_barren_land_500m_2008[i])+(2/3*NLCD$NLCD_barren_land_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_barren_land_500m[i]<-NLCD$NLCD_barren_land_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_barren_land_500m[i]<-(1/2*NLCD$NLCD_barren_land_500m_2011[i])+(1/2*NLCD$NLCD_barren_land_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_barren_land_500m[i]<-NLCD$NLCD_barren_land_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_barren_land_500m[i]<-(2/3*NLCD$NLCD_barren_land_500m_2013[i])+(1/3*NLCD$NLCD_barren_land_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_barren_land_500m[i]<-(1/3*NLCD$NLCD_barren_land_500m_2013[i])+(2/3*NLCD$NLCD_barren_land_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_barren_land_500m[i]<-NLCD$NLCD_barren_land_500m_2016[i]
}


NLCD$NLCD_decidious_forest_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_decidious_forest_500m[i]<-NLCD$NLCD_decidious_forest_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_decidious_forest_500m[i]<-(2/3*NLCD$NLCD_decidious_forest_500m_2001[i])+(1/3*NLCD$NLCD_decidious_forest_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_decidious_forest_500m[i]<-(1/3*NLCD$NLCD_decidious_forest_500m_2001[i])+(2/3*NLCD$NLCD_decidious_forest_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_decidious_forest_500m[i]<-NLCD$NLCD_decidious_forest_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_decidious_forest_500m[i]<-(1/2*NLCD$NLCD_decidious_forest_500m_2004[i])+(1/2*NLCD$NLCD_decidious_forest_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_decidious_forest_500m[i]<-NLCD$NLCD_decidious_forest_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_decidious_forest_500m[i]<-(1/2*NLCD$NLCD_decidious_forest_500m_2006[i])+(1/2*NLCD$NLCD_decidious_forest_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_decidious_forest_500m[i]<-NLCD$NLCD_decidious_forest_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_decidious_forest_500m[i]<-(2/3*NLCD$NLCD_decidious_forest_500m_2008[i])+(1/3*NLCD$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_decidious_forest_500m[i]<-(1/3*NLCD$NLCD_decidious_forest_500m_2008[i])+(2/3*NLCD$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_decidious_forest_500m[i]<-NLCD$NLCD_decidious_forest_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_decidious_forest_500m[i]<-(1/2*NLCD$NLCD_decidious_forest_500m_2011[i])+(1/2*NLCD$NLCD_decidious_forest_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_decidious_forest_500m[i]<-NLCD$NLCD_decidious_forest_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_decidious_forest_500m[i]<-(2/3*NLCD$NLCD_decidious_forest_500m_2013[i])+(1/3*NLCD$NLCD_decidious_forest_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_decidious_forest_500m[i]<-(1/3*NLCD$NLCD_decidious_forest_500m_2013[i])+(2/3*NLCD$NLCD_decidious_forest_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_decidious_forest_500m[i]<-NLCD$NLCD_decidious_forest_500m_2016[i]
}

NLCD$NLCD_evergreen_forest_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_evergreen_forest_500m[i]<-NLCD$NLCD_evergreen_forest_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_evergreen_forest_500m[i]<-(2/3*NLCD$NLCD_evergreen_forest_500m_2001[i])+(1/3*NLCD$NLCD_evergreen_forest_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_evergreen_forest_500m[i]<-(1/3*NLCD$NLCD_evergreen_forest_500m_2001[i])+(2/3*NLCD$NLCD_evergreen_forest_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_evergreen_forest_500m[i]<-NLCD$NLCD_evergreen_forest_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_evergreen_forest_500m[i]<-(1/2*NLCD$NLCD_evergreen_forest_500m_2004[i])+(1/2*NLCD$NLCD_evergreen_forest_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_evergreen_forest_500m[i]<-NLCD$NLCD_evergreen_forest_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_evergreen_forest_500m[i]<-(1/2*NLCD$NLCD_evergreen_forest_500m_2006[i])+(1/2*NLCD$NLCD_evergreen_forest_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_evergreen_forest_500m[i]<-NLCD$NLCD_evergreen_forest_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_evergreen_forest_500m[i]<-(2/3*NLCD$NLCD_evergreen_forest_500m_2008[i])+(1/3*NLCD$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_evergreen_forest_500m[i]<-(1/3*NLCD$NLCD_evergreen_forest_500m_2008[i])+(2/3*NLCD$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_evergreen_forest_500m[i]<-NLCD$NLCD_evergreen_forest_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_evergreen_forest_500m[i]<-(1/2*NLCD$NLCD_evergreen_forest_500m_2011[i])+(1/2*NLCD$NLCD_evergreen_forest_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_evergreen_forest_500m[i]<-NLCD$NLCD_evergreen_forest_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_evergreen_forest_500m[i]<-(2/3*NLCD$NLCD_evergreen_forest_500m_2013[i])+(1/3*NLCD$NLCD_evergreen_forest_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_evergreen_forest_500m[i]<-(1/3*NLCD$NLCD_evergreen_forest_500m_2013[i])+(2/3*NLCD$NLCD_evergreen_forest_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_evergreen_forest_500m[i]<-NLCD$NLCD_evergreen_forest_500m_2016[i]
}

NLCD$NLCD_mixed_forest_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_mixed_forest_500m[i]<-NLCD$NLCD_mixed_forest_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_mixed_forest_500m[i]<-(2/3*NLCD$NLCD_mixed_forest_500m_2001[i])+(1/3*NLCD$NLCD_mixed_forest_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_mixed_forest_500m[i]<-(1/3*NLCD$NLCD_mixed_forest_500m_2001[i])+(2/3*NLCD$NLCD_mixed_forest_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_mixed_forest_500m[i]<-NLCD$NLCD_mixed_forest_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_mixed_forest_500m[i]<-(1/2*NLCD$NLCD_mixed_forest_500m_2004[i])+(1/2*NLCD$NLCD_mixed_forest_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_mixed_forest_500m[i]<-NLCD$NLCD_mixed_forest_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_mixed_forest_500m[i]<-(1/2*NLCD$NLCD_mixed_forest_500m_2006[i])+(1/2*NLCD$NLCD_mixed_forest_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_mixed_forest_500m[i]<-NLCD$NLCD_mixed_forest_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_mixed_forest_500m[i]<-(2/3*NLCD$NLCD_mixed_forest_500m_2008[i])+(1/3*NLCD$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_mixed_forest_500m[i]<-(1/3*NLCD$NLCD_mixed_forest_500m_2008[i])+(2/3*NLCD$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_mixed_forest_500m[i]<-NLCD$NLCD_mixed_forest_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_mixed_forest_500m[i]<-(1/2*NLCD$NLCD_mixed_forest_500m_2011[i])+(1/2*NLCD$NLCD_mixed_forest_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_mixed_forest_500m[i]<-NLCD$NLCD_mixed_forest_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_mixed_forest_500m[i]<-(2/3*NLCD$NLCD_mixed_forest_500m_2013[i])+(1/3*NLCD$NLCD_mixed_forest_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_mixed_forest_500m[i]<-(1/3*NLCD$NLCD_mixed_forest_500m_2013[i])+(2/3*NLCD$NLCD_mixed_forest_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_mixed_forest_500m[i]<-NLCD$NLCD_mixed_forest_500m_2016[i]
}

NLCD$NLCD_dwarf_shrub_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_dwarf_shrub_500m[i]<-NLCD$NLCD_dwarf_shrub_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_dwarf_shrub_500m[i]<-(2/3*NLCD$NLCD_dwarf_shrub_500m_2001[i])+(1/3*NLCD$NLCD_dwarf_shrub_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_dwarf_shrub_500m[i]<-(1/3*NLCD$NLCD_dwarf_shrub_500m_2001[i])+(2/3*NLCD$NLCD_dwarf_shrub_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_dwarf_shrub_500m[i]<-NLCD$NLCD_dwarf_shrub_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_dwarf_shrub_500m[i]<-(1/2*NLCD$NLCD_dwarf_shrub_500m_2004[i])+(1/2*NLCD$NLCD_dwarf_shrub_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_dwarf_shrub_500m[i]<-NLCD$NLCD_dwarf_shrub_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_dwarf_shrub_500m[i]<-(1/2*NLCD$NLCD_dwarf_shrub_500m_2006[i])+(1/2*NLCD$NLCD_dwarf_shrub_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_dwarf_shrub_500m[i]<-NLCD$NLCD_dwarf_shrub_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_dwarf_shrub_500m[i]<-(2/3*NLCD$NLCD_dwarf_shrub_500m_2008[i])+(1/3*NLCD$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_dwarf_shrub_500m[i]<-(1/3*NLCD$NLCD_dwarf_shrub_500m_2008[i])+(2/3*NLCD$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_dwarf_shrub_500m[i]<-NLCD$NLCD_dwarf_shrub_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_dwarf_shrub_500m[i]<-(1/2*NLCD$NLCD_dwarf_shrub_500m_2011[i])+(1/2*NLCD$NLCD_dwarf_shrub_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_dwarf_shrub_500m[i]<-NLCD$NLCD_dwarf_shrub_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_dwarf_shrub_500m[i]<-(2/3*NLCD$NLCD_dwarf_shrub_500m_2013[i])+(1/3*NLCD$NLCD_dwarf_shrub_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_dwarf_shrub_500m[i]<-(1/3*NLCD$NLCD_dwarf_shrub_500m_2013[i])+(2/3*NLCD$NLCD_dwarf_shrub_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_dwarf_shrub_500m[i]<-NLCD$NLCD_dwarf_shrub_500m_2016[i]
}

NLCD$NLCD_shrub_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_shrub_500m[i]<-NLCD$NLCD_shrub_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_shrub_500m[i]<-(2/3*NLCD$NLCD_shrub_500m_2001[i])+(1/3*NLCD$NLCD_shrub_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_shrub_500m[i]<-(1/3*NLCD$NLCD_shrub_500m_2001[i])+(2/3*NLCD$NLCD_shrub_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_shrub_500m[i]<-NLCD$NLCD_shrub_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_shrub_500m[i]<-(1/2*NLCD$NLCD_shrub_500m_2004[i])+(1/2*NLCD$NLCD_shrub_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_shrub_500m[i]<-NLCD$NLCD_shrub_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_shrub_500m[i]<-(1/2*NLCD$NLCD_shrub_500m_2006[i])+(1/2*NLCD$NLCD_shrub_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_shrub_500m[i]<-NLCD$NLCD_shrub_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_shrub_500m[i]<-(2/3*NLCD$NLCD_shrub_500m_2008[i])+(1/3*NLCD$NLCD_shrub_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_shrub_500m[i]<-(1/3*NLCD$NLCD_shrub_500m_2008[i])+(2/3*NLCD$NLCD_shrub_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_shrub_500m[i]<-NLCD$NLCD_shrub_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_shrub_500m[i]<-(1/2*NLCD$NLCD_shrub_500m_2011[i])+(1/2*NLCD$NLCD_shrub_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_shrub_500m[i]<-NLCD$NLCD_shrub_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_shrub_500m[i]<-(2/3*NLCD$NLCD_shrub_500m_2013[i])+(1/3*NLCD$NLCD_shrub_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_shrub_500m[i]<-(1/3*NLCD$NLCD_shrub_500m_2013[i])+(2/3*NLCD$NLCD_shrub_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_shrub_500m[i]<-NLCD$NLCD_shrub_500m_2016[i]
}

NLCD$NLCD_grassland_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_grassland_500m[i]<-NLCD$NLCD_grassland_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_grassland_500m[i]<-(2/3*NLCD$NLCD_grassland_500m_2001[i])+(1/3*NLCD$NLCD_grassland_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_grassland_500m[i]<-(1/3*NLCD$NLCD_grassland_500m_2001[i])+(2/3*NLCD$NLCD_grassland_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_grassland_500m[i]<-NLCD$NLCD_grassland_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_grassland_500m[i]<-(1/2*NLCD$NLCD_grassland_500m_2004[i])+(1/2*NLCD$NLCD_grassland_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_grassland_500m[i]<-NLCD$NLCD_grassland_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_grassland_500m[i]<-(1/2*NLCD$NLCD_grassland_500m_2006[i])+(1/2*NLCD$NLCD_grassland_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_grassland_500m[i]<-NLCD$NLCD_grassland_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_grassland_500m[i]<-(2/3*NLCD$NLCD_grassland_500m_2008[i])+(1/3*NLCD$NLCD_grassland_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_grassland_500m[i]<-(1/3*NLCD$NLCD_grassland_500m_2008[i])+(2/3*NLCD$NLCD_grassland_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_grassland_500m[i]<-NLCD$NLCD_grassland_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_grassland_500m[i]<-(1/2*NLCD$NLCD_grassland_500m_2011[i])+(1/2*NLCD$NLCD_grassland_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_grassland_500m[i]<-NLCD$NLCD_grassland_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_grassland_500m[i]<-(2/3*NLCD$NLCD_grassland_500m_2013[i])+(1/3*NLCD$NLCD_grassland_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_grassland_500m[i]<-(1/3*NLCD$NLCD_grassland_500m_2013[i])+(2/3*NLCD$NLCD_grassland_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_grassland_500m[i]<-NLCD$NLCD_grassland_500m_2016[i]
}

NLCD$NLCD_sedge_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_sedge_500m[i]<-NLCD$NLCD_sedge_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_sedge_500m[i]<-(2/3*NLCD$NLCD_sedge_500m_2001[i])+(1/3*NLCD$NLCD_sedge_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_sedge_500m[i]<-(1/3*NLCD$NLCD_sedge_500m_2001[i])+(2/3*NLCD$NLCD_sedge_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_sedge_500m[i]<-NLCD$NLCD_sedge_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_sedge_500m[i]<-(1/2*NLCD$NLCD_sedge_500m_2004[i])+(1/2*NLCD$NLCD_sedge_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_sedge_500m[i]<-NLCD$NLCD_sedge_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_sedge_500m[i]<-(1/2*NLCD$NLCD_sedge_500m_2006[i])+(1/2*NLCD$NLCD_sedge_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_sedge_500m[i]<-NLCD$NLCD_sedge_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_sedge_500m[i]<-(2/3*NLCD$NLCD_sedge_500m_2008[i])+(1/3*NLCD$NLCD_sedge_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_sedge_500m[i]<-(1/3*NLCD$NLCD_sedge_500m_2008[i])+(2/3*NLCD$NLCD_sedge_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_sedge_500m[i]<-NLCD$NLCD_sedge_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_sedge_500m[i]<-(1/2*NLCD$NLCD_sedge_500m_2011[i])+(1/2*NLCD$NLCD_sedge_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_sedge_500m[i]<-NLCD$NLCD_sedge_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_sedge_500m[i]<-(2/3*NLCD$NLCD_sedge_500m_2013[i])+(1/3*NLCD$NLCD_sedge_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_sedge_500m[i]<-(1/3*NLCD$NLCD_sedge_500m_2013[i])+(2/3*NLCD$NLCD_sedge_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_sedge_500m[i]<-NLCD$NLCD_sedge_500m_2016[i]
}

NLCD$NLCD_lichens_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_lichens_500m[i]<-NLCD$NLCD_lichens_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_lichens_500m[i]<-(2/3*NLCD$NLCD_lichens_500m_2001[i])+(1/3*NLCD$NLCD_lichens_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_lichens_500m[i]<-(1/3*NLCD$NLCD_lichens_500m_2001[i])+(2/3*NLCD$NLCD_lichens_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_lichens_500m[i]<-NLCD$NLCD_lichens_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_lichens_500m[i]<-(1/2*NLCD$NLCD_lichens_500m_2004[i])+(1/2*NLCD$NLCD_lichens_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_lichens_500m[i]<-NLCD$NLCD_lichens_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_lichens_500m[i]<-(1/2*NLCD$NLCD_lichens_500m_2006[i])+(1/2*NLCD$NLCD_lichens_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_lichens_500m[i]<-NLCD$NLCD_lichens_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_lichens_500m[i]<-(2/3*NLCD$NLCD_lichens_500m_2008[i])+(1/3*NLCD$NLCD_lichens_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_lichens_500m[i]<-(1/3*NLCD$NLCD_lichens_500m_2008[i])+(2/3*NLCD$NLCD_lichens_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_lichens_500m[i]<-NLCD$NLCD_lichens_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_lichens_500m[i]<-(1/2*NLCD$NLCD_lichens_500m_2011[i])+(1/2*NLCD$NLCD_lichens_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_lichens_500m[i]<-NLCD$NLCD_lichens_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_lichens_500m[i]<-(2/3*NLCD$NLCD_lichens_500m_2013[i])+(1/3*NLCD$NLCD_lichens_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_lichens_500m[i]<-(1/3*NLCD$NLCD_lichens_500m_2013[i])+(2/3*NLCD$NLCD_lichens_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_lichens_500m[i]<-NLCD$NLCD_lichens_500m_2016[i]
}

NLCD$NLCD_moss_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_moss_500m[i]<-NLCD$NLCD_moss_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_moss_500m[i]<-(2/3*NLCD$NLCD_moss_500m_2001[i])+(1/3*NLCD$NLCD_moss_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_moss_500m[i]<-(1/3*NLCD$NLCD_moss_500m_2001[i])+(2/3*NLCD$NLCD_moss_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_moss_500m[i]<-NLCD$NLCD_moss_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_moss_500m[i]<-(1/2*NLCD$NLCD_moss_500m_2004[i])+(1/2*NLCD$NLCD_moss_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_moss_500m[i]<-NLCD$NLCD_moss_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_moss_500m[i]<-(1/2*NLCD$NLCD_moss_500m_2006[i])+(1/2*NLCD$NLCD_moss_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_moss_500m[i]<-NLCD$NLCD_moss_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_moss_500m[i]<-(2/3*NLCD$NLCD_moss_500m_2008[i])+(1/3*NLCD$NLCD_moss_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_moss_500m[i]<-(1/3*NLCD$NLCD_moss_500m_2008[i])+(2/3*NLCD$NLCD_moss_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_moss_500m[i]<-NLCD$NLCD_moss_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_moss_500m[i]<-(1/2*NLCD$NLCD_moss_500m_2011[i])+(1/2*NLCD$NLCD_moss_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_moss_500m[i]<-NLCD$NLCD_moss_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_moss_500m[i]<-(2/3*NLCD$NLCD_moss_500m_2013[i])+(1/3*NLCD$NLCD_moss_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_moss_500m[i]<-(1/3*NLCD$NLCD_moss_500m_2013[i])+(2/3*NLCD$NLCD_moss_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_moss_500m[i]<-NLCD$NLCD_moss_500m_2016[i]
}

NLCD$NLCD_pasture_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_pasture_500m[i]<-NLCD$NLCD_pasture_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_pasture_500m[i]<-(2/3*NLCD$NLCD_pasture_500m_2001[i])+(1/3*NLCD$NLCD_pasture_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_pasture_500m[i]<-(1/3*NLCD$NLCD_pasture_500m_2001[i])+(2/3*NLCD$NLCD_pasture_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_pasture_500m[i]<-NLCD$NLCD_pasture_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_pasture_500m[i]<-(1/2*NLCD$NLCD_pasture_500m_2004[i])+(1/2*NLCD$NLCD_pasture_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_pasture_500m[i]<-NLCD$NLCD_pasture_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_pasture_500m[i]<-(1/2*NLCD$NLCD_pasture_500m_2006[i])+(1/2*NLCD$NLCD_pasture_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_pasture_500m[i]<-NLCD$NLCD_pasture_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_pasture_500m[i]<-(2/3*NLCD$NLCD_pasture_500m_2008[i])+(1/3*NLCD$NLCD_pasture_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_pasture_500m[i]<-(1/3*NLCD$NLCD_pasture_500m_2008[i])+(2/3*NLCD$NLCD_pasture_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_pasture_500m[i]<-NLCD$NLCD_pasture_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_pasture_500m[i]<-(1/2*NLCD$NLCD_pasture_500m_2011[i])+(1/2*NLCD$NLCD_pasture_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_pasture_500m[i]<-NLCD$NLCD_pasture_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_pasture_500m[i]<-(2/3*NLCD$NLCD_pasture_500m_2013[i])+(1/3*NLCD$NLCD_pasture_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_pasture_500m[i]<-(1/3*NLCD$NLCD_pasture_500m_2013[i])+(2/3*NLCD$NLCD_pasture_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_pasture_500m[i]<-NLCD$NLCD_pasture_500m_2016[i]
}

NLCD$NLCD_cultivated_crops_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_cultivated_crops_500m[i]<-NLCD$NLCD_cultivated_crops_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_cultivated_crops_500m[i]<-(2/3*NLCD$NLCD_cultivated_crops_500m_2001[i])+(1/3*NLCD$NLCD_cultivated_crops_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_cultivated_crops_500m[i]<-(1/3*NLCD$NLCD_cultivated_crops_500m_2001[i])+(2/3*NLCD$NLCD_cultivated_crops_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_cultivated_crops_500m[i]<-NLCD$NLCD_cultivated_crops_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_cultivated_crops_500m[i]<-(1/2*NLCD$NLCD_cultivated_crops_500m_2004[i])+(1/2*NLCD$NLCD_cultivated_crops_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_cultivated_crops_500m[i]<-NLCD$NLCD_cultivated_crops_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_cultivated_crops_500m[i]<-(1/2*NLCD$NLCD_cultivated_crops_500m_2006[i])+(1/2*NLCD$NLCD_cultivated_crops_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_cultivated_crops_500m[i]<-NLCD$NLCD_cultivated_crops_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_cultivated_crops_500m[i]<-(2/3*NLCD$NLCD_cultivated_crops_500m_2008[i])+(1/3*NLCD$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_cultivated_crops_500m[i]<-(1/3*NLCD$NLCD_cultivated_crops_500m_2008[i])+(2/3*NLCD$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_cultivated_crops_500m[i]<-NLCD$NLCD_cultivated_crops_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_cultivated_crops_500m[i]<-(1/2*NLCD$NLCD_cultivated_crops_500m_2011[i])+(1/2*NLCD$NLCD_cultivated_crops_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_cultivated_crops_500m[i]<-NLCD$NLCD_cultivated_crops_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_cultivated_crops_500m[i]<-(2/3*NLCD$NLCD_cultivated_crops_500m_2013[i])+(1/3*NLCD$NLCD_cultivated_crops_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_cultivated_crops_500m[i]<-(1/3*NLCD$NLCD_cultivated_crops_500m_2013[i])+(2/3*NLCD$NLCD_cultivated_crops_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_cultivated_crops_500m[i]<-NLCD$NLCD_cultivated_crops_500m_2016[i]
}

NLCD$NLCD_woody_wetland_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_woody_wetland_500m[i]<-NLCD$NLCD_woody_wetland_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_woody_wetland_500m[i]<-(2/3*NLCD$NLCD_woody_wetland_500m_2001[i])+(1/3*NLCD$NLCD_woody_wetland_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_woody_wetland_500m[i]<-(1/3*NLCD$NLCD_woody_wetland_500m_2001[i])+(2/3*NLCD$NLCD_woody_wetland_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_woody_wetland_500m[i]<-NLCD$NLCD_woody_wetland_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_woody_wetland_500m[i]<-(1/2*NLCD$NLCD_woody_wetland_500m_2004[i])+(1/2*NLCD$NLCD_woody_wetland_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_woody_wetland_500m[i]<-NLCD$NLCD_woody_wetland_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_woody_wetland_500m[i]<-(1/2*NLCD$NLCD_woody_wetland_500m_2006[i])+(1/2*NLCD$NLCD_woody_wetland_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_woody_wetland_500m[i]<-NLCD$NLCD_woody_wetland_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_woody_wetland_500m[i]<-(2/3*NLCD$NLCD_woody_wetland_500m_2008[i])+(1/3*NLCD$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_woody_wetland_500m[i]<-(1/3*NLCD$NLCD_woody_wetland_500m_2008[i])+(2/3*NLCD$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_woody_wetland_500m[i]<-NLCD$NLCD_woody_wetland_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_woody_wetland_500m[i]<-(1/2*NLCD$NLCD_woody_wetland_500m_2011[i])+(1/2*NLCD$NLCD_woody_wetland_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_woody_wetland_500m[i]<-NLCD$NLCD_woody_wetland_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_woody_wetland_500m[i]<-(2/3*NLCD$NLCD_woody_wetland_500m_2013[i])+(1/3*NLCD$NLCD_woody_wetland_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_woody_wetland_500m[i]<-(1/3*NLCD$NLCD_woody_wetland_500m_2013[i])+(2/3*NLCD$NLCD_woody_wetland_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_woody_wetland_500m[i]<-NLCD$NLCD_woody_wetland_500m_2016[i]
}

NLCD$NLCD_emergent_herbaceous_wetlands_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2004[i])+(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2006[i])+(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2008[i])+(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2008[i])+(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2011[i])+(1/2*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2013[i])+(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-(1/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2013[i])+(2/3*NLCD$NLCD_emergent_herbaceous_wetlands_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD$NLCD_emergent_herbaceous_wetlands_500m_2016[i]
}

NLCD$NLCD_unkown_500m<-rep(0,nrow(NLCD))
for(i in 1:nrow(NLCD)){
  if (NLCD$year.int[i]<2002) {NLCD$NLCD_unkown_500m[i]<-NLCD$NLCD_unkown_500m_2001[i]}
  else if (NLCD$year.int[i]==2002) NLCD$NLCD_unkown_500m[i]<-(2/3*NLCD$NLCD_unkown_500m_2001[i])+(1/3*NLCD$NLCD_unkown_500m_2004[i])
  else if (NLCD$year.int[i]==2003) NLCD$NLCD_unkown_500m[i]<-(1/3*NLCD$NLCD_unkown_500m_2001[i])+(2/3*NLCD$NLCD_unkown_500m_2004[i])
  else if (NLCD$year.int[i]==2004) NLCD$NLCD_unkown_500m[i]<-NLCD$NLCD_unkown_500m_2004[i]
  else if (NLCD$year.int[i]==2005) NLCD$NLCD_unkown_500m[i]<-(1/2*NLCD$NLCD_unkown_500m_2004[i])+(1/2*NLCD$NLCD_unkown_500m_2006[i])
  else if (NLCD$year.int[i]==2006) NLCD$NLCD_unkown_500m[i]<-NLCD$NLCD_unkown_500m_2006[i]
  else if (NLCD$year.int[i]==2007) NLCD$NLCD_unkown_500m[i]<-(1/2*NLCD$NLCD_unkown_500m_2006[i])+(1/2*NLCD$NLCD_unkown_500m_2008[i])
  else if (NLCD$year.int[i]==2008) NLCD$NLCD_unkown_500m[i]<-NLCD$NLCD_unkown_500m_2008[i]
  else if (NLCD$year.int[i]==2009) NLCD$NLCD_unkown_500m[i]<-(2/3*NLCD$NLCD_unkown_500m_2008[i])+(1/3*NLCD$NLCD_unkown_500m_2011[i])
  else if (NLCD$year.int[i]==2010) NLCD$NLCD_unkown_500m[i]<-(1/3*NLCD$NLCD_unkown_500m_2008[i])+(2/3*NLCD$NLCD_unkown_500m_2011[i])
  else if (NLCD$year.int[i]==2011) NLCD$NLCD_unkown_500m[i]<-NLCD$NLCD_unkown_500m_2011[i]
  else if (NLCD$year.int[i]==2012) NLCD$NLCD_unkown_500m[i]<-(1/2*NLCD$NLCD_unkown_500m_2011[i])+(1/2*NLCD$NLCD_unkown_500m_2013[i])
  else if (NLCD$year.int[i]==2013) NLCD$NLCD_unkown_500m[i]<-NLCD$NLCD_unkown_500m_2013[i]
  else if (NLCD$year.int[i]==2014) NLCD$NLCD_unkown_500m[i]<-(2/3*NLCD$NLCD_unkown_500m_2013[i])+(1/3*NLCD$NLCD_unkown_500m_2016[i])
  else if (NLCD$year.int[i]==2015) NLCD$NLCD_unkown_500m[i]<-(1/3*NLCD$NLCD_unkown_500m_2013[i])+(2/3*NLCD$NLCD_unkown_500m_2016[i])
  else if (NLCD$year.int[i]>2015) NLCD$NLCD_unkown_500m[i]<-NLCD$NLCD_unkown_500m_2016[i]
}





######
#######
##########
##############
#Alaska
###For Open water
NLCD_AK$NLCD_open_water_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_open_water_2km[i]<-NLCD_AK$NLCD_open_water_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_open_water_2km[i]<-(9/10*NLCD_AK$NLCD_open_water_2km_2001[i])+(1/10*NLCD_AK$NLCD_open_water_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_open_water_2km[i]<-(8/10*NLCD_AK$NLCD_open_water_2km_2001[i])+(2/10*NLCD_AK$NLCD_open_water_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_open_water_2km[i]<-(7/10*NLCD_AK$NLCD_open_water_2km_2001[i])+(3/10*NLCD_AK$NLCD_open_water_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_open_water_2km[i]<-(6/10*NLCD_AK$NLCD_open_water_2km_2001[i])+(4/10*NLCD_AK$NLCD_open_water_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_open_water_2km[i]<-(5/10*NLCD_AK$NLCD_open_water_2km_2001[i])+(5/10*NLCD_AK$NLCD_open_water_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_open_water_2km[i]<-(4/10*NLCD_AK$NLCD_open_water_2km_2001[i])+(6/10*NLCD_AK$NLCD_open_water_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_open_water_2km[i]<-(3/10*NLCD_AK$NLCD_open_water_2km_2001[i])+(7/10*NLCD_AK$NLCD_open_water_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_open_water_2km[i]<-(2/10*NLCD_AK$NLCD_open_water_2km_2001[i])+(8/10*NLCD_AK$NLCD_open_water_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_open_water_2km[i]<-(1/10*NLCD_AK$NLCD_open_water_2km_2001[i])+(9/10*NLCD_AK$NLCD_open_water_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_open_water_2km[i]<-NLCD_AK$NLCD_open_water_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_open_water_2km[i]<-(4/5*NLCD_AK$NLCD_open_water_2km_2011[i])+(1/5*NLCD_AK$NLCD_open_water_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_open_water_2km[i]<-(3/5*NLCD_AK$NLCD_open_water_2km_2011[i])+(2/5*NLCD_AK$NLCD_open_water_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_open_water_2km[i]<-(2/5*NLCD_AK$NLCD_open_water_2km_2011[i])+(3/5*NLCD_AK$NLCD_open_water_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_open_water_2km[i]<-(1/5*NLCD_AK$NLCD_open_water_2km_2011[i])+(4/5*NLCD_AK$NLCD_open_water_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_open_water_2km[i]<-NLCD_AK$NLCD_open_water_2km_2016[i]
}

###For NLCD_ice
NLCD_AK$NLCD_ice_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_ice_2km[i]<-NLCD_AK$NLCD_ice_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_ice_2km[i]<-(9/10*NLCD_AK$NLCD_ice_2km_2001[i])+(1/10*NLCD_AK$NLCD_ice_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_ice_2km[i]<-(8/10*NLCD_AK$NLCD_ice_2km_2001[i])+(2/10*NLCD_AK$NLCD_ice_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_ice_2km[i]<-(7/10*NLCD_AK$NLCD_ice_2km_2001[i])+(3/10*NLCD_AK$NLCD_ice_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_ice_2km[i]<-(6/10*NLCD_AK$NLCD_ice_2km_2001[i])+(4/10*NLCD_AK$NLCD_ice_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_ice_2km[i]<-(5/10*NLCD_AK$NLCD_ice_2km_2001[i])+(5/10*NLCD_AK$NLCD_ice_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_ice_2km[i]<-(4/10*NLCD_AK$NLCD_ice_2km_2001[i])+(6/10*NLCD_AK$NLCD_ice_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_ice_2km[i]<-(3/10*NLCD_AK$NLCD_ice_2km_2001[i])+(7/10*NLCD_AK$NLCD_ice_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_ice_2km[i]<-(2/10*NLCD_AK$NLCD_ice_2km_2001[i])+(8/10*NLCD_AK$NLCD_ice_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_ice_2km[i]<-(1/10*NLCD_AK$NLCD_ice_2km_2001[i])+(9/10*NLCD_AK$NLCD_ice_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_ice_2km[i]<-NLCD_AK$NLCD_ice_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_ice_2km[i]<-(4/5*NLCD_AK$NLCD_ice_2km_2011[i])+(1/5*NLCD_AK$NLCD_ice_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_ice_2km[i]<-(3/5*NLCD_AK$NLCD_ice_2km_2011[i])+(2/5*NLCD_AK$NLCD_ice_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_ice_2km[i]<-(2/5*NLCD_AK$NLCD_ice_2km_2011[i])+(3/5*NLCD_AK$NLCD_ice_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_ice_2km[i]<-(1/5*NLCD_AK$NLCD_ice_2km_2011[i])+(4/5*NLCD_AK$NLCD_ice_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_ice_2km[i]<-NLCD_AK$NLCD_ice_2km_2016[i]
}

###For Development open
NLCD_AK$NLCD_developed_open_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_open_2km[i]<-NLCD_AK$NLCD_developed_open_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_open_2km[i]<-(9/10*NLCD_AK$NLCD_developed_open_2km_2001[i])+(1/10*NLCD_AK$NLCD_developed_open_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_open_2km[i]<-(8/10*NLCD_AK$NLCD_developed_open_2km_2001[i])+(2/10*NLCD_AK$NLCD_developed_open_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_open_2km[i]<-(7/10*NLCD_AK$NLCD_developed_open_2km_2001[i])+(3/10*NLCD_AK$NLCD_developed_open_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_open_2km[i]<-(6/10*NLCD_AK$NLCD_developed_open_2km_2001[i])+(4/10*NLCD_AK$NLCD_developed_open_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_open_2km[i]<-(5/10*NLCD_AK$NLCD_developed_open_2km_2001[i])+(5/10*NLCD_AK$NLCD_developed_open_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_open_2km[i]<-(4/10*NLCD_AK$NLCD_developed_open_2km_2001[i])+(6/10*NLCD_AK$NLCD_developed_open_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_open_2km[i]<-(3/10*NLCD_AK$NLCD_developed_open_2km_2001[i])+(7/10*NLCD_AK$NLCD_developed_open_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_open_2km[i]<-(2/10*NLCD_AK$NLCD_developed_open_2km_2001[i])+(8/10*NLCD_AK$NLCD_developed_open_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_open_2km[i]<-(1/10*NLCD_AK$NLCD_developed_open_2km_2001[i])+(9/10*NLCD_AK$NLCD_developed_open_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_open_2km[i]<-NLCD_AK$NLCD_developed_open_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_open_2km[i]<-(4/5*NLCD_AK$NLCD_developed_open_2km_2011[i])+(1/5*NLCD_AK$NLCD_developed_open_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_open_2km[i]<-(3/5*NLCD_AK$NLCD_developed_open_2km_2011[i])+(2/5*NLCD_AK$NLCD_developed_open_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_open_2km[i]<-(2/5*NLCD_AK$NLCD_developed_open_2km_2011[i])+(3/5*NLCD_AK$NLCD_developed_open_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_open_2km[i]<-(1/5*NLCD_AK$NLCD_developed_open_2km_2011[i])+(4/5*NLCD_AK$NLCD_developed_open_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_open_2km[i]<-NLCD_AK$NLCD_developed_open_2km_2016[i]
}

###For Development low
NLCD_AK$NLCD_developed_low_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_low_2km[i]<-NLCD_AK$NLCD_developed_low_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_low_2km[i]<-(9/10*NLCD_AK$NLCD_developed_low_2km_2001[i])+(1/10*NLCD_AK$NLCD_developed_low_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_low_2km[i]<-(8/10*NLCD_AK$NLCD_developed_low_2km_2001[i])+(2/10*NLCD_AK$NLCD_developed_low_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_low_2km[i]<-(7/10*NLCD_AK$NLCD_developed_low_2km_2001[i])+(3/10*NLCD_AK$NLCD_developed_low_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_low_2km[i]<-(6/10*NLCD_AK$NLCD_developed_low_2km_2001[i])+(4/10*NLCD_AK$NLCD_developed_low_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_low_2km[i]<-(5/10*NLCD_AK$NLCD_developed_low_2km_2001[i])+(5/10*NLCD_AK$NLCD_developed_low_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_low_2km[i]<-(4/10*NLCD_AK$NLCD_developed_low_2km_2001[i])+(6/10*NLCD_AK$NLCD_developed_low_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_low_2km[i]<-(3/10*NLCD_AK$NLCD_developed_low_2km_2001[i])+(7/10*NLCD_AK$NLCD_developed_low_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_low_2km[i]<-(2/10*NLCD_AK$NLCD_developed_low_2km_2001[i])+(8/10*NLCD_AK$NLCD_developed_low_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_low_2km[i]<-(1/10*NLCD_AK$NLCD_developed_low_2km_2001[i])+(9/10*NLCD_AK$NLCD_developed_low_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_low_2km[i]<-NLCD_AK$NLCD_developed_low_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_low_2km[i]<-(4/5*NLCD_AK$NLCD_developed_low_2km_2011[i])+(1/5*NLCD_AK$NLCD_developed_low_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_low_2km[i]<-(3/5*NLCD_AK$NLCD_developed_low_2km_2011[i])+(2/5*NLCD_AK$NLCD_developed_low_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_low_2km[i]<-(2/5*NLCD_AK$NLCD_developed_low_2km_2011[i])+(3/5*NLCD_AK$NLCD_developed_low_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_low_2km[i]<-(1/5*NLCD_AK$NLCD_developed_low_2km_2011[i])+(4/5*NLCD_AK$NLCD_developed_low_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_low_2km[i]<-NLCD_AK$NLCD_developed_low_2km_2016[i]
}

###For Deveelopment medium
NLCD_AK$NLCD_developed_medium_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_medium_2km[i]<-NLCD_AK$NLCD_developed_medium_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_medium_2km[i]<-(9/10*NLCD_AK$NLCD_developed_medium_2km_2001[i])+(1/10*NLCD_AK$NLCD_developed_medium_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_medium_2km[i]<-(8/10*NLCD_AK$NLCD_developed_medium_2km_2001[i])+(2/10*NLCD_AK$NLCD_developed_medium_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_medium_2km[i]<-(7/10*NLCD_AK$NLCD_developed_medium_2km_2001[i])+(3/10*NLCD_AK$NLCD_developed_medium_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_medium_2km[i]<-(6/10*NLCD_AK$NLCD_developed_medium_2km_2001[i])+(4/10*NLCD_AK$NLCD_developed_medium_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_medium_2km[i]<-(5/10*NLCD_AK$NLCD_developed_medium_2km_2001[i])+(5/10*NLCD_AK$NLCD_developed_medium_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_medium_2km[i]<-(4/10*NLCD_AK$NLCD_developed_medium_2km_2001[i])+(6/10*NLCD_AK$NLCD_developed_medium_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_medium_2km[i]<-(3/10*NLCD_AK$NLCD_developed_medium_2km_2001[i])+(7/10*NLCD_AK$NLCD_developed_medium_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_medium_2km[i]<-(2/10*NLCD_AK$NLCD_developed_medium_2km_2001[i])+(8/10*NLCD_AK$NLCD_developed_medium_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_medium_2km[i]<-(1/10*NLCD_AK$NLCD_developed_medium_2km_2001[i])+(9/10*NLCD_AK$NLCD_developed_medium_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_medium_2km[i]<-NLCD_AK$NLCD_developed_medium_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_medium_2km[i]<-(4/5*NLCD_AK$NLCD_developed_medium_2km_2011[i])+(1/5*NLCD_AK$NLCD_developed_medium_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_medium_2km[i]<-(3/5*NLCD_AK$NLCD_developed_medium_2km_2011[i])+(2/5*NLCD_AK$NLCD_developed_medium_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_medium_2km[i]<-(2/5*NLCD_AK$NLCD_developed_medium_2km_2011[i])+(3/5*NLCD_AK$NLCD_developed_medium_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_medium_2km[i]<-(1/5*NLCD_AK$NLCD_developed_medium_2km_2011[i])+(4/5*NLCD_AK$NLCD_developed_medium_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_medium_2km[i]<-NLCD_AK$NLCD_developed_medium_2km_2016[i]
}

###For NLCD_developed high
NLCD_AK$NLCD_developed_high_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_high_2km[i]<-NLCD_AK$NLCD_developed_high_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_high_2km[i]<-(9/10*NLCD_AK$NLCD_developed_high_2km_2001[i])+(1/10*NLCD_AK$NLCD_developed_high_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_high_2km[i]<-(8/10*NLCD_AK$NLCD_developed_high_2km_2001[i])+(2/10*NLCD_AK$NLCD_developed_high_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_high_2km[i]<-(7/10*NLCD_AK$NLCD_developed_high_2km_2001[i])+(3/10*NLCD_AK$NLCD_developed_high_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_high_2km[i]<-(6/10*NLCD_AK$NLCD_developed_high_2km_2001[i])+(4/10*NLCD_AK$NLCD_developed_high_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_high_2km[i]<-(5/10*NLCD_AK$NLCD_developed_high_2km_2001[i])+(5/10*NLCD_AK$NLCD_developed_high_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_high_2km[i]<-(4/10*NLCD_AK$NLCD_developed_high_2km_2001[i])+(6/10*NLCD_AK$NLCD_developed_high_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_high_2km[i]<-(3/10*NLCD_AK$NLCD_developed_high_2km_2001[i])+(7/10*NLCD_AK$NLCD_developed_high_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_high_2km[i]<-(2/10*NLCD_AK$NLCD_developed_high_2km_2001[i])+(8/10*NLCD_AK$NLCD_developed_high_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_high_2km[i]<-(1/10*NLCD_AK$NLCD_developed_high_2km_2001[i])+(9/10*NLCD_AK$NLCD_developed_high_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_high_2km[i]<-NLCD_AK$NLCD_developed_high_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_high_2km[i]<-(4/5*NLCD_AK$NLCD_developed_high_2km_2011[i])+(1/5*NLCD_AK$NLCD_developed_high_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_high_2km[i]<-(3/5*NLCD_AK$NLCD_developed_high_2km_2011[i])+(2/5*NLCD_AK$NLCD_developed_high_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_high_2km[i]<-(2/5*NLCD_AK$NLCD_developed_high_2km_2011[i])+(3/5*NLCD_AK$NLCD_developed_high_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_high_2km[i]<-(1/5*NLCD_AK$NLCD_developed_high_2km_2011[i])+(4/5*NLCD_AK$NLCD_developed_high_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_high_2km[i]<-NLCD_AK$NLCD_developed_high_2km_2016[i]
}

###For Barren land
NLCD_AK$NLCD_barren_land_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_barren_land_2km[i]<-NLCD_AK$NLCD_barren_land_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_barren_land_2km[i]<-(9/10*NLCD_AK$NLCD_barren_land_2km_2001[i])+(1/10*NLCD_AK$NLCD_barren_land_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_barren_land_2km[i]<-(8/10*NLCD_AK$NLCD_barren_land_2km_2001[i])+(2/10*NLCD_AK$NLCD_barren_land_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_barren_land_2km[i]<-(7/10*NLCD_AK$NLCD_barren_land_2km_2001[i])+(3/10*NLCD_AK$NLCD_barren_land_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_barren_land_2km[i]<-(6/10*NLCD_AK$NLCD_barren_land_2km_2001[i])+(4/10*NLCD_AK$NLCD_barren_land_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_barren_land_2km[i]<-(5/10*NLCD_AK$NLCD_barren_land_2km_2001[i])+(5/10*NLCD_AK$NLCD_barren_land_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_barren_land_2km[i]<-(4/10*NLCD_AK$NLCD_barren_land_2km_2001[i])+(6/10*NLCD_AK$NLCD_barren_land_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_barren_land_2km[i]<-(3/10*NLCD_AK$NLCD_barren_land_2km_2001[i])+(7/10*NLCD_AK$NLCD_barren_land_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_barren_land_2km[i]<-(2/10*NLCD_AK$NLCD_barren_land_2km_2001[i])+(8/10*NLCD_AK$NLCD_barren_land_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_barren_land_2km[i]<-(1/10*NLCD_AK$NLCD_barren_land_2km_2001[i])+(9/10*NLCD_AK$NLCD_barren_land_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_barren_land_2km[i]<-NLCD_AK$NLCD_barren_land_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_barren_land_2km[i]<-(4/5*NLCD_AK$NLCD_barren_land_2km_2011[i])+(1/5*NLCD_AK$NLCD_barren_land_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_barren_land_2km[i]<-(3/5*NLCD_AK$NLCD_barren_land_2km_2011[i])+(2/5*NLCD_AK$NLCD_barren_land_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_barren_land_2km[i]<-(2/5*NLCD_AK$NLCD_barren_land_2km_2011[i])+(3/5*NLCD_AK$NLCD_barren_land_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_barren_land_2km[i]<-(1/5*NLCD_AK$NLCD_barren_land_2km_2011[i])+(4/5*NLCD_AK$NLCD_barren_land_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_barren_land_2km[i]<-NLCD_AK$NLCD_barren_land_2km_2016[i]
}

###For Decidious forest
NLCD_AK$NLCD_decidious_forest_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_decidious_forest_2km[i]<-NLCD_AK$NLCD_decidious_forest_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_decidious_forest_2km[i]<-(9/10*NLCD_AK$NLCD_decidious_forest_2km_2001[i])+(1/10*NLCD_AK$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_decidious_forest_2km[i]<-(8/10*NLCD_AK$NLCD_decidious_forest_2km_2001[i])+(2/10*NLCD_AK$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_decidious_forest_2km[i]<-(7/10*NLCD_AK$NLCD_decidious_forest_2km_2001[i])+(3/10*NLCD_AK$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_decidious_forest_2km[i]<-(6/10*NLCD_AK$NLCD_decidious_forest_2km_2001[i])+(4/10*NLCD_AK$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_decidious_forest_2km[i]<-(5/10*NLCD_AK$NLCD_decidious_forest_2km_2001[i])+(5/10*NLCD_AK$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_decidious_forest_2km[i]<-(4/10*NLCD_AK$NLCD_decidious_forest_2km_2001[i])+(6/10*NLCD_AK$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_decidious_forest_2km[i]<-(3/10*NLCD_AK$NLCD_decidious_forest_2km_2001[i])+(7/10*NLCD_AK$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_decidious_forest_2km[i]<-(2/10*NLCD_AK$NLCD_decidious_forest_2km_2001[i])+(8/10*NLCD_AK$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_decidious_forest_2km[i]<-(1/10*NLCD_AK$NLCD_decidious_forest_2km_2001[i])+(9/10*NLCD_AK$NLCD_decidious_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_decidious_forest_2km[i]<-NLCD_AK$NLCD_decidious_forest_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_decidious_forest_2km[i]<-(4/5*NLCD_AK$NLCD_decidious_forest_2km_2011[i])+(1/5*NLCD_AK$NLCD_decidious_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_decidious_forest_2km[i]<-(3/5*NLCD_AK$NLCD_decidious_forest_2km_2011[i])+(2/5*NLCD_AK$NLCD_decidious_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_decidious_forest_2km[i]<-(2/5*NLCD_AK$NLCD_decidious_forest_2km_2011[i])+(3/5*NLCD_AK$NLCD_decidious_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_decidious_forest_2km[i]<-(1/5*NLCD_AK$NLCD_decidious_forest_2km_2011[i])+(4/5*NLCD_AK$NLCD_decidious_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_decidious_forest_2km[i]<-NLCD_AK$NLCD_decidious_forest_2km_2016[i]
}


###For Evergreen forest
NLCD_AK$NLCD_evergreen_forest_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_evergreen_forest_2km[i]<-NLCD_AK$NLCD_evergreen_forest_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(9/10*NLCD_AK$NLCD_evergreen_forest_2km_2001[i])+(1/10*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(8/10*NLCD_AK$NLCD_evergreen_forest_2km_2001[i])+(2/10*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(7/10*NLCD_AK$NLCD_evergreen_forest_2km_2001[i])+(3/10*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(6/10*NLCD_AK$NLCD_evergreen_forest_2km_2001[i])+(4/10*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(5/10*NLCD_AK$NLCD_evergreen_forest_2km_2001[i])+(5/10*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(4/10*NLCD_AK$NLCD_evergreen_forest_2km_2001[i])+(6/10*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(3/10*NLCD_AK$NLCD_evergreen_forest_2km_2001[i])+(7/10*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(2/10*NLCD_AK$NLCD_evergreen_forest_2km_2001[i])+(8/10*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(1/10*NLCD_AK$NLCD_evergreen_forest_2km_2001[i])+(9/10*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_evergreen_forest_2km[i]<-NLCD_AK$NLCD_evergreen_forest_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(4/5*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])+(1/5*NLCD_AK$NLCD_evergreen_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(3/5*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])+(2/5*NLCD_AK$NLCD_evergreen_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(2/5*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])+(3/5*NLCD_AK$NLCD_evergreen_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_evergreen_forest_2km[i]<-(1/5*NLCD_AK$NLCD_evergreen_forest_2km_2011[i])+(4/5*NLCD_AK$NLCD_evergreen_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_evergreen_forest_2km[i]<-NLCD_AK$NLCD_evergreen_forest_2km_2016[i]
}

###For Mixed forest
NLCD_AK$NLCD_mixed_forest_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_mixed_forest_2km[i]<-NLCD_AK$NLCD_mixed_forest_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_mixed_forest_2km[i]<-(9/10*NLCD_AK$NLCD_mixed_forest_2km_2001[i])+(1/10*NLCD_AK$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_mixed_forest_2km[i]<-(8/10*NLCD_AK$NLCD_mixed_forest_2km_2001[i])+(2/10*NLCD_AK$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_mixed_forest_2km[i]<-(7/10*NLCD_AK$NLCD_mixed_forest_2km_2001[i])+(3/10*NLCD_AK$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_mixed_forest_2km[i]<-(6/10*NLCD_AK$NLCD_mixed_forest_2km_2001[i])+(4/10*NLCD_AK$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_mixed_forest_2km[i]<-(5/10*NLCD_AK$NLCD_mixed_forest_2km_2001[i])+(5/10*NLCD_AK$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_mixed_forest_2km[i]<-(4/10*NLCD_AK$NLCD_mixed_forest_2km_2001[i])+(6/10*NLCD_AK$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_mixed_forest_2km[i]<-(3/10*NLCD_AK$NLCD_mixed_forest_2km_2001[i])+(7/10*NLCD_AK$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_mixed_forest_2km[i]<-(2/10*NLCD_AK$NLCD_mixed_forest_2km_2001[i])+(8/10*NLCD_AK$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_mixed_forest_2km[i]<-(1/10*NLCD_AK$NLCD_mixed_forest_2km_2001[i])+(9/10*NLCD_AK$NLCD_mixed_forest_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_mixed_forest_2km[i]<-NLCD_AK$NLCD_mixed_forest_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_mixed_forest_2km[i]<-(4/5*NLCD_AK$NLCD_mixed_forest_2km_2011[i])+(1/5*NLCD_AK$NLCD_mixed_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_mixed_forest_2km[i]<-(3/5*NLCD_AK$NLCD_mixed_forest_2km_2011[i])+(2/5*NLCD_AK$NLCD_mixed_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_mixed_forest_2km[i]<-(2/5*NLCD_AK$NLCD_mixed_forest_2km_2011[i])+(3/5*NLCD_AK$NLCD_mixed_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_mixed_forest_2km[i]<-(1/5*NLCD_AK$NLCD_mixed_forest_2km_2011[i])+(4/5*NLCD_AK$NLCD_mixed_forest_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_mixed_forest_2km[i]<-NLCD_AK$NLCD_mixed_forest_2km_2016[i]
}

###For Dwrf NLCD_shrub
NLCD_AK$NLCD_dwarf_shrub_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_dwarf_shrub_2km[i]<-NLCD_AK$NLCD_dwarf_shrub_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(9/10*NLCD_AK$NLCD_dwarf_shrub_2km_2001[i])+(1/10*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(8/10*NLCD_AK$NLCD_dwarf_shrub_2km_2001[i])+(2/10*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(7/10*NLCD_AK$NLCD_dwarf_shrub_2km_2001[i])+(3/10*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(6/10*NLCD_AK$NLCD_dwarf_shrub_2km_2001[i])+(4/10*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(5/10*NLCD_AK$NLCD_dwarf_shrub_2km_2001[i])+(5/10*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(4/10*NLCD_AK$NLCD_dwarf_shrub_2km_2001[i])+(6/10*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(3/10*NLCD_AK$NLCD_dwarf_shrub_2km_2001[i])+(7/10*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(2/10*NLCD_AK$NLCD_dwarf_shrub_2km_2001[i])+(8/10*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(1/10*NLCD_AK$NLCD_dwarf_shrub_2km_2001[i])+(9/10*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-NLCD_AK$NLCD_dwarf_shrub_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(4/5*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])+(1/5*NLCD_AK$NLCD_dwarf_shrub_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(3/5*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])+(2/5*NLCD_AK$NLCD_dwarf_shrub_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(2/5*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])+(3/5*NLCD_AK$NLCD_dwarf_shrub_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-(1/5*NLCD_AK$NLCD_dwarf_shrub_2km_2011[i])+(4/5*NLCD_AK$NLCD_dwarf_shrub_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_dwarf_shrub_2km[i]<-NLCD_AK$NLCD_dwarf_shrub_2km_2016[i]
}


###For NLCD_shrub
NLCD_AK$NLCD_shrub_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_shrub_2km[i]<-NLCD_AK$NLCD_shrub_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_shrub_2km[i]<-(9/10*NLCD_AK$NLCD_shrub_2km_2001[i])+(1/10*NLCD_AK$NLCD_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_shrub_2km[i]<-(8/10*NLCD_AK$NLCD_shrub_2km_2001[i])+(2/10*NLCD_AK$NLCD_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_shrub_2km[i]<-(7/10*NLCD_AK$NLCD_shrub_2km_2001[i])+(3/10*NLCD_AK$NLCD_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_shrub_2km[i]<-(6/10*NLCD_AK$NLCD_shrub_2km_2001[i])+(4/10*NLCD_AK$NLCD_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_shrub_2km[i]<-(5/10*NLCD_AK$NLCD_shrub_2km_2001[i])+(5/10*NLCD_AK$NLCD_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_shrub_2km[i]<-(4/10*NLCD_AK$NLCD_shrub_2km_2001[i])+(6/10*NLCD_AK$NLCD_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_shrub_2km[i]<-(3/10*NLCD_AK$NLCD_shrub_2km_2001[i])+(7/10*NLCD_AK$NLCD_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_shrub_2km[i]<-(2/10*NLCD_AK$NLCD_shrub_2km_2001[i])+(8/10*NLCD_AK$NLCD_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_shrub_2km[i]<-(1/10*NLCD_AK$NLCD_shrub_2km_2001[i])+(9/10*NLCD_AK$NLCD_shrub_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_shrub_2km[i]<-NLCD_AK$NLCD_shrub_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_shrub_2km[i]<-(4/5*NLCD_AK$NLCD_shrub_2km_2011[i])+(1/5*NLCD_AK$NLCD_shrub_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_shrub_2km[i]<-(3/5*NLCD_AK$NLCD_shrub_2km_2011[i])+(2/5*NLCD_AK$NLCD_shrub_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_shrub_2km[i]<-(2/5*NLCD_AK$NLCD_shrub_2km_2011[i])+(3/5*NLCD_AK$NLCD_shrub_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_shrub_2km[i]<-(1/5*NLCD_AK$NLCD_shrub_2km_2011[i])+(4/5*NLCD_AK$NLCD_shrub_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_shrub_2km[i]<-NLCD_AK$NLCD_shrub_2km_2016[i]
}


###For NLCD_grassland
NLCD_AK$NLCD_grassland_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_grassland_2km[i]<-NLCD_AK$NLCD_grassland_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_grassland_2km[i]<-(9/10*NLCD_AK$NLCD_grassland_2km_2001[i])+(1/10*NLCD_AK$NLCD_grassland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_grassland_2km[i]<-(8/10*NLCD_AK$NLCD_grassland_2km_2001[i])+(2/10*NLCD_AK$NLCD_grassland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_grassland_2km[i]<-(7/10*NLCD_AK$NLCD_grassland_2km_2001[i])+(3/10*NLCD_AK$NLCD_grassland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_grassland_2km[i]<-(6/10*NLCD_AK$NLCD_grassland_2km_2001[i])+(4/10*NLCD_AK$NLCD_grassland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_grassland_2km[i]<-(5/10*NLCD_AK$NLCD_grassland_2km_2001[i])+(5/10*NLCD_AK$NLCD_grassland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_grassland_2km[i]<-(4/10*NLCD_AK$NLCD_grassland_2km_2001[i])+(6/10*NLCD_AK$NLCD_grassland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_grassland_2km[i]<-(3/10*NLCD_AK$NLCD_grassland_2km_2001[i])+(7/10*NLCD_AK$NLCD_grassland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_grassland_2km[i]<-(2/10*NLCD_AK$NLCD_grassland_2km_2001[i])+(8/10*NLCD_AK$NLCD_grassland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_grassland_2km[i]<-(1/10*NLCD_AK$NLCD_grassland_2km_2001[i])+(9/10*NLCD_AK$NLCD_grassland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_grassland_2km[i]<-NLCD_AK$NLCD_grassland_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_grassland_2km[i]<-(4/5*NLCD_AK$NLCD_grassland_2km_2011[i])+(1/5*NLCD_AK$NLCD_grassland_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_grassland_2km[i]<-(3/5*NLCD_AK$NLCD_grassland_2km_2011[i])+(2/5*NLCD_AK$NLCD_grassland_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_grassland_2km[i]<-(2/5*NLCD_AK$NLCD_grassland_2km_2011[i])+(3/5*NLCD_AK$NLCD_grassland_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_grassland_2km[i]<-(1/5*NLCD_AK$NLCD_grassland_2km_2011[i])+(4/5*NLCD_AK$NLCD_grassland_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_grassland_2km[i]<-NLCD_AK$NLCD_grassland_2km_2016[i]
}


###For NLCD_sedge
NLCD_AK$NLCD_sedge_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_sedge_2km[i]<-NLCD_AK$NLCD_sedge_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_sedge_2km[i]<-(9/10*NLCD_AK$NLCD_sedge_2km_2001[i])+(1/10*NLCD_AK$NLCD_sedge_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_sedge_2km[i]<-(8/10*NLCD_AK$NLCD_sedge_2km_2001[i])+(2/10*NLCD_AK$NLCD_sedge_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_sedge_2km[i]<-(7/10*NLCD_AK$NLCD_sedge_2km_2001[i])+(3/10*NLCD_AK$NLCD_sedge_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_sedge_2km[i]<-(6/10*NLCD_AK$NLCD_sedge_2km_2001[i])+(4/10*NLCD_AK$NLCD_sedge_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_sedge_2km[i]<-(5/10*NLCD_AK$NLCD_sedge_2km_2001[i])+(5/10*NLCD_AK$NLCD_sedge_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_sedge_2km[i]<-(4/10*NLCD_AK$NLCD_sedge_2km_2001[i])+(6/10*NLCD_AK$NLCD_sedge_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_sedge_2km[i]<-(3/10*NLCD_AK$NLCD_sedge_2km_2001[i])+(7/10*NLCD_AK$NLCD_sedge_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_sedge_2km[i]<-(2/10*NLCD_AK$NLCD_sedge_2km_2001[i])+(8/10*NLCD_AK$NLCD_sedge_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_sedge_2km[i]<-(1/10*NLCD_AK$NLCD_sedge_2km_2001[i])+(9/10*NLCD_AK$NLCD_sedge_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_sedge_2km[i]<-NLCD_AK$NLCD_sedge_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_sedge_2km[i]<-(4/5*NLCD_AK$NLCD_sedge_2km_2011[i])+(1/5*NLCD_AK$NLCD_sedge_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_sedge_2km[i]<-(3/5*NLCD_AK$NLCD_sedge_2km_2011[i])+(2/5*NLCD_AK$NLCD_sedge_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_sedge_2km[i]<-(2/5*NLCD_AK$NLCD_sedge_2km_2011[i])+(3/5*NLCD_AK$NLCD_sedge_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_sedge_2km[i]<-(1/5*NLCD_AK$NLCD_sedge_2km_2011[i])+(4/5*NLCD_AK$NLCD_sedge_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_sedge_2km[i]<-NLCD_AK$NLCD_sedge_2km_2016[i]
}


###For NLCD_lichens
NLCD_AK$NLCD_lichens_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_lichens_2km[i]<-NLCD_AK$NLCD_lichens_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_lichens_2km[i]<-(9/10*NLCD_AK$NLCD_lichens_2km_2001[i])+(1/10*NLCD_AK$NLCD_lichens_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_lichens_2km[i]<-(8/10*NLCD_AK$NLCD_lichens_2km_2001[i])+(2/10*NLCD_AK$NLCD_lichens_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_lichens_2km[i]<-(7/10*NLCD_AK$NLCD_lichens_2km_2001[i])+(3/10*NLCD_AK$NLCD_lichens_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_lichens_2km[i]<-(6/10*NLCD_AK$NLCD_lichens_2km_2001[i])+(4/10*NLCD_AK$NLCD_lichens_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_lichens_2km[i]<-(5/10*NLCD_AK$NLCD_lichens_2km_2001[i])+(5/10*NLCD_AK$NLCD_lichens_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_lichens_2km[i]<-(4/10*NLCD_AK$NLCD_lichens_2km_2001[i])+(6/10*NLCD_AK$NLCD_lichens_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_lichens_2km[i]<-(3/10*NLCD_AK$NLCD_lichens_2km_2001[i])+(7/10*NLCD_AK$NLCD_lichens_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_lichens_2km[i]<-(2/10*NLCD_AK$NLCD_lichens_2km_2001[i])+(8/10*NLCD_AK$NLCD_lichens_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_lichens_2km[i]<-(1/10*NLCD_AK$NLCD_lichens_2km_2001[i])+(9/10*NLCD_AK$NLCD_lichens_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_lichens_2km[i]<-NLCD_AK$NLCD_lichens_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_lichens_2km[i]<-(4/5*NLCD_AK$NLCD_lichens_2km_2011[i])+(1/5*NLCD_AK$NLCD_lichens_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_lichens_2km[i]<-(3/5*NLCD_AK$NLCD_lichens_2km_2011[i])+(2/5*NLCD_AK$NLCD_lichens_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_lichens_2km[i]<-(2/5*NLCD_AK$NLCD_lichens_2km_2011[i])+(3/5*NLCD_AK$NLCD_lichens_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_lichens_2km[i]<-(1/5*NLCD_AK$NLCD_lichens_2km_2011[i])+(4/5*NLCD_AK$NLCD_lichens_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_lichens_2km[i]<-NLCD_AK$NLCD_lichens_2km_2016[i]
}


###For NLCD_moss
NLCD_AK$NLCD_moss_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_moss_2km[i]<-NLCD_AK$NLCD_moss_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_moss_2km[i]<-(9/10*NLCD_AK$NLCD_moss_2km_2001[i])+(1/10*NLCD_AK$NLCD_moss_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_moss_2km[i]<-(8/10*NLCD_AK$NLCD_moss_2km_2001[i])+(2/10*NLCD_AK$NLCD_moss_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_moss_2km[i]<-(7/10*NLCD_AK$NLCD_moss_2km_2001[i])+(3/10*NLCD_AK$NLCD_moss_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_moss_2km[i]<-(6/10*NLCD_AK$NLCD_moss_2km_2001[i])+(4/10*NLCD_AK$NLCD_moss_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_moss_2km[i]<-(5/10*NLCD_AK$NLCD_moss_2km_2001[i])+(5/10*NLCD_AK$NLCD_moss_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_moss_2km[i]<-(4/10*NLCD_AK$NLCD_moss_2km_2001[i])+(6/10*NLCD_AK$NLCD_moss_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_moss_2km[i]<-(3/10*NLCD_AK$NLCD_moss_2km_2001[i])+(7/10*NLCD_AK$NLCD_moss_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_moss_2km[i]<-(2/10*NLCD_AK$NLCD_moss_2km_2001[i])+(8/10*NLCD_AK$NLCD_moss_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_moss_2km[i]<-(1/10*NLCD_AK$NLCD_moss_2km_2001[i])+(9/10*NLCD_AK$NLCD_moss_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_moss_2km[i]<-NLCD_AK$NLCD_moss_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_moss_2km[i]<-(4/5*NLCD_AK$NLCD_moss_2km_2011[i])+(1/5*NLCD_AK$NLCD_moss_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_moss_2km[i]<-(3/5*NLCD_AK$NLCD_moss_2km_2011[i])+(2/5*NLCD_AK$NLCD_moss_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_moss_2km[i]<-(2/5*NLCD_AK$NLCD_moss_2km_2011[i])+(3/5*NLCD_AK$NLCD_moss_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_moss_2km[i]<-(1/5*NLCD_AK$NLCD_moss_2km_2011[i])+(4/5*NLCD_AK$NLCD_moss_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_moss_2km[i]<-NLCD_AK$NLCD_moss_2km_2016[i]
}


###For NLCD_pasture
NLCD_AK$NLCD_pasture_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_pasture_2km[i]<-NLCD_AK$NLCD_pasture_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_pasture_2km[i]<-(9/10*NLCD_AK$NLCD_pasture_2km_2001[i])+(1/10*NLCD_AK$NLCD_pasture_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_pasture_2km[i]<-(8/10*NLCD_AK$NLCD_pasture_2km_2001[i])+(2/10*NLCD_AK$NLCD_pasture_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_pasture_2km[i]<-(7/10*NLCD_AK$NLCD_pasture_2km_2001[i])+(3/10*NLCD_AK$NLCD_pasture_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_pasture_2km[i]<-(6/10*NLCD_AK$NLCD_pasture_2km_2001[i])+(4/10*NLCD_AK$NLCD_pasture_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_pasture_2km[i]<-(5/10*NLCD_AK$NLCD_pasture_2km_2001[i])+(5/10*NLCD_AK$NLCD_pasture_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_pasture_2km[i]<-(4/10*NLCD_AK$NLCD_pasture_2km_2001[i])+(6/10*NLCD_AK$NLCD_pasture_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_pasture_2km[i]<-(3/10*NLCD_AK$NLCD_pasture_2km_2001[i])+(7/10*NLCD_AK$NLCD_pasture_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_pasture_2km[i]<-(2/10*NLCD_AK$NLCD_pasture_2km_2001[i])+(8/10*NLCD_AK$NLCD_pasture_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_pasture_2km[i]<-(1/10*NLCD_AK$NLCD_pasture_2km_2001[i])+(9/10*NLCD_AK$NLCD_pasture_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_pasture_2km[i]<-NLCD_AK$NLCD_pasture_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_pasture_2km[i]<-(4/5*NLCD_AK$NLCD_pasture_2km_2011[i])+(1/5*NLCD_AK$NLCD_pasture_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_pasture_2km[i]<-(3/5*NLCD_AK$NLCD_pasture_2km_2011[i])+(2/5*NLCD_AK$NLCD_pasture_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_pasture_2km[i]<-(2/5*NLCD_AK$NLCD_pasture_2km_2011[i])+(3/5*NLCD_AK$NLCD_pasture_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_pasture_2km[i]<-(1/5*NLCD_AK$NLCD_pasture_2km_2011[i])+(4/5*NLCD_AK$NLCD_pasture_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_pasture_2km[i]<-NLCD_AK$NLCD_pasture_2km_2016[i]
}

###For Cultivated crops
NLCD_AK$NLCD_cultivated_crops_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_cultivated_crops_2km[i]<-NLCD_AK$NLCD_cultivated_crops_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(9/10*NLCD_AK$NLCD_cultivated_crops_2km_2001[i])+(1/10*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(8/10*NLCD_AK$NLCD_cultivated_crops_2km_2001[i])+(2/10*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(7/10*NLCD_AK$NLCD_cultivated_crops_2km_2001[i])+(3/10*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(6/10*NLCD_AK$NLCD_cultivated_crops_2km_2001[i])+(4/10*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(5/10*NLCD_AK$NLCD_cultivated_crops_2km_2001[i])+(5/10*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(4/10*NLCD_AK$NLCD_cultivated_crops_2km_2001[i])+(6/10*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(3/10*NLCD_AK$NLCD_cultivated_crops_2km_2001[i])+(7/10*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(2/10*NLCD_AK$NLCD_cultivated_crops_2km_2001[i])+(8/10*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(1/10*NLCD_AK$NLCD_cultivated_crops_2km_2001[i])+(9/10*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_cultivated_crops_2km[i]<-NLCD_AK$NLCD_cultivated_crops_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(4/5*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])+(1/5*NLCD_AK$NLCD_cultivated_crops_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(3/5*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])+(2/5*NLCD_AK$NLCD_cultivated_crops_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(2/5*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])+(3/5*NLCD_AK$NLCD_cultivated_crops_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_cultivated_crops_2km[i]<-(1/5*NLCD_AK$NLCD_cultivated_crops_2km_2011[i])+(4/5*NLCD_AK$NLCD_cultivated_crops_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_cultivated_crops_2km[i]<-NLCD_AK$NLCD_cultivated_crops_2km_2016[i]
}


###For Woody wetland
NLCD_AK$NLCD_woody_wetland_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_woody_wetland_2km[i]<-NLCD_AK$NLCD_woody_wetland_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_woody_wetland_2km[i]<-(9/10*NLCD_AK$NLCD_woody_wetland_2km_2001[i])+(1/10*NLCD_AK$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_woody_wetland_2km[i]<-(8/10*NLCD_AK$NLCD_woody_wetland_2km_2001[i])+(2/10*NLCD_AK$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_woody_wetland_2km[i]<-(7/10*NLCD_AK$NLCD_woody_wetland_2km_2001[i])+(3/10*NLCD_AK$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_woody_wetland_2km[i]<-(6/10*NLCD_AK$NLCD_woody_wetland_2km_2001[i])+(4/10*NLCD_AK$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_woody_wetland_2km[i]<-(5/10*NLCD_AK$NLCD_woody_wetland_2km_2001[i])+(5/10*NLCD_AK$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_woody_wetland_2km[i]<-(4/10*NLCD_AK$NLCD_woody_wetland_2km_2001[i])+(6/10*NLCD_AK$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_woody_wetland_2km[i]<-(3/10*NLCD_AK$NLCD_woody_wetland_2km_2001[i])+(7/10*NLCD_AK$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_woody_wetland_2km[i]<-(2/10*NLCD_AK$NLCD_woody_wetland_2km_2001[i])+(8/10*NLCD_AK$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_woody_wetland_2km[i]<-(1/10*NLCD_AK$NLCD_woody_wetland_2km_2001[i])+(9/10*NLCD_AK$NLCD_woody_wetland_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_woody_wetland_2km[i]<-NLCD_AK$NLCD_woody_wetland_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_woody_wetland_2km[i]<-(4/5*NLCD_AK$NLCD_woody_wetland_2km_2011[i])+(1/5*NLCD_AK$NLCD_woody_wetland_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_woody_wetland_2km[i]<-(3/5*NLCD_AK$NLCD_woody_wetland_2km_2011[i])+(2/5*NLCD_AK$NLCD_woody_wetland_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_woody_wetland_2km[i]<-(2/5*NLCD_AK$NLCD_woody_wetland_2km_2011[i])+(3/5*NLCD_AK$NLCD_woody_wetland_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_woody_wetland_2km[i]<-(1/5*NLCD_AK$NLCD_woody_wetland_2km_2011[i])+(4/5*NLCD_AK$NLCD_woody_wetland_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_woody_wetland_2km[i]<-NLCD_AK$NLCD_woody_wetland_2km_2016[i]
}


###For Emergent herbaceous wetlands
NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(9/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(1/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(8/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(2/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(7/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(3/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(6/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(4/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(5/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(5/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(4/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(6/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(3/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(7/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(2/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(8/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(1/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2001[i])+(9/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(4/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])+(1/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(3/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])+(2/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(2/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])+(3/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-(1/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2011[i])+(4/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km[i]<-NLCD_AK$NLCD_emergent_herbaceous_wetlands_2km_2016[i]
}


###For Unknown
NLCD_AK$NLCD_unkown_2km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_unkown_2km[i]<-NLCD_AK$NLCD_unkown_2km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_unkown_2km[i]<-(9/10*NLCD_AK$NLCD_unkown_2km_2001[i])+(1/10*NLCD_AK$NLCD_unkown_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_unkown_2km[i]<-(8/10*NLCD_AK$NLCD_unkown_2km_2001[i])+(2/10*NLCD_AK$NLCD_unkown_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_unkown_2km[i]<-(7/10*NLCD_AK$NLCD_unkown_2km_2001[i])+(3/10*NLCD_AK$NLCD_unkown_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_unkown_2km[i]<-(6/10*NLCD_AK$NLCD_unkown_2km_2001[i])+(4/10*NLCD_AK$NLCD_unkown_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_unkown_2km[i]<-(5/10*NLCD_AK$NLCD_unkown_2km_2001[i])+(5/10*NLCD_AK$NLCD_unkown_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_unkown_2km[i]<-(4/10*NLCD_AK$NLCD_unkown_2km_2001[i])+(6/10*NLCD_AK$NLCD_unkown_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_unkown_2km[i]<-(3/10*NLCD_AK$NLCD_unkown_2km_2001[i])+(7/10*NLCD_AK$NLCD_unkown_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_unkown_2km[i]<-(2/10*NLCD_AK$NLCD_unkown_2km_2001[i])+(8/10*NLCD_AK$NLCD_unkown_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_unkown_2km[i]<-(1/10*NLCD_AK$NLCD_unkown_2km_2001[i])+(9/10*NLCD_AK$NLCD_unkown_2km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_unkown_2km[i]<-NLCD_AK$NLCD_unkown_2km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_unkown_2km[i]<-(4/5*NLCD_AK$NLCD_unkown_2km_2011[i])+(1/5*NLCD_AK$NLCD_unkown_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_unkown_2km[i]<-(3/5*NLCD_AK$NLCD_unkown_2km_2011[i])+(2/5*NLCD_AK$NLCD_unkown_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_unkown_2km[i]<-(2/5*NLCD_AK$NLCD_unkown_2km_2011[i])+(3/5*NLCD_AK$NLCD_unkown_2km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_unkown_2km[i]<-(1/5*NLCD_AK$NLCD_unkown_2km_2011[i])+(4/5*NLCD_AK$NLCD_unkown_2km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_unkown_2km[i]<-NLCD_AK$NLCD_unkown_2km_2016[i]
}

###For Open water
NLCD_AK$NLCD_open_water_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_open_water_1km[i]<-NLCD_AK$NLCD_open_water_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_open_water_1km[i]<-(9/10*NLCD_AK$NLCD_open_water_1km_2001[i])+(1/10*NLCD_AK$NLCD_open_water_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_open_water_1km[i]<-(8/10*NLCD_AK$NLCD_open_water_1km_2001[i])+(2/10*NLCD_AK$NLCD_open_water_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_open_water_1km[i]<-(7/10*NLCD_AK$NLCD_open_water_1km_2001[i])+(3/10*NLCD_AK$NLCD_open_water_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_open_water_1km[i]<-(6/10*NLCD_AK$NLCD_open_water_1km_2001[i])+(4/10*NLCD_AK$NLCD_open_water_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_open_water_1km[i]<-(5/10*NLCD_AK$NLCD_open_water_1km_2001[i])+(5/10*NLCD_AK$NLCD_open_water_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_open_water_1km[i]<-(4/10*NLCD_AK$NLCD_open_water_1km_2001[i])+(6/10*NLCD_AK$NLCD_open_water_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_open_water_1km[i]<-(3/10*NLCD_AK$NLCD_open_water_1km_2001[i])+(7/10*NLCD_AK$NLCD_open_water_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_open_water_1km[i]<-(2/10*NLCD_AK$NLCD_open_water_1km_2001[i])+(8/10*NLCD_AK$NLCD_open_water_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_open_water_1km[i]<-(1/10*NLCD_AK$NLCD_open_water_1km_2001[i])+(9/10*NLCD_AK$NLCD_open_water_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_open_water_1km[i]<-NLCD_AK$NLCD_open_water_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_open_water_1km[i]<-(4/5*NLCD_AK$NLCD_open_water_1km_2011[i])+(1/5*NLCD_AK$NLCD_open_water_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_open_water_1km[i]<-(3/5*NLCD_AK$NLCD_open_water_1km_2011[i])+(2/5*NLCD_AK$NLCD_open_water_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_open_water_1km[i]<-(2/5*NLCD_AK$NLCD_open_water_1km_2011[i])+(3/5*NLCD_AK$NLCD_open_water_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_open_water_1km[i]<-(1/5*NLCD_AK$NLCD_open_water_1km_2011[i])+(4/5*NLCD_AK$NLCD_open_water_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_open_water_1km[i]<-NLCD_AK$NLCD_open_water_1km_2016[i]
}

###For NLCD_ice
NLCD_AK$NLCD_ice_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_ice_1km[i]<-NLCD_AK$NLCD_ice_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_ice_1km[i]<-(9/10*NLCD_AK$NLCD_ice_1km_2001[i])+(1/10*NLCD_AK$NLCD_ice_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_ice_1km[i]<-(8/10*NLCD_AK$NLCD_ice_1km_2001[i])+(2/10*NLCD_AK$NLCD_ice_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_ice_1km[i]<-(7/10*NLCD_AK$NLCD_ice_1km_2001[i])+(3/10*NLCD_AK$NLCD_ice_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_ice_1km[i]<-(6/10*NLCD_AK$NLCD_ice_1km_2001[i])+(4/10*NLCD_AK$NLCD_ice_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_ice_1km[i]<-(5/10*NLCD_AK$NLCD_ice_1km_2001[i])+(5/10*NLCD_AK$NLCD_ice_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_ice_1km[i]<-(4/10*NLCD_AK$NLCD_ice_1km_2001[i])+(6/10*NLCD_AK$NLCD_ice_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_ice_1km[i]<-(3/10*NLCD_AK$NLCD_ice_1km_2001[i])+(7/10*NLCD_AK$NLCD_ice_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_ice_1km[i]<-(2/10*NLCD_AK$NLCD_ice_1km_2001[i])+(8/10*NLCD_AK$NLCD_ice_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_ice_1km[i]<-(1/10*NLCD_AK$NLCD_ice_1km_2001[i])+(9/10*NLCD_AK$NLCD_ice_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_ice_1km[i]<-NLCD_AK$NLCD_ice_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_ice_1km[i]<-(4/5*NLCD_AK$NLCD_ice_1km_2011[i])+(1/5*NLCD_AK$NLCD_ice_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_ice_1km[i]<-(3/5*NLCD_AK$NLCD_ice_1km_2011[i])+(2/5*NLCD_AK$NLCD_ice_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_ice_1km[i]<-(2/5*NLCD_AK$NLCD_ice_1km_2011[i])+(3/5*NLCD_AK$NLCD_ice_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_ice_1km[i]<-(1/5*NLCD_AK$NLCD_ice_1km_2011[i])+(4/5*NLCD_AK$NLCD_ice_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_ice_1km[i]<-NLCD_AK$NLCD_ice_1km_2016[i]
}

###For Development open
NLCD_AK$NLCD_developed_open_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_open_1km[i]<-NLCD_AK$NLCD_developed_open_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_open_1km[i]<-(9/10*NLCD_AK$NLCD_developed_open_1km_2001[i])+(1/10*NLCD_AK$NLCD_developed_open_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_open_1km[i]<-(8/10*NLCD_AK$NLCD_developed_open_1km_2001[i])+(2/10*NLCD_AK$NLCD_developed_open_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_open_1km[i]<-(7/10*NLCD_AK$NLCD_developed_open_1km_2001[i])+(3/10*NLCD_AK$NLCD_developed_open_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_open_1km[i]<-(6/10*NLCD_AK$NLCD_developed_open_1km_2001[i])+(4/10*NLCD_AK$NLCD_developed_open_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_open_1km[i]<-(5/10*NLCD_AK$NLCD_developed_open_1km_2001[i])+(5/10*NLCD_AK$NLCD_developed_open_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_open_1km[i]<-(4/10*NLCD_AK$NLCD_developed_open_1km_2001[i])+(6/10*NLCD_AK$NLCD_developed_open_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_open_1km[i]<-(3/10*NLCD_AK$NLCD_developed_open_1km_2001[i])+(7/10*NLCD_AK$NLCD_developed_open_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_open_1km[i]<-(2/10*NLCD_AK$NLCD_developed_open_1km_2001[i])+(8/10*NLCD_AK$NLCD_developed_open_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_open_1km[i]<-(1/10*NLCD_AK$NLCD_developed_open_1km_2001[i])+(9/10*NLCD_AK$NLCD_developed_open_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_open_1km[i]<-NLCD_AK$NLCD_developed_open_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_open_1km[i]<-(4/5*NLCD_AK$NLCD_developed_open_1km_2011[i])+(1/5*NLCD_AK$NLCD_developed_open_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_open_1km[i]<-(3/5*NLCD_AK$NLCD_developed_open_1km_2011[i])+(2/5*NLCD_AK$NLCD_developed_open_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_open_1km[i]<-(2/5*NLCD_AK$NLCD_developed_open_1km_2011[i])+(3/5*NLCD_AK$NLCD_developed_open_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_open_1km[i]<-(1/5*NLCD_AK$NLCD_developed_open_1km_2011[i])+(4/5*NLCD_AK$NLCD_developed_open_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_open_1km[i]<-NLCD_AK$NLCD_developed_open_1km_2016[i]
}

###For Development low
NLCD_AK$NLCD_developed_low_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_low_1km[i]<-NLCD_AK$NLCD_developed_low_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_low_1km[i]<-(9/10*NLCD_AK$NLCD_developed_low_1km_2001[i])+(1/10*NLCD_AK$NLCD_developed_low_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_low_1km[i]<-(8/10*NLCD_AK$NLCD_developed_low_1km_2001[i])+(2/10*NLCD_AK$NLCD_developed_low_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_low_1km[i]<-(7/10*NLCD_AK$NLCD_developed_low_1km_2001[i])+(3/10*NLCD_AK$NLCD_developed_low_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_low_1km[i]<-(6/10*NLCD_AK$NLCD_developed_low_1km_2001[i])+(4/10*NLCD_AK$NLCD_developed_low_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_low_1km[i]<-(5/10*NLCD_AK$NLCD_developed_low_1km_2001[i])+(5/10*NLCD_AK$NLCD_developed_low_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_low_1km[i]<-(4/10*NLCD_AK$NLCD_developed_low_1km_2001[i])+(6/10*NLCD_AK$NLCD_developed_low_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_low_1km[i]<-(3/10*NLCD_AK$NLCD_developed_low_1km_2001[i])+(7/10*NLCD_AK$NLCD_developed_low_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_low_1km[i]<-(2/10*NLCD_AK$NLCD_developed_low_1km_2001[i])+(8/10*NLCD_AK$NLCD_developed_low_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_low_1km[i]<-(1/10*NLCD_AK$NLCD_developed_low_1km_2001[i])+(9/10*NLCD_AK$NLCD_developed_low_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_low_1km[i]<-NLCD_AK$NLCD_developed_low_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_low_1km[i]<-(4/5*NLCD_AK$NLCD_developed_low_1km_2011[i])+(1/5*NLCD_AK$NLCD_developed_low_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_low_1km[i]<-(3/5*NLCD_AK$NLCD_developed_low_1km_2011[i])+(2/5*NLCD_AK$NLCD_developed_low_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_low_1km[i]<-(2/5*NLCD_AK$NLCD_developed_low_1km_2011[i])+(3/5*NLCD_AK$NLCD_developed_low_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_low_1km[i]<-(1/5*NLCD_AK$NLCD_developed_low_1km_2011[i])+(4/5*NLCD_AK$NLCD_developed_low_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_low_1km[i]<-NLCD_AK$NLCD_developed_low_1km_2016[i]
}

###For Deveelopment medium
NLCD_AK$NLCD_developed_medium_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_medium_1km[i]<-NLCD_AK$NLCD_developed_medium_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_medium_1km[i]<-(9/10*NLCD_AK$NLCD_developed_medium_1km_2001[i])+(1/10*NLCD_AK$NLCD_developed_medium_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_medium_1km[i]<-(8/10*NLCD_AK$NLCD_developed_medium_1km_2001[i])+(2/10*NLCD_AK$NLCD_developed_medium_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_medium_1km[i]<-(7/10*NLCD_AK$NLCD_developed_medium_1km_2001[i])+(3/10*NLCD_AK$NLCD_developed_medium_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_medium_1km[i]<-(6/10*NLCD_AK$NLCD_developed_medium_1km_2001[i])+(4/10*NLCD_AK$NLCD_developed_medium_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_medium_1km[i]<-(5/10*NLCD_AK$NLCD_developed_medium_1km_2001[i])+(5/10*NLCD_AK$NLCD_developed_medium_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_medium_1km[i]<-(4/10*NLCD_AK$NLCD_developed_medium_1km_2001[i])+(6/10*NLCD_AK$NLCD_developed_medium_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_medium_1km[i]<-(3/10*NLCD_AK$NLCD_developed_medium_1km_2001[i])+(7/10*NLCD_AK$NLCD_developed_medium_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_medium_1km[i]<-(2/10*NLCD_AK$NLCD_developed_medium_1km_2001[i])+(8/10*NLCD_AK$NLCD_developed_medium_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_medium_1km[i]<-(1/10*NLCD_AK$NLCD_developed_medium_1km_2001[i])+(9/10*NLCD_AK$NLCD_developed_medium_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_medium_1km[i]<-NLCD_AK$NLCD_developed_medium_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_medium_1km[i]<-(4/5*NLCD_AK$NLCD_developed_medium_1km_2011[i])+(1/5*NLCD_AK$NLCD_developed_medium_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_medium_1km[i]<-(3/5*NLCD_AK$NLCD_developed_medium_1km_2011[i])+(2/5*NLCD_AK$NLCD_developed_medium_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_medium_1km[i]<-(2/5*NLCD_AK$NLCD_developed_medium_1km_2011[i])+(3/5*NLCD_AK$NLCD_developed_medium_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_medium_1km[i]<-(1/5*NLCD_AK$NLCD_developed_medium_1km_2011[i])+(4/5*NLCD_AK$NLCD_developed_medium_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_medium_1km[i]<-NLCD_AK$NLCD_developed_medium_1km_2016[i]
}

###For NLCD_developed high
NLCD_AK$NLCD_developed_high_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_high_1km[i]<-NLCD_AK$NLCD_developed_high_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_high_1km[i]<-(9/10*NLCD_AK$NLCD_developed_high_1km_2001[i])+(1/10*NLCD_AK$NLCD_developed_high_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_high_1km[i]<-(8/10*NLCD_AK$NLCD_developed_high_1km_2001[i])+(2/10*NLCD_AK$NLCD_developed_high_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_high_1km[i]<-(7/10*NLCD_AK$NLCD_developed_high_1km_2001[i])+(3/10*NLCD_AK$NLCD_developed_high_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_high_1km[i]<-(6/10*NLCD_AK$NLCD_developed_high_1km_2001[i])+(4/10*NLCD_AK$NLCD_developed_high_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_high_1km[i]<-(5/10*NLCD_AK$NLCD_developed_high_1km_2001[i])+(5/10*NLCD_AK$NLCD_developed_high_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_high_1km[i]<-(4/10*NLCD_AK$NLCD_developed_high_1km_2001[i])+(6/10*NLCD_AK$NLCD_developed_high_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_high_1km[i]<-(3/10*NLCD_AK$NLCD_developed_high_1km_2001[i])+(7/10*NLCD_AK$NLCD_developed_high_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_high_1km[i]<-(2/10*NLCD_AK$NLCD_developed_high_1km_2001[i])+(8/10*NLCD_AK$NLCD_developed_high_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_high_1km[i]<-(1/10*NLCD_AK$NLCD_developed_high_1km_2001[i])+(9/10*NLCD_AK$NLCD_developed_high_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_high_1km[i]<-NLCD_AK$NLCD_developed_high_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_high_1km[i]<-(4/5*NLCD_AK$NLCD_developed_high_1km_2011[i])+(1/5*NLCD_AK$NLCD_developed_high_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_high_1km[i]<-(3/5*NLCD_AK$NLCD_developed_high_1km_2011[i])+(2/5*NLCD_AK$NLCD_developed_high_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_high_1km[i]<-(2/5*NLCD_AK$NLCD_developed_high_1km_2011[i])+(3/5*NLCD_AK$NLCD_developed_high_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_high_1km[i]<-(1/5*NLCD_AK$NLCD_developed_high_1km_2011[i])+(4/5*NLCD_AK$NLCD_developed_high_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_high_1km[i]<-NLCD_AK$NLCD_developed_high_1km_2016[i]
}

###For Barren land
NLCD_AK$NLCD_barren_land_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_barren_land_1km[i]<-NLCD_AK$NLCD_barren_land_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_barren_land_1km[i]<-(9/10*NLCD_AK$NLCD_barren_land_1km_2001[i])+(1/10*NLCD_AK$NLCD_barren_land_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_barren_land_1km[i]<-(8/10*NLCD_AK$NLCD_barren_land_1km_2001[i])+(2/10*NLCD_AK$NLCD_barren_land_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_barren_land_1km[i]<-(7/10*NLCD_AK$NLCD_barren_land_1km_2001[i])+(3/10*NLCD_AK$NLCD_barren_land_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_barren_land_1km[i]<-(6/10*NLCD_AK$NLCD_barren_land_1km_2001[i])+(4/10*NLCD_AK$NLCD_barren_land_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_barren_land_1km[i]<-(5/10*NLCD_AK$NLCD_barren_land_1km_2001[i])+(5/10*NLCD_AK$NLCD_barren_land_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_barren_land_1km[i]<-(4/10*NLCD_AK$NLCD_barren_land_1km_2001[i])+(6/10*NLCD_AK$NLCD_barren_land_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_barren_land_1km[i]<-(3/10*NLCD_AK$NLCD_barren_land_1km_2001[i])+(7/10*NLCD_AK$NLCD_barren_land_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_barren_land_1km[i]<-(2/10*NLCD_AK$NLCD_barren_land_1km_2001[i])+(8/10*NLCD_AK$NLCD_barren_land_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_barren_land_1km[i]<-(1/10*NLCD_AK$NLCD_barren_land_1km_2001[i])+(9/10*NLCD_AK$NLCD_barren_land_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_barren_land_1km[i]<-NLCD_AK$NLCD_barren_land_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_barren_land_1km[i]<-(4/5*NLCD_AK$NLCD_barren_land_1km_2011[i])+(1/5*NLCD_AK$NLCD_barren_land_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_barren_land_1km[i]<-(3/5*NLCD_AK$NLCD_barren_land_1km_2011[i])+(2/5*NLCD_AK$NLCD_barren_land_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_barren_land_1km[i]<-(2/5*NLCD_AK$NLCD_barren_land_1km_2011[i])+(3/5*NLCD_AK$NLCD_barren_land_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_barren_land_1km[i]<-(1/5*NLCD_AK$NLCD_barren_land_1km_2011[i])+(4/5*NLCD_AK$NLCD_barren_land_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_barren_land_1km[i]<-NLCD_AK$NLCD_barren_land_1km_2016[i]
}

###For Decidious forest
NLCD_AK$NLCD_decidious_forest_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_decidious_forest_1km[i]<-NLCD_AK$NLCD_decidious_forest_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_decidious_forest_1km[i]<-(9/10*NLCD_AK$NLCD_decidious_forest_1km_2001[i])+(1/10*NLCD_AK$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_decidious_forest_1km[i]<-(8/10*NLCD_AK$NLCD_decidious_forest_1km_2001[i])+(2/10*NLCD_AK$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_decidious_forest_1km[i]<-(7/10*NLCD_AK$NLCD_decidious_forest_1km_2001[i])+(3/10*NLCD_AK$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_decidious_forest_1km[i]<-(6/10*NLCD_AK$NLCD_decidious_forest_1km_2001[i])+(4/10*NLCD_AK$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_decidious_forest_1km[i]<-(5/10*NLCD_AK$NLCD_decidious_forest_1km_2001[i])+(5/10*NLCD_AK$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_decidious_forest_1km[i]<-(4/10*NLCD_AK$NLCD_decidious_forest_1km_2001[i])+(6/10*NLCD_AK$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_decidious_forest_1km[i]<-(3/10*NLCD_AK$NLCD_decidious_forest_1km_2001[i])+(7/10*NLCD_AK$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_decidious_forest_1km[i]<-(2/10*NLCD_AK$NLCD_decidious_forest_1km_2001[i])+(8/10*NLCD_AK$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_decidious_forest_1km[i]<-(1/10*NLCD_AK$NLCD_decidious_forest_1km_2001[i])+(9/10*NLCD_AK$NLCD_decidious_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_decidious_forest_1km[i]<-NLCD_AK$NLCD_decidious_forest_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_decidious_forest_1km[i]<-(4/5*NLCD_AK$NLCD_decidious_forest_1km_2011[i])+(1/5*NLCD_AK$NLCD_decidious_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_decidious_forest_1km[i]<-(3/5*NLCD_AK$NLCD_decidious_forest_1km_2011[i])+(2/5*NLCD_AK$NLCD_decidious_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_decidious_forest_1km[i]<-(2/5*NLCD_AK$NLCD_decidious_forest_1km_2011[i])+(3/5*NLCD_AK$NLCD_decidious_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_decidious_forest_1km[i]<-(1/5*NLCD_AK$NLCD_decidious_forest_1km_2011[i])+(4/5*NLCD_AK$NLCD_decidious_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_decidious_forest_1km[i]<-NLCD_AK$NLCD_decidious_forest_1km_2016[i]
}


###For Evergreen forest
NLCD_AK$NLCD_evergreen_forest_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_evergreen_forest_1km[i]<-NLCD_AK$NLCD_evergreen_forest_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(9/10*NLCD_AK$NLCD_evergreen_forest_1km_2001[i])+(1/10*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(8/10*NLCD_AK$NLCD_evergreen_forest_1km_2001[i])+(2/10*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(7/10*NLCD_AK$NLCD_evergreen_forest_1km_2001[i])+(3/10*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(6/10*NLCD_AK$NLCD_evergreen_forest_1km_2001[i])+(4/10*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(5/10*NLCD_AK$NLCD_evergreen_forest_1km_2001[i])+(5/10*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(4/10*NLCD_AK$NLCD_evergreen_forest_1km_2001[i])+(6/10*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(3/10*NLCD_AK$NLCD_evergreen_forest_1km_2001[i])+(7/10*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(2/10*NLCD_AK$NLCD_evergreen_forest_1km_2001[i])+(8/10*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(1/10*NLCD_AK$NLCD_evergreen_forest_1km_2001[i])+(9/10*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_evergreen_forest_1km[i]<-NLCD_AK$NLCD_evergreen_forest_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(4/5*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])+(1/5*NLCD_AK$NLCD_evergreen_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(3/5*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])+(2/5*NLCD_AK$NLCD_evergreen_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(2/5*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])+(3/5*NLCD_AK$NLCD_evergreen_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_evergreen_forest_1km[i]<-(1/5*NLCD_AK$NLCD_evergreen_forest_1km_2011[i])+(4/5*NLCD_AK$NLCD_evergreen_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_evergreen_forest_1km[i]<-NLCD_AK$NLCD_evergreen_forest_1km_2016[i]
}

###For Mixed forest
NLCD_AK$NLCD_mixed_forest_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_mixed_forest_1km[i]<-NLCD_AK$NLCD_mixed_forest_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_mixed_forest_1km[i]<-(9/10*NLCD_AK$NLCD_mixed_forest_1km_2001[i])+(1/10*NLCD_AK$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_mixed_forest_1km[i]<-(8/10*NLCD_AK$NLCD_mixed_forest_1km_2001[i])+(2/10*NLCD_AK$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_mixed_forest_1km[i]<-(7/10*NLCD_AK$NLCD_mixed_forest_1km_2001[i])+(3/10*NLCD_AK$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_mixed_forest_1km[i]<-(6/10*NLCD_AK$NLCD_mixed_forest_1km_2001[i])+(4/10*NLCD_AK$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_mixed_forest_1km[i]<-(5/10*NLCD_AK$NLCD_mixed_forest_1km_2001[i])+(5/10*NLCD_AK$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_mixed_forest_1km[i]<-(4/10*NLCD_AK$NLCD_mixed_forest_1km_2001[i])+(6/10*NLCD_AK$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_mixed_forest_1km[i]<-(3/10*NLCD_AK$NLCD_mixed_forest_1km_2001[i])+(7/10*NLCD_AK$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_mixed_forest_1km[i]<-(2/10*NLCD_AK$NLCD_mixed_forest_1km_2001[i])+(8/10*NLCD_AK$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_mixed_forest_1km[i]<-(1/10*NLCD_AK$NLCD_mixed_forest_1km_2001[i])+(9/10*NLCD_AK$NLCD_mixed_forest_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_mixed_forest_1km[i]<-NLCD_AK$NLCD_mixed_forest_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_mixed_forest_1km[i]<-(4/5*NLCD_AK$NLCD_mixed_forest_1km_2011[i])+(1/5*NLCD_AK$NLCD_mixed_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_mixed_forest_1km[i]<-(3/5*NLCD_AK$NLCD_mixed_forest_1km_2011[i])+(2/5*NLCD_AK$NLCD_mixed_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_mixed_forest_1km[i]<-(2/5*NLCD_AK$NLCD_mixed_forest_1km_2011[i])+(3/5*NLCD_AK$NLCD_mixed_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_mixed_forest_1km[i]<-(1/5*NLCD_AK$NLCD_mixed_forest_1km_2011[i])+(4/5*NLCD_AK$NLCD_mixed_forest_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_mixed_forest_1km[i]<-NLCD_AK$NLCD_mixed_forest_1km_2016[i]
}

###For Dwrf NLCD_shrub
NLCD_AK$NLCD_dwarf_shrub_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_dwarf_shrub_1km[i]<-NLCD_AK$NLCD_dwarf_shrub_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(9/10*NLCD_AK$NLCD_dwarf_shrub_1km_2001[i])+(1/10*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(8/10*NLCD_AK$NLCD_dwarf_shrub_1km_2001[i])+(2/10*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(7/10*NLCD_AK$NLCD_dwarf_shrub_1km_2001[i])+(3/10*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(6/10*NLCD_AK$NLCD_dwarf_shrub_1km_2001[i])+(4/10*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(5/10*NLCD_AK$NLCD_dwarf_shrub_1km_2001[i])+(5/10*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(4/10*NLCD_AK$NLCD_dwarf_shrub_1km_2001[i])+(6/10*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(3/10*NLCD_AK$NLCD_dwarf_shrub_1km_2001[i])+(7/10*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(2/10*NLCD_AK$NLCD_dwarf_shrub_1km_2001[i])+(8/10*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(1/10*NLCD_AK$NLCD_dwarf_shrub_1km_2001[i])+(9/10*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-NLCD_AK$NLCD_dwarf_shrub_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(4/5*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])+(1/5*NLCD_AK$NLCD_dwarf_shrub_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(3/5*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])+(2/5*NLCD_AK$NLCD_dwarf_shrub_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(2/5*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])+(3/5*NLCD_AK$NLCD_dwarf_shrub_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-(1/5*NLCD_AK$NLCD_dwarf_shrub_1km_2011[i])+(4/5*NLCD_AK$NLCD_dwarf_shrub_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_dwarf_shrub_1km[i]<-NLCD_AK$NLCD_dwarf_shrub_1km_2016[i]
}


###For NLCD_shrub
NLCD_AK$NLCD_shrub_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_shrub_1km[i]<-NLCD_AK$NLCD_shrub_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_shrub_1km[i]<-(9/10*NLCD_AK$NLCD_shrub_1km_2001[i])+(1/10*NLCD_AK$NLCD_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_shrub_1km[i]<-(8/10*NLCD_AK$NLCD_shrub_1km_2001[i])+(2/10*NLCD_AK$NLCD_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_shrub_1km[i]<-(7/10*NLCD_AK$NLCD_shrub_1km_2001[i])+(3/10*NLCD_AK$NLCD_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_shrub_1km[i]<-(6/10*NLCD_AK$NLCD_shrub_1km_2001[i])+(4/10*NLCD_AK$NLCD_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_shrub_1km[i]<-(5/10*NLCD_AK$NLCD_shrub_1km_2001[i])+(5/10*NLCD_AK$NLCD_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_shrub_1km[i]<-(4/10*NLCD_AK$NLCD_shrub_1km_2001[i])+(6/10*NLCD_AK$NLCD_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_shrub_1km[i]<-(3/10*NLCD_AK$NLCD_shrub_1km_2001[i])+(7/10*NLCD_AK$NLCD_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_shrub_1km[i]<-(2/10*NLCD_AK$NLCD_shrub_1km_2001[i])+(8/10*NLCD_AK$NLCD_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_shrub_1km[i]<-(1/10*NLCD_AK$NLCD_shrub_1km_2001[i])+(9/10*NLCD_AK$NLCD_shrub_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_shrub_1km[i]<-NLCD_AK$NLCD_shrub_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_shrub_1km[i]<-(4/5*NLCD_AK$NLCD_shrub_1km_2011[i])+(1/5*NLCD_AK$NLCD_shrub_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_shrub_1km[i]<-(3/5*NLCD_AK$NLCD_shrub_1km_2011[i])+(2/5*NLCD_AK$NLCD_shrub_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_shrub_1km[i]<-(2/5*NLCD_AK$NLCD_shrub_1km_2011[i])+(3/5*NLCD_AK$NLCD_shrub_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_shrub_1km[i]<-(1/5*NLCD_AK$NLCD_shrub_1km_2011[i])+(4/5*NLCD_AK$NLCD_shrub_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_shrub_1km[i]<-NLCD_AK$NLCD_shrub_1km_2016[i]
}


###For NLCD_grassland
NLCD_AK$NLCD_grassland_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_grassland_1km[i]<-NLCD_AK$NLCD_grassland_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_grassland_1km[i]<-(9/10*NLCD_AK$NLCD_grassland_1km_2001[i])+(1/10*NLCD_AK$NLCD_grassland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_grassland_1km[i]<-(8/10*NLCD_AK$NLCD_grassland_1km_2001[i])+(2/10*NLCD_AK$NLCD_grassland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_grassland_1km[i]<-(7/10*NLCD_AK$NLCD_grassland_1km_2001[i])+(3/10*NLCD_AK$NLCD_grassland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_grassland_1km[i]<-(6/10*NLCD_AK$NLCD_grassland_1km_2001[i])+(4/10*NLCD_AK$NLCD_grassland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_grassland_1km[i]<-(5/10*NLCD_AK$NLCD_grassland_1km_2001[i])+(5/10*NLCD_AK$NLCD_grassland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_grassland_1km[i]<-(4/10*NLCD_AK$NLCD_grassland_1km_2001[i])+(6/10*NLCD_AK$NLCD_grassland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_grassland_1km[i]<-(3/10*NLCD_AK$NLCD_grassland_1km_2001[i])+(7/10*NLCD_AK$NLCD_grassland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_grassland_1km[i]<-(2/10*NLCD_AK$NLCD_grassland_1km_2001[i])+(8/10*NLCD_AK$NLCD_grassland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_grassland_1km[i]<-(1/10*NLCD_AK$NLCD_grassland_1km_2001[i])+(9/10*NLCD_AK$NLCD_grassland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_grassland_1km[i]<-NLCD_AK$NLCD_grassland_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_grassland_1km[i]<-(4/5*NLCD_AK$NLCD_grassland_1km_2011[i])+(1/5*NLCD_AK$NLCD_grassland_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_grassland_1km[i]<-(3/5*NLCD_AK$NLCD_grassland_1km_2011[i])+(2/5*NLCD_AK$NLCD_grassland_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_grassland_1km[i]<-(2/5*NLCD_AK$NLCD_grassland_1km_2011[i])+(3/5*NLCD_AK$NLCD_grassland_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_grassland_1km[i]<-(1/5*NLCD_AK$NLCD_grassland_1km_2011[i])+(4/5*NLCD_AK$NLCD_grassland_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_grassland_1km[i]<-NLCD_AK$NLCD_grassland_1km_2016[i]
}


###For NLCD_sedge
NLCD_AK$NLCD_sedge_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_sedge_1km[i]<-NLCD_AK$NLCD_sedge_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_sedge_1km[i]<-(9/10*NLCD_AK$NLCD_sedge_1km_2001[i])+(1/10*NLCD_AK$NLCD_sedge_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_sedge_1km[i]<-(8/10*NLCD_AK$NLCD_sedge_1km_2001[i])+(2/10*NLCD_AK$NLCD_sedge_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_sedge_1km[i]<-(7/10*NLCD_AK$NLCD_sedge_1km_2001[i])+(3/10*NLCD_AK$NLCD_sedge_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_sedge_1km[i]<-(6/10*NLCD_AK$NLCD_sedge_1km_2001[i])+(4/10*NLCD_AK$NLCD_sedge_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_sedge_1km[i]<-(5/10*NLCD_AK$NLCD_sedge_1km_2001[i])+(5/10*NLCD_AK$NLCD_sedge_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_sedge_1km[i]<-(4/10*NLCD_AK$NLCD_sedge_1km_2001[i])+(6/10*NLCD_AK$NLCD_sedge_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_sedge_1km[i]<-(3/10*NLCD_AK$NLCD_sedge_1km_2001[i])+(7/10*NLCD_AK$NLCD_sedge_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_sedge_1km[i]<-(2/10*NLCD_AK$NLCD_sedge_1km_2001[i])+(8/10*NLCD_AK$NLCD_sedge_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_sedge_1km[i]<-(1/10*NLCD_AK$NLCD_sedge_1km_2001[i])+(9/10*NLCD_AK$NLCD_sedge_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_sedge_1km[i]<-NLCD_AK$NLCD_sedge_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_sedge_1km[i]<-(4/5*NLCD_AK$NLCD_sedge_1km_2011[i])+(1/5*NLCD_AK$NLCD_sedge_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_sedge_1km[i]<-(3/5*NLCD_AK$NLCD_sedge_1km_2011[i])+(2/5*NLCD_AK$NLCD_sedge_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_sedge_1km[i]<-(2/5*NLCD_AK$NLCD_sedge_1km_2011[i])+(3/5*NLCD_AK$NLCD_sedge_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_sedge_1km[i]<-(1/5*NLCD_AK$NLCD_sedge_1km_2011[i])+(4/5*NLCD_AK$NLCD_sedge_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_sedge_1km[i]<-NLCD_AK$NLCD_sedge_1km_2016[i]
}


###For NLCD_lichens
NLCD_AK$NLCD_lichens_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_lichens_1km[i]<-NLCD_AK$NLCD_lichens_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_lichens_1km[i]<-(9/10*NLCD_AK$NLCD_lichens_1km_2001[i])+(1/10*NLCD_AK$NLCD_lichens_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_lichens_1km[i]<-(8/10*NLCD_AK$NLCD_lichens_1km_2001[i])+(2/10*NLCD_AK$NLCD_lichens_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_lichens_1km[i]<-(7/10*NLCD_AK$NLCD_lichens_1km_2001[i])+(3/10*NLCD_AK$NLCD_lichens_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_lichens_1km[i]<-(6/10*NLCD_AK$NLCD_lichens_1km_2001[i])+(4/10*NLCD_AK$NLCD_lichens_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_lichens_1km[i]<-(5/10*NLCD_AK$NLCD_lichens_1km_2001[i])+(5/10*NLCD_AK$NLCD_lichens_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_lichens_1km[i]<-(4/10*NLCD_AK$NLCD_lichens_1km_2001[i])+(6/10*NLCD_AK$NLCD_lichens_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_lichens_1km[i]<-(3/10*NLCD_AK$NLCD_lichens_1km_2001[i])+(7/10*NLCD_AK$NLCD_lichens_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_lichens_1km[i]<-(2/10*NLCD_AK$NLCD_lichens_1km_2001[i])+(8/10*NLCD_AK$NLCD_lichens_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_lichens_1km[i]<-(1/10*NLCD_AK$NLCD_lichens_1km_2001[i])+(9/10*NLCD_AK$NLCD_lichens_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_lichens_1km[i]<-NLCD_AK$NLCD_lichens_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_lichens_1km[i]<-(4/5*NLCD_AK$NLCD_lichens_1km_2011[i])+(1/5*NLCD_AK$NLCD_lichens_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_lichens_1km[i]<-(3/5*NLCD_AK$NLCD_lichens_1km_2011[i])+(2/5*NLCD_AK$NLCD_lichens_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_lichens_1km[i]<-(2/5*NLCD_AK$NLCD_lichens_1km_2011[i])+(3/5*NLCD_AK$NLCD_lichens_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_lichens_1km[i]<-(1/5*NLCD_AK$NLCD_lichens_1km_2011[i])+(4/5*NLCD_AK$NLCD_lichens_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_lichens_1km[i]<-NLCD_AK$NLCD_lichens_1km_2016[i]
}


###For NLCD_moss
NLCD_AK$NLCD_moss_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_moss_1km[i]<-NLCD_AK$NLCD_moss_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_moss_1km[i]<-(9/10*NLCD_AK$NLCD_moss_1km_2001[i])+(1/10*NLCD_AK$NLCD_moss_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_moss_1km[i]<-(8/10*NLCD_AK$NLCD_moss_1km_2001[i])+(2/10*NLCD_AK$NLCD_moss_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_moss_1km[i]<-(7/10*NLCD_AK$NLCD_moss_1km_2001[i])+(3/10*NLCD_AK$NLCD_moss_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_moss_1km[i]<-(6/10*NLCD_AK$NLCD_moss_1km_2001[i])+(4/10*NLCD_AK$NLCD_moss_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_moss_1km[i]<-(5/10*NLCD_AK$NLCD_moss_1km_2001[i])+(5/10*NLCD_AK$NLCD_moss_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_moss_1km[i]<-(4/10*NLCD_AK$NLCD_moss_1km_2001[i])+(6/10*NLCD_AK$NLCD_moss_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_moss_1km[i]<-(3/10*NLCD_AK$NLCD_moss_1km_2001[i])+(7/10*NLCD_AK$NLCD_moss_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_moss_1km[i]<-(2/10*NLCD_AK$NLCD_moss_1km_2001[i])+(8/10*NLCD_AK$NLCD_moss_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_moss_1km[i]<-(1/10*NLCD_AK$NLCD_moss_1km_2001[i])+(9/10*NLCD_AK$NLCD_moss_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_moss_1km[i]<-NLCD_AK$NLCD_moss_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_moss_1km[i]<-(4/5*NLCD_AK$NLCD_moss_1km_2011[i])+(1/5*NLCD_AK$NLCD_moss_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_moss_1km[i]<-(3/5*NLCD_AK$NLCD_moss_1km_2011[i])+(2/5*NLCD_AK$NLCD_moss_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_moss_1km[i]<-(2/5*NLCD_AK$NLCD_moss_1km_2011[i])+(3/5*NLCD_AK$NLCD_moss_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_moss_1km[i]<-(1/5*NLCD_AK$NLCD_moss_1km_2011[i])+(4/5*NLCD_AK$NLCD_moss_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_moss_1km[i]<-NLCD_AK$NLCD_moss_1km_2016[i]
}


###For NLCD_pasture
NLCD_AK$NLCD_pasture_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_pasture_1km[i]<-NLCD_AK$NLCD_pasture_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_pasture_1km[i]<-(9/10*NLCD_AK$NLCD_pasture_1km_2001[i])+(1/10*NLCD_AK$NLCD_pasture_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_pasture_1km[i]<-(8/10*NLCD_AK$NLCD_pasture_1km_2001[i])+(2/10*NLCD_AK$NLCD_pasture_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_pasture_1km[i]<-(7/10*NLCD_AK$NLCD_pasture_1km_2001[i])+(3/10*NLCD_AK$NLCD_pasture_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_pasture_1km[i]<-(6/10*NLCD_AK$NLCD_pasture_1km_2001[i])+(4/10*NLCD_AK$NLCD_pasture_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_pasture_1km[i]<-(5/10*NLCD_AK$NLCD_pasture_1km_2001[i])+(5/10*NLCD_AK$NLCD_pasture_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_pasture_1km[i]<-(4/10*NLCD_AK$NLCD_pasture_1km_2001[i])+(6/10*NLCD_AK$NLCD_pasture_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_pasture_1km[i]<-(3/10*NLCD_AK$NLCD_pasture_1km_2001[i])+(7/10*NLCD_AK$NLCD_pasture_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_pasture_1km[i]<-(2/10*NLCD_AK$NLCD_pasture_1km_2001[i])+(8/10*NLCD_AK$NLCD_pasture_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_pasture_1km[i]<-(1/10*NLCD_AK$NLCD_pasture_1km_2001[i])+(9/10*NLCD_AK$NLCD_pasture_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_pasture_1km[i]<-NLCD_AK$NLCD_pasture_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_pasture_1km[i]<-(4/5*NLCD_AK$NLCD_pasture_1km_2011[i])+(1/5*NLCD_AK$NLCD_pasture_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_pasture_1km[i]<-(3/5*NLCD_AK$NLCD_pasture_1km_2011[i])+(2/5*NLCD_AK$NLCD_pasture_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_pasture_1km[i]<-(2/5*NLCD_AK$NLCD_pasture_1km_2011[i])+(3/5*NLCD_AK$NLCD_pasture_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_pasture_1km[i]<-(1/5*NLCD_AK$NLCD_pasture_1km_2011[i])+(4/5*NLCD_AK$NLCD_pasture_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_pasture_1km[i]<-NLCD_AK$NLCD_pasture_1km_2016[i]
}

###For Cultivated crops
NLCD_AK$NLCD_cultivated_crops_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_cultivated_crops_1km[i]<-NLCD_AK$NLCD_cultivated_crops_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(9/10*NLCD_AK$NLCD_cultivated_crops_1km_2001[i])+(1/10*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(8/10*NLCD_AK$NLCD_cultivated_crops_1km_2001[i])+(2/10*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(7/10*NLCD_AK$NLCD_cultivated_crops_1km_2001[i])+(3/10*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(6/10*NLCD_AK$NLCD_cultivated_crops_1km_2001[i])+(4/10*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(5/10*NLCD_AK$NLCD_cultivated_crops_1km_2001[i])+(5/10*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(4/10*NLCD_AK$NLCD_cultivated_crops_1km_2001[i])+(6/10*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(3/10*NLCD_AK$NLCD_cultivated_crops_1km_2001[i])+(7/10*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(2/10*NLCD_AK$NLCD_cultivated_crops_1km_2001[i])+(8/10*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(1/10*NLCD_AK$NLCD_cultivated_crops_1km_2001[i])+(9/10*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_cultivated_crops_1km[i]<-NLCD_AK$NLCD_cultivated_crops_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(4/5*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])+(1/5*NLCD_AK$NLCD_cultivated_crops_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(3/5*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])+(2/5*NLCD_AK$NLCD_cultivated_crops_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(2/5*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])+(3/5*NLCD_AK$NLCD_cultivated_crops_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_cultivated_crops_1km[i]<-(1/5*NLCD_AK$NLCD_cultivated_crops_1km_2011[i])+(4/5*NLCD_AK$NLCD_cultivated_crops_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_cultivated_crops_1km[i]<-NLCD_AK$NLCD_cultivated_crops_1km_2016[i]
}


###For Woody wetland
NLCD_AK$NLCD_woody_wetland_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_woody_wetland_1km[i]<-NLCD_AK$NLCD_woody_wetland_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_woody_wetland_1km[i]<-(9/10*NLCD_AK$NLCD_woody_wetland_1km_2001[i])+(1/10*NLCD_AK$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_woody_wetland_1km[i]<-(8/10*NLCD_AK$NLCD_woody_wetland_1km_2001[i])+(2/10*NLCD_AK$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_woody_wetland_1km[i]<-(7/10*NLCD_AK$NLCD_woody_wetland_1km_2001[i])+(3/10*NLCD_AK$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_woody_wetland_1km[i]<-(6/10*NLCD_AK$NLCD_woody_wetland_1km_2001[i])+(4/10*NLCD_AK$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_woody_wetland_1km[i]<-(5/10*NLCD_AK$NLCD_woody_wetland_1km_2001[i])+(5/10*NLCD_AK$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_woody_wetland_1km[i]<-(4/10*NLCD_AK$NLCD_woody_wetland_1km_2001[i])+(6/10*NLCD_AK$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_woody_wetland_1km[i]<-(3/10*NLCD_AK$NLCD_woody_wetland_1km_2001[i])+(7/10*NLCD_AK$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_woody_wetland_1km[i]<-(2/10*NLCD_AK$NLCD_woody_wetland_1km_2001[i])+(8/10*NLCD_AK$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_woody_wetland_1km[i]<-(1/10*NLCD_AK$NLCD_woody_wetland_1km_2001[i])+(9/10*NLCD_AK$NLCD_woody_wetland_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_woody_wetland_1km[i]<-NLCD_AK$NLCD_woody_wetland_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_woody_wetland_1km[i]<-(4/5*NLCD_AK$NLCD_woody_wetland_1km_2011[i])+(1/5*NLCD_AK$NLCD_woody_wetland_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_woody_wetland_1km[i]<-(3/5*NLCD_AK$NLCD_woody_wetland_1km_2011[i])+(2/5*NLCD_AK$NLCD_woody_wetland_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_woody_wetland_1km[i]<-(2/5*NLCD_AK$NLCD_woody_wetland_1km_2011[i])+(3/5*NLCD_AK$NLCD_woody_wetland_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_woody_wetland_1km[i]<-(1/5*NLCD_AK$NLCD_woody_wetland_1km_2011[i])+(4/5*NLCD_AK$NLCD_woody_wetland_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_woody_wetland_1km[i]<-NLCD_AK$NLCD_woody_wetland_1km_2016[i]
}


###For Emergent herbaceous wetlands
NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(9/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(1/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(8/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(2/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(7/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(3/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(6/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(4/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(5/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(5/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(4/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(6/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(3/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(7/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(2/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(8/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(1/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2001[i])+(9/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(4/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])+(1/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(3/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])+(2/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(2/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])+(3/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-(1/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2011[i])+(4/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km[i]<-NLCD_AK$NLCD_emergent_herbaceous_wetlands_1km_2016[i]
}


###For Unknown
NLCD_AK$NLCD_unkown_1km<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_unkown_1km[i]<-NLCD_AK$NLCD_unkown_1km_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_unkown_1km[i]<-(9/10*NLCD_AK$NLCD_unkown_1km_2001[i])+(1/10*NLCD_AK$NLCD_unkown_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_unkown_1km[i]<-(8/10*NLCD_AK$NLCD_unkown_1km_2001[i])+(2/10*NLCD_AK$NLCD_unkown_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_unkown_1km[i]<-(7/10*NLCD_AK$NLCD_unkown_1km_2001[i])+(3/10*NLCD_AK$NLCD_unkown_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_unkown_1km[i]<-(6/10*NLCD_AK$NLCD_unkown_1km_2001[i])+(4/10*NLCD_AK$NLCD_unkown_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_unkown_1km[i]<-(5/10*NLCD_AK$NLCD_unkown_1km_2001[i])+(5/10*NLCD_AK$NLCD_unkown_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_unkown_1km[i]<-(4/10*NLCD_AK$NLCD_unkown_1km_2001[i])+(6/10*NLCD_AK$NLCD_unkown_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_unkown_1km[i]<-(3/10*NLCD_AK$NLCD_unkown_1km_2001[i])+(7/10*NLCD_AK$NLCD_unkown_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_unkown_1km[i]<-(2/10*NLCD_AK$NLCD_unkown_1km_2001[i])+(8/10*NLCD_AK$NLCD_unkown_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_unkown_1km[i]<-(1/10*NLCD_AK$NLCD_unkown_1km_2001[i])+(9/10*NLCD_AK$NLCD_unkown_1km_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_unkown_1km[i]<-NLCD_AK$NLCD_unkown_1km_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_unkown_1km[i]<-(4/5*NLCD_AK$NLCD_unkown_1km_2011[i])+(1/5*NLCD_AK$NLCD_unkown_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_unkown_1km[i]<-(3/5*NLCD_AK$NLCD_unkown_1km_2011[i])+(2/5*NLCD_AK$NLCD_unkown_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_unkown_1km[i]<-(2/5*NLCD_AK$NLCD_unkown_1km_2011[i])+(3/5*NLCD_AK$NLCD_unkown_1km_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_unkown_1km[i]<-(1/5*NLCD_AK$NLCD_unkown_1km_2011[i])+(4/5*NLCD_AK$NLCD_unkown_1km_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_unkown_1km[i]<-NLCD_AK$NLCD_unkown_1km_2016[i]
}

###For Open water
NLCD_AK$NLCD_open_water_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_open_water_500m[i]<-NLCD_AK$NLCD_open_water_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_open_water_500m[i]<-(9/10*NLCD_AK$NLCD_open_water_500m_2001[i])+(1/10*NLCD_AK$NLCD_open_water_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_open_water_500m[i]<-(8/10*NLCD_AK$NLCD_open_water_500m_2001[i])+(2/10*NLCD_AK$NLCD_open_water_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_open_water_500m[i]<-(7/10*NLCD_AK$NLCD_open_water_500m_2001[i])+(3/10*NLCD_AK$NLCD_open_water_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_open_water_500m[i]<-(6/10*NLCD_AK$NLCD_open_water_500m_2001[i])+(4/10*NLCD_AK$NLCD_open_water_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_open_water_500m[i]<-(5/10*NLCD_AK$NLCD_open_water_500m_2001[i])+(5/10*NLCD_AK$NLCD_open_water_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_open_water_500m[i]<-(4/10*NLCD_AK$NLCD_open_water_500m_2001[i])+(6/10*NLCD_AK$NLCD_open_water_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_open_water_500m[i]<-(3/10*NLCD_AK$NLCD_open_water_500m_2001[i])+(7/10*NLCD_AK$NLCD_open_water_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_open_water_500m[i]<-(2/10*NLCD_AK$NLCD_open_water_500m_2001[i])+(8/10*NLCD_AK$NLCD_open_water_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_open_water_500m[i]<-(1/10*NLCD_AK$NLCD_open_water_500m_2001[i])+(9/10*NLCD_AK$NLCD_open_water_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_open_water_500m[i]<-NLCD_AK$NLCD_open_water_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_open_water_500m[i]<-(4/5*NLCD_AK$NLCD_open_water_500m_2011[i])+(1/5*NLCD_AK$NLCD_open_water_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_open_water_500m[i]<-(3/5*NLCD_AK$NLCD_open_water_500m_2011[i])+(2/5*NLCD_AK$NLCD_open_water_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_open_water_500m[i]<-(2/5*NLCD_AK$NLCD_open_water_500m_2011[i])+(3/5*NLCD_AK$NLCD_open_water_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_open_water_500m[i]<-(1/5*NLCD_AK$NLCD_open_water_500m_2011[i])+(4/5*NLCD_AK$NLCD_open_water_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_open_water_500m[i]<-NLCD_AK$NLCD_open_water_500m_2016[i]
}

###For NLCD_ice
NLCD_AK$NLCD_ice_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_ice_500m[i]<-NLCD_AK$NLCD_ice_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_ice_500m[i]<-(9/10*NLCD_AK$NLCD_ice_500m_2001[i])+(1/10*NLCD_AK$NLCD_ice_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_ice_500m[i]<-(8/10*NLCD_AK$NLCD_ice_500m_2001[i])+(2/10*NLCD_AK$NLCD_ice_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_ice_500m[i]<-(7/10*NLCD_AK$NLCD_ice_500m_2001[i])+(3/10*NLCD_AK$NLCD_ice_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_ice_500m[i]<-(6/10*NLCD_AK$NLCD_ice_500m_2001[i])+(4/10*NLCD_AK$NLCD_ice_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_ice_500m[i]<-(5/10*NLCD_AK$NLCD_ice_500m_2001[i])+(5/10*NLCD_AK$NLCD_ice_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_ice_500m[i]<-(4/10*NLCD_AK$NLCD_ice_500m_2001[i])+(6/10*NLCD_AK$NLCD_ice_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_ice_500m[i]<-(3/10*NLCD_AK$NLCD_ice_500m_2001[i])+(7/10*NLCD_AK$NLCD_ice_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_ice_500m[i]<-(2/10*NLCD_AK$NLCD_ice_500m_2001[i])+(8/10*NLCD_AK$NLCD_ice_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_ice_500m[i]<-(1/10*NLCD_AK$NLCD_ice_500m_2001[i])+(9/10*NLCD_AK$NLCD_ice_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_ice_500m[i]<-NLCD_AK$NLCD_ice_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_ice_500m[i]<-(4/5*NLCD_AK$NLCD_ice_500m_2011[i])+(1/5*NLCD_AK$NLCD_ice_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_ice_500m[i]<-(3/5*NLCD_AK$NLCD_ice_500m_2011[i])+(2/5*NLCD_AK$NLCD_ice_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_ice_500m[i]<-(2/5*NLCD_AK$NLCD_ice_500m_2011[i])+(3/5*NLCD_AK$NLCD_ice_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_ice_500m[i]<-(1/5*NLCD_AK$NLCD_ice_500m_2011[i])+(4/5*NLCD_AK$NLCD_ice_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_ice_500m[i]<-NLCD_AK$NLCD_ice_500m_2016[i]
}

###For Development open
NLCD_AK$NLCD_developed_open_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_open_500m[i]<-NLCD_AK$NLCD_developed_open_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_open_500m[i]<-(9/10*NLCD_AK$NLCD_developed_open_500m_2001[i])+(1/10*NLCD_AK$NLCD_developed_open_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_open_500m[i]<-(8/10*NLCD_AK$NLCD_developed_open_500m_2001[i])+(2/10*NLCD_AK$NLCD_developed_open_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_open_500m[i]<-(7/10*NLCD_AK$NLCD_developed_open_500m_2001[i])+(3/10*NLCD_AK$NLCD_developed_open_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_open_500m[i]<-(6/10*NLCD_AK$NLCD_developed_open_500m_2001[i])+(4/10*NLCD_AK$NLCD_developed_open_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_open_500m[i]<-(5/10*NLCD_AK$NLCD_developed_open_500m_2001[i])+(5/10*NLCD_AK$NLCD_developed_open_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_open_500m[i]<-(4/10*NLCD_AK$NLCD_developed_open_500m_2001[i])+(6/10*NLCD_AK$NLCD_developed_open_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_open_500m[i]<-(3/10*NLCD_AK$NLCD_developed_open_500m_2001[i])+(7/10*NLCD_AK$NLCD_developed_open_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_open_500m[i]<-(2/10*NLCD_AK$NLCD_developed_open_500m_2001[i])+(8/10*NLCD_AK$NLCD_developed_open_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_open_500m[i]<-(1/10*NLCD_AK$NLCD_developed_open_500m_2001[i])+(9/10*NLCD_AK$NLCD_developed_open_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_open_500m[i]<-NLCD_AK$NLCD_developed_open_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_open_500m[i]<-(4/5*NLCD_AK$NLCD_developed_open_500m_2011[i])+(1/5*NLCD_AK$NLCD_developed_open_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_open_500m[i]<-(3/5*NLCD_AK$NLCD_developed_open_500m_2011[i])+(2/5*NLCD_AK$NLCD_developed_open_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_open_500m[i]<-(2/5*NLCD_AK$NLCD_developed_open_500m_2011[i])+(3/5*NLCD_AK$NLCD_developed_open_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_open_500m[i]<-(1/5*NLCD_AK$NLCD_developed_open_500m_2011[i])+(4/5*NLCD_AK$NLCD_developed_open_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_open_500m[i]<-NLCD_AK$NLCD_developed_open_500m_2016[i]
}

###For Development low
NLCD_AK$NLCD_developed_low_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_low_500m[i]<-NLCD_AK$NLCD_developed_low_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_low_500m[i]<-(9/10*NLCD_AK$NLCD_developed_low_500m_2001[i])+(1/10*NLCD_AK$NLCD_developed_low_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_low_500m[i]<-(8/10*NLCD_AK$NLCD_developed_low_500m_2001[i])+(2/10*NLCD_AK$NLCD_developed_low_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_low_500m[i]<-(7/10*NLCD_AK$NLCD_developed_low_500m_2001[i])+(3/10*NLCD_AK$NLCD_developed_low_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_low_500m[i]<-(6/10*NLCD_AK$NLCD_developed_low_500m_2001[i])+(4/10*NLCD_AK$NLCD_developed_low_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_low_500m[i]<-(5/10*NLCD_AK$NLCD_developed_low_500m_2001[i])+(5/10*NLCD_AK$NLCD_developed_low_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_low_500m[i]<-(4/10*NLCD_AK$NLCD_developed_low_500m_2001[i])+(6/10*NLCD_AK$NLCD_developed_low_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_low_500m[i]<-(3/10*NLCD_AK$NLCD_developed_low_500m_2001[i])+(7/10*NLCD_AK$NLCD_developed_low_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_low_500m[i]<-(2/10*NLCD_AK$NLCD_developed_low_500m_2001[i])+(8/10*NLCD_AK$NLCD_developed_low_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_low_500m[i]<-(1/10*NLCD_AK$NLCD_developed_low_500m_2001[i])+(9/10*NLCD_AK$NLCD_developed_low_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_low_500m[i]<-NLCD_AK$NLCD_developed_low_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_low_500m[i]<-(4/5*NLCD_AK$NLCD_developed_low_500m_2011[i])+(1/5*NLCD_AK$NLCD_developed_low_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_low_500m[i]<-(3/5*NLCD_AK$NLCD_developed_low_500m_2011[i])+(2/5*NLCD_AK$NLCD_developed_low_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_low_500m[i]<-(2/5*NLCD_AK$NLCD_developed_low_500m_2011[i])+(3/5*NLCD_AK$NLCD_developed_low_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_low_500m[i]<-(1/5*NLCD_AK$NLCD_developed_low_500m_2011[i])+(4/5*NLCD_AK$NLCD_developed_low_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_low_500m[i]<-NLCD_AK$NLCD_developed_low_500m_2016[i]
}

###For Deveelopment medium
NLCD_AK$NLCD_developed_medium_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_medium_500m[i]<-NLCD_AK$NLCD_developed_medium_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_medium_500m[i]<-(9/10*NLCD_AK$NLCD_developed_medium_500m_2001[i])+(1/10*NLCD_AK$NLCD_developed_medium_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_medium_500m[i]<-(8/10*NLCD_AK$NLCD_developed_medium_500m_2001[i])+(2/10*NLCD_AK$NLCD_developed_medium_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_medium_500m[i]<-(7/10*NLCD_AK$NLCD_developed_medium_500m_2001[i])+(3/10*NLCD_AK$NLCD_developed_medium_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_medium_500m[i]<-(6/10*NLCD_AK$NLCD_developed_medium_500m_2001[i])+(4/10*NLCD_AK$NLCD_developed_medium_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_medium_500m[i]<-(5/10*NLCD_AK$NLCD_developed_medium_500m_2001[i])+(5/10*NLCD_AK$NLCD_developed_medium_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_medium_500m[i]<-(4/10*NLCD_AK$NLCD_developed_medium_500m_2001[i])+(6/10*NLCD_AK$NLCD_developed_medium_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_medium_500m[i]<-(3/10*NLCD_AK$NLCD_developed_medium_500m_2001[i])+(7/10*NLCD_AK$NLCD_developed_medium_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_medium_500m[i]<-(2/10*NLCD_AK$NLCD_developed_medium_500m_2001[i])+(8/10*NLCD_AK$NLCD_developed_medium_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_medium_500m[i]<-(1/10*NLCD_AK$NLCD_developed_medium_500m_2001[i])+(9/10*NLCD_AK$NLCD_developed_medium_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_medium_500m[i]<-NLCD_AK$NLCD_developed_medium_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_medium_500m[i]<-(4/5*NLCD_AK$NLCD_developed_medium_500m_2011[i])+(1/5*NLCD_AK$NLCD_developed_medium_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_medium_500m[i]<-(3/5*NLCD_AK$NLCD_developed_medium_500m_2011[i])+(2/5*NLCD_AK$NLCD_developed_medium_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_medium_500m[i]<-(2/5*NLCD_AK$NLCD_developed_medium_500m_2011[i])+(3/5*NLCD_AK$NLCD_developed_medium_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_medium_500m[i]<-(1/5*NLCD_AK$NLCD_developed_medium_500m_2011[i])+(4/5*NLCD_AK$NLCD_developed_medium_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_medium_500m[i]<-NLCD_AK$NLCD_developed_medium_500m_2016[i]
}

###For NLCD_developed high
NLCD_AK$NLCD_developed_high_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_developed_high_500m[i]<-NLCD_AK$NLCD_developed_high_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_developed_high_500m[i]<-(9/10*NLCD_AK$NLCD_developed_high_500m_2001[i])+(1/10*NLCD_AK$NLCD_developed_high_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_developed_high_500m[i]<-(8/10*NLCD_AK$NLCD_developed_high_500m_2001[i])+(2/10*NLCD_AK$NLCD_developed_high_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_developed_high_500m[i]<-(7/10*NLCD_AK$NLCD_developed_high_500m_2001[i])+(3/10*NLCD_AK$NLCD_developed_high_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_developed_high_500m[i]<-(6/10*NLCD_AK$NLCD_developed_high_500m_2001[i])+(4/10*NLCD_AK$NLCD_developed_high_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_developed_high_500m[i]<-(5/10*NLCD_AK$NLCD_developed_high_500m_2001[i])+(5/10*NLCD_AK$NLCD_developed_high_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_developed_high_500m[i]<-(4/10*NLCD_AK$NLCD_developed_high_500m_2001[i])+(6/10*NLCD_AK$NLCD_developed_high_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_developed_high_500m[i]<-(3/10*NLCD_AK$NLCD_developed_high_500m_2001[i])+(7/10*NLCD_AK$NLCD_developed_high_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_developed_high_500m[i]<-(2/10*NLCD_AK$NLCD_developed_high_500m_2001[i])+(8/10*NLCD_AK$NLCD_developed_high_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_developed_high_500m[i]<-(1/10*NLCD_AK$NLCD_developed_high_500m_2001[i])+(9/10*NLCD_AK$NLCD_developed_high_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_developed_high_500m[i]<-NLCD_AK$NLCD_developed_high_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_developed_high_500m[i]<-(4/5*NLCD_AK$NLCD_developed_high_500m_2011[i])+(1/5*NLCD_AK$NLCD_developed_high_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_developed_high_500m[i]<-(3/5*NLCD_AK$NLCD_developed_high_500m_2011[i])+(2/5*NLCD_AK$NLCD_developed_high_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_developed_high_500m[i]<-(2/5*NLCD_AK$NLCD_developed_high_500m_2011[i])+(3/5*NLCD_AK$NLCD_developed_high_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_developed_high_500m[i]<-(1/5*NLCD_AK$NLCD_developed_high_500m_2011[i])+(4/5*NLCD_AK$NLCD_developed_high_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_developed_high_500m[i]<-NLCD_AK$NLCD_developed_high_500m_2016[i]
}

###For Barren land
NLCD_AK$NLCD_barren_land_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_barren_land_500m[i]<-NLCD_AK$NLCD_barren_land_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_barren_land_500m[i]<-(9/10*NLCD_AK$NLCD_barren_land_500m_2001[i])+(1/10*NLCD_AK$NLCD_barren_land_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_barren_land_500m[i]<-(8/10*NLCD_AK$NLCD_barren_land_500m_2001[i])+(2/10*NLCD_AK$NLCD_barren_land_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_barren_land_500m[i]<-(7/10*NLCD_AK$NLCD_barren_land_500m_2001[i])+(3/10*NLCD_AK$NLCD_barren_land_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_barren_land_500m[i]<-(6/10*NLCD_AK$NLCD_barren_land_500m_2001[i])+(4/10*NLCD_AK$NLCD_barren_land_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_barren_land_500m[i]<-(5/10*NLCD_AK$NLCD_barren_land_500m_2001[i])+(5/10*NLCD_AK$NLCD_barren_land_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_barren_land_500m[i]<-(4/10*NLCD_AK$NLCD_barren_land_500m_2001[i])+(6/10*NLCD_AK$NLCD_barren_land_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_barren_land_500m[i]<-(3/10*NLCD_AK$NLCD_barren_land_500m_2001[i])+(7/10*NLCD_AK$NLCD_barren_land_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_barren_land_500m[i]<-(2/10*NLCD_AK$NLCD_barren_land_500m_2001[i])+(8/10*NLCD_AK$NLCD_barren_land_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_barren_land_500m[i]<-(1/10*NLCD_AK$NLCD_barren_land_500m_2001[i])+(9/10*NLCD_AK$NLCD_barren_land_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_barren_land_500m[i]<-NLCD_AK$NLCD_barren_land_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_barren_land_500m[i]<-(4/5*NLCD_AK$NLCD_barren_land_500m_2011[i])+(1/5*NLCD_AK$NLCD_barren_land_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_barren_land_500m[i]<-(3/5*NLCD_AK$NLCD_barren_land_500m_2011[i])+(2/5*NLCD_AK$NLCD_barren_land_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_barren_land_500m[i]<-(2/5*NLCD_AK$NLCD_barren_land_500m_2011[i])+(3/5*NLCD_AK$NLCD_barren_land_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_barren_land_500m[i]<-(1/5*NLCD_AK$NLCD_barren_land_500m_2011[i])+(4/5*NLCD_AK$NLCD_barren_land_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_barren_land_500m[i]<-NLCD_AK$NLCD_barren_land_500m_2016[i]
}

###For Decidious forest
NLCD_AK$NLCD_decidious_forest_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_decidious_forest_500m[i]<-NLCD_AK$NLCD_decidious_forest_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_decidious_forest_500m[i]<-(9/10*NLCD_AK$NLCD_decidious_forest_500m_2001[i])+(1/10*NLCD_AK$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_decidious_forest_500m[i]<-(8/10*NLCD_AK$NLCD_decidious_forest_500m_2001[i])+(2/10*NLCD_AK$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_decidious_forest_500m[i]<-(7/10*NLCD_AK$NLCD_decidious_forest_500m_2001[i])+(3/10*NLCD_AK$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_decidious_forest_500m[i]<-(6/10*NLCD_AK$NLCD_decidious_forest_500m_2001[i])+(4/10*NLCD_AK$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_decidious_forest_500m[i]<-(5/10*NLCD_AK$NLCD_decidious_forest_500m_2001[i])+(5/10*NLCD_AK$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_decidious_forest_500m[i]<-(4/10*NLCD_AK$NLCD_decidious_forest_500m_2001[i])+(6/10*NLCD_AK$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_decidious_forest_500m[i]<-(3/10*NLCD_AK$NLCD_decidious_forest_500m_2001[i])+(7/10*NLCD_AK$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_decidious_forest_500m[i]<-(2/10*NLCD_AK$NLCD_decidious_forest_500m_2001[i])+(8/10*NLCD_AK$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_decidious_forest_500m[i]<-(1/10*NLCD_AK$NLCD_decidious_forest_500m_2001[i])+(9/10*NLCD_AK$NLCD_decidious_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_decidious_forest_500m[i]<-NLCD_AK$NLCD_decidious_forest_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_decidious_forest_500m[i]<-(4/5*NLCD_AK$NLCD_decidious_forest_500m_2011[i])+(1/5*NLCD_AK$NLCD_decidious_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_decidious_forest_500m[i]<-(3/5*NLCD_AK$NLCD_decidious_forest_500m_2011[i])+(2/5*NLCD_AK$NLCD_decidious_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_decidious_forest_500m[i]<-(2/5*NLCD_AK$NLCD_decidious_forest_500m_2011[i])+(3/5*NLCD_AK$NLCD_decidious_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_decidious_forest_500m[i]<-(1/5*NLCD_AK$NLCD_decidious_forest_500m_2011[i])+(4/5*NLCD_AK$NLCD_decidious_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_decidious_forest_500m[i]<-NLCD_AK$NLCD_decidious_forest_500m_2016[i]
}


###For Evergreen forest
NLCD_AK$NLCD_evergreen_forest_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_evergreen_forest_500m[i]<-NLCD_AK$NLCD_evergreen_forest_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(9/10*NLCD_AK$NLCD_evergreen_forest_500m_2001[i])+(1/10*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(8/10*NLCD_AK$NLCD_evergreen_forest_500m_2001[i])+(2/10*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(7/10*NLCD_AK$NLCD_evergreen_forest_500m_2001[i])+(3/10*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(6/10*NLCD_AK$NLCD_evergreen_forest_500m_2001[i])+(4/10*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(5/10*NLCD_AK$NLCD_evergreen_forest_500m_2001[i])+(5/10*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(4/10*NLCD_AK$NLCD_evergreen_forest_500m_2001[i])+(6/10*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(3/10*NLCD_AK$NLCD_evergreen_forest_500m_2001[i])+(7/10*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(2/10*NLCD_AK$NLCD_evergreen_forest_500m_2001[i])+(8/10*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(1/10*NLCD_AK$NLCD_evergreen_forest_500m_2001[i])+(9/10*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_evergreen_forest_500m[i]<-NLCD_AK$NLCD_evergreen_forest_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(4/5*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])+(1/5*NLCD_AK$NLCD_evergreen_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(3/5*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])+(2/5*NLCD_AK$NLCD_evergreen_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(2/5*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])+(3/5*NLCD_AK$NLCD_evergreen_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_evergreen_forest_500m[i]<-(1/5*NLCD_AK$NLCD_evergreen_forest_500m_2011[i])+(4/5*NLCD_AK$NLCD_evergreen_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_evergreen_forest_500m[i]<-NLCD_AK$NLCD_evergreen_forest_500m_2016[i]
}

###For Mixed forest
NLCD_AK$NLCD_mixed_forest_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_mixed_forest_500m[i]<-NLCD_AK$NLCD_mixed_forest_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_mixed_forest_500m[i]<-(9/10*NLCD_AK$NLCD_mixed_forest_500m_2001[i])+(1/10*NLCD_AK$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_mixed_forest_500m[i]<-(8/10*NLCD_AK$NLCD_mixed_forest_500m_2001[i])+(2/10*NLCD_AK$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_mixed_forest_500m[i]<-(7/10*NLCD_AK$NLCD_mixed_forest_500m_2001[i])+(3/10*NLCD_AK$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_mixed_forest_500m[i]<-(6/10*NLCD_AK$NLCD_mixed_forest_500m_2001[i])+(4/10*NLCD_AK$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_mixed_forest_500m[i]<-(5/10*NLCD_AK$NLCD_mixed_forest_500m_2001[i])+(5/10*NLCD_AK$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_mixed_forest_500m[i]<-(4/10*NLCD_AK$NLCD_mixed_forest_500m_2001[i])+(6/10*NLCD_AK$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_mixed_forest_500m[i]<-(3/10*NLCD_AK$NLCD_mixed_forest_500m_2001[i])+(7/10*NLCD_AK$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_mixed_forest_500m[i]<-(2/10*NLCD_AK$NLCD_mixed_forest_500m_2001[i])+(8/10*NLCD_AK$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_mixed_forest_500m[i]<-(1/10*NLCD_AK$NLCD_mixed_forest_500m_2001[i])+(9/10*NLCD_AK$NLCD_mixed_forest_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_mixed_forest_500m[i]<-NLCD_AK$NLCD_mixed_forest_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_mixed_forest_500m[i]<-(4/5*NLCD_AK$NLCD_mixed_forest_500m_2011[i])+(1/5*NLCD_AK$NLCD_mixed_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_mixed_forest_500m[i]<-(3/5*NLCD_AK$NLCD_mixed_forest_500m_2011[i])+(2/5*NLCD_AK$NLCD_mixed_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_mixed_forest_500m[i]<-(2/5*NLCD_AK$NLCD_mixed_forest_500m_2011[i])+(3/5*NLCD_AK$NLCD_mixed_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_mixed_forest_500m[i]<-(1/5*NLCD_AK$NLCD_mixed_forest_500m_2011[i])+(4/5*NLCD_AK$NLCD_mixed_forest_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_mixed_forest_500m[i]<-NLCD_AK$NLCD_mixed_forest_500m_2016[i]
}

###For Dwrf NLCD_shrub
NLCD_AK$NLCD_dwarf_shrub_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_dwarf_shrub_500m[i]<-NLCD_AK$NLCD_dwarf_shrub_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(9/10*NLCD_AK$NLCD_dwarf_shrub_500m_2001[i])+(1/10*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(8/10*NLCD_AK$NLCD_dwarf_shrub_500m_2001[i])+(2/10*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(7/10*NLCD_AK$NLCD_dwarf_shrub_500m_2001[i])+(3/10*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(6/10*NLCD_AK$NLCD_dwarf_shrub_500m_2001[i])+(4/10*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(5/10*NLCD_AK$NLCD_dwarf_shrub_500m_2001[i])+(5/10*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(4/10*NLCD_AK$NLCD_dwarf_shrub_500m_2001[i])+(6/10*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(3/10*NLCD_AK$NLCD_dwarf_shrub_500m_2001[i])+(7/10*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(2/10*NLCD_AK$NLCD_dwarf_shrub_500m_2001[i])+(8/10*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(1/10*NLCD_AK$NLCD_dwarf_shrub_500m_2001[i])+(9/10*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-NLCD_AK$NLCD_dwarf_shrub_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(4/5*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])+(1/5*NLCD_AK$NLCD_dwarf_shrub_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(3/5*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])+(2/5*NLCD_AK$NLCD_dwarf_shrub_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(2/5*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])+(3/5*NLCD_AK$NLCD_dwarf_shrub_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-(1/5*NLCD_AK$NLCD_dwarf_shrub_500m_2011[i])+(4/5*NLCD_AK$NLCD_dwarf_shrub_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_dwarf_shrub_500m[i]<-NLCD_AK$NLCD_dwarf_shrub_500m_2016[i]
}


###For NLCD_shrub
NLCD_AK$NLCD_shrub_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_shrub_500m[i]<-NLCD_AK$NLCD_shrub_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_shrub_500m[i]<-(9/10*NLCD_AK$NLCD_shrub_500m_2001[i])+(1/10*NLCD_AK$NLCD_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_shrub_500m[i]<-(8/10*NLCD_AK$NLCD_shrub_500m_2001[i])+(2/10*NLCD_AK$NLCD_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_shrub_500m[i]<-(7/10*NLCD_AK$NLCD_shrub_500m_2001[i])+(3/10*NLCD_AK$NLCD_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_shrub_500m[i]<-(6/10*NLCD_AK$NLCD_shrub_500m_2001[i])+(4/10*NLCD_AK$NLCD_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_shrub_500m[i]<-(5/10*NLCD_AK$NLCD_shrub_500m_2001[i])+(5/10*NLCD_AK$NLCD_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_shrub_500m[i]<-(4/10*NLCD_AK$NLCD_shrub_500m_2001[i])+(6/10*NLCD_AK$NLCD_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_shrub_500m[i]<-(3/10*NLCD_AK$NLCD_shrub_500m_2001[i])+(7/10*NLCD_AK$NLCD_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_shrub_500m[i]<-(2/10*NLCD_AK$NLCD_shrub_500m_2001[i])+(8/10*NLCD_AK$NLCD_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_shrub_500m[i]<-(1/10*NLCD_AK$NLCD_shrub_500m_2001[i])+(9/10*NLCD_AK$NLCD_shrub_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_shrub_500m[i]<-NLCD_AK$NLCD_shrub_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_shrub_500m[i]<-(4/5*NLCD_AK$NLCD_shrub_500m_2011[i])+(1/5*NLCD_AK$NLCD_shrub_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_shrub_500m[i]<-(3/5*NLCD_AK$NLCD_shrub_500m_2011[i])+(2/5*NLCD_AK$NLCD_shrub_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_shrub_500m[i]<-(2/5*NLCD_AK$NLCD_shrub_500m_2011[i])+(3/5*NLCD_AK$NLCD_shrub_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_shrub_500m[i]<-(1/5*NLCD_AK$NLCD_shrub_500m_2011[i])+(4/5*NLCD_AK$NLCD_shrub_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_shrub_500m[i]<-NLCD_AK$NLCD_shrub_500m_2016[i]
}


###For NLCD_grassland
NLCD_AK$NLCD_grassland_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_grassland_500m[i]<-NLCD_AK$NLCD_grassland_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_grassland_500m[i]<-(9/10*NLCD_AK$NLCD_grassland_500m_2001[i])+(1/10*NLCD_AK$NLCD_grassland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_grassland_500m[i]<-(8/10*NLCD_AK$NLCD_grassland_500m_2001[i])+(2/10*NLCD_AK$NLCD_grassland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_grassland_500m[i]<-(7/10*NLCD_AK$NLCD_grassland_500m_2001[i])+(3/10*NLCD_AK$NLCD_grassland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_grassland_500m[i]<-(6/10*NLCD_AK$NLCD_grassland_500m_2001[i])+(4/10*NLCD_AK$NLCD_grassland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_grassland_500m[i]<-(5/10*NLCD_AK$NLCD_grassland_500m_2001[i])+(5/10*NLCD_AK$NLCD_grassland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_grassland_500m[i]<-(4/10*NLCD_AK$NLCD_grassland_500m_2001[i])+(6/10*NLCD_AK$NLCD_grassland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_grassland_500m[i]<-(3/10*NLCD_AK$NLCD_grassland_500m_2001[i])+(7/10*NLCD_AK$NLCD_grassland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_grassland_500m[i]<-(2/10*NLCD_AK$NLCD_grassland_500m_2001[i])+(8/10*NLCD_AK$NLCD_grassland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_grassland_500m[i]<-(1/10*NLCD_AK$NLCD_grassland_500m_2001[i])+(9/10*NLCD_AK$NLCD_grassland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_grassland_500m[i]<-NLCD_AK$NLCD_grassland_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_grassland_500m[i]<-(4/5*NLCD_AK$NLCD_grassland_500m_2011[i])+(1/5*NLCD_AK$NLCD_grassland_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_grassland_500m[i]<-(3/5*NLCD_AK$NLCD_grassland_500m_2011[i])+(2/5*NLCD_AK$NLCD_grassland_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_grassland_500m[i]<-(2/5*NLCD_AK$NLCD_grassland_500m_2011[i])+(3/5*NLCD_AK$NLCD_grassland_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_grassland_500m[i]<-(1/5*NLCD_AK$NLCD_grassland_500m_2011[i])+(4/5*NLCD_AK$NLCD_grassland_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_grassland_500m[i]<-NLCD_AK$NLCD_grassland_500m_2016[i]
}


###For NLCD_sedge
NLCD_AK$NLCD_sedge_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_sedge_500m[i]<-NLCD_AK$NLCD_sedge_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_sedge_500m[i]<-(9/10*NLCD_AK$NLCD_sedge_500m_2001[i])+(1/10*NLCD_AK$NLCD_sedge_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_sedge_500m[i]<-(8/10*NLCD_AK$NLCD_sedge_500m_2001[i])+(2/10*NLCD_AK$NLCD_sedge_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_sedge_500m[i]<-(7/10*NLCD_AK$NLCD_sedge_500m_2001[i])+(3/10*NLCD_AK$NLCD_sedge_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_sedge_500m[i]<-(6/10*NLCD_AK$NLCD_sedge_500m_2001[i])+(4/10*NLCD_AK$NLCD_sedge_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_sedge_500m[i]<-(5/10*NLCD_AK$NLCD_sedge_500m_2001[i])+(5/10*NLCD_AK$NLCD_sedge_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_sedge_500m[i]<-(4/10*NLCD_AK$NLCD_sedge_500m_2001[i])+(6/10*NLCD_AK$NLCD_sedge_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_sedge_500m[i]<-(3/10*NLCD_AK$NLCD_sedge_500m_2001[i])+(7/10*NLCD_AK$NLCD_sedge_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_sedge_500m[i]<-(2/10*NLCD_AK$NLCD_sedge_500m_2001[i])+(8/10*NLCD_AK$NLCD_sedge_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_sedge_500m[i]<-(1/10*NLCD_AK$NLCD_sedge_500m_2001[i])+(9/10*NLCD_AK$NLCD_sedge_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_sedge_500m[i]<-NLCD_AK$NLCD_sedge_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_sedge_500m[i]<-(4/5*NLCD_AK$NLCD_sedge_500m_2011[i])+(1/5*NLCD_AK$NLCD_sedge_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_sedge_500m[i]<-(3/5*NLCD_AK$NLCD_sedge_500m_2011[i])+(2/5*NLCD_AK$NLCD_sedge_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_sedge_500m[i]<-(2/5*NLCD_AK$NLCD_sedge_500m_2011[i])+(3/5*NLCD_AK$NLCD_sedge_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_sedge_500m[i]<-(1/5*NLCD_AK$NLCD_sedge_500m_2011[i])+(4/5*NLCD_AK$NLCD_sedge_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_sedge_500m[i]<-NLCD_AK$NLCD_sedge_500m_2016[i]
}


###For NLCD_lichens
NLCD_AK$NLCD_lichens_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_lichens_500m[i]<-NLCD_AK$NLCD_lichens_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_lichens_500m[i]<-(9/10*NLCD_AK$NLCD_lichens_500m_2001[i])+(1/10*NLCD_AK$NLCD_lichens_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_lichens_500m[i]<-(8/10*NLCD_AK$NLCD_lichens_500m_2001[i])+(2/10*NLCD_AK$NLCD_lichens_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_lichens_500m[i]<-(7/10*NLCD_AK$NLCD_lichens_500m_2001[i])+(3/10*NLCD_AK$NLCD_lichens_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_lichens_500m[i]<-(6/10*NLCD_AK$NLCD_lichens_500m_2001[i])+(4/10*NLCD_AK$NLCD_lichens_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_lichens_500m[i]<-(5/10*NLCD_AK$NLCD_lichens_500m_2001[i])+(5/10*NLCD_AK$NLCD_lichens_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_lichens_500m[i]<-(4/10*NLCD_AK$NLCD_lichens_500m_2001[i])+(6/10*NLCD_AK$NLCD_lichens_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_lichens_500m[i]<-(3/10*NLCD_AK$NLCD_lichens_500m_2001[i])+(7/10*NLCD_AK$NLCD_lichens_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_lichens_500m[i]<-(2/10*NLCD_AK$NLCD_lichens_500m_2001[i])+(8/10*NLCD_AK$NLCD_lichens_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_lichens_500m[i]<-(1/10*NLCD_AK$NLCD_lichens_500m_2001[i])+(9/10*NLCD_AK$NLCD_lichens_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_lichens_500m[i]<-NLCD_AK$NLCD_lichens_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_lichens_500m[i]<-(4/5*NLCD_AK$NLCD_lichens_500m_2011[i])+(1/5*NLCD_AK$NLCD_lichens_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_lichens_500m[i]<-(3/5*NLCD_AK$NLCD_lichens_500m_2011[i])+(2/5*NLCD_AK$NLCD_lichens_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_lichens_500m[i]<-(2/5*NLCD_AK$NLCD_lichens_500m_2011[i])+(3/5*NLCD_AK$NLCD_lichens_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_lichens_500m[i]<-(1/5*NLCD_AK$NLCD_lichens_500m_2011[i])+(4/5*NLCD_AK$NLCD_lichens_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_lichens_500m[i]<-NLCD_AK$NLCD_lichens_500m_2016[i]
}


###For NLCD_moss
NLCD_AK$NLCD_moss_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_moss_500m[i]<-NLCD_AK$NLCD_moss_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_moss_500m[i]<-(9/10*NLCD_AK$NLCD_moss_500m_2001[i])+(1/10*NLCD_AK$NLCD_moss_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_moss_500m[i]<-(8/10*NLCD_AK$NLCD_moss_500m_2001[i])+(2/10*NLCD_AK$NLCD_moss_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_moss_500m[i]<-(7/10*NLCD_AK$NLCD_moss_500m_2001[i])+(3/10*NLCD_AK$NLCD_moss_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_moss_500m[i]<-(6/10*NLCD_AK$NLCD_moss_500m_2001[i])+(4/10*NLCD_AK$NLCD_moss_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_moss_500m[i]<-(5/10*NLCD_AK$NLCD_moss_500m_2001[i])+(5/10*NLCD_AK$NLCD_moss_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_moss_500m[i]<-(4/10*NLCD_AK$NLCD_moss_500m_2001[i])+(6/10*NLCD_AK$NLCD_moss_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_moss_500m[i]<-(3/10*NLCD_AK$NLCD_moss_500m_2001[i])+(7/10*NLCD_AK$NLCD_moss_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_moss_500m[i]<-(2/10*NLCD_AK$NLCD_moss_500m_2001[i])+(8/10*NLCD_AK$NLCD_moss_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_moss_500m[i]<-(1/10*NLCD_AK$NLCD_moss_500m_2001[i])+(9/10*NLCD_AK$NLCD_moss_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_moss_500m[i]<-NLCD_AK$NLCD_moss_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_moss_500m[i]<-(4/5*NLCD_AK$NLCD_moss_500m_2011[i])+(1/5*NLCD_AK$NLCD_moss_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_moss_500m[i]<-(3/5*NLCD_AK$NLCD_moss_500m_2011[i])+(2/5*NLCD_AK$NLCD_moss_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_moss_500m[i]<-(2/5*NLCD_AK$NLCD_moss_500m_2011[i])+(3/5*NLCD_AK$NLCD_moss_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_moss_500m[i]<-(1/5*NLCD_AK$NLCD_moss_500m_2011[i])+(4/5*NLCD_AK$NLCD_moss_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_moss_500m[i]<-NLCD_AK$NLCD_moss_500m_2016[i]
}


###For NLCD_pasture
NLCD_AK$NLCD_pasture_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_pasture_500m[i]<-NLCD_AK$NLCD_pasture_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_pasture_500m[i]<-(9/10*NLCD_AK$NLCD_pasture_500m_2001[i])+(1/10*NLCD_AK$NLCD_pasture_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_pasture_500m[i]<-(8/10*NLCD_AK$NLCD_pasture_500m_2001[i])+(2/10*NLCD_AK$NLCD_pasture_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_pasture_500m[i]<-(7/10*NLCD_AK$NLCD_pasture_500m_2001[i])+(3/10*NLCD_AK$NLCD_pasture_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_pasture_500m[i]<-(6/10*NLCD_AK$NLCD_pasture_500m_2001[i])+(4/10*NLCD_AK$NLCD_pasture_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_pasture_500m[i]<-(5/10*NLCD_AK$NLCD_pasture_500m_2001[i])+(5/10*NLCD_AK$NLCD_pasture_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_pasture_500m[i]<-(4/10*NLCD_AK$NLCD_pasture_500m_2001[i])+(6/10*NLCD_AK$NLCD_pasture_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_pasture_500m[i]<-(3/10*NLCD_AK$NLCD_pasture_500m_2001[i])+(7/10*NLCD_AK$NLCD_pasture_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_pasture_500m[i]<-(2/10*NLCD_AK$NLCD_pasture_500m_2001[i])+(8/10*NLCD_AK$NLCD_pasture_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_pasture_500m[i]<-(1/10*NLCD_AK$NLCD_pasture_500m_2001[i])+(9/10*NLCD_AK$NLCD_pasture_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_pasture_500m[i]<-NLCD_AK$NLCD_pasture_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_pasture_500m[i]<-(4/5*NLCD_AK$NLCD_pasture_500m_2011[i])+(1/5*NLCD_AK$NLCD_pasture_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_pasture_500m[i]<-(3/5*NLCD_AK$NLCD_pasture_500m_2011[i])+(2/5*NLCD_AK$NLCD_pasture_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_pasture_500m[i]<-(2/5*NLCD_AK$NLCD_pasture_500m_2011[i])+(3/5*NLCD_AK$NLCD_pasture_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_pasture_500m[i]<-(1/5*NLCD_AK$NLCD_pasture_500m_2011[i])+(4/5*NLCD_AK$NLCD_pasture_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_pasture_500m[i]<-NLCD_AK$NLCD_pasture_500m_2016[i]
}

###For Cultivated crops
NLCD_AK$NLCD_cultivated_crops_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_cultivated_crops_500m[i]<-NLCD_AK$NLCD_cultivated_crops_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(9/10*NLCD_AK$NLCD_cultivated_crops_500m_2001[i])+(1/10*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(8/10*NLCD_AK$NLCD_cultivated_crops_500m_2001[i])+(2/10*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(7/10*NLCD_AK$NLCD_cultivated_crops_500m_2001[i])+(3/10*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(6/10*NLCD_AK$NLCD_cultivated_crops_500m_2001[i])+(4/10*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(5/10*NLCD_AK$NLCD_cultivated_crops_500m_2001[i])+(5/10*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(4/10*NLCD_AK$NLCD_cultivated_crops_500m_2001[i])+(6/10*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(3/10*NLCD_AK$NLCD_cultivated_crops_500m_2001[i])+(7/10*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(2/10*NLCD_AK$NLCD_cultivated_crops_500m_2001[i])+(8/10*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(1/10*NLCD_AK$NLCD_cultivated_crops_500m_2001[i])+(9/10*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_cultivated_crops_500m[i]<-NLCD_AK$NLCD_cultivated_crops_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(4/5*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])+(1/5*NLCD_AK$NLCD_cultivated_crops_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(3/5*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])+(2/5*NLCD_AK$NLCD_cultivated_crops_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(2/5*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])+(3/5*NLCD_AK$NLCD_cultivated_crops_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_cultivated_crops_500m[i]<-(1/5*NLCD_AK$NLCD_cultivated_crops_500m_2011[i])+(4/5*NLCD_AK$NLCD_cultivated_crops_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_cultivated_crops_500m[i]<-NLCD_AK$NLCD_cultivated_crops_500m_2016[i]
}


###For Woody wetland
NLCD_AK$NLCD_woody_wetland_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_woody_wetland_500m[i]<-NLCD_AK$NLCD_woody_wetland_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_woody_wetland_500m[i]<-(9/10*NLCD_AK$NLCD_woody_wetland_500m_2001[i])+(1/10*NLCD_AK$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_woody_wetland_500m[i]<-(8/10*NLCD_AK$NLCD_woody_wetland_500m_2001[i])+(2/10*NLCD_AK$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_woody_wetland_500m[i]<-(7/10*NLCD_AK$NLCD_woody_wetland_500m_2001[i])+(3/10*NLCD_AK$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_woody_wetland_500m[i]<-(6/10*NLCD_AK$NLCD_woody_wetland_500m_2001[i])+(4/10*NLCD_AK$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_woody_wetland_500m[i]<-(5/10*NLCD_AK$NLCD_woody_wetland_500m_2001[i])+(5/10*NLCD_AK$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_woody_wetland_500m[i]<-(4/10*NLCD_AK$NLCD_woody_wetland_500m_2001[i])+(6/10*NLCD_AK$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_woody_wetland_500m[i]<-(3/10*NLCD_AK$NLCD_woody_wetland_500m_2001[i])+(7/10*NLCD_AK$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_woody_wetland_500m[i]<-(2/10*NLCD_AK$NLCD_woody_wetland_500m_2001[i])+(8/10*NLCD_AK$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_woody_wetland_500m[i]<-(1/10*NLCD_AK$NLCD_woody_wetland_500m_2001[i])+(9/10*NLCD_AK$NLCD_woody_wetland_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_woody_wetland_500m[i]<-NLCD_AK$NLCD_woody_wetland_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_woody_wetland_500m[i]<-(4/5*NLCD_AK$NLCD_woody_wetland_500m_2011[i])+(1/5*NLCD_AK$NLCD_woody_wetland_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_woody_wetland_500m[i]<-(3/5*NLCD_AK$NLCD_woody_wetland_500m_2011[i])+(2/5*NLCD_AK$NLCD_woody_wetland_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_woody_wetland_500m[i]<-(2/5*NLCD_AK$NLCD_woody_wetland_500m_2011[i])+(3/5*NLCD_AK$NLCD_woody_wetland_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_woody_wetland_500m[i]<-(1/5*NLCD_AK$NLCD_woody_wetland_500m_2011[i])+(4/5*NLCD_AK$NLCD_woody_wetland_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_woody_wetland_500m[i]<-NLCD_AK$NLCD_woody_wetland_500m_2016[i]
}


###For Emergent herbaceous wetlands
NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(9/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(1/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(8/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(2/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(7/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(3/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(6/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(4/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(5/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(5/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(4/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(6/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(3/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(7/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(2/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(8/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(1/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2001[i])+(9/10*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(4/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])+(1/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(3/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])+(2/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(2/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])+(3/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-(1/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2011[i])+(4/5*NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m[i]<-NLCD_AK$NLCD_emergent_herbaceous_wetlands_500m_2016[i]
}


###For Unknown
NLCD_AK$NLCD_unkown_500m<-rep(0,nrow(NLCD_AK))
for(i in 1:nrow(NLCD_AK)){
  if (NLCD_AK$year.int[i]<2002) {NLCD_AK$NLCD_unkown_500m[i]<-NLCD_AK$NLCD_unkown_500m_2001[i]}
  else if (NLCD_AK$year.int[i]==2002) NLCD_AK$NLCD_unkown_500m[i]<-(9/10*NLCD_AK$NLCD_unkown_500m_2001[i])+(1/10*NLCD_AK$NLCD_unkown_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2003) NLCD_AK$NLCD_unkown_500m[i]<-(8/10*NLCD_AK$NLCD_unkown_500m_2001[i])+(2/10*NLCD_AK$NLCD_unkown_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2004) NLCD_AK$NLCD_unkown_500m[i]<-(7/10*NLCD_AK$NLCD_unkown_500m_2001[i])+(3/10*NLCD_AK$NLCD_unkown_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2005) NLCD_AK$NLCD_unkown_500m[i]<-(6/10*NLCD_AK$NLCD_unkown_500m_2001[i])+(4/10*NLCD_AK$NLCD_unkown_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2006) NLCD_AK$NLCD_unkown_500m[i]<-(5/10*NLCD_AK$NLCD_unkown_500m_2001[i])+(5/10*NLCD_AK$NLCD_unkown_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2007) NLCD_AK$NLCD_unkown_500m[i]<-(4/10*NLCD_AK$NLCD_unkown_500m_2001[i])+(6/10*NLCD_AK$NLCD_unkown_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2008) NLCD_AK$NLCD_unkown_500m[i]<-(3/10*NLCD_AK$NLCD_unkown_500m_2001[i])+(7/10*NLCD_AK$NLCD_unkown_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2009) NLCD_AK$NLCD_unkown_500m[i]<-(2/10*NLCD_AK$NLCD_unkown_500m_2001[i])+(8/10*NLCD_AK$NLCD_unkown_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2010) NLCD_AK$NLCD_unkown_500m[i]<-(1/10*NLCD_AK$NLCD_unkown_500m_2001[i])+(9/10*NLCD_AK$NLCD_unkown_500m_2011[i])
  else if (NLCD_AK$year.int[i]==2011) NLCD_AK$NLCD_unkown_500m[i]<-NLCD_AK$NLCD_unkown_500m_2011[i]
  else if (NLCD_AK$year.int[i]==2012) NLCD_AK$NLCD_unkown_500m[i]<-(4/5*NLCD_AK$NLCD_unkown_500m_2011[i])+(1/5*NLCD_AK$NLCD_unkown_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2013) NLCD_AK$NLCD_unkown_500m[i]<-(3/5*NLCD_AK$NLCD_unkown_500m_2011[i])+(2/5*NLCD_AK$NLCD_unkown_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2014) NLCD_AK$NLCD_unkown_500m[i]<-(2/5*NLCD_AK$NLCD_unkown_500m_2011[i])+(3/5*NLCD_AK$NLCD_unkown_500m_2016[i])
  else if (NLCD_AK$year.int[i]==2015) NLCD_AK$NLCD_unkown_500m[i]<-(1/5*NLCD_AK$NLCD_unkown_500m_2011[i])+(4/5*NLCD_AK$NLCD_unkown_500m_2016[i])
  else if (NLCD_AK$year.int[i]>2015) NLCD_AK$NLCD_unkown_500m[i]<-NLCD_AK$NLCD_unkown_500m_2016[i]
}



#########
###Cleaning and merging data bases to a single data frame
NLCD_clean <- NLCD[,c(1,2,66:70,449:458,460:479,481:500,502:514)]
NLCD_AK_clean <- NLCD_AK[,c(1,2,66:70,197:259)]

NLCD_clean <- NLCD_clean[!(NLCD_clean$LOC_ID %in% c("L10198950", "L10198973", "L10201680", "L10201685", "L10201690", "L10201696", "L10210851", "L10210853" ,"L10210856" ,"L10210862" ,"L10210867" ,"L10235777",
                                                    "L10235778", "L10235782" ,"L10235788", "L10235793" ,"L10235802" ,"L10235806" ,"L10235813" ,"L10235817", "L10235822", "L106296",   "L106297",   "L106300",  
                                                    "L106301"  , "L106302"  , "L106415"  , "L106419"  , "L106427" ,  "L1188134"  ,"L128009"  , "L128019" ,  "L128020",   "L128026" ,  "L128029" ,  "L128590",  
                                                     "L1461969" , "L1564760"  ,"L158863"  , "L158924"  , "L158926"  , "L159860" ,  "L159862" ,  "L159924"  , "L159931",   "L159994" ,  "L159996"  , "L159997" , 
                                                    "L159998"  , "L160001" ,  "L160239" ,  "L1629660" , "L1643295",  "L208982"  , "L213514"  , "L240617"  , "L240618"  , "L240619" ,  "L3004215" , "L3011547", 
                                                    "L338738" ,  "L338747"  , "L353535" ,  "L353536"  , "L353538"  , "L353541" ,  "L353542"  , "L353543" ,  "L353544"   ,"L353545" ,  "L4043979" , "L4444178" ,
                                                    "L4444195" , "L4574885"  ,"L4634176" , "L4660019" , "L5154762" , "L5154778"  ,"L5154782" , "L5154790" , "L5154796"  ,"L5154802",  "L5154806" , "L5154812", 
                                                     "L5154821" , "L5155038" , "L5155041" , "L5155065" , "L5155068" , "L5155073" , "L5155097",  "L5155101",  "L5155104",  "L5155108",  "L5155113" , "L5155117" ,
                                                     "L5199837" , "L5199844" , "L5199849" , "L5199865" , "L5199874" , "L5199878" , "L5199880",  "L5199891",  "L5199908",  "L573018",   "L5778325"  ,"L5967040" ,
                                                     "L70152"  ,  "L70158"  ,  "L70159"  ,  "L70167"  ,  "L70168"  ,  "L70169"  ,  "L70170",    "L70171",    "L7162783",  "L754570",   "L754573" ,  "L754579"  ,
                                                     "L754584" ,  "L754587"  , "L7564319",  "L764954" ,  "L778707" ,  "L778708" ,  "L778709",   "L779983",   "L779984" ,  "L779985" ,  "L779990" ,  "L779993"  ,
                                                     "L779995" ,  "L779996"  , "L780000"  , "L780001" ,  "L780002"  , "L780005" ,  "L791915" ,  "L791916",   "L792371" ,  "L792372"  , "L792373" ,  "L792374"  ,
                                                     "L793016" ,  "L793018" ,  "L793021" ,  "L793024"  , "L793027"  , "L793643" ,  "L793650" ,  "L793655",   "L793663" ,  "L793667" ,  "L793676" ,  "L793702"  ,
                                                     "L793703" ,  "L793704" ,  "L793705" ,  "L8431367",  "L8431371",  "L8431396" , "L8431400",  "L8431409",  "L8431411",  "L8431418",  "L8431423",  "L8431430" ,
                                                     "L8431440" , "L8459296" , "L8459305",  "L8459310" , "L8459326" , "L8459331",  "L8459350",  "L8459358" , "L8459369" , "L8459764" , "L8459773",  "L8459785" ,
                                                     "L8459798",  "L8459819",  "L8459828" , "L8459881",  "L8472194",  "L8472209",  "L8472223",  "L8472234",  "L8472241",  "L8472244",  "L8472251",  "L8472254" ,
                                                    "L8472260" , "L8482674" , "L8488310",  "L8488314",  "L8488322",  "L8488326" , "L8488331",  "L8488338",  "L8488341",  "L8488346",  "L8488351",  "L8488356" ,
                                                     "L8494716",  "L8494723",  "L8494735",  "L8539774",  "L8539777" , "L8539779",  "L8539783",  "L8539790",  "L8559643",  "L8559650",  "L8559657",  "L8559667" ,
                                                     "L8559674" , "L8573743" , "L8573752",  "L8573769" , "L8631935",  "L8631951",  "L8631960" , "L8631970",  "L8631982",  "L8631988",  "L8652846",  "L8652860" ,
                                                    "L8652874",  "L8652884",  "L8652913",  "L8652955" , "L8652968" , "L8652981" , "L952944" )),]

NLCD_clean$LOC_ID <- as.factor(as.character(NLCD_clean$LOC_ID))

NLCD_clean <- rbind(NLCD_clean,NLCD_AK_clean)
NLCD_clean$LOC_ID <- as.factor(as.character(NLCD_clean$LOC_ID ))

names(NLCD_clean)[2:4] <- c("year","lon","lat")

write.csv (NLCD_clean, "/Users/DrBohemio/Google Drive/NestwatchProject/Data/NLCD/NLCD_prop.csv")
NLCD_prop <- read.csv ( "/Users/DrBohemio/Google Drive/NestwatchProject/Data/NLCD/NLCD_prop.csv")
#write.csv (NLCD_2km_clean, "/Users/DrBohemio/Google Drive/NestwatchProject/Saved/NLCD_2km_prop_clean.csv")









