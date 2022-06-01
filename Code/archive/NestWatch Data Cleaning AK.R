##Alison's NestWatch data cleaning code

#only need to run the install packages code once
install.packages("readr")
install.packages("dpylr")
install.packages("tidyverse")
install.packages("tibble")
install.packages("lubridate")

#load packages
library(tidyverse)
library(readr)
library(dpylr)
library(tibble)
library(lubridate)

#read in the data
nest=read_csv("~/Google Drive/NestWatch Alison/NestwatchData.csv")

#make it a tibble, so it is easier to work with
nest2<-as_tibble(nest)

#see how many rows and columns there are
dim(nest2)

#for getting rid of unreasonable values, I think we will be mostly using the filter function from the dpylr package

#see how filter works
?dplyr::filter

#first, let's look at the column names of the dataframe and see which values need to be filtered. A dataframe is like a data table, with columns of the same length. The columns do not need to contain the same type of information. For example, one column can be numbers and another can be characters. If you have a table of all numbers, that's a "matrix"
colnames(nest2)

#let's remove some columns we don't care about
nest2<-dplyr::select(nest2,-c(HEIGHT_M, CAVITY_ENTRANCE_DIAM_CM,USER_ID, ENTRANCE_ORIENTATION, ELEVATION_M))

#Now, we can start going through the columns to look for bad values

#we may want to restrict latitude and longitude to be just in the USA. If so, latitude between 24.5 and 71, longitude between -160.3, 172.44. Otherwise, it seems like all locations are correctly categorized into their state or region in the USA or Canada.

summary(nest2$LATITUDE)
#need to filter the points too far south
summary(nest2$LONGITUDE)
#already good for longitude

#for now, we will not take out NA's for substrate. There are only 353.

#it seems like when there is a code required when people enter data, the data is in good shape, with nothing weird or extreme
summary(factor(nest2$HABITAT_CODE_1))

#for all the dates, let's read them in a nicer format
nest2$FIRST_LAY_DT<-dmy_hms(nest2$FIRST_LAY_DT)
nest2$HATCH_DT<-dmy_hms(nest2$HATCH_DT)
nest2$FLEDGE_DT<-dmy_hms(nest2$FLEDGE_DT)
nest2$OBS_DT<-dmy_hms(nest2$OBS_DT)

#for species, let's first remove the numbers of species codes, for example make whbnut2 to be whbnut
nest2$SPECIES_CODE<-gsub('[[:digit:]]+', '', nest2$SPECIES_CODE)
#then, lets remove species with less than 1000 rows. We can choose whatever cut off we want and plug it in. There is a weird species called "y". What is that?

#look at summary
SpeciesSum<-summary(factor(nest2$SPECIES_CODE))

#select those over 1000, and then remove the very last one, which is "other"
SpeciesSelect<-SpeciesSum[SpeciesSum>1000][-length(SpeciesSum[SpeciesSum>1000])]

#view what we did, then you can see it's added to the filter function
names(SpeciesSelect)

#for clutch size host at least, EGGS_HOST_ATLEAST, let's get rid of some weird values. not sure what the cutoff should be, but let's just say 30 for now
#set cutoff to be 30 for: young host total at least, young host fledged at least, eggs host unhatched at least, YOUNG_HOST_LIVE_ATLEAST, YOUNG_HOST_LIVE_ATMOST, YOUNG_HOST_DEAD_ATMOST

#QUESTION: Do the rows with ridiculous values have an outcome code?
nest2$OUTCOME_CODE_LIST[which(nest2$EGGS_HOST_ATLEAST>30)]
nest2$SPECIES_CODE[which(nest2$EGGS_HOST_ATLEAST>30)]
#Answer: most of them do! So if we only use the outcome code, we can keep these data. There aren't a crazy number of bad values, anyway. It will depend on our question.

#YOUNG_HOST_DEAD_ATLEAST seems ok for now with a max of 19

summary(nest2$EGGS_HOST_ATMOST)
#eggs host at most has a negative value. let's make sure it goes from minimum 0 to maximum 29

summary(nest2$ADULT_ACTIVITY_CODE_LIST)
#there is one value of "rc" which is not in the data dictionary. leave it for now.

summary(factor(nest2$YOUNG_HOST_MAX_DEVEL))
#there are some unknown codes here too. What are ly, n, sy, vy?

#for EGGS_HOST_ATMOST, YOUNG_HOST_LIVE_ATMOST, and YOUNG_HOST_DEAD_ATMOST, there are values of 999999999. Let's change those to NA
nest2$EGGS_HOST_ATMOST[which(nest2$EGGS_HOST_ATMOST==999999999)]<-NA
nest2$YOUNG_HOST_LIVE_ATMOST[which(nest2$YOUNG_HOST_LIVE_ATMOST==999999999)]<-NA
nest2$YOUNG_HOST_DEAD_ATMOST[which(nest2$YOUNG_HOST_DEAD_ATMOST==999999999)]<-NA


##final data frame
nest.filtered<- nest2 %>%
  dplyr::filter(LATITUDE>24.5,
                SPECIES_CODE %in% names(SpeciesSelect),
                CLUTCH_SIZE_HOST_ATLEAST<30|is.na(CLUTCH_SIZE_HOST_ATLEAST),
                YOUNG_HOST_TOTAL_ATLEAST<30|is.na(YOUNG_HOST_TOTAL_ATLEAST<30),
                YOUNG_HOST_FLEDGED_ATLEAST<30|is.na(YOUNG_HOST_FLEDGED_ATLEAST),
                EGGS_HOST_UNH_ATLEAST<30|is.na(EGGS_HOST_UNH_ATLEAST),
                EGGS_HOST_ATLEAST<30|is.na(EGGS_HOST_ATLEAST),
                EGGS_HOST_ATMOST>=0|is.na(EGGS_HOST_ATMOST),
                EGGS_HOST_ATMOST<30|is.na(EGGS_HOST_ATMOST),
                YOUNG_HOST_LIVE_ATLEAST<30|is.na(YOUNG_HOST_LIVE_ATLEAST),
                YOUNG_HOST_LIVE_ATMOST<30|is.na(YOUNG_HOST_LIVE_ATMOST),
                YOUNG_HOST_DEAD_ATMOST<30|is.na(YOUNG_HOST_DEAD_ATMOST))

#describing the first part of this function: our new data frame called nest.filtered is: take nest2, and in the package dplyr, use the filter function. Only select the rows with latitude >24.5, a species code in our pre-made list of species names, a value for clutch size host that is either less than 30 OR NA, etc...

#this is the number of rows we eliminated: 29,651
dim(nest2)[1]-dim(nest.filtered)[1]


#optional practice questions

#1. What is the most common habitat code 2?

#2. Create a new data frame from nest2, but only select the species with over 50,000 visits and a clutch size (CLUTCH_SIZE_HOST_ATLEAST) of 5. (hint: to write "equals", use the equals sign twice ==). What are the dimensions of the new data frame?
