rm(list=ls())

library(tidyverse)
library(lme4)
library(boot)
library(sjPlot)
library(cowplot)

########### read in data original data###############
nest <- readRDS("Data/failure-cleaned.rds")
nest$attempt <- as.factor(nest$attempt)
nest$substrate_binary  <- as.factor(nest$substrate_binary)


###Predictions data
bill <- read.csv("Data/MACA_CMIP5ClimateProjections/Nestwatch_Site_Dates.csv")
bill <- as.data.frame(bill$attempt)
colnames(bill) <- "attempt"

#RCP45 x 3 models
temp.rcp45m.gfdl <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45m_gfdl.csv')
temp.rcp45e.gfdl <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45e_gfdl.csv')
pcp.rcp45m.gfdl <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45m_gfdl.csv') %>%
  filter(V1!="NA")
pcp.rcp45e.gfdl <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45e_gfdl.csv')

temp.rcp45m.canesm <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45m_canesm.csv')
temp.rcp45e.canesm <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45e_canesm.csv')
pcp.rcp45m.canesm <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45m_canesm.csv')
pcp.rcp45e.canesm <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45e_canesm.csv')

temp.rcp45m.mri <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45m_mri.csv')
temp.rcp45e.mri <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45e_mri.csv')
pcp.rcp45m.mri <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45m_mri.csv')
pcp.rcp45e.mri <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45e_mri.csv')



#RCP85 x 3 models
temp.rcp85m.gfdl <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85m_gfdl.csv')
temp.rcp85e.gfdl <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85e_gfdl.csv')
pcp.rcp85m.gfdl <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85m_gfdl.csv')
pcp.rcp85e.gfdl <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85e_gfdl.csv')

temp.rcp85m.canesm <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85m_canesm.csv')
temp.rcp85e.canesm <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85e_canesm.csv')
pcp.rcp85m.canesm <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85m_canesm.csv')
pcp.rcp85e.canesm <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85e_canesm.csv')

temp.rcp85m.mri <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85m_mri.csv')
temp.rcp85e.mri <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85e_mri.csv')
pcp.rcp85m.mri <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85m_mri.csv')
pcp.rcp85e.mri <- read.csv('Data/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85e_mri.csv')

################ reformat MACA climate projection data ###############
#mod.success <- readRDS('results/Question 1-2/success~stdmax2laydate2way.AK.RDS')
#summary(mod.success)
#var names in original data:
#Tmax_std_gridmet
#Tmin_std_gridmet
#pcpbefore_raw_gridmet

##Precip key
#V5: Precipitation, Mean, 365d before, Raw value
#V11: Precipitation, 10th percentile, 365d before, Raw value
#V17: Precipitation, 90th percentile, 365d before, Raw value

# For future gridmet data, take all the PcpBefore_raw values,
#subtract mean(nest$PcpBefore_raw), and divide by sd(nest$PcpBefore_raw) 


##Temp key
#V4: Tmin, Mean, 0-45d after lay, Standardized z-score
#V7: Tmax, Mean, 0-45d after lay, Standardized z-score
#V10: Tmin, 10th percentile, 0-45d after lay, Standardized z-score
#V13: Tmax, 10th percentile, 0-45d after lay, Standardized z-score
#V16: Tmin, 90th percentile, 0-45d after lay, Standardized z-score
#V19: Tmax, 90th percentile 0-45d after lay, Standardized z-score


###### precip for mid century, RCP4.5 #########

pcp.rcp45m.gfdl <- pcp.rcp45m.gfdl %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45m.gfdl.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.gfdl.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.gfdl.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp45m.canesm <- pcp.rcp45m.canesm %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45m.canesm.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.canesm.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.canesm.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp45m.mri <- pcp.rcp45m.mri %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45m.mri.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.mri.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.mri.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

###### precip for end of century, RCP4.5 #########
pcp.rcp45e.gfdl <- pcp.rcp45e.gfdl %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45e.gfdl.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.gfdl.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.gfdl.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp45e.canesm <- pcp.rcp45e.canesm %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45e.canesm.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.canesm.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.canesm.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp45e.mri <- pcp.rcp45e.mri %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45e.mri.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.mri.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.mri.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))



###### precip for mid century, RCP8.5 #########
pcp.rcp85m.gfdl <- pcp.rcp85m.gfdl %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85m.gfdl.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.gfdl.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.gfdl.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp85m.canesm <- pcp.rcp85m.canesm %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85m.canesm.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.canesm.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.canesm.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp85m.mri <- pcp.rcp85m.mri %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85m.mri.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.mri.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.mri.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))


###### precip for end of century, RCP8.5 #########
pcp.rcp85e.gfdl <- pcp.rcp85e.gfdl %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85e.gfdl.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.gfdl.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.gfdl.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp85e.canesm <- pcp.rcp85e.canesm %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85e.canesm.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.canesm.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.canesm.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp85e.mri <- pcp.rcp85e.mri %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85e.mri.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.mri.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.mri.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))


###### temp for mid century, RCP4.5 #########

temp.rcp45m.gfdl <- temp.rcp45m.gfdl %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45m.gfdl.mean = V4,
                Tmax_std_gridmet.rcp45m.gfdl.mean = V7,
                Tmin_std_gridmet.rcp45m.gfdl.10 = V10,
                Tmax_std_gridmet.rcp45m.gfdl.10 = V13,
                Tmin_std_gridmet.rcp45m.gfdl.90 = V16,
                Tmax_std_gridmet.rcp45m.gfdl.90 = V19)

temp.rcp45m.canesm <- temp.rcp45m.canesm %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45m.canesm.mean = V4,
                Tmax_std_gridmet.rcp45m.canesm.mean = V7,
                Tmin_std_gridmet.rcp45m.canesm.10 = V10,
                Tmax_std_gridmet.rcp45m.canesm.10 = V13,
                Tmin_std_gridmet.rcp45m.canesm.90 = V16,
                Tmax_std_gridmet.rcp45m.canesm.90 = V19)

temp.rcp45m.mri <- temp.rcp45m.mri %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45m.mri.mean = V4,
                Tmax_std_gridmet.rcp45m.mri.mean = V7,
                Tmin_std_gridmet.rcp45m.mri.10 = V10,
                Tmax_std_gridmet.rcp45m.mri.10 = V13,
                Tmin_std_gridmet.rcp45m.mri.90 = V16,
                Tmax_std_gridmet.rcp45m.mri.90 = V19)

###### temp for end of century, RCP4.5 #########

temp.rcp45e.gfdl <- temp.rcp45e.gfdl %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45e.gfdl.mean = V4,
                Tmax_std_gridmet.rcp45e.gfdl.mean = V7,
                Tmin_std_gridmet.rcp45e.gfdl.10 = V10,
                Tmax_std_gridmet.rcp45e.gfdl.10 = V13,
                Tmin_std_gridmet.rcp45e.gfdl.90 = V16,
                Tmax_std_gridmet.rcp45e.gfdl.90 = V19)

temp.rcp45e.canesm <- temp.rcp45e.canesm %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45e.canesm.mean = V4,
                Tmax_std_gridmet.rcp45e.canesm.mean = V7,
                Tmin_std_gridmet.rcp45e.canesm.10 = V10,
                Tmax_std_gridmet.rcp45e.canesm.10 = V13,
                Tmin_std_gridmet.rcp45e.canesm.90 = V16,
                Tmax_std_gridmet.rcp45e.canesm.90 = V19)

temp.rcp45e.mri <- temp.rcp45e.mri %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45e.mri.mean = V4,
                Tmax_std_gridmet.rcp45e.mri.mean = V7,
                Tmin_std_gridmet.rcp45e.mri.10 = V10,
                Tmax_std_gridmet.rcp45e.mri.10 = V13,
                Tmin_std_gridmet.rcp45e.mri.90 = V16,
                Tmax_std_gridmet.rcp45e.mri.90 = V19)


###### temp for mid century, RCP8.5 #########

temp.rcp85m.gfdl <- temp.rcp85m.gfdl %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85m.gfdl.mean = V4,
                Tmax_std_gridmet.rcp85m.gfdl.mean = V7,
                Tmin_std_gridmet.rcp85m.gfdl.10 = V10,
                Tmax_std_gridmet.rcp85m.gfdl.10 = V13,
                Tmin_std_gridmet.rcp85m.gfdl.90 = V16,
                Tmax_std_gridmet.rcp85m.gfdl.90 = V19)

temp.rcp85m.canesm <- temp.rcp85m.canesm %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85m.canesm.mean = V4,
                Tmax_std_gridmet.rcp85m.canesm.mean = V7,
                Tmin_std_gridmet.rcp85m.canesm.10 = V10,
                Tmax_std_gridmet.rcp85m.canesm.10 = V13,
                Tmin_std_gridmet.rcp85m.canesm.90 = V16,
                Tmax_std_gridmet.rcp85m.canesm.90 = V19)

temp.rcp85m.mri <- temp.rcp85m.mri %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85m.mri.mean = V4,
                Tmax_std_gridmet.rcp85m.mri.mean = V7,
                Tmin_std_gridmet.rcp85m.mri.10 = V10,
                Tmax_std_gridmet.rcp85m.mri.10 = V13,
                Tmin_std_gridmet.rcp85m.mri.90 = V16,
                Tmax_std_gridmet.rcp85m.mri.90 = V19)


###### temp for end of century, RCP8.5 #########


temp.rcp85e.gfdl <- temp.rcp85e.gfdl %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85e.gfdl.mean = V4,
                Tmax_std_gridmet.rcp85e.gfdl.mean = V7,
                Tmin_std_gridmet.rcp85e.gfdl.10 = V10,
                Tmax_std_gridmet.rcp85e.gfdl.10 = V13,
                Tmin_std_gridmet.rcp85e.gfdl.90 = V16,
                Tmax_std_gridmet.rcp85e.gfdl.90 = V19)

temp.rcp85e.canesm <- temp.rcp85e.canesm %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85e.canesm.mean = V4,
                Tmax_std_gridmet.rcp85e.canesm.mean = V7,
                Tmin_std_gridmet.rcp85e.canesm.10 = V10,
                Tmax_std_gridmet.rcp85e.canesm.10 = V13,
                Tmin_std_gridmet.rcp85e.canesm.90 = V16,
                Tmax_std_gridmet.rcp85e.canesm.90 = V19)

temp.rcp85e.mri <- temp.rcp85e.mri %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85e.mri.mean = V4,
                Tmax_std_gridmet.rcp85e.mri.mean = V7,
                Tmin_std_gridmet.rcp85e.mri.10 = V10,
                Tmax_std_gridmet.rcp85e.mri.10 = V13,
                Tmin_std_gridmet.rcp85e.mri.90 = V16,
                Tmax_std_gridmet.rcp85e.mri.90 = V19)










############# merge temp and precip data ##############

proj <- cbind(pcp.rcp45e.gfdl, pcp.rcp45e.canesm, pcp.rcp45e.mri,
              pcp.rcp45m.gfdl, pcp.rcp45m.canesm, pcp.rcp45m.mri,
              pcp.rcp85e.gfdl, pcp.rcp85e.canesm, pcp.rcp85e.mri,
              pcp.rcp85m.gfdl, pcp.rcp85m.canesm, pcp.rcp85m.mri,
              
              temp.rcp45e.gfdl, temp.rcp45e.canesm, temp.rcp45e.mri,
              temp.rcp45m.gfdl, temp.rcp45m.canesm, temp.rcp45m.mri,
              temp.rcp85e.gfdl, temp.rcp85e.canesm, temp.rcp85e.mri,
              temp.rcp85m.gfdl, temp.rcp85m.canesm, temp.rcp85m.mri)

#data check
#108 vars? 2 RCPs * 2 time periods * 3 models * 3 parts of distribution * 3climate vars = 108 

proj <- merge(proj, bill, by="row.names")



####### merge original nest data with MACA data #############

nest.proj <- merge(nest, proj, by.x="attempt", by.y="attempt", all.x=T, all.y=F)

write.csv(nest.proj, 'Data/failure_projections.csv', row.names = F)
