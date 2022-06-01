rm(list=ls())

library(tidyverse)
library(lme4)
library(boot)
library(sjPlot)
library(cowplot)

########### read in data original data###############
nest <- readRDS("Data/active/success-cleaned.rds")
nest$attempt <- as.factor(nest$attempt)
nest$substrate_binary  <- as.factor(nest$substrate_binary)


###Predictions data
bill <- read.csv("Data/active/MACA_CMIP5ClimateProjections/Nestwatch_Site_Dates.csv")
bill <- as.data.frame(bill$attempt)
colnames(bill) <- "attempt"

#### read in updated base model to see which predictors need to be included ########
model <- readRDS("results/spatial/success~tnestpd_meanmax_gridmet_tmax.rds")


#RCP45 x 5 models
temp.rcp45m.gfdl <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45m_gfdl.csv')
temp.rcp45e.gfdl <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45e_gfdl.csv')
pcp.rcp45m.gfdl <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45m_gfdl.csv') %>%
  filter(V1!="NA")
pcp.rcp45e.gfdl <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45e_gfdl.csv')

temp.rcp45m.canesm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45m_canesm.csv')
temp.rcp45e.canesm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45e_canesm.csv')
pcp.rcp45m.canesm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45m_canesm.csv')
pcp.rcp45e.canesm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45e_canesm.csv')

temp.rcp45m.mri <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45m_mri.csv')
temp.rcp45e.mri <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45e_mri.csv')
pcp.rcp45m.mri <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45m_mri.csv')
pcp.rcp45e.mri <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45e_mri.csv')

temp.rcp45m.miroc <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45m_miroc.csv')
temp.rcp45e.miroc <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45e_miroc.csv')
pcp.rcp45m.miroc <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45m_miroc.csv')
pcp.rcp45e.miroc <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45e_miroc.csv')

temp.rcp45m.noresm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45m_noresm.csv')
temp.rcp45e.noresm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp45e_noresm.csv')
pcp.rcp45m.noresm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45m_noresm.csv')
pcp.rcp45e.noresm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp45e_noresm.csv')


#RCP85 x 5 models
temp.rcp85m.gfdl <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85m_gfdl.csv')
temp.rcp85e.gfdl <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85e_gfdl.csv')
pcp.rcp85m.gfdl <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85m_gfdl.csv')
pcp.rcp85e.gfdl <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85e_gfdl.csv')

temp.rcp85m.canesm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85m_canesm.csv')
temp.rcp85e.canesm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85e_canesm.csv')
pcp.rcp85m.canesm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85m_canesm.csv')
pcp.rcp85e.canesm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85e_canesm.csv')

temp.rcp85m.mri <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85m_mri.csv')
temp.rcp85e.mri <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85e_mri.csv')
pcp.rcp85m.mri <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85m_mri.csv')
pcp.rcp85e.mri <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85e_mri.csv')

temp.rcp85m.miroc <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85m_miroc.csv')
temp.rcp85e.miroc <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85e_miroc.csv')
pcp.rcp85m.miroc <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85m_miroc.csv')
pcp.rcp85e.miroc <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85e_miroc.csv')

temp.rcp85m.noresm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85m_noresm.csv')
temp.rcp85e.noresm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_temp_rcp85e_noresm.csv')
pcp.rcp85m.noresm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85m_noresm.csv')
pcp.rcp85e.noresm <- read.csv('Data/active/MACA_CMIP5ClimateProjections/MACA_v1_pcp_rcp85e_noresm.csv')


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

pcp.rcp45m.miroc <- pcp.rcp45m.miroc %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45m.miroc.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.miroc.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.miroc.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp45m.noresm <- pcp.rcp45m.noresm %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45m.noresm.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.noresm.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45m.noresm.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
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

pcp.rcp45e.miroc <- pcp.rcp45e.miroc %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45e.miroc.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.miroc.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.miroc.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp45e.noresm <- pcp.rcp45e.noresm %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp45e.noresm.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.noresm.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp45e.noresm.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
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

pcp.rcp85m.miroc <- pcp.rcp85m.miroc %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85m.miroc.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.miroc.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.miroc.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp85m.noresm <- pcp.rcp85m.noresm %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85m.noresm.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.noresm.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85m.noresm.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
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

pcp.rcp85e.miroc <- pcp.rcp85e.miroc %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85e.miroc.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.miroc.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.miroc.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
  select(-c(V5, V11, V17))

pcp.rcp85e.noresm <- pcp.rcp85e.noresm %>%
  select(V5, V11, V17) %>%
  mutate(pcpbefore_raw_gridmet.rcp85e.noresm.mean = (V5/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.noresm.10 = (V11/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw),
         pcpbefore_raw_gridmet.rcp85e.noresm.90 = (V17/10-mean(nest$PcpBefore_raw))/sd(nest$PcpBefore_raw)) %>%
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

temp.rcp45m.miroc <- temp.rcp45m.miroc %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45m.miroc.mean = V4,
                Tmax_std_gridmet.rcp45m.miroc.mean = V7,
                Tmin_std_gridmet.rcp45m.miroc.10 = V10,
                Tmax_std_gridmet.rcp45m.miroc.10 = V13,
                Tmin_std_gridmet.rcp45m.miroc.90 = V16,
                Tmax_std_gridmet.rcp45m.miroc.90 = V19)

temp.rcp45m.noresm <- temp.rcp45m.noresm %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45m.noresm.mean = V4,
                Tmax_std_gridmet.rcp45m.noresm.mean = V7,
                Tmin_std_gridmet.rcp45m.noresm.10 = V10,
                Tmax_std_gridmet.rcp45m.noresm.10 = V13,
                Tmin_std_gridmet.rcp45m.noresm.90 = V16,
                Tmax_std_gridmet.rcp45m.noresm.90 = V19)

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

temp.rcp45e.miroc <- temp.rcp45e.miroc %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45e.miroc.mean = V4,
                Tmax_std_gridmet.rcp45e.miroc.mean = V7,
                Tmin_std_gridmet.rcp45e.miroc.10 = V10,
                Tmax_std_gridmet.rcp45e.miroc.10 = V13,
                Tmin_std_gridmet.rcp45e.miroc.90 = V16,
                Tmax_std_gridmet.rcp45e.miroc.90 = V19)

temp.rcp45e.noresm <- temp.rcp45e.noresm %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp45e.noresm.mean = V4,
                Tmax_std_gridmet.rcp45e.noresm.mean = V7,
                Tmin_std_gridmet.rcp45e.noresm.10 = V10,
                Tmax_std_gridmet.rcp45e.noresm.10 = V13,
                Tmin_std_gridmet.rcp45e.noresm.90 = V16,
                Tmax_std_gridmet.rcp45e.noresm.90 = V19)

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

temp.rcp85m.miroc <- temp.rcp85m.miroc %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85m.miroc.mean = V4,
                Tmax_std_gridmet.rcp85m.miroc.mean = V7,
                Tmin_std_gridmet.rcp85m.miroc.10 = V10,
                Tmax_std_gridmet.rcp85m.miroc.10 = V13,
                Tmin_std_gridmet.rcp85m.miroc.90 = V16,
                Tmax_std_gridmet.rcp85m.miroc.90 = V19)

temp.rcp85m.noresm <- temp.rcp85m.noresm %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85m.noresm.mean = V4,
                Tmax_std_gridmet.rcp85m.noresm.mean = V7,
                Tmin_std_gridmet.rcp85m.noresm.10 = V10,
                Tmax_std_gridmet.rcp85m.noresm.10 = V13,
                Tmin_std_gridmet.rcp85m.noresm.90 = V16,
                Tmax_std_gridmet.rcp85m.noresm.90 = V19)

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

temp.rcp85e.miroc <- temp.rcp85e.miroc %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85e.miroc.mean = V4,
                Tmax_std_gridmet.rcp85e.miroc.mean = V7,
                Tmin_std_gridmet.rcp85e.miroc.10 = V10,
                Tmax_std_gridmet.rcp85e.miroc.10 = V13,
                Tmin_std_gridmet.rcp85e.miroc.90 = V16,
                Tmax_std_gridmet.rcp85e.miroc.90 = V19)

temp.rcp85e.noresm <- temp.rcp85e.noresm %>%
  select(V4,V7,V10,V13,V16,V19) %>%
  dplyr::rename(Tmin_std_gridmet.rcp85e.noresm.mean = V4,
                Tmax_std_gridmet.rcp85e.noresm.mean = V7,
                Tmin_std_gridmet.rcp85e.noresm.10 = V10,
                Tmax_std_gridmet.rcp85e.noresm.10 = V13,
                Tmin_std_gridmet.rcp85e.noresm.90 = V16,
                Tmax_std_gridmet.rcp85e.noresm.90 = V19)

############# merge temp and precip data ##############

proj <- cbind(pcp.rcp45e.gfdl, pcp.rcp45e.canesm, pcp.rcp45e.mri, pcp.rcp45e.miroc, pcp.rcp45e.noresm,
              pcp.rcp45m.gfdl, pcp.rcp45m.canesm, pcp.rcp45m.mri, pcp.rcp45m.miroc, pcp.rcp45m.noresm,
              pcp.rcp85e.gfdl, pcp.rcp85e.canesm, pcp.rcp85e.mri, pcp.rcp85e.miroc, pcp.rcp85e.noresm,
              pcp.rcp85m.gfdl, pcp.rcp85m.canesm, pcp.rcp85m.mri, pcp.rcp85m.miroc, pcp.rcp85m.noresm,
              
              temp.rcp45e.gfdl, temp.rcp45e.canesm, temp.rcp45e.mri, temp.rcp45e.miroc, temp.rcp45e.noresm,
              temp.rcp45m.gfdl, temp.rcp45m.canesm, temp.rcp45m.mri, temp.rcp45m.miroc, temp.rcp45m.noresm,
              temp.rcp85e.gfdl, temp.rcp85e.canesm, temp.rcp85e.mri, temp.rcp85e.miroc, temp.rcp85e.noresm,
              temp.rcp85m.gfdl, temp.rcp85m.canesm, temp.rcp85m.mri, temp.rcp85m.miroc, temp.rcp85m.noresm)

#data check
#180 vars? 2 RCPs * 2 time periods * 5 models * 3 parts of distribution * 3climate vars = 108 

proj <- merge(proj, bill, by="row.names")



####### merge original nest data with MACA data #############

nest.proj <- merge(nest, proj, by.x="attempt", by.y="attempt", all.x=T, all.y=F)

#data check: are all model covariates included in database?
covars <- names(summary(model)$coeff[-1,1])
RE <- names(ranef(model)); gsub('UnCoor:Region', 'UnCoor', RE)
covars <- c(covars, RE)
covars %in% colnames(nest.proj)
setdiff(covars, colnames(nest.proj))
#only covars not in databse are interactions or multiple levels of categorical vars

write.csv(nest.proj, 'Data/active/success_projections.csv', row.names = F)
