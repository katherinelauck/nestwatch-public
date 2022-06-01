library(MuMIn)
library(tidyverse)
library(lme4)

args <- commandArgs()

setwd('/home/kslauck/projects/nestwatch')

nest <- read_rds('Data/data-scaled.rds') %>%
  filter(!is.na(at_least_one_success),!is.na(Tmax_std_gridmet),!is.na(tnestpd_meanmax_gridmet),!is.na(NewLU1),!is.na(pcpbefore_raw_gridmet),!is.na(NLCD_p_forest),!is.na(NLCD_p_human),!is.na(NLCD_p_ag),!is.na(species),!is.na(year),!is.na(Region),!is.na(UnCoor),!is.na(substrate),!is.na(substrate_binary))
mod <- read_rds('results/success~meanmax3way_gridmet_nomin_subbin.rds')

options(na.action = "na.fail")
mods <- dredge(mod, beta = 'none', evaluate = FALSE, fixed = c("pcpbefore_raw_gridmet",'NLCD_p_forest','NLCD_p_human','NLCD_p_ag','substrate_binary'))
options(na.action = "na.omit")

res <- eval(mods[[as.numeric(args[1])]])

save_rds(res, paste0("results/subbin",args[1],'_out.rds'))
