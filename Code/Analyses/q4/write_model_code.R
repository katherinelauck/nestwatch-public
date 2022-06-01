##### Write model files for spatial analysis
##### Author: Katherine Lauck
##### 21 June 2021

library(tidyverse)
library(lme4)


write_spatial_models <- function(var,filename,land_use=NA) {
  
  if(is.na(land_use)){
    lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_3way.R") %>%
      str_replace_all("tmeanmax_avgnestpd_gridmet_scaled",var) %>% 
      str_replace_all("tavgnestpd_meanmax_gridmet",filename)
    cat(lines,file = paste0("Code/Analyses/q4/",filename,"_3way.R"), sep = "\n")
    
    lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_3way.sh") %>%
      str_replace_all("tavgnestpd_meanmax_gridmet", filename)
    cat(lines,file = paste0("Code/Analyses/q4/",filename,"_3way.sh"), sep = "\n")
    
    lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_2way.R") %>%
      str_replace_all("tmeanmax_avgnestpd_gridmet_scaled",var) %>% 
      str_replace_all("tavgnestpd_meanmax_gridmet", filename)
    cat(lines,file = paste0("Code/Analyses/q4/",filename,"_2way.R"), sep = "\n")
    
    lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_2way.sh") %>%
      str_replace_all("tavgnestpd_meanmax_gridmet", filename)
    cat(lines, file = paste0("Code/Analyses/q4/",filename,"_2way.sh"), sep = "\n")
      
      lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_nohot.R") %>%
        str_replace_all("tmeanmax_avgnestpd_gridmet_scaled",var) %>% 
        str_replace_all("tavgnestpd_meanmax_gridmet", filename)
      cat(lines,file = paste0("Code/Analyses/q4/",filename,"_nohot.R"), sep = "\n")
      
      lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_nohot.sh") %>%
        str_replace_all("tavgnestpd_meanmax_gridmet", filename)
      cat(lines, file = paste0("Code/Analyses/q4/",filename,"_nohot.sh"), sep = "\n")
    
  } else {
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_Ag_3way.R") %>%
    str_replace_all("tmeanmax_avgnestpd_gridmet_scaled",var) %>% 
    str_replace_all("tavgnestpd_meanmax_gridmet_Ag", paste0(filename,"_",land_use)) %>%
    str_replace_all("Ag", land_use)
    cat(lines,file = paste0("Code/Analyses/q4/",filename,"_",land_use,"_3way.R"),sep = "\n")
  
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_3way.sh") %>%
    str_replace_all("tavgnestpd_meanmax_gridmet", paste0(filename,"_",land_use))
    cat(lines,file = paste0("Code/Analyses/q4/",filename,"_",land_use,"_3way.sh"), sep = "\n")
  
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_Ag_2way.R") %>%
    str_replace_all("tmeanmax_avgnestpd_gridmet_scaled",var) %>% 
    str_replace_all("tavgnestpd_meanmax_gridmet_Ag", paste0(filename,"_",land_use)) %>%
    str_replace_all("Ag", land_use)
    cat(lines,file = paste0("Code/Analyses/q4/",filename,"_",land_use,"_2way.R"), sep = "\n")
  
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_2way.sh") %>%
    str_replace_all("tavgnestpd_meanmax_gridmet", paste0(filename,"_",land_use))
    cat(lines,file = paste0("Code/Analyses/q4/",filename,"_",land_use,"_2way.sh"), sep = "\n")
  }
}

basemodel <- read_rds("results/q12/success~stdmax2laydate2way.AK.RDS")
getCall(basemodel)


write_spatial_tempsq_LRT_models <- function(var, filename) {
  
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_tmaxsq.R") %>%
    str_replace_all("tmeanmax_avgnestpd_gridmet_scaled",var) %>% 
    str_replace_all("tavgnestpd_meanmax_gridmet",filename)
  cat(lines,file = paste0("Code/Analyses/q4/",filename,"_tmaxsq.R"), sep = "\n")
  
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_3way.sh") %>%
    str_replace_all("tavgnestpd_meanmax_gridmet_3way", paste0(filename,"_tmaxsq"))
  cat(lines,file = paste0("Code/Analyses/q4/",filename,"_tmaxsq.sh"), sep = "\n")
  
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_tmax.R") %>%
    str_replace_all("tmeanmax_avgnestpd_gridmet_scaled",var) %>% 
    str_replace_all("tavgnestpd_meanmax_gridmet", filename)
  cat(lines,file = paste0("Code/Analyses/q4/",filename,"_tmax.R"), sep = "\n")
  
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_3way.sh") %>%
    str_replace_all("tavgnestpd_meanmax_gridmet_3way", paste0(filename,"_tmax"))
  cat(lines, file = paste0("Code/Analyses/q4/",filename,"_tmax.sh"), sep = "\n")
  
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_noint.R") %>%
    str_replace_all("tmeanmax_avgnestpd_gridmet_scaled",var) %>% 
    str_replace_all("tavgnestpd_meanmax_gridmet", filename)
  cat(lines,file = paste0("Code/Analyses/q4/",filename,"_noint.R"), sep = "\n")
  
  lines <- readLines("Code/Analyses/q4/tavgnestpd_meanmax_gridmet_3way.sh") %>%
    str_replace_all("tavgnestpd_meanmax_gridmet_3way", paste0(filename,"_noint"))
  cat(lines, file = paste0("Code/Analyses/q4/",filename,"_noint.sh"), sep = "\n")
}

models <- tibble(var = c("tnestpd_rel2sheard_z","tnestpd_rel2sheard_anom","tnestpd_meanmax_gridmet","tmeanmax_avgnestpd_gridmet_scaled","tmean_rel2sp_anom","tmean_rel2sp_z","tnestpd_stdmaxsp_gridmet",rep(c("tnestpd_rel2sheard_z","tnestpd_rel2sheard_anom","tnestpd_meanmax_gridmet","tmeanmax_avgnestpd_gridmet_scaled","tmean_rel2sp_anom","tmean_rel2sp_z","tnestpd_stdmaxsp_gridmet"),each = 4)),
                 filename = c("tnestpd_rel2sheard_z","tnestpd_rel2sheard_anom","tnestpd_meanmax_gridmet","tavgnestpd_meanmax_gridmet","tmean_rel2sp_anom","tmean_rel2sp_z","tnestpd_stdmaxsp_gridmet",rep(c("tnestpd_rel2sheard_z","tnestpd_rel2sheard_anom","tnestpd_meanmax_gridmet","tavgnestpd_meanmax_gridmet","tmean_rel2sp_anom","tmean_rel2sp_z","tnestpd_stdmaxsp_gridmet"),each = 4)),
                 land_use = c(rep(NA,times = 7), rep(c("Ag","Human","Forest","Natural_open"), times = 7)))

## write spatial models for assessing significance of hotness measures
pwalk(models, write_spatial_models)

## write spatial models for assessing inclusion of interaction between hotness and squared term in final prediction model
pwalk(select(models,c(var,filename))[1:7,],write_spatial_tempsq_LRT_models)

cat("#!/bin/bash",paste0("sbatch /home/kslauck/projects/nestwatch/Code/Analyses/q4/",list.files("Code/Analyses/q4","((tmaxsq)|(tmax)|(noint))\\.sh$")),file = "Code/Analyses/q4/controller-spatial-lrt.sh",sep = "\n")

