#!/usr/bin/env Rscript

### Create distance matrices for use in Moran's I tests

library(tidyverse)
library(purrr)

args=commandArgs(trailingOnly=TRUE)

if(args[1] == "noautocorr"){
  
  files <- list.files("results/revisions/morani_inla/",pattern="noautocorr_\\d+.csv",full.names = TRUE)
  outfile <- "results/revisions/morani_inla_noautocorr.rds"
  
  
} else {
  
  files <- list.files("results/revisions/morani_inla/",pattern=paste0(args[2],"_",args[1],"_\\d+.csv"), full.names = TRUE)
  outfile <- paste0("results/revisions/morani_inla_",args[2],"_",args[1],".rds")
  
}

out <- map(files,read_csv) %>% list_rbind()

write_rds(out,outfile)
