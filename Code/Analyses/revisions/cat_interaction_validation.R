#!/usr/bin/env Rscript

##### Combining validation of interaction tables for all INLA models
##### Author: Katherine Lauck
##### Last updated: 20230221

library(tidyverse)

files <- list.files("results/revisions","inla_intval_(\\d+_\\d+|noautocorr).csv",full.names = TRUE)

out <- map(files,read_csv) %>% list_rbind()

write_csv(out,"results/revisions/inla_intval.csv")
