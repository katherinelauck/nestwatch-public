#!/usr/bin/env Rscript

##### Validation of interaction with INLA models
##### Author: Katherine Lauck
##### Last updated: 20230221

library(tidyverse)

args=commandArgs(trailingOnly=TRUE)

files <- list.files(path = "results/revisions",pattern = paste0("^inla_(quad|linear|noint)(_",args[1],")?.rds"),full.names = TRUE)
type <- files %>% str_extract("quad|linear|noint")

output <- map(files,read_rds)
table <- map2(output,type,\(x,type) tibble(type = type, waic = x$waic$waic,p.eff = x$waic$p.eff)) %>% 
  list_rbind() %>%
  mutate(type = factor(type,levels = c("quad","linear","noint"))) %>%
  mutate(mesh = args[1]) %>%
  arrange(type)

write_csv(table,paste0("results/revisions/inla_intval_",args[1],".csv"))

if(!file.exists("results/revisions/inla_intval_noautocorr.csv")){
  output <- read_rds("results/revisions/inla_noautocorr.rds")
  table <- tibble(type = "noautocorr", waic = output$waic$waic,p.eff = output$waic$p.eff)
  write_csv(table,"results/revisions/inla_intval_noautocorr.csv")
}

