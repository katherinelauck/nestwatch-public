#!/usr/bin/env Rscript

### Run Moran's I

library(tidyverse)
library(ape)
library(purrr)
library(lme4)
library(INLA)
library(terra)

args=commandArgs(trailingOnly=TRUE)

UnCoor <- read_rds('results/revisions/inla_data.rds') %>% pull(UnCoor)

if(args[1] == "noautocorr"){
  
  res <- read_rds("results/revisions/resid_inla_noautocorr.rds") %>% tibble()
  model <- "results/revisions/inla_noautocorr.rds"
  outfile <- paste0("results/revisions/morani_inla/noautocorr_",args[2],".csv")
  
} else {
  meshsize <- args[1] %>% str_split_1("_") %>% as.numeric()
  res <- paste0("results/revisions/resid_inla_",
                args[3],
                "_",
                args[1],
                ".rds") %>%
         read_rds() %>%
         tibble()
  model <- paste0("results/revisions/inla_",
                  args[3],
                  "_",
                  args[1],
                  ".rds")
  outfile <- paste0("results/revisions/morani_inla/",args[3],"_",args[1],"_",args[2],".csv")
}

loc <- str_split_fixed(UnCoor,"_",n=2) %>%
  as_tibble(.name_repair = "universal") %>% 
  rename(lon = "...1",lat = "...2") %>%
  mutate(row_num = row_number()) %>%
  mutate(lon = as.numeric(lon),lat = as.numeric(lat)) %>%
  vect(geom = c("lon","lat"),crs = "EPSG:4326")
proj_loc <- project(loc,"ESRI:102010")
loc_utm <- as.data.frame(proj_loc,geom = "XY") %>% as_tibble()
loc_sample <- slice_sample(loc_utm, prop = .1)

nest.dists.inv <- 1/as.matrix(dist(cbind(as.numeric(loc_sample$x),as.numeric(loc_sample$y))))
nest.dists.inv[is.infinite(nest.dists.inv)] <- 0
p <- res %>% slice(c(pull(loc_sample,row_num))) %>% pull() %>% Moran.I(nest.dists.inv)

p %>% t() %>% as_tibble() %>% 
  unnest(cols = c(observed,expected,sd,p.value)) %>%
  mutate(model = model %>% str_replace("results/revisions/","")) %>%
  write_csv(file=outfile)
