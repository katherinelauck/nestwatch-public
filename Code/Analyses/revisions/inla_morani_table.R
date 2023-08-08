#!/usr/bin/env Rscript

##### Validation of interaction with INLA models
##### Author: Katherine Lauck
##### Last updated: 20230221

library(tidyverse)

files <- list.files("results/revisions","morani_inla_(noint|linear|quad|noautocorr)(_\\d+_\\d+)?.rds",full.names = TRUE)

type <- str_extract(files,"(noint|linear|quad|noautocorr)")
mesh <- str_extract(files, "\\d+_\\d+")

morani_results <- map(files,read_rds) %>% 
  list_rbind() %>%
  group_by(model) %>%
  summarize(p.value = mean(p.value),observed = mean(observed)) %>%
  mutate(type = str_extract(model,"(noint|linear|quad|noautocorr)")) %>%
  mutate(mesh = str_extract(model,"\\d+_\\d+")) %>%
  ungroup() %>%
  mutate(type = factor(type,levels = c("quad","linear","noint","noautocorr"))) %>%
  arrange(type) %>%
  arrange(mesh)
  
extract_row <- function(mod,morani_results) {
  name <- mod %>%
    str_replace("\\.rds","")
  print(name)
  tmp <- paste0("results/revisions/",mod) %>%
    read_rds()
  m <- tmp %>%
    summary()
  p_value <- morani_results %>%
    filter(model == mod) %>%
    pull(p.value)
  observed <- morani_results %>%
    filter(model == mod) %>%
    pull(observed)
  type <- morani_results %>%
    filter(model == mod) %>%
    pull(type)
  fix <- tmp$summary.fixed
  if(type == "noautocorr"){
    beta_ag <- fix["Tmax_std_gridmet:NewLU1Ag","mean"] %>%
      as.numeric()
    beta_ag_low <- fix["Tmax_std_gridmet:NewLU1Ag","0.025quant"] %>%
      as.numeric()
    beta_ag_high <- fix["Tmax_std_gridmet:NewLU1Ag","0.975quant"] %>%
      as.numeric()
    beta2_ag <- fix["NewLU1Ag:Tmax_std_gridmet_sq","mean"] %>%
      as.numeric()
    beta2_ag_low <- fix["NewLU1Ag:Tmax_std_gridmet_sq","0.025quant"] %>%
      as.numeric()
    beta2_ag_high <- fix["NewLU1Ag:Tmax_std_gridmet_sq","0.975quant"] %>%
      as.numeric()
    beta_forest <- fix["Tmax_std_gridmet","mean"] %>%
      as.numeric()
    beta_forest_low <- fix["Tmax_std_gridmet","0.025quant"] %>%
      as.numeric()
    beta_forest_high <- fix["Tmax_std_gridmet","0.975quant"] %>%
      as.numeric()
    beta2_forest <- fix["Tmax_std_gridmet_sq","mean"] %>%
      as.numeric()
    beta2_forest_low <- fix["Tmax_std_gridmet_sq","0.025quant"] %>%
      as.numeric()
    beta2_forest_high <- fix["Tmax_std_gridmet_sq","0.975quant"] %>%
      as.numeric()
    beta_open <- fix["Tmax_std_gridmet:NewLU1Natural_open","mean"] %>%
      as.numeric()
    beta_open_low <- fix["Tmax_std_gridmet:NewLU1Natural_open","0.025quant"] %>%
      as.numeric()
    beta_open_high <- fix["Tmax_std_gridmet:NewLU1Natural_open","0.975quant"] %>%
      as.numeric()
    beta2_open <- fix["NewLU1Natural_open:Tmax_std_gridmet_sq","mean"] %>%
      as.numeric()
    beta2_open_low <- fix["NewLU1Natural_open:Tmax_std_gridmet_sq","0.025quant"] %>%
      as.numeric()
    beta2_open_high <- fix["NewLU1Natural_open:Tmax_std_gridmet_sq","0.975quant"] %>%
      as.numeric()
    beta_human <- fix["Tmax_std_gridmet:NewLU1Human","mean"] %>%
      as.numeric()
    beta_human_low <- fix["Tmax_std_gridmet:NewLU1Human","0.025quant"] %>%
      as.numeric()
    beta_human_high <- fix["Tmax_std_gridmet:NewLU1Human","0.975quant"] %>%
      as.numeric()
    beta2_human <- fix["NewLU1Human:Tmax_std_gridmet_sq","mean"] %>%
      as.numeric()
    beta2_human_low <- fix["NewLU1Human:Tmax_std_gridmet_sq","0.025quant"] %>%
      as.numeric()
    beta2_human_high <- fix["NewLU1Human:Tmax_std_gridmet_sq","0.975quant"] %>%
      as.numeric()
  } else {
    beta_ag <- fix["Tmax_std_gridmet.NewLU1Ag","mean"] %>%
      as.numeric()
    beta_ag_low <- fix["Tmax_std_gridmet.NewLU1Ag","0.025quant"] %>%
      as.numeric()
    beta_ag_high <- fix["Tmax_std_gridmet.NewLU1Ag","0.975quant"] %>%
      as.numeric()
    beta2_ag <- fix["NewLU1Ag.Tmax_std_gridmet_sq","mean"] %>%
      as.numeric()
    beta2_ag_low <- fix["NewLU1Ag.Tmax_std_gridmet_sq","0.025quant"] %>%
      as.numeric()
    beta2_ag_high <- fix["NewLU1Ag.Tmax_std_gridmet_sq","0.975quant"] %>%
      as.numeric()
    beta_forest <- fix["Tmax_std_gridmet","mean"] %>%
      as.numeric()
    beta_forest_low <- fix["Tmax_std_gridmet","0.025quant"] %>%
      as.numeric()
    beta_forest_high <- fix["Tmax_std_gridmet","0.975quant"] %>%
      as.numeric()
    beta2_forest <- fix["Tmax_std_gridmet_sq","mean"] %>%
      as.numeric()
    beta2_forest_low <- fix["Tmax_std_gridmet_sq","0.025quant"] %>%
      as.numeric()
    beta2_forest_high <- fix["Tmax_std_gridmet_sq","0.975quant"] %>%
      as.numeric()
    beta_open <- fix["Tmax_std_gridmet.NewLU1Natural_open","mean"] %>%
      as.numeric()
    beta_open_low <- fix["Tmax_std_gridmet.NewLU1Natural_open","0.025quant"] %>%
      as.numeric()
    beta_open_high <- fix["Tmax_std_gridmet.NewLU1Natural_open","0.975quant"] %>%
      as.numeric()
    beta2_open <- fix["NewLU1Natural_open.Tmax_std_gridmet_sq","mean"] %>%
      as.numeric()
    beta2_open_low <- fix["NewLU1Natural_open.Tmax_std_gridmet_sq","0.025quant"] %>%
      as.numeric()
    beta2_open_high <- fix["NewLU1Natural_open.Tmax_std_gridmet_sq","0.975quant"] %>%
      as.numeric()
    beta_human <- fix["Tmax_std_gridmet.NewLU1Human","mean"] %>%
      as.numeric()
    beta_human_low <- fix["Tmax_std_gridmet.NewLU1Human","0.025quant"] %>%
      as.numeric()
    beta_human_high <- fix["Tmax_std_gridmet.NewLU1Human","0.975quant"] %>%
      as.numeric()
    beta2_human <- fix["NewLU1Human.Tmax_std_gridmet_sq","mean"] %>%
      as.numeric()
    beta2_human_low <- fix["NewLU1Human.Tmax_std_gridmet_sq","0.025quant"] %>%
      as.numeric()
    beta2_human_high <- fix["NewLU1Human.Tmax_std_gridmet_sq","0.975quant"] %>%
      as.numeric()
  }
  
  tibble(model = name,
         beta_ag_mean = beta_ag,
         beta_ag_low = beta_ag_low,
         beta_ag_high = beta_ag_high,
         beta2_ag_mean = beta2_ag,
         beta2_ag_low = beta2_ag_low,
         beta2_ag_high = beta2_ag_high,
         beta_forest_mean = beta_forest,
         beta_forest_low = beta_forest_low,
         beta_forest_high = beta_forest_high,
         beta2_forest_mean = beta2_forest,
         beta2_forest_low = beta2_forest_low,
         beta2_forest_high = beta2_forest_high,
         beta_open_mean = beta_open,
         beta_open_low = beta_open_low,
         beta_open_high = beta_open_high,
         beta2_open_mean = beta2_open,
         beta2_open_low = beta2_open_low,
         beta2_open_high = beta2_open_high,
         beta_human_mean = beta_human,
         beta_human_low = beta_human_low,
         beta_human_high = beta_human_high,
         beta2_human_mean = beta2_human,
         beta2_human_low = beta2_human_low,
         beta2_human_high = beta2_human_high,
         p.value = p_value,
         observed = observed) %>% return()
}

map(pull(morani_results,model),extract_row,morani_results = morani_results) %>% 
  list_rbind() %>%
  mutate(type = morani_results$type,mesh = morani_results$mesh,.before = beta_ag_mean) %>%
  write_csv(file = "results/revisions/morani_inla.csv")



