# Thin data to reduce spatial autocorrelation
# Author: Katherine S Lauck
# Last updated: 10 Oct 2022

# Dependencies

library(tidyverse)
library(magrittr)
library(lme4)
library(furrr)
library(dggridR)

d <- read_rds("Data/active/success-cleaned.rds")

hex_res <- seq(6,20) # res 6 approximately corresponds to 200km between centers of neighboring hexes. See: http://cran.nexr.com/web/packages/dggridR/vignettes/dggridR.html and convert area to side length
grids <- hex_res %>% map(~ dgconstruct(res = .))

make_thinned_dataset <- function(data,grid) {
  # make grid cell designation
  id <- grid %>%
    dgGEO_to_SEQNUM(data$lon,data$lat) %>%
    extract2("seqnum") %>%
    as.character()
  # add grid cell designation to dataset
  tmp <- data %>%
    ungroup() %>%
    mutate(gridcell = id)
  # randomly choose one row per grid cell to keep
  tmp <- tmp %>%
    group_by(gridcell) %>%
    slice_sample(n = 1)
  # return thinned dataset
  return(tmp)
}

l <- map(grids, ~ make_thinned_dataset(data = d, grid = .)) %>%
  set_names(paste0("res",hex_res))

model <- function(data,name) {
  library("lme4"); library(tidyverse)
  mod<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet * NewLU1 + Tmax_std_gridmet_sq * NewLU1 + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region), data = data, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06)))
  write_rds(mod,paste0("results/revisions/mainv1_oneperhex",name,"quad.rds"))
  # mod<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet * NewLU1 + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region), data = data, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06)))
  # write_rds(mod,paste0("results/revisions/mainv1_",name,"linear.rds"))
  # mod<-glmer(formula = at_least_one_success ~ Tmax_std_gridmet + NewLU1 + pcpbefore_raw_gridmet + NLCD_p_forest + NLCD_p_human + NLCD_p_ag + substrate_binary + laydate_scaled + (1 | species) + (1 | year) + (1 | Region), data = data, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+06)))
  # write_rds(mod,paste0("results/revisions/mainv1_",name,"noint.rds"))
  write_rds(data,paste0("results/revisions/mainv1_",name,"data.rds"))
}

plan(multisession)

future_walk2(.x = l,.y = names(l),model)
