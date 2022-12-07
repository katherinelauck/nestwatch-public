#### Run Moran.I on cluster
#### DO NOT CLOSE
require(tidyverse)
require(ape)
require(furrr)
require(purrr)
require(lme4)
require(future.apply)

# models <- list.files("results/revisions","(region([[:digit:]])*\\.rds|region([[:digit:]])*\\.RDS)",full.names = TRUE)
models <- c("results/revisions/mainv1_region150eletemplonlat.rds",
            "results/revisions/mainv1_region150eletemplat.rds",
            "results/revisions/mainv1_region150eletemp.rds",
            "results/revisions/mainv1_region150lonlat.rds",
            "results/revisions/mainv1_region150lat.rds",
            "results/revisions/mainv1_eletemplonlat.rds",
            "results/revisions/mainv1_eletemplat.rds",
            "results/revisions/mainv1_eletemp.rds",
            "results/revisions/mainv1_lonlat.rds",
            "results/revisions/mainv1_lat.rds")

data <- read_rds("Data/active/success-cleaned.rds")

loc <- str_split_fixed(data$UnCoor,"_",n=2) %>%
  as_tibble(.name_repair = "universal") %>% 
  rename(lon = "...1",lat = "...2") %>%
  mutate(row_num = row_number())

bootstrap.moran.i <- function(model,loc,prop = .1){
  loc_sample <- slice_sample(loc, prop = prop)
  nest.dists.inv <- 1/as.matrix(dist(cbind(as.numeric(loc_sample$lon),as.numeric(loc_sample$lat))))
  nest.dists.inv[is.infinite(nest.dists.inv)] <- 0
  p <- read_rds(model) %>% resid() %>% tibble() %>% slice(c(pull(loc_sample,row_num))) %>% pull() %>% Moran.I(nest.dists.inv)
}

rep_moran <- function(model,loc,prop = .1,n = 500){
  future_replicate(n,{
    bootstrap.moran.i(model,loc,prop)
  },.options = furrr_options(seed = TRUE)) %>% t() %>% 
    as_tibble() %>% 
    unnest(cols = c(observed,expected,sd,p.value)) %>%
    mutate(model = model %>% str_replace("results/revisions/",""))
}


plan(list(
  tweak(multisession, workers = 7),
  sequential
))

out <- future_map_dfr(models,rep_moran,loc = loc, prop = .1,n = 500,.options = furrr_options(seed = TRUE))
write_rds(out,"results/revisions/moran.i_region150eletemplonlat.rds")
