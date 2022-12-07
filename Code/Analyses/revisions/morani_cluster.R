run_moran_i <- function(models,prop = 1,n = 1,plan = list(sequential),outfile) {
  # models: character vector of model files (full names needed for compatibility with Linux/cluster) that you want to run Moran's I test on
  # prop: proportion of rows to sample for bootstrapping, if needed
  # n: number of replicate samples per model for bootstrapping, if needed
  # plan: parallel processing plan, see package furrr for more details. Must be a list. Parallel processing may operate on one or both levels, the top level being per model and the second level being per sample. So far I've had the best results just parallel processing at the model level.
  # outfile: string specifying save location of output
  require(tidyverse)
  require(ape)
  require(furrr)
  require(purrr)
  require(lme4)
  require(future.batchtools)
  require(future.apply)
  
  bootstrap.moran.i <- function(model,prop){
    m <- read_rds(model)
    UnCoor <- m@frame %>% pull(UnCoor)
    loc <- str_split_fixed(UnCoor,"_",n=2) %>%
      as_tibble(.name_repair = "universal") %>% 
      rename(lon = "...1",lat = "...2") %>%
      mutate(row_num = row_number())
    loc_sample <- slice_sample(loc, prop = prop)
    nest.dists.inv <- 1/as.matrix(dist(cbind(as.numeric(loc_sample$lon),as.numeric(loc_sample$lat))))
    nest.dists.inv[is.infinite(nest.dists.inv)] <- 0
    p <- m %>% resid() %>% tibble() %>% slice(c(pull(loc_sample,row_num))) %>% pull() %>% Moran.I(nest.dists.inv)
  }
  
  rep_moran <- function(model,prop,n){
    gc()
    print(paste0("starting ",model))
    future_replicate(n,{
      bootstrap.moran.i(model,prop)
    }, .options = furrr_options(seed = TRUE)) %>% t() %>% 
      as_tibble() %>% 
      unnest(cols = c(observed,expected,sd,p.value)) %>%
      mutate(model = model %>% str_replace("results/revisions/",""))
  }
  
  plan(plan)
  
  out <- future_map_dfr(models,rep_moran, prop = prop,n = n,.options = furrr_options(seed = TRUE))
  write_rds(out,outfile)
}

models <- list.files(path = "results/revisions",pattern = "((mainv1_lat\\.rds$)|(mainv1_lonlat\\.rds$)|(eletemp)|(mainv1_region150lat\\.rds$)|(mainv1_region150lonlat\\.rds$))",full.names = TRUE)
run_moran_i(models,prop = .1,n = 500,plan = list(batchtools_slurm,multisession),outfile = "results/revisions/moran.i_region150eletemplonlat.rds")
