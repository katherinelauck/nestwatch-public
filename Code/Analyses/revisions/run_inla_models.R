#!/usr/bin/env Rscript

#### Run models

args=commandArgs(trailingOnly=TRUE)

library(INLA)
library(tidyverse)

meshsize <- args[1] %>% str_split_1("_") %>% as.numeric()
type = args[2]

StackHost <- read_rds(paste0("results/revisions/stackhost_",
                             args[1],
                             ".rds"))
Hosts.spde <- read_rds(paste0("results/revisions/spde_",
                              args[1],
                              ".rds"))

quad <- as.formula(paste0("at_least_one_success ~ -1 + Intercept + ",
                          paste0(colnames(select(StackHost$effects$data,Tmax_std_gridmet:NewLU1Human.Tmax_std_gridmet_sq)),
                                 collapse = " + "),
                          " + f(species, model = 'iid')",
                          " + f(year, model = 'iid')",
                          " + f(UnCoor, model = 'iid')",
                          " + f(w, model = Hosts.spde)"))

linear <- quad %>% 
  as.character() %>% 
  str_remove_all(" \\+ NewLU1(Ag|Forest|Natural_open|Human)\\.Tmax_std_gridmet_sq") %>%
  nth(3) %>%
  paste0("at_least_one_success ~ ",.) %>%
  as.formula()

noint <- linear %>% 
  as.character() %>% 
  str_remove_all(" \\+ Tmax_std_gridmet.NewLU1(Ag|Forest|Natural_open|Human)") %>%
  nth(3) %>%
  paste0("at_least_one_success ~ ",.) %>%
  as.formula()

m <- inla(get(type),
          family = "binomial",
          data = inla.stack.data(StackHost),
          control.compute = list(dic = TRUE, residuals = TRUE,waic = TRUE),
          control.predictor = list(A = inla.stack.A(StackHost)),
          control.fixed = list(correlation.matrix=TRUE))
write_rds(m,file = paste0("results/revisions/",
                          "inla_",
                          type,
                          "_",
                          args[1],
                          ".rds"))
write_rds(m$residuals$deviance.residuals,
          paste0("results/revisions/resid_inla_",
                 type,
                 "_",
                 args[1],
                 ".rds"))
