#!/usr/bin/env Rscript

### Make plots
### 

library(INLA); library(ggplot2); library(ggregplot)
library(tidyverse)
library(magrittr)

args=commandArgs(trailingOnly=TRUE)
meshsize <- args[1] %>% str_split_1("_") %>% as.numeric()

noautocorr <- read_rds("results/revisions/inla_noautocorr.rds")
quad <- read_rds(paste0("results/revisions/inla_quad_",
                             args[1],
                             ".rds"))
linear <- read_rds(paste0("results/revisions/inla_linear_",
                             args[1],
                             ".rds"))
noint <- read_rds(paste0("results/revisions/inla_noint_",
                             args[1],
                             ".rds"))

effects <- Efxplot(list(noautocorr,quad,linear,noint))
ggsave(paste0("figures/effects_plot",
              "_",
              args[1],
              ".png"), effects, width = 4, height = 7)

SpatialHostList <- list(noautocorr,quad,linear,noint)

dic <- INLADICFig(SpatialHostList)
ggsave(paste0("figures/dic_plot",
              "_",
              args[1],
              ".png"),dic,width = 4, height = 7)
