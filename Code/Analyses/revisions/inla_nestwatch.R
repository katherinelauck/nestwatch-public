#!/usr/bin/env Rscript

args=commandArgs(trailingOnly=TRUE)

##### Implement INLA with nestwatch data to check for sensitivity to spatial autocorrelation
##### Author: Katherine Lauck
##### Last updated: 20230221

library(INLA); library(ggplot2); library(ggregplot)
library(tidyverse)
library(RColorBrewer)
library(magrittr)
library(terra)
library(furrr)

d <- read_rds("Data/active/success-cleaned.rds")

m <- read_rds("results/revisions/mainv1_withregion.rds")

head(d)

phen <- c("Region", "species","UnCoor","year","lon", "lat") # Base columns with spatial information we'll need

resp <- "at_least_one_success" # Response variable

covar <- c("Tmax_std_gridmet", # Temp anomaly
           "NewLU1", # Local land use (Ag, Forest, Natural_open, Human)
           "Tmax_std_gridmet_sq", # Temp anomaly squared
           "pcpbefore_raw_gridmet", # precipitation
           "NLCD_p_forest", # Landscape forest % cover
           "NLCD_p_human", # landscape human % cover
           "NLCD_p_ag", # landscape ag % cover
           "substrate_binary", # binary indication of whether in nestbox or not
           "laydate_scaled") # Julian date laydate

TestHosts <- na.omit(d[, c(phen, resp, covar)]) # Getting rid of NA's, picking adults
# We are using the [] to subset and only extract specific columns

TestHosts$UnCoor <- as.factor(TestHosts$UnCoor)

# Setting up a custom theme
THEME <- theme(axis.text.x = element_text(size = 12,colour = "black"),
               axis.text.y = element_text(size = 12, colour = "black"),
               axis.title.x = element_text(vjust = -0.35),
               axis.title.y = element_text(vjust = 1.2)) + theme_bw()

(samp_locations <- ggplot(TestHosts, aes(lon, lat)) + 
    geom_point() + 
    THEME)

fixed.effects <- formula(m) %>% as.character() %>% str_replace(pattern = " \\+ \\(.*\\)","")

mainv1_formula_noRegion <- as.formula(paste0(resp, " ~ ",
                                    fixed.effects[3],
                                    " + f(species, model = 'iid')",
                                    " + f(year, model = 'iid')",
                                    " + f(UnCoor, model = 'iid')"))


mainv1_formula_withRegion <- as.formula(paste0(resp, " ~ ",
                                        fixed.effects[3],
                                        " + f(species, model = 'iid')",
                                        " + f(year, model = 'iid')",
                                        " + f(UnCoor, model = 'iid')",
                                        " + f(Region, model = 'iid')"))

TestHosts_samp <- TestHosts # slice_sample(TestHosts,prop = .1)
write_rds(TestHosts_samp,"results/revisions/autocorr_data.rds")

inla_noautocorr <- inla(mainv1_formula_withRegion,
                       family = "binomial",
                       data = TestHosts_samp,
                       control.compute = list(residuals = TRUE, dic = TRUE, waic = TRUE),
                       control.fixed = list(correlation.matrix=TRUE))
write_rds(inla_noautocorr,"results/revisions/inla_noautocorr.rds")

write_rds(inla_noautocorr$residuals$deviance.residuals,"results/revisions/resid_noautocorr.rds")

Efxplot(inla_noautocorr)

inla_noautocorr <- read_rds('results/revisions/inla_noautocorr.rds')

Locations <- data.frame(lon = TestHosts_samp$lon,lat = TestHosts_samp$lat)
loc <- vect(Locations,geom = c("lon","lat"),crs = "+init=epsg:4326")
proj_loc <- project(loc,"ESRI:102010")
Locations <- as.data.frame(proj_loc,geom = "XY") %>% as.matrix()

meshsize = c(args[1],args[2])

MeshC <- inla.mesh.2d(Locations, max.edge = meshsize)

# plot(MeshC)
# points(Locations,col = "red",pch = 2)

# Making the A matrix

HostsA <- inla.spde.make.A(MeshC, loc = Locations) # Making A matrix
Hosts.spde = inla.spde2.pcmatern(mesh = MeshC, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
w.Host <- inla.spde.make.index('w', n.spde = Hosts.spde$n.spde) # making the w


# Making the model matrix #### 



X0 <- model.matrix(as.formula(paste0(" ~ -1 + ", fixed.effects[3])), data = TestHosts_samp) # make the model matrix using the final model selection formula without a response variable.

X <- as.data.frame(X0[,-which(colnames(X0)%in%c("NewLU1Forest"))]) # convert to a data frame. Eliminate the base level of the first categorical variable if applicable (you will manually specify an intercept below) 

head(X)

names(X) <- str_replace(names(X),"\\:",".") # interaction character not allowed in inla.stack. How to match names with formula?

# Making the stack ####

N <- nrow(TestHosts_samp)

StackHost <- inla.stack(
  data = list(y = TestHosts_samp[,resp]), # specify the response variable
  
  A = list(1, 1, 1, 1, 1, HostsA), # Vector of Multiplication factors for random and fixed effects              
  
  effects = list(
    
    Intercept = rep(1, N), # specify the manual intercept!
    
    X = X, # attach the model matrix
    
    species = TestHosts_samp$species, # insert vectors of any random effects
    UnCoor = TestHosts_samp$UnCoor,
    year = TestHosts_samp$year,
    
    w = w.Host)) # attach the w 

f_autocorr <- as.formula(paste0("at_least_one_success ~ -1 + Intercept + ",
                                paste0(colnames(X), collapse = " + "),
                                " + f(species, model = 'iid')",
                                " + f(year, model = 'iid')",
                                " + f(UnCoor, model = 'iid')"))

f_linear <- f_autocorr %>% 
  as.character() %>% 
  str_remove_all(" \\+ NewLU1(Ag|Forest|Natural_open|Human)\\.Tmax_std_gridmet_sq") %>%
  nth(3) %>%
  paste0("at_least_one_success ~ ",.) %>%
  as.formula()

f_noint <- f_linear %>% 
  as.character() %>% 
  str_remove_all(" \\+ Tmax_std_gridmet.NewLU1(Ag|Forest|Natural_open|Human)") %>%
  nth(3) %>%
  paste0("at_least_one_success ~ ",.) %>%
  as.formula()

inla_autocorr <- inla(f_autocorr,
                      family = "binomial",
                      data = inla.stack.data(StackHost),
                      control.compute = list(dic = TRUE, residuals = TRUE,waic = TRUE),
                      control.predictor = list(A = inla.stack.A(StackHost)),
                      control.fixed = list(correlation.matrix=TRUE))
write_rds(inla_autocorr,file = paste0("results/revisions/inla_autocorr",
                                      "_",
                                      paste(as.character(meshsize),sep = "-",collapse = ""),
                                      ".rds"))
write_rds(inla_autocorr$residuals$deviance.residuals,
          paste0("results/revisions/resid_autocorr",
                 "_",
                 paste(as.character(meshsize),sep = "-",collapse = ""),
                 ".rds"))

inla_autocorr_linear <- inla(f_linear,
                      family = "binomial",
                      data = inla.stack.data(StackHost),
                      control.compute = list(dic = TRUE, residuals = TRUE,waic = TRUE),
                      control.predictor = list(A = inla.stack.A(StackHost)),
                      control.fixed = list(correlation.matrix=TRUE))
write_rds(inla_autocorr_linear,paste0("results/revisions/inla_autocorr_linear",
                                      "_",
                                      paste(as.character(meshsize),sep = "-",collapse = ""),
                                      ".rds"))
write_rds(inla_autocorr_linear$residuals$deviance.residuals,
          paste0("results/revisions/resid_autocorr_linear",
                 "_",
                 paste(as.character(meshsize),sep = "-",collapse = ""),
                 ".rds"))

inla_autocorr_noint <- inla(f_noint,
                             family = "binomial",
                             data = inla.stack.data(StackHost),
                             control.compute = list(dic = TRUE, residuals = TRUE,waic = TRUE),
                             control.predictor = list(A = inla.stack.A(StackHost)),
                            control.fixed = list(correlation.matrix=TRUE))
write_rds(inla_autocorr_noint,paste0("results/revisions/inla_autocorr_noint",
                                     "_",
                                     paste(as.character(meshsize),sep = "-",collapse = ""),
                                     ".rds"))
write_rds(inla_autocorr_noint$residuals$deviance.residuals,
          paste0("results/revisions/resid_autocorr_noint",
                 "_",
                 paste(as.character(meshsize),sep = "-",collapse = ""),
                 ".rds"))

effects <- Efxplot(list(inla_noautocorr,inla_autocorr, inla_autocorr_linear, inla_autocorr_noint))
ggsave(paste0("figures/effects_plot",
              "_",
              paste(as.character(meshsize),sep = "-",collapse = ""),
              ".png"), effects, width = 4, height = 7)

SpatialHostList <- list(inla_noautocorr,inla_autocorr, inla_autocorr_linear, inla_autocorr_noint)

dic <- INLADICFig(SpatialHostList)
ggsave(paste0("figures/dic_plot",
              "_",
              paste(as.character(meshsize),sep = "-",collapse = ""),
              ".png"),dic,width = 4, height = 7)

# summary(inla_autocorr)$fixed['Tmax_std_gridmet.NewLU1Ag','mean']-summary(inla_noautocorr)$fixed['Tmax_std_gridmet:NewLU1Ag','mean']

rm(list = ls())
args=commandArgs(trailingOnly=TRUE)
meshsize = c(args[1],args[2])

source("Code/helper-functions.R")

models = c('results/revisions/inla_noautocorr.rds',
           paste0("results/revisions/inla_autocorr",
                  "_",
                  paste(as.character(meshsize),sep = "-",collapse = ""),
                  ".rds"),
           paste0("results/revisions/inla_autocorr_linear",
                  "_",
                  paste(as.character(meshsize),sep = "-",collapse = ""),
                  ".rds"),
           paste0("results/revisions/inla_autocorr_noint",
                  "_",
                  paste(as.character(meshsize),sep = "-",collapse = ""),
                  ".rds"))

run_moran_i(models,prop = .1, n = 500,
            plan = list(sequential,tweak(multisession, workers = 7)),
            outfile = paste0("results/revisions/morani_inla",
                              "_",
                              paste(as.character(meshsize),sep = "-",collapse = ""),
                              ".rds"))
   
