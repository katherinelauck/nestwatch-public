#!/usr/bin/env Rscript

args=commandArgs(trailingOnly=TRUE)

##### Implement INLA with nestwatch data to check for sensitivity to spatial autocorrelation
##### Author: Katherine Lauck
##### Last updated: 20230221

library(INLA)
library(tidyverse)

TestHosts <- read_rds('results/revisions/inla_data.rds')

Locations <- matrix(c(TestHosts$X,TestHosts$Y),ncol = 2)

meshsize <- args[1] %>% str_split_1("_") %>% as.numeric()

MeshC <- inla.mesh.2d(Locations, max.edge = meshsize)

# plot(MeshC)
# points(Locations,col = "red",pch = 2)

# Making the A matrix

HostsA <- inla.spde.make.A(MeshC, loc = Locations) # Making A matrix
Hosts.spde = inla.spde2.pcmatern(mesh = MeshC, prior.range = c(10, 0.5), prior.sigma = c(.5, .5)) # Making SPDE
w.Host <- inla.spde.make.index('w', n.spde = Hosts.spde$n.spde) # making the w


# Making the model matrix #### 

m <- read_rds("results/revisions/mainv1_withregion.rds")
resp <- "at_least_one_success"
fixed.effects <- formula(m) %>% as.character() %>% str_replace(pattern = " \\+ \\(.*\\)","")

X0 <- model.matrix(as.formula(paste0(" ~ -1 + ", fixed.effects[3])), data = TestHosts) # make the model matrix using the final model selection formula without a response variable.

X <- as.data.frame(X0[,-which(colnames(X0)%in%c("NewLU1Forest"))]) # convert to a data frame. Eliminate the base level of the first categorical variable if applicable (you will manually specify an intercept below) 

head(X)

names(X) <- str_replace(names(X),"\\:",".") # interaction character not allowed in inla.stack. How to match names with formula?

# Making the stack ####

N <- nrow(TestHosts)

StackHost <- inla.stack(
  data = list(y = TestHosts[,resp]), # specify the response variable
  
  A = list(1, 1, 1, 1, 1, HostsA), # Vector of Multiplication factors for random and fixed effects              
  
  effects = list(
    
    Intercept = rep(1, N), # specify the manual intercept!
    
    X = X, # attach the model matrix
    
    species = TestHosts$species, # insert vectors of any random effects
    UnCoor = TestHosts$UnCoor,
    year = TestHosts$year,
    
    w = w.Host)) # attach the w 

write_rds(StackHost,paste0("results/revisions/stackhost_",
                           args[1],
                           ".rds"))
write_rds(Hosts.spde,paste0("results/revisions/spde_",
                            args[1],
                            ".rds"))
