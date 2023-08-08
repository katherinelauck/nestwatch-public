#### Correlogram of Moran's I over various spatial lags
#### Author: Katherine Lauck
#### Last updated: 30 June 2023

# spatial_lag_morani <- function(start,stop,dists,res){
#   gc()
#   print(start)
#   mat <- dists
#   dists[dists <= start | dists > stop] <- 0
#   dists <- 1/dists
#   dists[is.infinite(dists)] <- 0
#   m <- res %>% Moran.I(dists) %>% t() %>% as_tibble() %>% mutate(start = start,stop = stop)
#   return(m)
# }
# 
# correl <- function(rep,type) {
#   
#   gc()
# library(tidyverse)
# library(ape)
# library(purrr)
# library(lme4)
# library(INLA)
# library(terra)
# 
# UnCoor <- read_rds('results/revisions/inla_data.rds') %>% pull(UnCoor)
# 
# if(type == "noautocorr"){
#   
#   res <- read_rds("results/revisions/resid_inla_noautocorr.rds") %>% tibble()
#   model <- "results/revisions/inla_noautocorr.rds"
#   outfile <- paste0("results/revisions/morani_inla/correlogram_noautocorr_",rep,".csv")
#   print(outfile)
# } else {
#   res <- paste0("results/revisions/resid_inla_quad_",
#                 type,
#                 ".rds") %>%
#     read_rds() %>%
#     tibble()
#   model <- paste0("results/revisions/inla_quad_",
#                   type,
#                   ".rds")
#   outfile <- paste0("results/revisions/morani_inla/correlogram_quad_",type,"_",rep,".csv")
#   print(outfile)
# }
# 
# loc <- str_split_fixed(UnCoor,"_",n=2) %>%
#   as_tibble(.name_repair = "universal") %>% 
#   rename(lon = "...1",lat = "...2") %>%
#   mutate(row_num = row_number()) %>%
#   mutate(lon = as.numeric(lon),lat = as.numeric(lat)) %>%
#   vect(geom = c("lon","lat"),crs = "EPSG:4326")
# proj_loc <- project(loc,"ESRI:102010")
# loc_utm <- as.data.frame(proj_loc,geom = "XY") %>% as_tibble()
# loc_sample <- slice_sample(loc_utm, prop = .1)
# 
# nest.dists <- as.matrix(dist(cbind(as.numeric(loc_sample$x),as.numeric(loc_sample$y))))
# z <- res %>% slice(c(pull(loc_sample,row_num))) %>% pull()
# step <- max(nest.dists)/20
# start <- seq(0,max(nest.dists),length.out = 21)
# stop <- start[-1]
# start <- start[-length(start)]
# 
# out <- map2(start,stop,spatial_lag_morani,dists = nest.dists,res = z) %>% list_rbind() %>%
#   mutate(model = model %>% str_replace("results/revisions/","")) %>% unnest_longer(c(observed,expected,sd,p.value))
# 
# write_csv(out,file = outfile)
# 
# }
# 
# num <- seq(1,500) %>% rep(2)
# type <- rep(c("noautocorr","2000_2500"),each = 500)
# 
# out <- walk2(num,type,correl)

# library(tidyverse)
# library(purrr)
# 
# files <- list.files("results/revisions/morani_inla/",pattern="correlogram.*",full.names = TRUE)
# outfile <- "results/revisions/correlogram.rds"
# 
# out <- map(files,read_csv) %>% list_rbind()
# 
write_rds(out,outfile)

d <- read_rds("results/revisions/correlogram.rds") %>%
  mutate(step = rep(seq.int(1,20),1000)) %>%
  summarize(p.value = mean(p.value),observed = mean(observed),.by = c(model,step)) %>%
  mutate(model = factor(model))

(plot <- ggplot(d,aes(y = observed, x = step, color = model)) +
  geom_line() +
  scale_color_discrete(name = "Model",labels = c("200km region random effect","2km mesh spatial autocorrelation")) +
  theme_classic() +
  xlab("Distance bin") +
  ylab("Observed Moran's I") +
  theme(legend.position = c(.55,.75)))

ggsave("figures/correlogram.png",plot,width = 4, height = 3)
