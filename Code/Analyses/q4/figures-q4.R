#### Build figures for Question 1
#### Author: Katherine Lauck
#### Last updated: 27 April 2021
## Needed: method for computing upper and lower confidence intervals


## Dependencies
library(tidyverse)
library(boot) # Needed for inv.logit function
library(ggpubr)
library(lme4)
library(scales)
library(grid)
library(gridExtra)

## Load baseline model results
m.tavgnestpd <- read_rds("results/spatial/success~tavgnestpd_meanmax_gridmet_3way.rds")
m.rel2sp_anom <- read_rds("results/spatial/success~tmean_rel2sp_anom_3way.rds")
m.rel2sp_z <- read_rds("results/spatial/success~tmean_rel2sp_z_3way.rds")
m.tnestpd <- read_rds('results/spatial/success~tnestpd_meanmax_gridmet_3way.rds')
m.tnestpd_stdsp <- read_rds('results/spatial/success~tnestpd_stdmaxsp_gridmet_3way.rds')
m.tnestpd_rel2sheard_anom <- read_rds('results/spatial/success~tnestpd_rel2sheard_anom_3way.rds')
m.tnestpd_rel2sheard_z <- read_rds('results/spatial/success~tnestpd_rel2sheard_z_3way.rds')


# ## Load data
# data <- read_rds("Data/active/success-cleaned.rds")
# 
# temp <- range(data$Tmax_std_gridmet) # c(-2, 2) # define range of predictor in standard scale
# temp <- seq(from = temp[1],to = temp[2], length.out = 200) # build dummy temp evenly spread across range

high.mean.low <- function(x) {
  c(mean(x)+sd(x),mean(x),mean(x)-sd(x))
}

data <- read_rds("Data/active/success-cleaned.rds")

tavgnestpd <- high.mean.low(data$tmeanmax_avgnestpd_gridmet_scaled)
rel2sp_anom <- high.mean.low(data$tmean_rel2sp_anom)
rel2sp_z <- high.mean.low(data$tmean_rel2sp_z)
tnestpd <- high.mean.low(data$tnestpd_meanmax_gridmet)
tnestpd_stdsp <- high.mean.low(data$tnestpd_stdmaxsp_gridmet)
tnestpd_rel2sheard_anom <- high.mean.low(data$tnestpd_rel2sheard_anom)
tnestpd_rel2sheard_z <- high.mean.low(data$tnestpd_rel2sheard_z)

make.grid <- function(...) {
  ## Load data
  data <- read_rds("Data/active/success-cleaned.rds")
  temp <- range(data$Tmax_std_gridmet) # c(-2, 2) # define range of predictor in standard scale
  temp <- seq(from = temp[1],to = temp[2], length.out = 200) # build dummy temp evenly spread across range
  grid <- expand.grid(Tmax_std_gridmet = temp, NewLU1= c("Forest","Ag","Natural_open","Human"), pcpbefore_raw_gridmet=0,NLCD_p_forest=0, NLCD_p_human=0, NLCD_p_ag=0,laydate_scaled=0,at_least_one_success=1,substrate_binary = 1,...) %>%
    mutate(Tmax_std_gridmet_sq = Tmax_std_gridmet*Tmax_std_gridmet,
           level = rep(c("High", "Mean", "Low"), each = 2400/3)) # build grid with predictors
}

grid.tavgnestpd <- make.grid(tmeanmax_avgnestpd_gridmet_scaled = tavgnestpd)
grid.rel2sp_anom <- make.grid(tmean_rel2sp_anom = rel2sp_anom)
grid.rel2sp_z <- make.grid(tmean_rel2sp_z = rel2sp_z)
grid.tnestpd <- make.grid(tnestpd_meanmax_gridmet = tnestpd)
grid.tnestpd_stdsp <- make.grid(tnestpd_stdmaxsp_gridmet = tnestpd_stdsp)
grid.tnestpd_rel2sheard_anom <- make.grid(tnestpd_rel2sheard_anom = tnestpd_rel2sheard_anom)
grid.tnestpd_rel2sheard_z <- make.grid(tnestpd_rel2sheard_z = tnestpd_rel2sheard_z)


temp_predict <- function(model,grid,...) {
  name <- gsub("grid\\.","",deparse(substitute(grid)))
  mm=model.matrix(terms(model), grid)
  predicted = mm %*% fixef(model) 
  pvar1 <- diag(mm %*% vcov(model) %*% t(mm))
  return(bind_cols(lu = grid$NewLU1,temp = grid$Tmax_std_gridmet, ..., predicted = inv.logit(predicted), lower = inv.logit(predicted-2*sqrt(pvar1)), upper = inv.logit(predicted+2*sqrt(pvar1)), model = name, level = grid$level)) # bind into dataframe & return
  
}

figdata.tnestpd_stdsp <- temp_predict(m.tnestpd_stdsp,grid.tnestpd_stdsp,hot = grid.tnestpd_stdsp$tnestpd_stdmaxsp_gridmet)
figdata.tavgnestpd <- temp_predict(m.tavgnestpd,grid.tavgnestpd,hot = grid.tavgnestpd$tmeanmax_avgnestpd_gridmet_scaled)
figdata.rel2sp_anom <- temp_predict(m.rel2sp_anom,grid.rel2sp_anom,hot = grid.rel2sp_anom$tmean_rel2sp_anom)
figdata.rel2sp_z <- temp_predict(m.rel2sp_z,grid.rel2sp_z,hot = grid.rel2sp_z$tmean_rel2sp_z)
figdata.tnestpd <- temp_predict(m.tnestpd,grid.tnestpd,hot = grid.tnestpd$tnestpd_meanmax_gridmet)
figdata.tnestpd_rel2sheard_anom <- temp_predict(m.tnestpd_rel2sheard_anom,grid.tnestpd_rel2sheard_anom,hot = grid.tnestpd_rel2sheard_anom$tnestpd_rel2sheard_anom)
figdata.tnestpd_rel2sheard_z <- temp_predict(m.tnestpd_rel2sheard_z,grid.tnestpd_rel2sheard_z,hot = grid.tnestpd_rel2sheard_z$tnestpd_rel2sheard_z)

bigfigdata <- bind_rows(figdata.tavgnestpd,figdata.rel2sp_anom,
                        #figdata.rel2sp_z, 
                        figdata.tnestpd, figdata.tnestpd_stdsp
                        #,figdata.tnestpd_rel2sheard_anom
                        #,figdata.tnestpd_rel2sheard_z
                        )



sig_lab <- data.frame(lu = rep(c("Ag", "Human", "Forest", "Natural_open"),times = 4), model = rep(unique(bigfigdata$model),each = 4),label = c("","","","","","*","","",
                                                                                                                                               #"","","","",
                                                                                                                                               "","**","","***","","*","","*"
                                                                                                                                               #,"","*","",""
                                                                                                                                               #,"","","",""
                                                                                                                                               )) ## significance labels for each facet

spatial.plot <- function(fig_data) {
  lu.labs <- c("Forest", "Agriculture", "Natural open", "Developed")
  names(lu.labs) <- c("Forest", "Ag", "Natural_open", "Human")
  model.labs <- c("Site mean max temp minus sp. mean",
                  #"Site mean max temp minus sp. mean divided by range",
                  "Site mean max temp over average nest period", "Site mean max temp over attempt nest period", "Site mean max temp over attempt nest period, standardized by Nestwatch species mean and SD"
                  #, "Site mean max temp over attempt nest period, minus species mean from Sheard dataset"
                  #,"Site mean max temp over attempt nest period, minus species mean from Sheard dataset, divided by range from same"
                  )
  names(model.labs) <- c("rel2sp_anom",
                         #"rel2sp_z",
                         "tavgnestpd", "tnestpd","tnestpd_stdsp"
                         #,"tnestpd_rel2sheard_anom"
                         #,"tnestpd_rel2sheard_z"
                         )
  #show_col(hue_pal()(4))
  p <- ggplot(data = fig_data, aes(temp, predicted)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, color = factor(level,levels = c("High","Mean","Low")), fill = factor(level,levels = c("High","Mean","Low"))), linetype = 2, alpha = .2) + # plot confidence intervals
    geom_line(aes(color = factor(level,levels = c("High","Mean","Low"))), size = 1) + # overlay line
    #facet_wrap(lu~model, nrow = 4, ncol = 5, labeller = labeller(lu=lu.labs)) + # facet by land use 
    facet_grid(vars(lu), vars(model),labeller = labeller(lu=lu.labs, model = model.labs)) +
    ylab("Proportion of nests with at least one offspring surviving to fledging") +
    xlab("Maximum temperature anomaly") +
    theme_classic() +
    guides(color=guide_legend(title="Historical site\ntemperature"),
           fill = guide_legend(title = "Historical site\ntemperature"))
    #theme(legend.position = "none") #+
    #geom_text(x = -2, y = .6, aes(label = label), data = sig_lab)
    #theme(axis.title.x=element_blank(),
    #      axis.text.x=element_blank(),
    #      axis.ticks.x=element_blank())
  
  #data <- read_rds("Data/active/success-cleaned.rds")
  #hist <- ggplot(data = data, aes(x = Tmax_std_gridmet)) +
    #geom_histogram() + 
    #xlab("Temperature anomaly") +
    #ylab("Frequency") +
    #theme_classic() +
    #theme(axis.title.y = element_blank())

  #plot <- grid.arrange(p, hist, hist, hist, hist, hist, ncol = 5, nrow = 2, heights = c(4,1),layout_matrix = rbind(c(1,1,1,1,1),c(2,3,4,5,6)))
  #ggsave(paste0("figures/q4-",name,".png"), plot, width = 3, height = 7.5)
  #return(plot)

}

#giant.plot <- spatial.plot(bigfigdata)
prez.plot <- spatial.plot(figdata.tavgnestpd)
ggsave("figures/q4-prezplot.png",prez.plot,width = 6, height = 6)


lu.labs <- c("Forest", "Agriculture", "Natural open", "Developed")
names(lu.labs) <- c("Forest", "Ag", "Natural_open", "Human")
model.labs <- c("Site mean max temp minus sp. mean",
                #"Site mean max temp minus sp. mean divided by range",
                "Site mean max temp over average nest period", "Site mean max temp over attempt nest period", "Site mean max temp over attempt nest period, standardized by Nestwatch species mean and SD"
                #, "Site mean max temp over attempt nest period, minus species mean from Sheard dataset"
                #,"Site mean max temp over attempt nest period, minus species mean from Sheard dataset, divided by range from same"
)
names(model.labs) <- c("rel2sp_anom",
                       #"rel2sp_z",
                       "tavgnestpd", "tnestpd","tnestpd_stdsp"
                       #,"tnestpd_rel2sheard_anom"
                       #,"tnestpd_rel2sheard_z"
)
#show_col(hue_pal()(4))


qe.plot <- ggplot(data = filter(figdata.tavgnestpd,lu == "Ag"), mapping = aes(x = temp, y = predicted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, color = factor(level,levels = c("High","Mean","Low")), fill = factor(level,levels = c("High","Mean","Low"))), linetype = 2, alpha = .2) + # plot confidence intervals
  geom_line(mapping = aes(color = factor(level,levels = c("High","Mean","Low"))), size = 1) + # overlay line
  #facet_wrap(lu~model, nrow = 4, ncol = 5, labeller = labeller(lu=lu.labs)) + # facet by land use 
  #facet_grid(vars(lu), vars(model),labeller = labeller(lu=lu.labs, model = model.labs)) +
  ylab("Proportion of nests with at least\none offspring surviving to fledging") +
  xlab("Maximum temperature anomaly") +
  theme_classic() +
  guides(color=guide_legend(title="Historical site\ntemperature"),
         fill = guide_legend(title = "Historical site\ntemperature")
         )
ggsave("figures/q4_qe.png",qe.plot,width = 6, height = 4)

# p.tavgnestpd <- spatial.plot(figdata.tavgnestpd)
# p.rel2sp_anom <- spatial.plot(figdata.rel2sp_anom)
# p.rel2sp_z <- spatial.plot(figdata.rel2sp_z)
# p.tnestpd <- spatial.plot(figdata.tnestpd)
# p.tnestpd_stdsp <- spatial.plot(figdata.tnestpd_stdsp)
# 
# giant.plot <- ggarrange(p.tavgnestpd + rremove("y.title"),
#                         p.rel2sp_anom + rremove("ylab"),
#                         p.rel2sp_z + rremove("ylab"),
#                         p.tnestpd,p.tnestpd_stdsp + rremove("ylab"),
#                         ncol = 5, nrow = 1, align = "h") %>%
#   annotate_figure(left = textGrob("Proportion of nests with at least one offspring surviving to fledging",rot = 90, vjust = 1))
ggsave("figures/q4-comparespatial.png",giant.plot,width = 9, height = 7.5)


# Supplemental figure success~tmean_rel2sp_anom_tmax.rds
# x axis: temp anomaly; y axis: success; series of lines depicting different levels of tmax

tmean <- read_rds("results/spatial/success~tmean_rel2sp_anom_tmax.rds")

make.grid <- function() {
  ## Load data
  data <- read_rds("Data/active/success-cleaned.rds")
  temp <- range(data$Tmax_std_gridmet) # c(-2, 2) # define range of predictor in standard scale
  temp <- seq(from = temp[1],to = temp[2], length.out = 200) # build dummy temp evenly spread across range
  tmean <- high.mean.low(data$tmean_rel2sp_anom) # Build dummy conservation score
  grid <- expand.grid(Tmax_std_gridmet = temp, NewLU1= c("Forest","Ag","Natural_open","Human"), pcpbefore_raw_gridmet=0,NLCD_p_forest=0, NLCD_p_human=0, NLCD_p_ag=0,laydate_scaled=0,at_least_one_success=1,substrate_binary = 1,tmean_rel2sp_anom = tmean) %>%
    mutate(Tmax_std_gridmet_sq = Tmax_std_gridmet*Tmax_std_gridmet,
           level = rep(c("High", "Mean", "Low"), each = 2400/3)) # build grid with predictors
}

tmean.grid <- make.grid()
figdata.tmean <- temp_predict(tmean,tmean.grid,hot = tmean.grid$tmean_rel2sp_anom)

tmean.plot <- spatial.plot(figdata.tmean)
tmean.plot

ggsave("figures/q4-tmean_rel2sp_anom.png",tmean.plot,width = 3, height = 6)
