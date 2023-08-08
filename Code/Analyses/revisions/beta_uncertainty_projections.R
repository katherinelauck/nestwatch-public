# Model uncertainty violin plot
# Author: Katherine Lauck
# Last updated: 12 July 2023

library(tidyverse)
library(lme4)
library(boot)
library(biscale)
library(ggpubr)
library(scales)
library(cowplot)

## 1. Gather data: success_projections.csv and outputs of main model

d <- read_csv("Data/active/success_projections_computed.csv")
m <- read_rds('results/spatial/success~tnestpd_meanmax_gridmet_tmax.rds')

###Getting models parameters
fixef <- as.data.frame(summary(m)$coefficients)

lu_coef <- tibble(NewLU1 = c("Forest","Ag","Natural_open","Human"),
                  LU = c(0,fixef[3:5,1]),
                  int_mean = c(0,fixef[14:16,1]),
                  int_sq_mean = c(0,fixef[17:19,1]),
                  int_5 = c(0,(fixef[14:16,1]-1.2816*fixef[14:16,2])),
                  int_95 = c(0,(fixef[14:16,1]+1.2816*fixef[14:16,2])),
                  int_sq_5 = c(0, (fixef[17:19,1]-1.2816*fixef[17:19,2])),
                  int_sq_95 = c(0,(fixef[17:19,1]+1.2816*fixef[17:19,2])),
                  intercept = fixef[1,1],
                  Tmax_std_gridmet_beta = fixef[2,1],
                  Tmax_std_gridmet_beta_5 = fixef[2,1]-1.2816*fixef[2,2],
                  Tmax_std_gridmet_beta_95 = fixef[2,1]+1.2816*fixef[2,2],
                  Tmax_std_gridmet_sq_beta = fixef[6,1],
                  Tmax_std_gridmet_sq_beta_5 = fixef[6,1]-1.2816*fixef[6,2],
                  Tmax_std_gridmet_sq_beta_95 = fixef[6,1]+1.2816*fixef[6,2],
                  tnestpd_meanmax_gridmet_beta = fixef[7,1],
                  PcpBefore_raw_gridmet = fixef[8,1],
                  NLCD_p_forest_beta = fixef[9,1],
                  NLCD_p_human_beta = fixef[10,1],
                  NLCD_p_ag_beta = fixef[11,1],
                  substrate = fixef[12,1],
                  laydate_scaled_beta = fixef[13,1],
                  int.Tmax.tnestpd_meanmax = fixef[20,1])

d <- left_join(d,lu_coef,by = "NewLU1")

## 2. Project at mean of each model (x5) and scenario (x4) across 95% & 5% of luxtemp beta and luxtemp2 beta

project <- function(scenario,data=d) {
  
  data <- mutate(data,
                 !!paste0("y.fut.",scenario,".5.5") := inv.logit(intercept + 
                                    Tmax_std_gridmet_beta_5*get(paste0("Tmax_std_gridmet.",scenario,".mean")) + 
                                    LU.y +
                                    int_5*get(paste0("Tmax_std_gridmet.",scenario,".mean")) +
                                    Tmax_std_gridmet_sq_beta_5*get(paste0("Tmax_std_gridmet.",scenario,".mean_sq")) +
                                    int_sq_5*get(paste0("Tmax_std_gridmet.",scenario,".mean_sq")) +
                                    int.Tmax.tnestpd_meanmax*get(paste0("Tmax_std_gridmet.",scenario,".mean"))*tnestpd_meanmax_gridmet +
                                    tnestpd_meanmax_gridmet_beta*tnestpd_meanmax_gridmet +
                                    laydate_scaled_beta*laydate_scaled +
                                    PcpBefore_raw_gridmet*get(paste0("pcpbefore_raw_gridmet.",scenario,".mean"))  + 
                                    NLCD_p_forest_beta*NLCD_p_forest + 
                                    NLCD_p_human_beta*NLCD_p_human  + 
                                    NLCD_p_ag_beta*NLCD_p_ag  + 
                                    substrate*substrate_binary),
                 !!paste0("y.fut.",scenario,".95.95") := inv.logit(intercept + 
                                    Tmax_std_gridmet_beta_95*get(paste0("Tmax_std_gridmet.",scenario,".mean")) + 
                                    LU.y +
                                    int_95*get(paste0("Tmax_std_gridmet.",scenario,".mean")) +
                                    Tmax_std_gridmet_sq_beta_95*get(paste0("Tmax_std_gridmet.",scenario,".mean_sq")) +
                                    int_sq_95*get(paste0("Tmax_std_gridmet.",scenario,".mean_sq")) +
                                    int.Tmax.tnestpd_meanmax*get(paste0("Tmax_std_gridmet.",scenario,".mean"))*tnestpd_meanmax_gridmet +
                                    tnestpd_meanmax_gridmet_beta*tnestpd_meanmax_gridmet +
                                    laydate_scaled_beta*laydate_scaled +
                                    PcpBefore_raw_gridmet*get(paste0("pcpbefore_raw_gridmet.",scenario,".mean"))  + 
                                    NLCD_p_forest_beta*NLCD_p_forest + 
                                    NLCD_p_human_beta*NLCD_p_human  + 
                                    NLCD_p_ag_beta*NLCD_p_ag  + 
                                    substrate*substrate_binary),
                 !!paste0("y.fut.dif.tt2.",scenario) := abs(get(paste0("y.fut.",scenario,".95.95"))-get(paste0("y.fut.",scenario,".5.5"))))
  
  dplyr::select(data,as.name(paste0("y.fut.dif.tt2.",scenario)))
  
}

scenarios <- expand.grid(clim = c("rcp45m","rcp45e","rcp85m","rcp85e"),
                         mod = c("gfdl","canesm","mri","miroc","noresm")) %>%
  tibble() %>%
  mutate(scenario = str_c(clim,mod,sep = ".")) %>%
  pull(scenario)

out <- map(scenarios,project,data = d) %>% list_cbind() %>% # map over 80 scenarios
  mutate(attempt = pull(d,attempt),NewLU1 = pull(d,NewLU1),UnCoor = pull(d,UnCoor)) %>% # add attempt and NewLU1 so can make maps with this data frame directly
  pivot_longer(!c("attempt","NewLU1","UnCoor"),names_to = "scenario") %>% # pull colnames into column
  mutate(mod = str_replace(scenario,"(\\.gfdl)|(\\.canesm)|(\\.mri)|(\\.miroc)|(\\.noresm)","")) %>% # split colnames to obtain grouping variable
  summarize(y.fut.dif = mean(value),.by = c(attempt,mod,NewLU1,UnCoor)) %>%
  mutate(mod = str_replace(mod,"y\\.fut\\.dif\\.",""),
         scenario = str_replace(mod,"(tt2mean\\.)|(tt2\\.)",""),
         group = str_replace(mod,"(\\.rcp45m)|(\\.rcp45e)|(\\.rcp85m)|(\\.rcp85e)","")) %>%
  dplyr::select(!mod) %>%
  mutate(scenario = factor(scenario,levels = c("rcp45m","rcp45e","rcp85m","rcp85e")))
  
## 6. Use as y value for violin plot

labs.LU <- c("Agriculture", "Forest", "Developed", 'Natural open')
names(labs.LU) <- c("Ag", "Forest", "Human", 'Natural_open')

labs.scenario <- c("RCP 4.5\n2050","RCP 4.5\n2100","RCP 8.5\n2050","RCP 8.5\n2100")
names(labs.scenario) <- c("rcp45m", "rcp45e",
                          "rcp85m", "rcp85e")

(ptt2 <- ggplot(filter(out,group == "tt2"), aes(x=scenario, y=y.fut.dif)) +
  geom_violin(aes(fill=scenario, color=scenario))+
  geom_boxplot(width=.1, color='gray20', fill='gray20',
               outlier.shape = NA) + #remove outliers
  stat_summary(fun=median, geom="point", size=.8, color="white")+
  scale_x_discrete(labels=labs.scenario) +
  scale_fill_manual(values = c("#fbb61a","#fbb61a","#320a5e","#320a5e")) +
  scale_color_manual(values = c("#fbb61a","#fbb61a","#320a5e","#320a5e")) +
  labs(x='Climate scenario',
       y='Statistical uncertainty')+
  theme_bw() +
    ylim(0,.784) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", linewidth=1),
        legend.position = "none") +
  facet_grid(rows = vars(NewLU1),
             labeller = labeller(NewLU1 = labs.LU)))

ggsave("figures/statuncertainty_tt2both.png",ptt2,width = 3, height = 5)

(plot <- cowplot::plot_grid(p1, p3,ptt2, # requires p1 & p3 from Code/SuccessProjectionsBoxplot.R
                  ncol = 3, nrow=1,
                  labels = c("A","B","C"), label_size = 20))

ggsave('figures/violinplots.projections.png',
       width = 8.5, height = 5)


#### Statistical uncertainty map 

locations <- d %>% dplyr::select(UnCoor,lon,lat) %>% unique()

df.all <- out %>%
  filter(scenario == "rcp85e") %>%
  group_by(UnCoor) %>% #take mean for unique coordinate location
  dplyr::summarize(across(c(y.fut.dif), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F)

df.ag <- out %>%
  filter(NewLU1 =="Ag",scenario == "rcp85e") %>%
  group_by(UnCoor) %>%
  dplyr::summarize(across(c(y.fut.dif), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F)

df.forest <- out %>%
  filter(NewLU1 =="Forest",scenario == "rcp85e") %>%
  group_by(UnCoor) %>%
  dplyr::summarize(across(c(y.fut.dif), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F)

df.natopen <- out %>%
  filter(NewLU1 =="Natural_open",scenario == "rcp85e") %>%
  group_by(UnCoor) %>%
  dplyr::summarize(across(c(y.fut.dif), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F)

df.dev <- out %>%
  filter(NewLU1 =="Human",scenario == "rcp85e") %>%
  group_by(UnCoor) %>%
  dplyr::summarize(across(c(y.fut.dif), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F)

map.usa <- map_data("usa")

my.theme.map = theme(
  title= element_text(size =12),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.margin = unit(c(0, 0, 0, 0), "cm"), 
  legend.key.width=unit(2,"cm"),
  legend.text=element_text(size=12),
  plot.title = element_text(hjust = .2,vjust = -4))

(ag.uncertainty <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = df.ag,
             mapping = aes(x=lon, y=lat, colour = y.fut.dif),
             size =1.5, alpha = 0.5) +
    scale_color_gradient(name = "",low ="forestgreen" ,high = "hotpink2",labels = scales::number_format(accuracy = .01,scale=1,c(expression("","","","",">"))),breaks = seq(from = 0, to = .6, by = .15),limits = c(0,.6), oob = squish) +
    ggtitle("Agriculture") +
  theme(legend.position = "") +
  labs(x=NULL, y=NULL))

(forest.uncertainty <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = df.forest,
             mapping = aes(x=lon, y=lat, colour = y.fut.dif),
             size =1.5, alpha = 0.5) +
    scale_color_gradient(name = "",low ="forestgreen" ,high = "hotpink2",labels = scales::number_format(accuracy = .01,scale=1,c(expression("","","","",">"))),breaks = seq(from = 0, to = .6, by = .15),limits = c(0,.6), oob = squish) +
    ggtitle("Forest") +
  theme(legend.position = "") +
  labs(x=NULL, y=NULL))

(dev.uncertainty <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = df.dev,
             mapping = aes(x=lon, y=lat, colour = y.fut.dif),
             size =1.5, alpha = 0.5) +
    scale_color_gradient(name = "",low ="forestgreen" ,high = "hotpink2",labels = scales::number_format(accuracy = .01,scale=1,c(expression("","","","",">"))),breaks = seq(from = 0, to = .6, by = .15),limits = c(0,.6), oob = squish) +
    ggtitle("Developed") +
  theme(legend.position = "") +
  labs(x=NULL, y=NULL))

(natopen.uncertainty <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = df.natopen,
             mapping = aes(x=lon, y=lat, colour = y.fut.dif),
             size =1.5, alpha = 0.5) +
    scale_color_gradient(name = "",low ="forestgreen" ,high = "hotpink2",labels = scales::number_format(accuracy = .01,scale=1,c(expression("","","","",">"))),breaks = seq(from = 0, to = .6, by = .15),limits = c(0,.6), oob = squish) +
    ggtitle("Natural open") +
  theme(legend.position = "") +
  labs(x=NULL, y=NULL))

(map.uncertainty <- ggarrange(forest.uncertainty,
                              ag.uncertainty,
                              natopen.uncertainty,
                              dev.uncertainty,
                              nrow=4,ncol=1,
                              widths=c(0.5,1),
                              common.legend = TRUE, legend="bottom"))

(map.uncertainty <- annotate_figure(map.uncertainty,
                bottom = text_grob("Statistical uncertainty (2100 climate for RCP 8.5)", 
                                   color = "black", size = 16)))


(bigmap <- cowplot::plot_grid(map.uncertainty.average.points,map.uncertainty,
                     ncol=2,
                     nrow=1,
                     labels = c("A","B"),
                     label_size = 20))


ggsave(bigmap,device="png",filename = "figures/bigmap.png",
       width = 300, height = 350, units = c( "mm"))
