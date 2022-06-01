rm(list=ls())

library(tidyverse)
library(biscale)
library(cowplot)
library(ggpubr)


#load data
data <- read.csv("Data/active/success_projections_computed.csv")

##### create data sets for all LUs, each LU ######

locations <- data %>% select(UnCoor,lon,lat) %>% unique()

df.all <- data %>%
  mutate(y.dif.average.mean.rcp.85e = abs(y.dif.average.mean.rcp.85e),
         y.dif.average.uncertainty.rcp.85e = abs(y.dif.average.90.rcp.85e - y.dif.average.10.rcp.85e)) %>%
  group_by(UnCoor) %>% #take mean for unique coordinate location
  dplyr::summarize(across(c(y.dif.average.mean.rcp.85e,
                            y.dif.average.uncertainty.rcp.85e), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F) 

df.ag <- data %>%
  filter(NewLU1 =="Ag") %>%
  mutate(y.dif.average.uncertainty.rcp.85e = abs(y.dif.average.90.rcp.85e - y.dif.average.10.rcp.85e)) %>%
  group_by(UnCoor) %>%
  dplyr::summarize(across(c(y.dif.average.mean.rcp.85e,
                            y.dif.average.uncertainty.rcp.85e), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F)

df.forest <- data %>%
  filter(NewLU1 =="Forest") %>%
  mutate(y.dif.average.uncertainty.rcp.85e = abs(y.dif.average.90.rcp.85e - y.dif.average.10.rcp.85e)) %>%
  group_by(UnCoor) %>%
  dplyr::summarize(across(c(y.dif.average.mean.rcp.85e,
                            y.dif.average.uncertainty.rcp.85e), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F)

df.natopen <- data %>%
  filter(NewLU1 =="Natural_open") %>%
  mutate(y.dif.average.uncertainty.rcp.85e = abs(y.dif.average.90.rcp.85e - y.dif.average.10.rcp.85e)) %>%
  group_by(UnCoor) %>%
  dplyr::summarize(across(c(y.dif.average.mean.rcp.85e,
                            y.dif.average.uncertainty.rcp.85e), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F)

df.dev <- data %>%
  filter(NewLU1 =="Human") %>%
  mutate(y.dif.average.uncertainty.rcp.85e = abs(y.dif.average.90.rcp.85e - y.dif.average.10.rcp.85e)) %>%
  group_by(UnCoor) %>%
  dplyr::summarize(across(c(y.dif.average.mean.rcp.85e,
                            y.dif.average.uncertainty.rcp.85e), mean)) %>%
  merge(locations, by='UnCoor', all.x=T, all.y=F)


########## biplots: change in nest success vs uncertainty #########

map.usa <- map_data("usa")

my.theme.map = theme(
  
  title= element_text(size =18),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.margin = unit(c(0, 0, 0, 0), "cm"), 
  legend.key.width=unit(2,"cm"))

#bi_class_ag <- bi_class(df.ag, x = y.dif.average.mean.rcp.85e, y = y.dif.average.uncertainty.rcp.85e,
#                          style = "quantile", dim = 3)
#bi_class_forest <- bi_class(df.forest, x = y.dif.average.mean.rcp.85e, y = y.dif.average.uncertainty.rcp.85e,
#                        style = "quantile", dim = 3)
#bi_class_dev <- bi_class(df.dev, x = y.dif.average.mean.rcp.85e, y = y.dif.average.uncertainty.rcp.85e,
#                        style = "quantile", dim = 3)
#bi_class_natopen <- bi_class(df.natopen, x = y.dif.average.mean.rcp.85e, y = y.dif.average.uncertainty.rcp.85e,
#                        style = "quantile", dim = 3)


#make my own cut-offs for biplot data to standardize categories across land use maps
#nest success categories are: <-5%, -5% to 5%, >5%
#uncertainty categories are: <.05, 0.05-.10, >.10
bi_class_ag <- df.ag %>%
  mutate(dif.cat = case_when(-0.05>y.dif.average.mean.rcp.85e ~ 1,
                             y.dif.average.mean.rcp.85e>=-0.05 & y.dif.average.mean.rcp.85e<=0.05 ~ 2,
                             y.dif.average.mean.rcp.85e>0.05 ~ 3)) %>%
  mutate(uncert.cat = case_when(y.dif.average.uncertainty.rcp.85e<0.05 ~ 1,
                             y.dif.average.uncertainty.rcp.85e>=0.05 & y.dif.average.uncertainty.rcp.85e<=0.1 ~ 2,
                             y.dif.average.uncertainty.rcp.85e>0.1 ~ 3)) %>%
  mutate(bi_class = paste(dif.cat,uncert.cat,sep='-'))

bi_class_forest <- df.forest %>%
  mutate(dif.cat = case_when(-0.05>y.dif.average.mean.rcp.85e ~ 1,
                             y.dif.average.mean.rcp.85e>=-0.05 & y.dif.average.mean.rcp.85e<=0.05 ~ 2,
                             y.dif.average.mean.rcp.85e>0.05 ~ 3)) %>%
  mutate(uncert.cat = case_when(y.dif.average.uncertainty.rcp.85e<0.05 ~ 1,
                                y.dif.average.uncertainty.rcp.85e>=0.05 & y.dif.average.uncertainty.rcp.85e<=0.1 ~ 2,
                                y.dif.average.uncertainty.rcp.85e>0.1 ~ 3)) %>%
  mutate(bi_class = paste(dif.cat,uncert.cat,sep='-'))

bi_class_dev <- df.dev %>%
  mutate(dif.cat = case_when(-0.05>y.dif.average.mean.rcp.85e ~ 1,
                             y.dif.average.mean.rcp.85e>=-0.05 & y.dif.average.mean.rcp.85e<=0.05 ~ 2,
                             y.dif.average.mean.rcp.85e>0.05 ~ 3)) %>%
  mutate(uncert.cat = case_when(y.dif.average.uncertainty.rcp.85e<0.05 ~ 1,
                                y.dif.average.uncertainty.rcp.85e>=0.05 & y.dif.average.uncertainty.rcp.85e<=0.1 ~ 2,
                                y.dif.average.uncertainty.rcp.85e>0.1 ~ 3)) %>%
  mutate(bi_class = paste(dif.cat,uncert.cat,sep='-'))

bi_class_natopen <- df.natopen %>%
  mutate(dif.cat = case_when(-0.05>y.dif.average.mean.rcp.85e ~ 1,
                             y.dif.average.mean.rcp.85e>=-0.05 & y.dif.average.mean.rcp.85e<=0.05 ~ 2,
                             y.dif.average.mean.rcp.85e>0.05 ~ 3)) %>%
  mutate(uncert.cat = case_when(y.dif.average.uncertainty.rcp.85e<0.05 ~ 1,
                                y.dif.average.uncertainty.rcp.85e>=0.05 & y.dif.average.uncertainty.rcp.85e<=0.1 ~ 2,
                                y.dif.average.uncertainty.rcp.85e>0.1 ~ 3)) %>%
  mutate(bi_class = paste(dif.cat,uncert.cat,sep='-'))


map.ag <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = bi_class_ag,
             mapping = aes(x=lon, y=lat, colour = bi_class),
             size =1.5, alpha = 0.5) +
  bi_scale_color(pal = "DkViolet", dim = 3) +
  theme(legend.position = "none") +
  labs(x=NULL, y=NULL) 
map.ag

map.forest <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = bi_class_forest,
             mapping = aes(x=lon, y=lat, colour = bi_class),
             size =1.5, alpha = 0.5) +
  bi_scale_color(pal = "DkViolet", dim = 3) +
  theme(legend.position = "none") +
  labs(x=NULL, y=NULL) 
map.forest

map.natopen <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = bi_class_natopen,
             mapping = aes(x=lon, y=lat, colour = bi_class),
             size =1.5, alpha = 0.5) +
  bi_scale_color(pal = "DkViolet", dim = 3) +
  theme(legend.position = "none") +
  labs(x=NULL, y=NULL) 
map.natopen

map.dev <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = bi_class_dev,
             mapping = aes(x=lon, y=lat, colour = bi_class),
             size =1.5, alpha = 0.5) +
  bi_scale_color(pal = "DkViolet", dim = 3) +
  theme(legend.position = "none") +
  labs(x=NULL, y=NULL) 
map.dev


legend <- bi_legend(pal = "DkViolet",
                    dim = 3,
                    xlab = "Difference in nest success",
                    ylab = "Uncertainty",
                    size = 8)

# test: combine map with legend
#finalPlot <- ggdraw() +
#  draw_plot(map.ag, 0, 0, 1, 1) + #x,y,width, height
#  draw_plot(legend, 0.72, .07, 0.3, 0.3)
#finalPlot

map.biplots <- ggarrange(NULL, NULL,
                         map.ag, map.forest,
                         NULL, NULL,
                         map.dev,map.natopen,
                         nrow=4,ncol=2,
                         heights=c(.15,1,.15,1),
                         labels=c('Agriculture','Forest','','','Developed','Natural Open','',''))
map.biplots

map.biplots2 <- ggarrange(map.biplots,NULL,
                          nrow=1,ncol=2,
                          widths=c(1,.3))

map.biplots3 <- ggdraw() +
  draw_plot(map.biplots2, 0, 0, 1, 1) + #x,y,width, height
  draw_plot(legend, 0.7, .4, 0.3, 0.3)
map.biplots3

 



########## uncertainty maps #######

MyLimits <- c(0, .2)

all.uncertainty <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = df.all,
             mapping = aes(x=lon, y=lat, colour = y.dif.average.uncertainty.rcp.85e),
             size =1.5, alpha = 0.2) +
  scale_color_gradient(name = "",low="seagreen1",high = "deeppink", limits = MyLimits, oob = squish) +
  theme(legend.position = "none") +
  labs(x=NULL, y=NULL) 
all.uncertainty




ag.uncertainty <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = df.ag,
             mapping = aes(x=lon, y=lat, colour = y.dif.average.uncertainty.rcp.85e),
             size =1.5, alpha = 0.25) +
  scale_color_gradient(name = "Uncertainty",low="seagreen1",high = "deeppink", limits = MyLimits, oob = squish) +
  theme(legend.position = "none") +
  labs(x=NULL, y=NULL) 
ag.uncertainty

forest.uncertainty <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = df.forest,
             mapping = aes(x=lon, y=lat, colour = y.dif.average.uncertainty.rcp.85e),
             size =1.5, alpha = 0.25) +
  scale_color_gradient(name = "Uncertainty",low="seagreen1",high = "deeppink", limits = MyLimits, oob = squish) +
  theme(legend.position = "none") +
  labs(x=NULL, y=NULL) 
forest.uncertainty

dev.uncertainty <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = df.dev,
             mapping = aes(x=lon, y=lat, colour = y.dif.average.uncertainty.rcp.85e),
             size =1.5, alpha = 0.25) +
  scale_color_gradient(name = "Uncertainty",low="seagreen1",high = "deeppink", limits = MyLimits, oob = squish) +
  theme(legend.position = "none") +
  labs(x=NULL, y=NULL)
dev.uncertainty

natopen.uncertainty <- ggplot() +
  #US outline
  geom_polygon(data = map.usa,
               aes(x=long, y = lat, group = group),
               fill = NA, color = "black") +
  my.theme.map +
  geom_point(data = df.natopen,
             mapping = aes(x=lon, y=lat, colour = y.dif.average.uncertainty.rcp.85e),
             size =1.5, alpha = 0.25) +
  scale_color_gradient(name = "Uncertainty",low="seagreen1",high = "deeppink", limits = MyLimits, oob = squish) +
  #theme(legend.position = "none") +
  labs(x=NULL, y=NULL) 
natopen.uncertainty

map.uncertainty <- ggarrange(NULL, NULL,
                         ag.uncertainty, forest.uncertainty,
                         NULL, NULL,
                         dev.uncertainty, natopen.uncertainty,
                         nrow=4,ncol=2,
                         heights=c(.15,1,.15,1),
                         labels=c('Agriculture','Forest','','','Developed','Natural Open','',''),
                         common.legend = TRUE, legend="right")
map.uncertainty

