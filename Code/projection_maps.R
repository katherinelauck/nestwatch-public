rm(list=ls())

library(tidyverse)
library(lme4)
library(boot)
library(sjPlot)
library(cowplot)
library(ggpubr)
library(reshape)
library("scales")
library("ggbeeswarm")
library(raster)
library(ggridges)

#loading data
data <- read.csv("Data/active/success_projections_computed.csv")

###Subsetting by Land Use
projections.ag<- subset(data, NewLU1 =="Ag")
projections.forest<- subset(data, NewLU1 =="Forest")
projections.natural.open<- subset(data, NewLU1 =="Natural_open")
projections.developed<- subset(data, NewLU1 =="Human")

###Getting average values for tables
##Means for projections
mean.rcp.85e.projections <- c(mean(projections.forest$y.dif.average.mean.rcp.85e),mean(projections.ag$y.dif.average.mean.rcp.85e),mean(projections.natural.open$y.dif.average.mean.rcp.85e),mean(projections.developed$y.dif.average.mean.rcp.85e))
mean.rcp.45e.projections <- c(mean(projections.forest$y.dif.average.mean.rcp.45e),mean(projections.ag$y.dif.average.mean.rcp.45e),mean(projections.natural.open$y.dif.average.mean.rcp.45e),mean(projections.developed$y.dif.average.mean.rcp.45e))

mean.rcp.85m.projections <- c(mean(projections.forest$y.dif.average.mean.rcp.85m),mean(projections.ag$y.dif.average.mean.rcp.85m),mean(projections.natural.open$y.dif.average.mean.rcp.85m),mean(projections.developed$y.dif.average.mean.rcp.85m))
mean.rcp.45m.projections <- c(mean(projections.forest$y.dif.average.mean.rcp.45m),mean(projections.ag$y.dif.average.mean.rcp.45m),mean(projections.natural.open$y.dif.average.mean.rcp.45m),mean(projections.developed$y.dif.average.mean.rcp.45m))

land.use <- c("Forest","Agriculture","Natural open","Developed")

mean.projections <- data.frame(land.use,mean.rcp.85e.projections,mean.rcp.45e.projections,mean.rcp.85m.projections,mean.rcp.45m.projections)
mean.projections

##SD for projections
sd.rcp.85e.projections <- c(sd(projections.forest$y.dif.average.mean.rcp.85e),sd(projections.ag$y.dif.average.mean.rcp.85e),sd(projections.natural.open$y.dif.average.mean.rcp.85e),sd(projections.developed$y.dif.average.mean.rcp.85e))
sd.rcp.45e.projections <- c(sd(projections.forest$y.dif.average.mean.rcp.45e),sd(projections.ag$y.dif.average.mean.rcp.45e),sd(projections.natural.open$y.dif.average.mean.rcp.45e),sd(projections.developed$y.dif.average.mean.rcp.45e))

sd.rcp.85m.projections <- c(sd(projections.forest$y.dif.average.mean.rcp.85m),sd(projections.ag$y.dif.average.mean.rcp.85m),sd(projections.natural.open$y.dif.average.mean.rcp.85m),sd(projections.developed$y.dif.average.mean.rcp.85m))
sd.rcp.45m.projections <- c(sd(projections.forest$y.dif.average.mean.rcp.45m),sd(projections.ag$y.dif.average.mean.rcp.45m),sd(projections.natural.open$y.dif.average.mean.rcp.45m),sd(projections.developed$y.dif.average.mean.rcp.45m))

sd.projections <- data.frame(land.use,sd.rcp.85e.projections,sd.rcp.45e.projections,sd.rcp.85m.projections,sd.rcp.45m.projections)
sd.projections

####Uncertainty
##Means for uncertainty
mean.rcp.85e.uncertainty <- c(mean(projections.forest$rcp.85e.uncertainty),mean(projections.ag$rcp.85e.uncertainty),mean(projections.natural.open$rcp.85e.uncertainty),mean(projections.developed$rcp.85e.uncertainty))
mean.rcp.45e.uncertainty <- c(mean(projections.forest$rcp.45e.uncertainty),mean(projections.ag$rcp.45e.uncertainty),mean(projections.natural.open$rcp.45e.uncertainty),mean(projections.developed$rcp.45e.uncertainty))

mean.rcp.85m.uncertainty <- c(mean(projections.forest$rcp.85m.uncertainty),mean(projections.ag$rcp.85m.uncertainty),mean(projections.natural.open$rcp.85m.uncertainty),mean(projections.developed$rcp.85m.uncertainty))
mean.rcp.45m.uncertainty <- c(mean(projections.forest$rcp.45m.uncertainty),mean(projections.ag$rcp.45m.uncertainty),mean(projections.natural.open$rcp.45m.uncertainty),mean(projections.developed$rcp.45m.uncertainty))

mean.uncertainty <- data.frame(land.use,mean.rcp.85e.uncertainty,mean.rcp.45e.uncertainty,mean.rcp.85m.uncertainty,mean.rcp.45m.uncertainty)
mean.uncertainty

##SD for uncertainty
sd.rcp.85e.uncertainty <- c(sd(projections.forest$rcp.85e.uncertainty),sd(projections.ag$rcp.85e.uncertainty),sd(projections.natural.open$rcp.85e.uncertainty),sd(projections.developed$rcp.85e.uncertainty))
sd.rcp.45e.uncertainty <- c(sd(projections.forest$rcp.45e.uncertainty),sd(projections.ag$rcp.45e.uncertainty),sd(projections.natural.open$rcp.45e.uncertainty),sd(projections.developed$rcp.45e.uncertainty))

sd.rcp.85m.uncertainty <- c(sd(projections.forest$rcp.85m.uncertainty),sd(projections.ag$rcp.85m.uncertainty),sd(projections.natural.open$rcp.85m.uncertainty),sd(projections.developed$rcp.85m.uncertainty))
sd.rcp.45m.uncertainty <- c(sd(projections.forest$rcp.45m.uncertainty),sd(projections.ag$rcp.45m.uncertainty),sd(projections.natural.open$rcp.45m.uncertainty),sd(projections.developed$rcp.45m.uncertainty))

sd.uncertainty <- data.frame(land.use,sd.rcp.85e.uncertainty,sd.rcp.45e.uncertainty,sd.rcp.85m.uncertainty,sd.rcp.45m.uncertainty)
sd.uncertainty

##Set the plot theme
my.theme = theme(
  plot.title= element_text(size = 24, vjust=-4.5, hjust = 0.06),
  axis.title.x = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.line = element_line(size = 0.5, colour = "black"),
  panel.background = element_rect(fill = "white"),
  plot.margin = margin(0,0,0,0), 
  legend.key.width=unit(2,"cm"),
  legend.text=element_text(size=16))

my.theme.2 = theme(
  plot.title= element_text(size = 24, vjust=-4.5, hjust = 0.06),
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.line = element_line(size = 0.5, colour = "black"),
  panel.background = element_rect(fill = "white"),
  plot.margin = margin(0,0,0,0), 
  legend.key.width=unit(2,"cm"),
  legend.text=element_text(size=12))




##Plots difference in success_projections
###I first transform proportion to percentage
#Estimated change in fledge success \n between now and the end of the century XXI (%)
y.ag <- density(projections.ag$y.dif.average.mean.rcp.85e)
density.gradient.ag <- ggplot(data.frame(x = y.ag$x, y = y.ag$y), aes(x, y)) + geom_line() + my.theme.2 +
  geom_segment(aes(xend = x, yend = 0, colour = x*100)) + 
  geom_segment(aes(x=0, xend = 0, y=0, yend = 45), color="black", linetype="dashed", size=0.3) +
  ggtitle("Agriculture")+ 
  ylab("Density")+ xlab("") +
  scale_color_gradient2(name = "",mid="grey69",low = "firebrick2",high = "royalblue2",labels = scales::percent_format(accuracy = 1,scale=1,c(expression("<","","","",">"))),limits = c(-5,5), oob = squish) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale=1),limits=c(0,50))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits=c(-0.15,0.15))+
  theme(legend.position = "none") + labs(x=NULL, y=NULL) 
density.gradient.ag 

y.forest <- density(projections.forest$y.dif.average.mean.rcp.85e)
density.gradient.forest <- ggplot(data.frame(x = y.forest$x, y = y.forest$y), aes(x, y)) + geom_line() + my.theme.2 +
  geom_segment(aes(xend = x, yend = 0, colour = x*100)) + 
  geom_segment(aes(x=0, xend = 0, y=0, yend = 45), color="black", linetype="dashed", size=0.3) +
  ggtitle("Forest")+ 
  ylab("Density")+ xlab("") +
  scale_color_gradient2(name = "",mid="grey69",low = "firebrick2",high = "royalblue2",labels = scales::percent_format(accuracy = 1,scale=1,c(expression("<","","","",">"))),limits = c(-5,5), oob = squish) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale=1),limits=c(0,50))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits=c(-0.15,0.15))+
  theme(legend.position = "none") + labs(x=NULL, y=NULL) 
density.gradient.forest 

y.natural.open <- density(projections.natural.open$y.dif.average.mean.rcp.85e)
density.gradient.natural.open <- ggplot(data.frame(x = y.natural.open$x, y = y.natural.open$y), aes(x, y)) + geom_line() + my.theme.2 +
  geom_segment(aes(xend = x, yend = 0, colour = x*100)) + 
  geom_segment(aes(x=0, xend = 0, y=0, yend = 45), color="black", linetype="dashed", size=0.3) +
  ggtitle("Natural open")+ ylab("Density")+ xlab("") +
  scale_color_gradient2(name = "",mid="grey69",low = "firebrick2",high = "royalblue2",labels = scales::percent_format(accuracy = 1,scale=1,c(expression("<","","","",">"))),limits = c(-5,5), oob = squish) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale=1),limits=c(0,50))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits=c(-0.15,0.15))+
  theme(legend.position = "none") + labs(x=NULL, y=NULL) 
density.gradient.natural.open 

y.developed <- density(projections.developed$y.dif.average.mean.rcp.85e)
density.gradient.developed <- ggplot(data.frame(x = y.developed$x, y = y.developed$y), aes(x, y)) + geom_line() + my.theme +
  geom_segment(aes(xend = x, yend = 0, colour = x*100)) + 
  geom_segment(aes(x=0, xend = 0, y=0, yend = 45), color="black", linetype="dashed", size=0.3) +
  ggtitle("Developed")+ ylab("Density")+ xlab("") +
  scale_color_gradient2(name = "",mid="grey69",low = "firebrick2",high = "royalblue2",labels = scales::percent_format(accuracy = 1,scale=1,c(expression("<","","","",">"))),limits = c(-5,5), oob = squish) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale=1),limits=c(0,50))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits=c(-0.15,0.15))+
  theme(legend.position = "none") + labs(x=NULL, y=NULL) 
density.gradient.developed


##Not gradient fill----
density.y.dif.mean.ag.rcp85e <- ggplot(projections.ag, aes(x=y.dif.average.mean.rcp.85e))+ my.theme +
  geom_density(fill ="coral2", alpha=0.5) +
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Agriculture")+ ylab("Density")+ xlab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale=1),limits=c(0,50))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits=c(-0.15,0.15))+
  
  scale_fill_gradient2(name = "Future - Now",mid="grey48",low = "firebrick2",high = "royalblue2",limits = c(-0.05,0.05), oob = squish) 

density.y.dif.mean.ag.rcp85e



density.y.dif.mean.forest.rcp85e <- ggplot(projections.forest, aes(x=y.dif.average.mean.rcp.85e))+ my.theme +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Forest")+ ylab("Density")+ xlab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale=1),limits=c(0,50))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits=c(-0.15,0.15))

density.y.dif.mean.forest.rcp85e

density.y.dif.mean.natural.open.rcp85e <- ggplot(projections.natural.open, aes(x=y.dif.average.mean.rcp.85e))+ my.theme + 
  geom_density(fill ="mediumpurple3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("Natural open")+ ylab("Density")+ xlab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale=1),limits=c(0,50))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits=c(-0.15,0.15))

density.y.dif.mean.natural.open.rcp85e

density.y.dif.mean.developed.rcp85e <- ggplot(projections.developed, aes(x=y.dif.average.mean.rcp.85e))+ my.theme + 
  geom_density(fill ="turquoise3", alpha=0.5) + 
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=1)+
  ggtitle("developed")+ ylab("Density")+ xlab("") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,scale=1),limits=c(0,50))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),limits=c(-0.15,0.15))

density.y.dif.mean.developed.rcp85e

density.by.landuse.average.mean.rcp85e <- ggarrange(density.y.dif.mean.ag.rcp85e,density.y.dif.mean.forest.rcp85e,
                                                            density.y.dif.mean.natural.open.rcp85e,density.y.dif.mean.developed.rcp85e,nrow=2,ncol=2)

density.by.landuse.average.mean.rcp85e




####Map----
margin.zero <- margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
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
  legend.text=element_text(size=12))

##Map USA
map.usa <- map_data("usa")
map.states <- map_data("state")

###Aggregating data into a single location
locations <- data.frame(UnCoor=data$UnCoor,lon=data$lon,lat=data$lat)
locations <- (unique(locations))
locations.qe <- data.frame(lon = data$lon, lat = data$lat, lu = data$NewLU1) %>% unique()


agg.y.dif.average.mean.rcp.85e.ag <- aggregate(projections.ag$y.dif.average.mean.rcp.85e, by=list(projections.ag$UnCoor), mean)
names(agg.y.dif.average.mean.rcp.85e.ag) <- c("UnCoor","y.dif.average.mean.rcp.85e")

average.per.location.y.dif.rcp.85e.ag <- merge(locations,agg.y.dif.average.mean.rcp.85e.ag,by="UnCoor")

agg.y.dif.average.mean.rcp.85e.forest <- aggregate(projections.forest$y.dif.average.mean.rcp.85e, by=list(projections.forest$UnCoor), mean)
names(agg.y.dif.average.mean.rcp.85e.forest) <- c("UnCoor","y.dif.average.mean.rcp.85e")

average.per.location.y.dif.rcp.85e.forest <- merge(locations,agg.y.dif.average.mean.rcp.85e.forest,by="UnCoor")

agg.y.dif.average.mean.rcp.85e.natural.open <- aggregate(projections.natural.open$y.dif.average.mean.rcp.85e, by=list(projections.natural.open$UnCoor), mean)
names(agg.y.dif.average.mean.rcp.85e.natural.open) <- c("UnCoor","y.dif.average.mean.rcp.85e")

average.per.location.y.dif.rcp.85e.natural.open <- merge(locations,agg.y.dif.average.mean.rcp.85e.natural.open,by="UnCoor")

agg.y.dif.average.mean.rcp.85e.developed <- aggregate(projections.developed$y.dif.average.mean.rcp.85e, by=list(projections.developed$UnCoor), mean)
names(agg.y.dif.average.mean.rcp.85e.developed) <- c("UnCoor","y.dif.average.mean.rcp.85e")

average.per.location.y.dif.rcp.85e.developed <- merge(locations,agg.y.dif.average.mean.rcp.85e.developed,by="UnCoor")

###POINT MAP AVERAGE PER LOCATION
map.ag <- ggplot() + geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  + my.theme.map +
  geom_point(data = average.per.location.y.dif.rcp.85e.ag, mapping = aes(x=lon, y=lat, colour = y.dif.average.mean.rcp.85e), size =1.5, alpha = 0.5) +
  scale_color_gradient2(name = "",mid="grey69",low = "firebrick2",high = "royalblue2",limits = c(-0.05,0.05), oob = squish) +
  theme(legend.position = "none") + labs(x=NULL, y=NULL) +
  ggtitle("")
  
map.ag

map.forest <- ggplot() + geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  + my.theme.map +
  geom_point(data = average.per.location.y.dif.rcp.85e.forest, mapping = aes(x=lon, y=lat, colour = y.dif.average.mean.rcp.85e),size =1.5, alpha =0.5) +
  scale_color_gradient2(name = "",mid="grey69",low = "firebrick2",high = "royalblue2",limits = c(-0.05,0.05), oob = squish) +
  theme(legend.position = "none")+ labs(x=NULL, y=NULL) +
  ggtitle("")

map.forest

map.natural.open <- ggplot() + geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  + my.theme.map +
  geom_point(data = average.per.location.y.dif.rcp.85e.natural.open, mapping = aes(x=lon, y=lat, colour = y.dif.average.mean.rcp.85e),size =1.5, alpha =0.5) +
  scale_color_gradient2(name = "",mid="grey69",low = "firebrick2",high = "royalblue2",limits = c(-0.05,0.05), oob = squish) +
  theme(legend.position = "none")+ labs(x=NULL, y=NULL)+
  ggtitle("")

map.natural.open

map.developed <- ggplot() + geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  + my.theme.map +
  geom_point(data = average.per.location.y.dif.rcp.85e.developed, mapping = aes(x=lon, y=lat, colour = y.dif.average.mean.rcp.85e),size =1.5, alpha =0.5) +
  scale_color_gradient2(name = "",mid="grey69",low = "firebrick2",high = "royalblue2",limits = c(-0.05,0.05), oob = squish) +
  theme(legend.position = "none")+ labs(x=NULL, y=NULL)+
  ggtitle("")

map.developed

###Final plot with averaged points and without density gradients----
#map.projection.average.points <- ggarrange(density.y.dif.mean.ag.rcp85e,map.ag,
#                                           density.y.dif.mean.forest.rcp85e, map.forest,
#                                           density.y.dif.mean.natural.open.rcp85e,map.natural.open,
#                                           density.y.dif.mean.developed.rcp85e,map.developed,
#                                           nrow=4,ncol=2,
#                                           widths=c(0.5,1))
#map.projection.average.points

#ggsave(map.projection.average.points,device="png",filename = "/Users/DrBohemio/Dropbox/NestWatch/tables and figures/map.projection.average.points.map.10percentpng",width = 250, height = 320, units = c( "mm"))



###Final plot  with averaged points and with density gradients----

map.projection.average.points.density.gradient <- ggarrange(density.gradient.forest, map.forest,
                                                            density.gradient.ag,map.ag,
                                                            density.gradient.natural.open,map.natural.open,
                                                            density.gradient.developed,map.developed,
                                                            nrow=4,ncol=2,
                                                            widths=c(0.5,1),heights=c(1,1,1,1),common.legend = TRUE, legend="bottom")
map.projection.average.points.density.gradient

map.projection.average.points.density.gradient <- annotate_figure(map.projection.average.points.density.gradient,
                bottom = text_grob("Projected change in nest success (current versus 2100; RCP 8.5)", color = "black", size = 20, just = c("centre","bottom")),
                left = text_grob("Density", color = "black", rot = 90,size=24))

map.projection.average.points.density.gradient 

ggsave(map.projection.average.points.density.gradient,device="png",filename = "figures/map.projection.average.points.density.gradient.5percent.rcp85e.png",
       width = 250, height = 350, units = c( "mm"))







###Other arrangement
map.projection.average.points.density.gradient.horizontal <- ggarrange(density.gradient.ag,map.ag,density.gradient.forest, map.forest,
                                                            
                                                            density.gradient.natural.open,map.natural.open,density.gradient.developed,map.developed,
                                                            
                                                            nrow=2,ncol=4,
                                                            widths=c(0.5,1,0.5,1),common.legend = TRUE, legend="bottom")
map.projection.average.points.density.gradient.horizontal

map.projection.average.points.density.gradient.horizontal <- annotate_figure(map.projection.average.points.density.gradient.horizontal,
                                                                  bottom = text_grob("Projected change in nest success (current versus 2100; RCP 8.5)", color = "black", size = 16),
                                                                  left = text_grob("Density", color = "black", rot = 90,size=15))

#Projected change in nest success (current versus 2100; RCP 8.5)

map.projection.average.points.density.gradient.horizontal 

###QE arrangement
qe.map.projection.average.points.density.gradient.horizontal <- ggarrange(density.gradient.ag,map.ag,density.gradient.forest, map.forest,
                                                                       
                                                                       nrow=1,ncol=4,
                                                                       widths=c(0.5,1,0.5,1),common.legend = TRUE, legend="bottom")
qe.map.projection.average.points.density.gradient.horizontal

qe.map.projection.average.points.density.gradient.horizontal <- annotate_figure(qe.map.projection.average.points.density.gradient.horizontal,
                                                                             bottom = text_grob("Projected change in nest success (current versus 2100; RCP 8.5)", color = "black", size = 12),
                                                                             left = text_grob("Density", color = "black", rot = 90,size=12))

#Projected change in nest success (current versus 2100; RCP 8.5)

qe.map.projection.average.points.density.gradient.horizontal

ggsave("figures/map_qe.png",qe.map.projection.average.points.density.gradient.horizontal,width = 9, height = 3, units = c( "in"))


###RASTER MAP----
proj2 <-  CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
dummy <- raster(ncols=122, nrows=50, xmn=-126, xmx=-65, ymn=25, ymx=50, crs=proj2) #0.25 degree dummy raster


######Values of differences for rcp85 end of the century
####Agriculture
v1.ag <- rasterize(projections.ag[,c(224,225)], dummy, projections.ag[,415], fun=mean, background=NA, mask=FALSE, na.rm=T)

v2.ag <- as.data.frame(v1.ag, xy=TRUE)

map.raster.ag <-  ggplot() + geom_raster(data = v2.ag, aes(x=x, y=y, fill=layer))+ my.theme.map +
  geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  +
  scale_fill_gradient2(name = "Future - Now",mid="grey69",low = "firebrick2",high = "royalblue2",limits = c(-0.10,0.10), oob = squish, na.value = 'white') + 
  theme(legend.position = "none")

map.raster.ag

####Forest
v1.forest <- rasterize(projections.forest[,c(224,225)], dummy, projections.forest[,415], fun=mean, background=NA, mask=FALSE, na.rm=T)

v2.forest <- as.data.frame(v1.forest, xy=TRUE)

map.raster.forest <-  ggplot() + geom_raster(data = v2.forest, aes(x=x, y=y, fill=layer))+ my.theme.map +
  geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  +
  scale_fill_gradient2(name = "Future - Now",mid="grey69",low = "firebrick2",high = "royalblue2",limits = c(-0.10,0.10), oob = squish, na.value = 'white') + 
  theme(legend.position = "none")

map.raster.forest

####natural.open
v1.natural.open <- rasterize(projections.natural.open[,c(224,225)], dummy, projections.natural.open[,415], fun=mean, background=NA, mask=FALSE, na.rm=T)

v2.natural.open <- as.data.frame(v1.natural.open, xy=TRUE)

map.raster.natural.open <-  ggplot() + geom_raster(data = v2.natural.open, aes(x=x, y=y, fill=layer))+ my.theme.map +
  geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  +
  scale_fill_gradient2(name = "Future - Now",mid="grey69",low = "firebrick2",high = "royalblue2",limits = c(-0.10,0.10), oob = squish, na.value = 'white') + 
  theme(legend.position = "none")

map.raster.natural.open

####developed
v1.developed <- rasterize(projections.developed[,c(224,225)], dummy, projections.developed[,415], fun=mean, background=NA, mask=FALSE, na.rm=T)

v2.developed <- as.data.frame(v1.developed, xy=TRUE)

map.raster.developed <-  ggplot() + geom_raster(data = v2.developed, aes(x=x, y=y, fill=layer))+ my.theme.map +
  geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  +
  scale_fill_gradient2(name = "Future - Now",mid="grey69",low = "firebrick2",high = "royalblue2",limits = c(-0.10,0.10), oob = squish, na.value = 'white') + 
  theme(legend.position = "none")

map.raster.developed

###Final plot with density gradients and raster map

map.projection.average.points.density.gradient.map.raster <- ggarrange(density.gradient.ag,map.raster.ag,
                                                                       density.gradient.forest, map.raster.forest,
                                                                       density.gradient.natural.open,map.raster.natural.open,
                                                                       density.gradient.developed,map.raster.developed,
                                                                       nrow=4,ncol=2,
                                                                       widths=c(0.5,1),common.legend = TRUE, legend="bottom")
map.projection.average.points.density.gradient.map.raster

map.projection.average.points.density.gradient.map.raster <- annotate_figure(map.projection.average.points.density.gradient.map.raster,
                                                                  bottom = text_grob("Estimated change in fledge success comparing now with the end of the century XXI", color = "black", size = 16),
                                                                  left = text_grob("Density", color = "black", rot = 90,size=15))

map.projection.average.points.density.gradient.map.raster

#ggsave(map.projection.average.points.density.gradient.map.raster,device="png",filename = "figures/map.projection.average.points.density.gradient.map.raster.2percent.png",
 #      width = 250, height = 350, units = c( "mm"))

###Other arrangement
map.projection.average.points.density.gradient.map.raster.horizontal <- ggarrange(density.gradient.ag,map.raster.ag,density.gradient.forest, map.raster.forest,
                                                                       
                                                                       density.gradient.natural.open,map.raster.natural.open,density.gradient.developed,map.raster.developed,
                                                                       
                                                                       nrow=2,ncol=4,
                                                                       widths=c(0.5,1,0.5,1),common.legend = TRUE, legend="bottom")
map.projection.average.points.density.gradient.map.raster.horizontal

map.projection.average.points.density.gradient.map.raster.horizontal <- annotate_figure(map.projection.average.points.density.gradient.map.raster.horizontal,
                                                                             bottom = text_grob("Estimated change in fledge success comparing now with the end of the century XXI", color = "black", size = 16),
                                                                             left = text_grob("Density", color = "black", rot = 90,size=15))

map.projection.average.points.density.gradient.map.raster.horizontal 

#ggsave(map.projection.average.points.density.gradient.map.raster.horizontal,device="png",filename = "/Users/DrBohemio/Dropbox/NestWatch/tables and figures/map.projection.average.points.density.gradient.horizontal.map.raster.10percent.png",
#       width = 420, height = 200, units = c( "mm"))


########UNCERTAINTY-----
######Values of uncertainty for rcp85 end of the century
##Plots difference in success_projections
density.y.dif.mean.ag.rcp85e <- ggplot(projections.ag, aes(x=rcp.85e.uncertainty))+ my.theme + ylim(min=0,max=50)+xlim(min=-0,max=0.5) +
  geom_density(fill ="coral2", alpha=0.5) + 
  ggtitle("Agriculture")+ ylab("Density")+ xlab("Uncertainty")

density.y.dif.mean.ag.rcp85e

density.y.dif.mean.forest.rcp85e <- ggplot(projections.forest, aes(x=rcp.85e.uncertainty))+ my.theme + ylim(min=0,max=50)+xlim(min=0,max=0.5) +
  geom_density(fill ="springgreen4", alpha=0.5) + 
  
  ggtitle("Forest")+ ylab("Density")+ xlab("Uncertainty")

density.y.dif.mean.forest.rcp85e

density.y.dif.mean.natural.open.rcp85e <- ggplot(projections.natural.open, aes(x=rcp.85e.uncertainty))+ my.theme + ylim(min=0,max=50)+xlim(min=0,max=0.5) +
  geom_density(fill ="mediumpurple3", alpha=0.5) +
  ggtitle("Natural open")+ ylab("Density")+ xlab("Uncertainty")

density.y.dif.mean.natural.open.rcp85e

density.y.dif.mean.developed.rcp85e <- ggplot(projections.developed, aes(x=rcp.85e.uncertainty ))+ my.theme + ylim(min=0,max=50)+ xlim(min=0,max=0.5) +
  geom_density(fill ="turquoise3", alpha=0.5) + 
 ggtitle("Developed")+ ylab("Density")+ xlab("Uncertainty")

density.y.dif.mean.developed.rcp85e


###Aggregating data into a single location for uncertainty
locations <- data.frame(UnCoor=data$UnCoor,lon=data$lon,lat=data$lat)
locations <- (unique(locations))


agg.rcp.85e.uncertainty.ag <- aggregate(projections.ag$rcp.85e.uncertainty, by=list(projections.ag$UnCoor), mean)
names(agg.rcp.85e.uncertainty.ag) <- c("UnCoor","rcp.85e.uncertainty")

average.per.location.rcp.85e.uncertainty.ag <- merge(locations,agg.rcp.85e.uncertainty.ag,by="UnCoor")

agg.rcp.85e.uncertainty.forest <- aggregate(projections.forest$rcp.85e.uncertainty, by=list(projections.forest$UnCoor), mean)
names(agg.rcp.85e.uncertainty.forest) <- c("UnCoor","rcp.85e.uncertainty")

average.per.location.rcp.85e.uncertainty.forest <- merge(locations,agg.rcp.85e.uncertainty.forest,by="UnCoor")

agg.rcp.85e.uncertainty.natural.open <- aggregate(projections.natural.open$rcp.85e.uncertainty, by=list(projections.natural.open$UnCoor), mean)
names(agg.rcp.85e.uncertainty.natural.open) <- c("UnCoor","rcp.85e.uncertainty")

average.per.location.rcp.85e.uncertainty.natural.open <- merge(locations,agg.rcp.85e.uncertainty.natural.open,by="UnCoor")

agg.rcp.85e.uncertainty.developed <- aggregate(projections.developed$rcp.85e.uncertainty, by=list(projections.developed$UnCoor), mean)
names(agg.rcp.85e.uncertainty.developed) <- c("UnCoor","rcp.85e.uncertainty")

average.per.location.rcp.85e.uncertainty.developed <- merge(locations,agg.rcp.85e.uncertainty.developed,by="UnCoor")

###POINT MAP AVERAGE PER LOCATION
map.forest.uncertainty <- ggplot() + geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  + my.theme.map +
  geom_point(data = average.per.location.rcp.85e.uncertainty.forest, mapping = aes(x=lon, y=lat, colour = rcp.85e.uncertainty*100),size =1.5, alpha =0.5) +
  scale_color_gradient(name = "",low ="forestgreen" ,high = "hotpink2",labels = scales::percent_format(accuracy = 1,scale=1,c(expression("","","","","",">"))),breaks = seq(from = 0, to = 20, by = 4),limits = c(0,20),oob = squish) +
  theme(legend.position = "") + ggtitle("Forest")+ labs(x=NULL, y=NULL) 
map.forest.uncertainty

map.ag.uncertainty <- ggplot() + geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  + my.theme.map +
  geom_point(data = average.per.location.rcp.85e.uncertainty.ag, mapping = aes(x=lon, y=lat, colour = rcp.85e.uncertainty*100), size =1.5, alpha = 0.5) +
  scale_color_gradient(name = "",low ="forestgreen" ,high = "hotpink2",labels = scales::percent_format(accuracy = 1,scale=1,c(expression("","","","","",">"))), breaks = seq(from = 0, to = 20, by = 4),limits = c(0,20), oob = squish) +
  theme(legend.position = "") + ggtitle("Agriculture") + labs(x=NULL, y=NULL) 

map.ag.uncertainty

map.natural.open.uncertainty <- ggplot() + geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  + my.theme.map +
  geom_point(data = average.per.location.rcp.85e.uncertainty.natural.open, mapping = aes(x=lon, y=lat, colour = rcp.85e.uncertainty*100),size =1.5, alpha =0.5) +
scale_color_gradient(name = "",low ="forestgreen" ,high = "hotpink2",labels = scales::percent_format(accuracy = 1,scale=1,c(expression("","","","","",">"))),breaks = seq(from = 0, to = 20, by = 4),limits = c(0,20), oob = squish) +
  theme(legend.position = "") + ggtitle("Natural open") + labs(x=NULL, y=NULL)
map.natural.open.uncertainty

map.developed.uncertainty <- ggplot() + geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  + my.theme.map +
  geom_point(data = average.per.location.rcp.85e.uncertainty.developed , mapping = aes(x=lon, y=lat, colour = rcp.85e.uncertainty*100),size =1.5, alpha =0.5) +
scale_color_gradient(name = "",low ="forestgreen" ,high = "hotpink2",labels = scales::percent_format(accuracy = 1,scale=1,c(expression("","","","","",">"))),breaks = seq(from = 0, to = 20, by = 4),limits = c(0,20), oob = squish) +
  theme(legend.position = "") + ggtitle("Developed") + labs(x=NULL, y=NULL)
map.developed.uncertainty




###Final plot  with averaged points and with density gradients----

map.uncertainty.average.points<- ggarrange(map.forest.uncertainty,
                                           map.ag.uncertainty,
                                           map.natural.open.uncertainty,
                                           map.developed.uncertainty,
                                           nrow=4,ncol=1,
                                           common.legend = TRUE, legend="bottom",
                                           widths=c(0.5,1))

map.uncertainty.average.points

map.uncertainty.average.points <- annotate_figure(map.uncertainty.average.points,
                                                                  bottom = text_grob("Climatic uncertainty (current versus 2100; RCP 8.5)", 
                                                                                     color = "black", size = 16))

map.uncertainty.average.points

ggsave(map.uncertainty.average.points,device="png",filename = "figures/map.uncertainty.average.points.20percent.rcp85e.png",
       width = 150, height = 350, units = c( "mm"))









####Agriculture-----
v1.unc.ag <- rasterize(projections.ag[,c(224,225)], dummy, projections.ag[,427], fun=mean, background=NA, mask=FALSE, na.rm=T)

v2.unc.ag <- as.data.frame(v1.unc.ag, xy=TRUE)

map.raster.unc.ag <-  ggplot() + geom_raster(data = v2.unc.ag, aes(x=x, y=y, fill=layer))+ my.theme.map +
  geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  +
  scale_fill_gradient(name = "Future - Now",low = "firebrick2",high = "royalblue2",limits = c(0,0.2), oob = squish, na.value = 'white')+ 
  theme(legend.position = "none") 

map.raster.unc.ag

####Forest
v1.unc.forest <- rasterize(projections.forest[,c(224,225)], dummy, projections.forest[,427], fun=mean, background=NA, mask=FALSE, na.rm=T)

v2.unc.forest <- as.data.frame(v1.unc.forest, xy=TRUE)

map.raster.unc.forest <-  ggplot() + geom_raster(data = v2.unc.forest, aes(x=x, y=y, fill=layer))+ my.theme.map +
  geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  +
  scale_fill_gradient(name = "Future - Now",low = "firebrick2",high = "royalblue2",limits = c(0,0.1), oob = squish, na.value = 'white') + 
  theme(legend.position = "none")

map.raster.unc.forest


####natural.open
v1.unc.natural.open <- rasterize(projections.natural.open[,c(224,225)], dummy, projections.natural.open[,427], fun=mean, background=NA, mask=FALSE, na.rm=T)

v2.unc.natural.open <- as.data.frame(v1.unc.natural.open, xy=TRUE)

map.raster.unc.natural.open <-  ggplot() + geom_raster(data = v2.unc.natural.open, aes(x=x, y=y, fill=layer))+ my.theme.map +
  geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  +
  scale_fill_gradient(name = "Future - Now",low = "firebrick2",high = "royalblue2",limits = c(0,0.1), oob = squish, na.value = 'white') + 
  theme(legend.position = "none")

map.raster.unc.natural.open


####developed
v1.unc.developed <- rasterize(projections.developed[,c(224,225)], dummy, projections.developed[,427], fun=mean, background=NA, mask=FALSE, na.rm=T)

v2.unc.developed <- as.data.frame(v1.unc.developed, xy=TRUE)

map.raster.unc.developed <-  ggplot() + geom_raster(data = v2.unc.developed, aes(x=x, y=y, fill=layer))+ my.theme.map +
  geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  +
  scale_fill_gradient(name = "Future - Now",low = "firebrick2",high = "royalblue2",limits = c(0,0.1), oob = squish, na.value = 'white') + 
  theme(legend.position = "none")

map.raster.unc.developed

map.raster.unc.projection <- ggarrange(map.raster.unc.ag,map.raster.unc.forest,map.raster.unc.natural.open,map.raster.unc.developed,nrow=2,ncol=2)
map.raster.unc.projection 

## Overview map of locations & land covers

map.qe <- ggplot() + geom_polygon(data = map.usa, aes(x=long, y = lat, group = group), fill = NA, color = "black")  + 
  theme_classic() +
  geom_point(data = locations.qe, mapping = aes(x=lon, y=lat, color = factor(lu,levels = c("Forest","Ag","Natural_open","Human"))), size =1.5, alpha = 0.5) +
  labs(x=NULL, y=NULL) +
  scale_color_manual(values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF"),
                     name = "Land cover", labels = c("Forest", "Agriculture", "Natural open","Developed")) +
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = c(.1,.15))
map.qe

ggsave("figures/usa-overview.png",map.qe,width = 8, height = 5)
