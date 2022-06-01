#### Build extra figures for ESA talk
#### Author: Katherine Lauck
#### Last updated: 21 July 2021

# Dependencies

library(tidyverse)
library(ggplot2)
library(maps)
library(sf)

# read in map of USA
usa = st_as_sf(map('usa', plot = FALSE, fill = TRUE))

# separate out coordinates
coor <- separate(read_rds("Data/active/success-cleaned.rds"), UnCoor, into = c("lon", "lat"),sep = "_") %>% ungroup %>% select(c(lon, lat, NewLU1)) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))

ggplot() + geom_sf(data = usa, color = "white") + 
  geom_point(data = coor, aes(x = lon, y = lat, color = NewLU1), alpha = .5) + 
  theme_void() + 
  theme(rect = element_rect(fill = "transparent"),legend.text=element_text(color="white",size=12),legend.title = element_text(color = "white", size = 12)) + 
  scale_color_manual(labels = c("Forest","Agriculture","Natural open","Human"),values = c("#7CAE00","#F8766D","#00BFC4","#C77CFF")) + 
  guides(color=guide_legend(title="Land use"))

ggsave("figures/usa-overview.png",width = 8, height = 6,bg = "transparent")

d <- read_rds("Data/active/success-cleaned.rds")
length(unique(d$species))
unique(d$year)
