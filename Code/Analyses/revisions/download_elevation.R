### download elevation data from USGS

require(tidyverse);require(rgdal);require(elevatr)

data <- read_rds('Data/active/success-cleaned.rds'); load("Data/active/raw-collapsed.RData")
loc <- str_split_fixed(data$UnCoor,"_",n=2) %>%
  as_tibble(.name_repair = "universal") %>% 
  rename(lon = "...1",lat = "...2")
loc <- data.frame(UnCoor = data$UnCoor,lon = as.numeric(loc$lon),lat = as.numeric(loc$lat))
loc <- loc[-which(duplicated(loc$UnCoor)),]
elevation <- get_elev_point(loc[,c("lon","lat")],prj = "EPSG:4326",src = "epqs")
ele_key <- cbind(loc,elevation = elevation$elevation) %>% tibble() %>% write_csv("Data/active/elevation.csv")
