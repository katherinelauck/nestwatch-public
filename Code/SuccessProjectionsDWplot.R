library(tidyverse)
library(plotrix)

df <- read.csv('Data/success_projections_all.csv')
colnames(df)

#subset columns for future projectsions and difference between future and present predictions
fut <- df %>% select(contains("y.fut."))
dif <- df %>% select(contains("y.dif."))

#remove NAs or summarise acorss can't calculate mean
#10 rows have NAs for all projections
fut <- fut[-which(is.na(df$y.fut.rcp85e.gfdl.90)),] 
dif <- dif[-which(is.na(df$y.dif.rcp85e.gfdl.90)),]

########### FUTURE PLOT ###############
#get mean and SE for all success predictions
fut.sum <- fut %>%
  summarise(across(.cols = everything(),list(mean = mean, se = std.error)))

fut.mean <- fut.sum %>%
  select(contains('_mean')) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(mean = V1)

fut.SE <- fut.sum %>%
  select(contains('_se')) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(SE = V1)

fut.sum <- cbind(fut.mean, fut.SE) %>%
  rownames_to_column("scenario") %>%
  mutate(RCP = ifelse(str_detect(scenario, "45"), 45, 85),
         Time = ifelse(str_detect(scenario, "45e|85e"), 2100, 2050),
         Model = ifelse(str_detect(scenario, "gfdl"), 'GFDL',
                        ifelse(str_detect(scenario, "mri"), "MRI",
                               ifelse(str_detect(scenario, "canesm"), "CANESM",
                                      "NA"))),
         Distribution = ifelse(str_detect(scenario,"mean_"), "mean",
                               ifelse(str_detect(scenario, '10_'), '10th percentile',
                                      ifelse(str_detect(scenario,'90_'), '90th percentile', "NA")))) %>%
  mutate(scenario = substr(scenario, 7, nchar(scenario))) %>%
  mutate(scenario = gsub('_mean|_se',"", scenario)) %>%
  dplyr::rename(Mean = fut.mean) %>%
  mutate(group = paste(RCP, Time, Model, sep="_"))

pd <- position_dodge(.6)

p1 <- ggplot(fut.sum, aes(x=Mean, y=scenario, color = group)) +
  geom_errorbar(aes(xmin=Mean-SE, xmax=Mean+SE), #manually specify color?
                width=0,
                position = pd,
                show.legend=F) + #don't show error bars in legend
  geom_point(size=3, position=pd)
p1  


#################### DIFFERENCE PLOT ##############3
#get mean and SE for all success predictions
dif.sum <- dif %>%
  summarise(across(.cols = everything(),list(mean = mean, se = std.error)))

dif.mean <- dif.sum %>%
  select(contains('_mean')) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(mean = V1)

dif.SE <- dif.sum %>%
  select(contains('_se')) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(SE = V1)

dif.sum <- cbind(dif.mean, dif.SE) %>%
  rownames_to_column("scenario") %>%
  mutate(RCP = ifelse(str_detect(scenario, "45"), 45, 85),
         Time = ifelse(str_detect(scenario, "45e|85e"), 2100, 2050),
         Model = ifelse(str_detect(scenario, "gfdl"), 'GFDL',
                        ifelse(str_detect(scenario, "mri"), "MRI",
                               ifelse(str_detect(scenario, "canesm"), "CANESM",
                                      "NA"))),
         Distribution = ifelse(str_detect(scenario,"mean_"), "mean",
                               ifelse(str_detect(scenario, '10_'), '10th percentile',
                                      ifelse(str_detect(scenario,'90_'), '90th percentile', "NA")))) %>%
  mutate(scenario = substr(scenario, 7, nchar(scenario))) %>%
  mutate(scenario = gsub('_mean|_se',"", scenario)) %>%
  dplyr::rename(Mean = mean) %>%
  mutate(group = paste(RCP, Time, Model, sep="_"))

pd <- position_dodge(.6)

p2 <- ggplot(dif.sum, aes(x=Mean, y=scenario, color = group, shape = Distribution)) +
  geom_errorbar(aes(xmin=Mean-SE, xmax=Mean+SE), #manually specify color?
                width=0,
                position = pd,
                show.legend=F) + #don't show error bars in legend
  geom_point(size=3, position=pd) +
  xlab("Difference in nest success")
p2 
#600x700
