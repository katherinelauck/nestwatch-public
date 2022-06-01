#### Authors: Katie and Alison
#### 

# Dependencies
library(tidyverse)
library(lme4)

# Import data
load() # Fill in the path

# Subset data
# Select 1000 rows at random from the processed dataset.

#####
# Answer
#####

subset <- processed %>%
  # filter(species == 'easblu') %>% # Choose Eastern Bluebirds
  sample_n(1000) # select 1000 random rows


# Building models with number fledged as response