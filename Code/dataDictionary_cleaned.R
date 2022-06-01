
library(tidyverse)

#success <- readRDS('Data/success-cleaned.rds')
failure <- read_rds('../Data/failure-cleaned.rds')

Variable <- c("at_least_one_success",names(failure))
Description <- c("Binary indicator of whether at least one chick fledged from this attempt",
                 "Attempt number as indicated in raw Nestwatch data",
                 "Bird species common name",
                 "USGS Breeding Bird Survey long-term population trend over years XXX, with value subtracted by mean and divided by standard deviation",
                 "NABCI Species Assessment Summary conservation concern score, available at https://www.stateofthebirds.org/2016/resources/species-assessments/",
                 "Scaled NABCI Species Assessment Summary conservation concern score, available at https://www.stateofthebirds.org/2016/resources/species-assessments/",
                 "Substrate of nest location, 1 indicating a nestbox and 0 not a nestbox",
                 "Substrate of nest location, 1 indicating a cavity, (nestbox or natural cavity), and 0 not a cavity",
                 "Julian lay date of attempt. Either as indicated in raw NestWatch data or calculated if appropriate by subtracting # of eggs first observed from that observation date.", "Julian date of lay date, with value subtracted by mean and divided by standard deviation",
                 "Year of attempt",
                 "Binary indicator of at least one chick died",
                 "Immediate habitat within one meter of the nest, classified into 4 land-use categories, Ag, Human, Forest, and Natural_open. Data is derived derived from nestwatch observations. Land use category Ag contains land classified as agricultural(NW-ag), christmas tree farm(NW-xmas), and orchard/vineyard(NW-orch-vin). Category Human consisted of land classified as airport(NW-airport), campus(NW-campus), cemetary(NW-cem), campground(NW cmpgrd), commercial/industrial(NW-com-ind), golf course(NW-golf), human-modified(NW-human), public park/green space(NW-park), landfill/gravel pit/strip mine(NW-pit), powerline(NW-pwrln), roadside(NW-road), and yard/residential(NW-ry). Category Forest contains classification forest(NW-forest). Finally, Natural_Open contains recently burned area(NW-burn), chaparral(NW-chap), desert(NW-des), grassland(NW-grass), (fresh water(NW-fw), salt water(NW-sw), beach(NW-beach), and recently clearcut land(NW-clearcut).",
                 "Unique region identifier, where regions are defined by placing hexgrids across the study region, with 200 km between cells","Site identifier, constructed by appending the latitude and longitude of the site",
                 "The maximum of the maximum daily temperatures over the 45 days after the laydate, minus the mean and divided by the standard deviation from 1980-2000, using Gridmet",
                 "The minimum of the minimum daily temperatures over the 45 days after the laydate, minus the mean and divided by the standard deviation from 1980-2000, using Gridmet",
                 "The maximum of the maximum daily temperatures over the 45 days after the laydate, averaged across the years 1980-2000",
                 "The maximum temperature of a site-date combination, relative to what the species usually experiences during nesting. To calculate, first calculate the maximum of the maximum daily temperatures over the 45 days after the laydate, averaged across the years 1980-2000. From this number, substract the mean and divide by the standard deviation of this metric across all other nesting attempts made by this species.",
                 "The minimum of the minimum daily temperatures over the 45 days after the laydate, averaged across the years 1980-2000",
                 "The minimum temperature of a site-date combination, relative to what the species usually experiences during nesting. To calculate, first calculate the minimum of the minimum daily temperatures over the 45 days after the laydate, averaged across the years 1980-2000. From this number, substract the mean and divide by the standard deviation of this metric across all other nesting attempts made by this species.",
                 "Proportion of the area of the National Land Cover Data class cultivated crops and pasture in a buffer of 2km radii weighted by years available. Only 2001, 2004,2006, 2008, 2011, 2013 and 2016 are currently (October 2020) available as resources to calculate the area of the different land uses. Data is expressed as a single value per year. To attribute a value to years in which NLCD is not available, values were weighted taken into account the values of the prior and posterior available data. For example, data for 2002 is calculated by adding 2/3 of the value for 2001 and 1/3 of the value for 2004. Data for Alaska is only available for the yeard 2001, 2011 and 2016.",
                 "Proportion of the area of the National Land Cover Data class deciduous forest, evergreen forest, and mixed forest in a buffer of 2km radii weighted by years available. Only 2001, 2004,2006, 2008, 2011, 2013 and 2016 are currently (October 2020) available as resources to calculate the area of the different land uses. Data is expressed as a single value per year. To attribute a value to years in which NLCD is not available, values were weighted taken into account the values of the prior and posterior available data. For example, data for 2002 is calculated by adding 2/3 of the value for 2001 and 1/3 of the value for 2004. Data for Alaska is only available for the yeard 2001, 2011 and 2016.",
                 "Proportion of the area of the National Land Cover Data class developed open, developed low, and developed high in a buffer of 2km radii weighted by years available. Only 2001, 2004,2006, 2008, 2011, 2013 and 2016 are currently (October 2020) available as resources to calculate the area of the different land uses. Data is expressed as a single value per year. To attribute a value to years in which NLCD is not available, values were weighted taken into account the values of the prior and posterior available data. For example, data for 2002 is calculated by adding 2/3 of the value for 2001 and 1/3 of the value for 2004. Data for Alaska is only available for the yeard 2001, 2011 and 2016.",
                 "The total precipitation over the 365 days before the laydate, using Gridmet",
                 "The total precipitation over the 365 days before the laydate, minus the mean and divided by the standard deviation from 1980-2000, using Gridmet",
                 "The total precipitation over the 45 days after the laydate, using Gridmet",
                 "The total precipitation over the 45 days after the laydate, minus the mean and divided by the standard deviation from 1980-2000, using Gridmet",
                 "0 if species has fewer than 100 nesting attempts. 1 if greater than or equal to 100 nesting attempts/species, across the whole dataset.",
                 "The total precipitation over the 365 days before the laydate, minus the mean and divided by the standard deviation from 1980-2000, using Gridmet",
                 "The total precipitation over the 45 days after the laydate, minus the mean and divided by the standard deviation from 1980-2000, using Gridmet",
                 "The minimum of the minimum daily temperatures over the 45 days after the laydate, averaged across the years 1980-2000, minus the mean and divided by the standard deviation",
                 "The maximum of the maximum daily temperatures over the 45 days after the laydate, averaged across the years 1980-2000, minus the mean and divided by the standard deviation"
                 )
# for value, indicate type of data and possible values, as well as units. Eg. Binary 0 or 1
Value <- c("Binary 0 or 1","Alpha/numeric","Alpha","Numeric","Integer","Numeric","Binary 0 or 1","Binary 0 or 1","yyyy-mm-dd","Numeric","Numeric","Binary 0 or 1","Human, Natural_open, Ag, or Forest","Alpha","Alpha","Numeric","Numeric","Numeric (degrees Celcius)","Numeric (degrees Celcius)","Numeric (degrees Celcius)","Numeric (degrees Celcius)","Numeric","Numeric","Numeric","Numeric (mm)","Numeric","Numeric (mm)","Numeric","Binary 0 or 1","Numeric","Numeric","Numeric","Numeric")

dd <- tibble(Variable = Variable, Description = Description, Value = Value)
