#this is the package for making generalized linear mixed-effects models (a mouthful)
install.packages("lme4")
library("lme4")
#sjPlot is the package for visualizing results 
install.packages("sjPlot")
library("sjPlot")
#read in data - you will need to change the folder directory to match your computer. This is the file Katie recently shared! Danny's dataset won't load for me and it's too last minute so we can look at that next week
nest<-read.csv("~/Google Drive/NestWatch Alison/nestwatch_one_row_per_attempt.csv")
load("~/Google Drive/NestWatch Alison/nestwatch_one_row_per_attempt_modis_worldclim_elevation.RDS")

#take a look at the local land use categories
summary(factor(nest$habitat1))

#let's make a new column that categorizes all the land-uses into Ag, Human, or Natural
nest$NewLU1<-NA
#the next line is saying: select the habitat1's that are ag, xmas, and orch-vin and call them all "Ag"
nest$NewLU1[nest$habitat1 %in% c("NW-ag","NW-xmas","NW-orch-vin")]<-"Ag"
nest$NewLU1[nest$habitat1 %in% c("NW-airport","NW-campus","NW-cem","NW-cmpgrd","NW-com-ind","NW-golf","NW-human","NW-park","NW-pit","NW-pwrln","NW-road","NW-ry")]<-"Human"
nest$NewLU1[nest$habitat1 %in% c("NW-for","NW-burn","NW-chap","NW-des","NW-grass","NW-fw","NW-sw")]<-"Natural"
#check results
summary(factor(nest$NewLU1))

#We will repeat the above code with the 50m land use.
#make a new column that categorizes all the 50m land-uses into Ag, Human, or Natural
nest$NewLU2<-NA 
nest$NewLU2[nest$habitat2 %in% c("NW-ag","NW-xmas","NW-orch-vin")]<-"Ag"
nest$NewLU2[nest$habitat2 %in% c("NW-airport","NW-campus","NW-cem","NW-cmpgrd","NW-com-ind","NW-golf","NW-human","NW-park","NW-pit","NW-pwrln","NW-road","NW-ry")]<-"Human"
nest$NewLU2[nest$habitat2 %in% c("NW-for","NW-burn","NW-chap","NW-des","NW-grass","NW-fw","NW-sw")]<-"Natural"
#check results
summary(factor(nest$NewLU2))

#Let's make a "species" variable for the model. First, like before, fix the species names with numbers in them. Substitute any "digit" with nothing ''
nest$species<-gsub('[[:digit:]]+', '', nest$species)

#I made up a rule. For every species with over 200 rows, keep the name. For others, call them all "other". I'm doing the same thing as in the data cleaning.
SpeciesSum<-summary(factor(nest$species))
SpeciesSelect<-SpeciesSum[SpeciesSum>200][-length(SpeciesSum[SpeciesSum>200])]
nest$species[nest$species %in% names(SpeciesSelect)==FALSE]<-"other"

#let's turn the longitude into categories so we can account for spatial effects in the model. It's now called lonCat
nest$lonCat<-factor(floor(nest$lon))

#scale latitude for the model. It's good to scale (also called z-score, meaning centered at 0 and divided by standard deviation). It's now called latS
nest$latS<-scale(nest$lat)




#let's model predation!
#2 models, one for local land-use x latitude and one for 50m land-use x latitude
#the response is predation, and it is 1 (predated) or 0 (not predated)
#We are trying to explain predation using the combination of NewLU1 (local land-use) and latitude (I am using latitude as a substitute for temperature)
#species and lonCat are "random effects". They are different than NewLU1 and latitude because in this model, we are just trying to account for them vs. studying their effects. That is not the best explanation but I can try to explain it better later.
#family=binomial means the response is 1 or 0, what we are doing is called logistic regression
#link=logit means we are modeling a probability (of predation), restricted to being between 0 and 1

mod.Pred1<-glmer(predation~NewLU1*latS+(1|species)+(1|lonCat),data=nest,family=binomial(link="logit")) #this line may take a minute or two

#looking at the summary will show the results. I can explain it later, but the plots actually show the results better
summary(mod.Pred1)
#the following plot shows odds ratios, the effect of a certain variable on predation. 1 means no change. If the red line does not overlap 1, it means it has a significant positive effect. Here, looks like nests in human land-uses tend to get predated more.
plot_model(mod.Pred1,type = "est")
#the following plots are predictions. In the first plot, based on our model, we predict the highest predation in human land-uses and lowest in ag. There is also higher predation in higher latitudes.
plot_model(mod.Pred1,type = "pred") #this makes 2 plots, click the "back" arrow to see the other
#the following plot looks at the interaction between land-use and latitude. It looks like in natural habitats, there is much higher predation in higher latitudes (blue line)
plot_model(mod.Pred1,type = "int")

#repeat all of the above but for NewLU2, the 50m land-use, instead of NewLU1
mod.Pred2<-glmer(predation~NewLU2*latS+(1|species)+(1|lonCat),data=nest,family=binomial(link="logit"))
plot_model(mod.Pred2,type = "est")
plot_model(mod.Pred2,type = "pred") #this makes 2 plots, click the "back" arrow to see the other
plot_model(mod.Pred2,type = "int")



###Tom and Kees: try to repeat what I did for predation, but for the variables "at_least_one_failure" and "at_least_one_success". Then, add all the results to the google doc using copy/paste or screenshots! For the summary() part, you can just copy/paste the part that says "Fixed effects". For plots, you can click "Export" and then "copy to clipboard"