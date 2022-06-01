
library("lme4")
library("sjPlot")
library("lubridate")

load("~/Google Drive/NestWatch Alison/current_data_for_analysis.Rdata")
nest<-processed

sort(unlist(c(unique(processed$species))))

#look at new variables
names(nest)

#At_least_one_success ~ max_breed_temp + total_annual_precipitation + local_land_use + local_land_use*max_breed_temp +  local_land_use*total_annual_precipitation + (1|Species) + (1|Year) + (1|LocalSiteID) 
#Note: local land use defined as: agriculture, forest, open natural habitats, urban  

nest$NewLU1<-NA
#the next line is saying: select the habitat1's that are ag, xmas, and orch-vin and call them all "Ag"
nest$NewLU1[nest$habitat1 %in% c("NW-ag","NW-xmas","NW-orch-vin")]<-"Ag"
nest$NewLU1[nest$habitat1 %in% c("NW-airport","NW-campus","NW-cem","NW-cmpgrd","NW-com-ind","NW-golf","NW-human","NW-park","NW-pit","NW-pwrln","NW-road","NW-ry")]<-"Human"
nest$NewLU1[nest$habitat1 %in% c("NW-for")]<-"Forest"
nest$NewLU1[nest$habitat1 %in% c("NW-burn","NW-chap","NW-des","NW-grass","NW-fw","NW-sw")]<-"Natural_open"

#check results
nest$NewLU1<-factor(nest$NewLU1)
summary(nest$NewLU1)

nest$precipS<-scale(nest$WorldClim_TotalAnnualPrecip)
nest$maxbreedtempS<-scale(nest$WorldClim_MaxBreedTemp)
nest$meanbreedtempS<-scale(nest$WorldClim_AveBreedTemp)
nest$minbreedtempS<-scale(nest$WorldClim_MinAnnualTemp)
nest$species2<-factor(nest$species)
nest$year<-factor(year(nest$laydate))
nest$locID<-factor(nest$siteID)

mod1<-glmer(at_least_one_success~maxbreedtempS*NewLU1+precipS*NewLU1+(1|species2)+(1|year)+(1|locID),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(mod1)


mod2<-glmer(at_least_one_success~meanbreedtempS*NewLU1+precipS*NewLU1+(1|species2)+(1|year)+(1|locID),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) 

mod3<-glmer(at_least_one_success~minbreedtempS*NewLU1+precipS*NewLU1+(1|species2)+(1|year)+(1|locID),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) 

mod4<-glmer(at_least_one_failure~maxbreedtempS*NewLU1+precipS*NewLU1+(1|species2)+(1|year)+(1|locID),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) 

mod5<-glmer(at_least_one_failure~meanbreedtempS*NewLU1+precipS*NewLU1+(1|species2)+(1|year)+(1|locID),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) 

mod6<-glmer(at_least_one_failure~minbreedtempS*NewLU1+precipS*NewLU1+(1|species2)+(1|year)+(1|locID),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) 

mod7<-glmer(predation~meanbreedtempS*NewLU1+meanbreedtempS*landscapeopenness+(1|species2)+(1|year)+(1|locID),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) 

summary(mod4)



plot_model(mod1,type = "est")
plot_model(mod1,type = "pred") #this makes multiple plots, click the "back" arrow to see others
plot_model(mod1,type = "int")

