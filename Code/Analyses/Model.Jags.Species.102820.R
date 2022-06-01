library("lme4")
library("sjPlot")
library("lubridate")
library("R2jags")
setwd("~/Google Drive/NestWatch Alison/")
load("oneAttemptOneRow_ModisWorldClimElevationNLCDDayMet.RData")
nest<-processed

nest$UnCoor<-factor(nest$UnCoor)
nest$Region<-factor(nest$Region200)
#NLCD categories: one that is forest vs not
#one that is human, forest, non-forest
#LU categories:agriculture, forest, open natural habitats, urban  

nest$NLCD_p_forest<-nest$NLCD_decidious_forest_2km+nest$NLCD_evergreen_forest_2km+nest$NLCD_mixed_forest_2km
nest$NLCD_p_human<-nest$NLCD_developed_open_2km+nest$NLCD_developed_low_2km+nest$NLCD_developed_high_2km+nest$NLCD_developed_medium_2km
nest$NLCD_p_ag<-nest$NLCD_cultivated_crops_2km+nest$NLCD_pasture_2km

nest$NewLU1<-NA
nest$NewLU1[nest$habitat1 %in% c("NW-ag","NW-xmas","NW-orch-vin")]<-"Ag"
nest$NewLU1[nest$habitat1 %in% c("NW-airport","NW-campus","NW-cem","NW-cmpgrd","NW-com-ind","NW-golf","NW-human","NW-park","NW-pit","NW-pwrln","NW-road","NW-ry")]<-"Human"
nest$NewLU1[nest$habitat1 %in% c("NW-for")]<-"Forest"
nest$NewLU1[nest$habitat1 %in% c("NW-burn","NW-chap","NW-des","NW-grass","NW-fw","NW-sw","NW-beach","NW-clrcut")]<-"Natural_open"
#what is com-ind, tun
nest$NewLU1<-factor(nest$NewLU1)
nest$substrate<-factor(nest$substrate)
summary(nest$NewLU1)

#temp variables: tmax_degc_next45, tmin45, tave45,tmax/ave/minprior365,
#precip var: precip_mmday_next45,precip_prior365

#modify: response variable, temperature max min, precip prior and next. create another variable that's temp max minus min?
nest$temprange<-nest$noanomoly_max_tmax_Next45-nest$noanomoly_min_tmin_Next45
nest$temprange<-c(scale(nest$temprange))
nest$tmax_degc_Next45<-c(scale(nest$noanomoly_max_tmax_Next45))
nest$tmin_degc_Next45<-c(scale(nest$noanomoly_min_tmin_Next45))
nest$precip_mmday_Prior365<-c(scale(nest$noanomoly_mean_precip_Prior365))
nest$precip_mmday_Next45<-c(scale(nest$noanomoly_mean_precip_Next45))
nest$zmaxanomalytemp<-c(scale(nest$z_anamoly_max_tmax_Next45))
nest$zminanomalytemp<-c(scale(nest$z_anamoly_mean_tmin_Next45))
nest$NLCD_p_ag<-c(scale(nest$NLCD_p_ag))
nest$NLCD_p_forest<-c(scale(nest$NLCD_p_forest))
nest$NLCD_p_human<-c(scale(nest$NLCD_p_human))

#only use most common species
summary(factor(nest$species))[1:20]

nest<-as_tibble(nest)
nest.filtered<-dplyr::filter(nest,
                             species %in% names(summary(factor(nest$species)))[1:20],
                             is.na(zmaxanomalytemp)==FALSE,
                            is.na(zminanomalytemp)==FALSE,
                            is.na(NewLU1)==FALSE,
                            is.na(precip_mmday_Prior365)==FALSE,
                            is.na(NLCD_p_forest)==FALSE,
                            is.na(NLCD_p_human)==FALSE,       
                            is.na(NLCD_p_ag)==FALSE,     
                            is.na(year)==FALSE,    
                            is.na(Region200)==FALSE,    
                            is.na(UnCoor)==FALSE)   


###Jags code

nestwatch.sp <- function(d,
                  ni=1000,
                  nt=1,
                  nb=100,
                  nc=3) {
  
  model.jags<-function() {
    
#priors    
    beta.tmax~dnorm(0,.001)
    beta.tmin~dnorm(0,.001)
    beta.precip~dnorm(0,.001)
    beta.NLCD.for~dnorm(0,.001)
    beta.NLCD.human~dnorm(0,.001)
    beta.NLCD.ag~dnorm(0,.001)
    mu.tmaxxLUxSp~dnorm(0,.001)
    mu.tminxLUxSp~dnorm(0,.001)
    sig.tminxLUxSp~dunif(0, 10)
    tau.tminxLUxSp<-1/sig.tminxLUxSp^2
    sig.tmaxxLUxSp~dunif(0, 10)
    tau.tmaxxLUxSp<-1/sig.tmaxxLUxSp^2
    sig.year~dunif(0, 10)
    tau.year<-1/sig.year^2
    sig.sp~dunif(0, 10)
    tau.sp<-1/sig.year^2
    sig.region~dunif(0, 10)
    tau.region<-1/sig.region^2
    sig.uncoor~dunif(0, 10)
    tau.uncoor<-1/sig.uncoor^2
    for(i in 1:4){
      alpha.LU[i]~dnorm(0,.001)
      for(j in 1:20){
        beta.tmaxxLUxSp[i,j]~dnorm(mu.tmaxxLUxSp,tau.tmaxxLUxSp)
        beta.tminxLUxSp[i,j]~dnorm(mu.tminxLUxSp,tau.tminxLUxSp)
      }
    }
    for(y in 1:24){
      alpha.year[y]~dnorm(0,tau.year)
    }
    for(sp in 1:20){
      alpha.sp[sp]~dnorm(0,tau.sp)
    }
 #   for(i in 1:36688){
  #    alpha.uncoor[i] ~ dnorm(0, tau.uncoor)}
    for(i in 1:278){
      alpha.region[i] ~ dnorm(0, tau.region)}
    
#likelihood
    for(i in 1:nrows){
at_least_one_success[i]~dbern(p[i])

#confused about how to do nested random effects
logit(p[i])<-beta.tmax*zmaxanomalytemp[i] +
            beta.tmin*zminanomalytemp[i] +
            alpha.LU[NewLU1[i]] +
            alpha.sp[Species[i]] +
            beta.tmaxxLUxSp[NewLU1[i],Species[i]]*zmaxanomalytemp[i] +
            beta.tminxLUxSp[NewLU1[i],Species[i]]*zminanomalytemp[i] +
            beta.precip*precip_mmday_Prior365[i] +
            beta.NLCD.for*NLCD_p_forest[i] +
            beta.NLCD.human*NLCD_p_human[i] +
            beta.NLCD.ag*NLCD_p_ag[i] +
            alpha.year[year[i]] +
            alpha.region[region[i]] #+
           # alpha.uncoor[UnCoor[i]] #+
           # Katie's suggestion for nested RE based on https://drive.google.com/open?id=1r6wf6Ey02ANMpeUkuFoNjNupwNu5QRwR: alpha.autocorr[region[UnCoor[i]]] #+
          #  alpha.nesttype[nesttype[i]]
}
  } 
            R2jags::jags(data=list(
              at_least_one_success=nest.filtered$at_least_one_success,
              zmaxanomalytemp=nest.filtered$zmaxanomalytemp,
              zminanomalytemp=nest.filtered$zminanomalytemp,
              NewLU1=as.numeric(factor(nest.filtered$NewLU1)),
              Species=as.numeric(factor(nest.filtered$species)),
              precip_mmday_Prior365=nest.filtered$precip_mmday_Prior365,
              NLCD_p_forest=nest.filtered$NLCD_p_forest,
              NLCD_p_human=nest.filtered$NLCD_p_human,
              NLCD_p_ag=nest.filtered$NLCD_p_ag,
              year=as.numeric(factor(nest.filtered$year)),
              region=as.numeric(factor(nest.filtered$Region200)),
             # uncoor=as.numeric(factor(nest.filtered$UnCoor)),
              nrows=nrow(nest.filtered)
            ),
            inits=d$inits,
            parameters.to.save=d$params,
            model.file=model.jags,
            n.chains=nc,
            n.thin=nt,
            n.iter=ni,
            n.burnin=nb,
            working.directory=NULL)
  }
  
  inits<-function(){list(list(beta.precip=0),
                         list(beta.precip=0),
                         list(beta.precip=0))}
  params<-c("beta.tmax",
            "beta.tmin",
            "alpha.LU",
            "alpha.sp",
            "beta.tmaxxLUxSp",
            "beta.tminxLUxSp",
            "beta.precip",
            "beta.NLCD.for",
            "beta.NLCD.human",
            "beta.NLCD.ag")
  
  dd<-list(data=dat, inits=inits(), params=params)
  
  out<-nestwatch.sp(dd,ni=100,
             nt=5,
             nb=2,
             nc=3)         
     
  mod14<-glmer(at_least_one_success~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

###
#success, max temp, prior precip
mod1<-glmer(at_least_one_success~tmax_degc_Next45*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(mod1)

mod1a<-glmer(at_least_one_success~tmax_degc_Next45*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(mod1)

#failure, max temp, prior precip
mod7<-glmer(at_least_one_failure~tmax_degc_Next45*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod7a<-glmer(at_least_one_failure~tmax_degc_Next45*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#success, temp anomalies, 365 precip
mod14<-glmer(at_least_one_success~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod14a<-glmer(at_least_one_success~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#failure, temp anomalies, 365 precip
mod16<-glmer(at_least_one_failure~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor)+(1|substrate),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mod16a<-glmer(at_least_one_failure~zmaxanomalytemp*NewLU1+zminanomalytemp*NewLU1+precip_mmday_Prior365*NewLU1+NLCD_p_forest+NLCD_p_human+NLCD_p_ag+(1|species)+(1|year)+(1|Region/UnCoor),data=nest,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
#plot_model(mod1,type = "est")
#plot_model(mod1,type = "pred") #this makes multiple plots, click the "back" arrow to see others
summary(mod1a)
plot_model(mod1,type = "int")
plot_model(mod7,type = "int")
plot_model(mod14,type = "int")
plot_model(mod16,type = "int")
