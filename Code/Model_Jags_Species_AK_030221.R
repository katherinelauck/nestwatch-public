library("lme4")
library("sjPlot")
library("lubridate")
library("R2jags")
setwd("~/Documents/nestwatch/Data/")
nest<-readRDS("success-cleaned.rds")

library(dplyr)

nest$greaterThan100
nest<-nest[nest$greaterThan100==1,]
nest$species<-factor(nest$species)

###Jags code

nestwatch.sp <- function(d,
                         ni=1000,
                         nt=1,
                         nb=100,
                         nc=3) {
  
  model.jags<-function() {
    
    #priors    
    #beta.tmax~dnorm(0,.001)
    #beta.tmin~dnorm(0,.001)
    beta.precip~dnorm(0,.001)
    beta.substrate~dnorm(0,.001)
    beta.NLCD.for~dnorm(0,.001)
    beta.NLCD.human~dnorm(0,.001)
    beta.NLCD.ag~dnorm(0,.001)
    mu.tmaxxLUxSp~dnorm(0,.001)
    mu.tmaxxLUxSp2~dnorm(0,.001)
    mu.tmaxxLUxSp3~dnorm(0,.001)
    mu.tmaxxLUxSp4~dnorm(0,.001)
    # mu.tminxLUxSp~dnorm(0,.001)
    #  sig.tminxLUxSp~dunif(0, 10)
    # tau.tminxLUxSp<-1/sig.tminxLUxSp^2
    sig.tmaxxLUxSp~dunif(0.001, 15)
    tau.tmaxxLUxSp<-1/sig.tmaxxLUxSp^2
    sig.tmaxxLUxSp2~dunif(0.001, 15)
    tau.tmaxxLUxSp2<-1/sig.tmaxxLUxSp2^2
    sig.tmaxxLUxSp3~dunif(0.001, 15)
    tau.tmaxxLUxSp3<-1/sig.tmaxxLUxSp3^2
    sig.tmaxxLUxSp4~dunif(0.001, 15)
    tau.tmaxxLUxSp4<-1/sig.tmaxxLUxSp4^2
    sig.year~dunif(0, 10)
    tau.year<-1/sig.year^2
    sig.sp~dunif(0, 10)
    tau.sp<-1/sig.sp^2
    sig.region~dunif(0, 10)
    tau.region<-1/sig.region^2
    # sig.uncoor~dunif(0, 10)
    # tau.uncoor<-1/sig.uncoor^2
    for(i in 1:4){
      alpha.LU[i]~dnorm(0,.001)
    }
    for(j in 1:38){
      beta.tmaxxLUxSp[1,j]~dnorm(mu.tmaxxLUxSp,tau.tmaxxLUxSp)
      beta.tmaxxLUxSp[2,j]~dnorm(mu.tmaxxLUxSp2,tau.tmaxxLUxSp2)
      beta.tmaxxLUxSp[3,j]~dnorm(mu.tmaxxLUxSp3,tau.tmaxxLUxSp3)
      beta.tmaxxLUxSp[4,j]~dnorm(mu.tmaxxLUxSp4,tau.tmaxxLUxSp4)
      #beta.tminxLUxSp[i,j]~dnorm(mu.tminxLUxSp,tau.tminxLUxSp)
    }
    
    for(y in 1:24){
      alpha.year[y]~dnorm(0,tau.year)
    }
    for(sp in 1:38){
      alpha.sp[sp]~dnorm(0,tau.sp)
    }
    #   for(i in 1:36688){
    #    alpha.uncoor[i] ~ dnorm(0, tau.uncoor)}
    for(i in 1:395){
      alpha.region[i] ~ dnorm(0, tau.region)}
    
    #likelihood
    for(i in 1:nrows){
      at_least_one_success[i]~dbern(p[i])
      
      #confused about how to do nested random effects
      logit(p[i])<-
        #beta.tmax*zmaxanomalytemp[i] +
        alpha.LU[NewLU1[i]] +
        alpha.sp[Species[i]] +
        beta.tmaxxLUxSp[NewLU1[i],Species[i]]*zmaxanomalytemp[i] +
        #beta.tmaxxLUxSp2[NewLU1[i],Species[i]]*zmaxanomalytemp2[i] +
        beta.precip*precip_mmday_Prior365[i] +
        beta.NLCD.for*NLCD_p_forest[i] +
        beta.NLCD.human*NLCD_p_human[i] +
        beta.NLCD.ag*NLCD_p_ag[i] +
        beta.substrate*substrate2[i] +
        alpha.year[year[i]] +
        alpha.region[region[i]] #+
      # alpha.uncoor[UnCoor[i]] 
      #  alpha.nesttype[nesttype[i]]
      ## missing laydate and tmax squared
    }
  } 
  R2jags::jags(data=list(
    substrate2=nest$substrate_binary,
    at_least_one_success=nest$at_least_one_success,
    zmaxanomalytemp=nest$Tmax_std_gridmet,
    #zminanomalytemp=nest.filtered$zminanomalytemp,
    NewLU1=as.numeric(nest$NewLU1),
    Species=as.numeric(nest$species),
    precip_mmday_Prior365=nest$pcpbefore_raw_gridmet,
    NLCD_p_forest=nest$NLCD_p_forest,
    NLCD_p_human=nest$NLCD_p_human,
    NLCD_p_ag=nest$NLCD_p_ag,
    year=as.numeric(nest$year),
    region=as.numeric(nest$Region),
    # uncoor=as.numeric(factor(nest.filtered$UnCoor)),
    nrows=nrow(nest)
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
params<-c(#"beta.tmax",
  #"beta.tmin",
  "alpha.LU",
  "mu.tmaxxLUxSp",
  "mu.tmaxxLUxSp2",
  "mu.tmaxxLUxSp3",
  "mu.tmaxxLUxSp4",
  "sig.tmaxxLUxSp",
  "sig.tmaxxLUxSp2",
  "sig.tmaxxLUxSp3",
  "sig.tmaxxLUxSp4",
  "alpha.sp",
  "beta.tmaxxLUxSp",
  #"beta.tminxLUxSp",
  "beta.precip",
  "beta.NLCD.for",
  "beta.NLCD.human",
  "beta.NLCD.ag",
  "beta.substrate",
  "sig.year",
  "sig.region")
dat=list(
  substrate2=nest$substrate_binary,
  at_least_one_success=nest$at_least_one_success,
  zmaxanomalytemp=nest$Tmax_std_gridmet,
  #zminanomalytemp=nest.filtered$zminanomalytemp,
  NewLU1=as.numeric(nest$NewLU1),
  Species=as.numeric(nest$species),
  precip_mmday_Prior365=nest$PcpBefore_raw,
  NLCD_p_forest=nest$NLCD_p_forest,
  NLCD_p_human=nest$NLCD_p_human,
  NLCD_p_ag=nest$NLCD_p_ag,
  year=as.numeric(nest$year),
  region=as.numeric(nest$Region),
  # uncoor=as.numeric(factor(nest.filtered$UnCoor)),
  nrows=nrow(nest)
)
dd<-list(data=dat, inits=inits(), params=params)

# out<-nestwatch.sp(dd,ni=1000,
#                   nt=2,
#                   nb=5,
#                   nc=3)     

out<-nestwatch.sp(dd,ni=55000,
                  nt=50,
                  nb=5000,
                  nc=3)      

setwd("~/Documents/nestwatch/results/Question 5/")


dput(out,"sp_analysis_jags_030921.R")
saveRDS(out,"sp_analysis_jags_030921.RDS")


summ<-data.frame(out$BUGSoutput$summary)

#order of factors: Forest Ag Natural_open Human

beta.tmax.summ<-summ[grep("beta.tmaxx",rownames(summ)),]

beta.tmax.summ.for<-beta.tmax.summ[seq(from=1,to=152,by=4),]
beta.tmax.summ.ag<-beta.tmax.summ[seq(from=2,to=152,by=4),]
beta.tmax.summ.natop<-beta.tmax.summ[seq(from=3,to=152,by=4),]
beta.tmax.summ.hum<-beta.tmax.summ[seq(from=4,to=152,by=4),]

beta.tmax.summ.for$species<-beta.tmax.summ.ag$species<-beta.tmax.summ.natop$species<-beta.tmax.summ.hum$species<-levels(nest$species)

library(ggplot2)
ggplot(beta.tmax.summ.for,aes(x=species,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

ggplot(beta.tmax.summ.ag,aes(x=species,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

ggplot(beta.tmax.summ.natop,aes(x=species,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

ggplot(beta.tmax.summ.hum,aes(x=species,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))


#mu values: Forest (sig. pos) Ag (sig. neg) Natural_open (cross 0) Human (cross 0)


#Forest Ag Natural_open Human, looks like natural open and human do not intersect 0 and forest and ag do
sigmaEDGE= bounded.hpp(out$BUGSoutput$sims.matrix[,206],lower=0,upper=Inf)
sigmaEDGE2= bounded.hpp(out$BUGSoutput$sims.matrix[,207],lower=0,upper=Inf)
sigmaEDGE3= bounded.hpp(out$BUGSoutput$sims.matrix[,208],lower=0,upper=Inf)
sigmaEDGE4= bounded.hpp(out$BUGSoutput$sims.matrix[,209],lower=0,upper=Inf)
