result<-readRDS("~/Documents/nestwatch/results/q5/sp_analysis_jags_summ_121021.RDS")

chains<-readRDS("~/Documents/nestwatch/results/q5/sp_analysis_jags_121021.RDS")

nest<-readRDS("~/Documents/nestwatch/Data/active/success-cleaned.rds")
nest<-nest[nest$greaterThan100==1,]
nest$species<-factor(nest$species)

library("coda")
library("ggplot2")

summ<-data.frame(result)

beta.tmax2<-summ[grep("beta.tmax2",rownames(summ)),]
beta.tmax2.ag<-beta.tmax2[seq(2, 152, 4),]
beta.tmax2.ag$names<-sort(as.character(unique(nest$species)))

beta.tmax2.for<-beta.tmax2[seq(1, 152, 4),]
beta.tmax2.for$names<-sort(as.character(unique(nest$species)))

beta.tmax2.hum<-beta.tmax2[seq(4, 152, 4),]
beta.tmax2.hum$names<-sort(as.character(unique(nest$species)))

beta.tmax2.natop<-beta.tmax2[seq(3, 152, 4),]
beta.tmax2.natop$names<-sort(as.character(unique(nest$species)))


ggplot(beta.tmax2.ag,aes(x=names,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

ggplot(beta.tmax2.for,aes(x=names,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

ggplot(beta.tmax2.hum,aes(x=names,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

ggplot(beta.tmax2.natop,aes(x=names,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

beta.tmaxx<-summ[grep("beta.tmaxx",rownames(summ)),]
beta.tmaxx.ag<-beta.tmaxx[seq(2, 152, 4),]
beta.tmaxx.ag$names<-sort(as.character(unique(nest$species)))

beta.tmaxx.for<-beta.tmaxx[seq(1, 152, 4),]
beta.tmaxx.for$names<-sort(as.character(unique(nest$species)))

beta.tmaxx.hum<-beta.tmaxx[seq(4, 152, 4),]
beta.tmaxx.hum$names<-sort(as.character(unique(nest$species)))

beta.tmaxx.natop<-beta.tmaxx[seq(3, 152, 4),]
beta.tmaxx.natop$names<-sort(as.character(unique(nest$species)))

ggplot(beta.tmaxx.ag,aes(x=names,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

ggplot(beta.tmaxx.for,aes(x=names,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

ggplot(beta.tmaxx.hum,aes(x=names,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))

ggplot(beta.tmaxx.natop,aes(x=names,y=X50.))+geom_point()+geom_errorbar(aes(ymin=X2.5., ymax=X97.5.))+theme_bw()+theme(axis.text.x = element_text(angle = 90))
