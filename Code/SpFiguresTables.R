result<-readRDS("~/Documents/nestwatch/results/q5/sp_analysis_jags_121021.RDS")
nest<-readRDS("~/Documents/nestwatch/Data/active/success-cleaned.rds")
nest<-nest[nest$greaterThan100==1,]
nest$species<-factor(nest$species)
all.sp<-sort(unique(nest$species))

sims<-result$BUGSoutput$sims.matrix

summ<-data.frame(result$BUGSoutput$summary)
which(summ$Rhat>1.1)

colnames(sims)

library("ggplot2")
library("gt")

beta.tmaxx<-sims[,grep("beta.tmaxx",colnames(sims))]

mu<-sims[,grep("mu",colnames(sims))]

beta.tmaxx.ag<-beta.tmaxx[,seq(2, 152, 4)]
mu.tmaxx.ag<-mu[,2]

ag.means<-c(mean(mu.tmaxx.ag), apply(beta.tmaxx.ag,2,mean))
ag.low<-c(quantile(mu.tmaxx.ag,.025), apply(beta.tmaxx.ag,2,function(x) quantile(x,.05)))
ag.high<-c(quantile(mu.tmaxx.ag,.975), apply(beta.tmaxx.ag,2,function(x) quantile(x,.95)))
ag.sig<-(ag.low<0&ag.high<0 | ag.low>0&ag.high>0)

ag.tab<-data.frame(Species = c("Community mean", as.character(all.sp)), ag.means, ag.low, ag.high, ag.sig)

ggplot(ag.tab,aes(x=Species,y=ag.means,color=c("red",rep("black",38)),shape=factor(ag.sig)))+
  geom_point()+geom_errorbar(aes(ymin=ag.low, ymax=ag.high))+
  coord_flip() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Effect of Max Temp Anomaly on Abundance")+
  xlab("Species")+
  geom_hline(yintercept=0, linetype="dashed", color = "blue", size=0.3)+
  scale_x_discrete(limits = rev(c("Community mean", as.character(all.sp))))+
  scale_color_manual(values = c("red" = "red","black"="black"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 8))+
  scale_shape_manual(values=c(1,19))+
  ylim(c(-0.6,0.6))


beta.tmaxx.for<-beta.tmaxx[,seq(1, 152, 4)]
mu.tmaxx.for<-mu[,1]

for.means<-c(mean(mu.tmaxx.for), apply(beta.tmaxx.for,2,mean))
for.low<-c(quantile(mu.tmaxx.for,.025), apply(beta.tmaxx.for,2,function(x) quantile(x,.05)))
for.high<-c(quantile(mu.tmaxx.for,.975), apply(beta.tmaxx.for,2,function(x) quantile(x,.95)))
for.sig<-(for.low<0&for.high<0 | for.low>0&for.high>0)

for.tab<-data.frame(Species = c("Community mean", as.character(all.sp)), for.means, for.low, for.high, for.sig)

ggplot(for.tab,aes(x=Species,y=for.means,color=c("red",rep("black",38)),shape=factor(for.sig)))+
  geom_point()+geom_errorbar(aes(ymin=for.low, ymax=for.high))+
  coord_flip() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Effect of Max Temp Anomaly on Abundance")+
  xlab("Species")+
  geom_hline(yintercept=0, linetype="dashed", color = "blue", size=0.3)+
  scale_x_discrete(limits = rev(c("Community mean", as.character(all.sp))))+
  scale_color_manual(values = c("red" = "red","black"="black"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 8))+
  scale_shape_manual(values=c(1,19))+
  ylim(c(-0.6,0.6))

beta.tmaxx.hum<-beta.tmaxx[,seq(4, 152, 4)]
mu.tmaxx.hum<-mu[,4]

hum.means<-c(mean(mu.tmaxx.hum), apply(beta.tmaxx.hum,2,mean))
hum.low<-c(quantile(mu.tmaxx.hum,.025), apply(beta.tmaxx.hum,2,function(x) quantile(x,.05)))
hum.high<-c(quantile(mu.tmaxx.hum,.975), apply(beta.tmaxx.hum,2,function(x) quantile(x,.95)))
hum.sig<-(hum.low<0&hum.high<0 | hum.low>0&hum.high>0)

hum.tab<-data.frame(Species = c("Community mean", as.character(all.sp)), hum.means, hum.low, hum.high, hum.sig)

ggplot(hum.tab,aes(x=Species,y=hum.means,color=c("red",rep("black",38)),shape=factor(hum.sig)))+
  geom_point()+geom_errorbar(aes(ymin=hum.low, ymax=hum.high))+
  coord_flip() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Effect of Max Temp Anomaly on Abundance")+
  xlab("Species")+
  geom_hline(yintercept=0, linetype="dashed", color = "blue", size=0.3)+
  scale_x_discrete(limits = rev(c("Community mean", as.character(all.sp))))+
  scale_color_manual(values = c("red" = "red","black"="black"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 8))+
  scale_shape_manual(values=c(1,19))+
  ylim(c(-0.6,0.6))

beta.tmaxx.natop<-beta.tmaxx[,seq(3, 152, 4)]
mu.tmaxx.natop<-mu[,3]

natop.means<-c(mean(mu.tmaxx.natop), apply(beta.tmaxx.natop,2,mean))
natop.low<-c(quantile(mu.tmaxx.natop,.025), apply(beta.tmaxx.natop,2,function(x) quantile(x,.05)))
natop.high<-c(quantile(mu.tmaxx.natop,.975), apply(beta.tmaxx.natop,2,function(x) quantile(x,.95)))
natop.sig<-(natop.low<0&natop.high<0 | natop.low>0&natop.high>0)

natop.tab<-data.frame(Species = c("Community mean", as.character(all.sp)), natop.means, natop.low, natop.high, natop.sig)

ggplot(natop.tab,aes(x=Species,y=natop.means,color=c("red",rep("black",38)),shape=factor(natop.sig)))+
  geom_point()+geom_errorbar(aes(ymin=natop.low, ymax=natop.high))+
  coord_flip() +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Effect of Max Temp Anomaly on Abundance")+
  xlab("Species")+
  geom_hline(yintercept=0, linetype="dashed", color = "blue", size=0.3)+
  scale_x_discrete(limits = rev(c("Community mean", as.character(all.sp))))+
  scale_color_manual(values = c("red" = "red","black"="black"))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(size = 8))+
  scale_shape_manual(values=c(1,19))+
  ylim(c(-0.6,0.6))

ag.tab.txt<-paste(sprintf('%.2f',ag.means), " (",sprintf('%.2f',ag.low), ", ",sprintf('%.2f',ag.high), ")", sep = "") 
for.tab.txt<-paste(sprintf('%.2f',for.means), " (",sprintf('%.2f',for.low), ", ",sprintf('%.2f',for.high), ")", sep = "") 
hum.tab.txt<-paste(sprintf('%.2f',hum.means), " (",sprintf('%.2f',hum.low), ", ",sprintf('%.2f',hum.high), ")", sep = "") 
natop.tab.txt<-paste(sprintf('%.2f',natop.means), " (",sprintf('%.2f',natop.low), ", ",sprintf('%.2f',natop.high), ")", sep = "") 

all.LU.tab<-data.frame(Species = c("Community mean", as.character(all.sp)), Agriculture = ag.tab.txt, Forest = for.tab.txt, Human = hum.tab.txt, Natural_Open=natop.tab.txt)

tab.sav<-
  all.LU.tab %>% 
  gt() %>% #use gt package to make a pretty table
  tab_header( #add title
    title = "Effect of Max Temp Anomaly on Nest Success") %>% 
  cols_label( 
    Natural_Open = "Natural Open",
    Human="Developed") %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(2,ag.sig))%>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(3,for.sig))%>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(4,hum.sig))%>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(5,natop.sig))%>%
  cols_align(
    align = "center",
    columns = 2:5)

tab.sav %>%
  gtsave(
    "species.table.121721.png", 
    path = "~/Documents/nestwatch/figures/"
  )   
