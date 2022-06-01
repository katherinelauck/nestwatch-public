data<-read.csv("~/Google Drive/NestWatch Alison/NestwatchData.csv")
head(data)
dim(data)
colnames(data)
length(unique(data$LOC_ID))
length(unique(data$ATTEMPT_ID))
par(mfrow=c(1,1))
par(las=2)
barplot(summary(factor(data$HABITAT_CODE_1)))
sum(data$HABITAT_CODE_1=="")/nrow(data)
barplot(summary(factor(data$HABITAT_CODE_2)))
sum(data$HABITAT_CODE_2=="")/nrow(data)

data$longlat<-paste(data$LONGITUDE,data$LONGITUDE)

nest<-data[!duplicated(data$ATTEMPT_ID), ]
barplot(summary(factor(nest$HABITAT_CODE_1)))
barplot(summary(factor(nest$HABITAT_CODE_2)))
barplot(summary(factor(nest$SPECIES_CODE)))

nest.test<-nest[1:50,]
ggplot(nest, aes(x = LONGITUDE, y = LATITUDE)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 geom = "polygon", data = nest.test) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

library(ggplot2)
ggplot(nest, aes(x = LONGITUDE, y = LATITUDE)) + 
  geom_point(size = 0.1, alpha = 0.05) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + xlim(c(-128, -60)) +ylim(c(22, 58))


ggplot(nest, aes(x = LONGITUDE, y = LATITUDE)) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') + 
  stat_density2d(aes(fill = ..level..), alpha = .5,
                 h = .02,
                 geom = "polygon", data = nest) + 
  scale_fill_viridis_c() + 
  theme(legend.position = 'none')

#change colors based on success/fail


#success outcomes: s, s1
#fail outcomes: f, f1, f2, f3, f4, f6

nest$SF<-c(NA)
nest$SF[nest$OUTCOME_CODE_LIST %in% c("f","f1","f2","f3","f4","f6")]<-"fail"
nest$SF[nest$OUTCOME_CODE_LIST %in% c("s","s1")]<-"success"

nest.outcome<-nest[is.na(nest$SF)==F,]
nest.outcome$floorlat<-floor(nest.outcome$LATITUDE)
nest.outcome$floorlong<-floor(nest.outcome$LONGITUDE)

lats<-(25:53)
long<-c(-159:-53)[35:91]
lat.failrate<-c()
for(i in 1:length(lats)){
  sub.nest<-nest.outcome[nest.outcome$floorlat==lats[i],]
  lat.failrate[i]<-sum(sub.nest$SF=="fail")/length(sub.nest$SF)
}

plot(lats,lat.failrate,xlab="latitude band",ylab="nest failure rate")
summary(lm(lat.failrate~lats))

long.failrate<-c()
for(i in 1:length(long)){
  sub.nest<-nest.outcome[nest.outcome$floorlong==long[i],]
  long.failrate[i]<-sum(sub.nest$SF=="fail")/length(sub.nest$SF)
}

plot(long,long.failrate,xlab="longitude band",ylab="nest failure rate")
summary(lm(long.failrate~long))


#basic check fail rate in ag and forest
ag.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-ag",]
sum(ag.nest$SF=="fail")/length(ag.nest$SF)

for.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-for",]
sum(for.nest$SF=="fail")/length(for.nest$SF)

grass.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-grass",]
sum(grass.nest$SF=="fail")/length(grass.nest$SF)

chap.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-chap",]
sum(chap.nest$SF=="fail")/length(chap.nest$SF)

des.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-des",]
sum(des.nest$SF=="fail")/length(des.nest$SF)

#f4 is cowbird. repeat everything for cowbird
nest$SF<-c(NA)
nest$SF[nest$OUTCOME_CODE_LIST %in% c("f4")]<-"fail"
nest$SF[nest$OUTCOME_CODE_LIST %in% c("s","s1","f","f1","f2","f3","f6")]<-"success"

nest.outcome<-nest[is.na(nest$SF)==F,]
nest.outcome$floorlat<-floor(nest.outcome$LATITUDE)
nest.outcome$floorlong<-floor(nest.outcome$LONGITUDE)

lats<-(25:53)
long<-c(-159:-53)[35:91]
lat.failrate<-c()
for(i in 1:length(lats)){
  sub.nest<-nest.outcome[nest.outcome$floorlat==lats[i],]
  lat.failrate[i]<-sum(sub.nest$SF=="fail")/length(sub.nest$SF)
}

plot(lats,lat.failrate,xlab="latitude band",ylab="cowbird failure rate")
summary(lm(lat.failrate~lats^2))

long.failrate<-c()
for(i in 1:length(long)){
  sub.nest<-nest.outcome[nest.outcome$floorlong==long[i],]
  long.failrate[i]<-sum(sub.nest$SF=="fail")/length(sub.nest$SF)
}

plot(long,long.failrate,xlab="longitude band",ylab="cowbird failure rate")
summary(lm(long.failrate~long))


#basic check cowbird fail rate in ag and forest
ag.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-ag",]
sum(ag.nest$SF=="fail")/length(ag.nest$SF)

for.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-for",]
sum(for.nest$SF=="fail")/length(for.nest$SF)

grass.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-grass",]
sum(grass.nest$SF=="fail")/length(grass.nest$SF)

chap.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-chap",]
sum(chap.nest$SF=="fail")/length(chap.nest$SF)

des.nest<-nest.outcome[nest.outcome$HABITAT_CODE_1=="NW-des",]
sum(des.nest$SF=="fail")/length(des.nest$SF)
#check nest box vs not

#nest boxes:
253853/nrow(nest.outcome)

plot(summary(factor(substr(nest.outcome$OBS_DT,6,9)))[2:22],xlab="year, 2000-2020",ylab="number of nests")
