####Rahel  Sollman´s approach for spatial autocorrelation
data <- read.csv("/Users/DrBohemio/Dropbox/NestWatch/data/coor.utm.csv")
data <- read.csv("/Users/tigerfawn/Dropbox/NestWatch/data/coor.utm.csv")
data <- subset(data,xcoord> (-6e+06))

data$xcoord <- data$xcoord-min(data$xcoord)
plot(data$xcoord,data$ycoord)

data$ID <- as.factor(paste(data$data.LONGITUDE,data$data.LATITUDE,sep="_"))

data.2 <- data.frame(ID=data$ID,xcoord=data$xcoord,ycoord=data$ycoord)


load("/Users/DrBohemio/Google Drive/NestwatchProject/Data/oneAttemptOneRow_ModisWorldClimElevationNLCD.Rdata")
data.nest<-processed
load("/Users/tigerfawn/Dropbox/NestWatch/data/oneAttemptOneRow_ModisWorldClimElevationNLCD.RData")
data.nest <- processed
str(data.nest)

data.nest$ID <- as.factor(paste(data.nest$lon,data.nest$lat,sep="_"))

data.nest.2 <- merge(data.nest,data.2, by="ID")
table(duplicated(data.nest.2))
data.nest.2 <- unique(data.nest.2)

data.success <- data.frame(UnCoor=as.factor(data.nest.2$UnCoor),at_least_one_successs=data.nest.2$at_least_one_success,xcoord=data.nest.2$xcoord,ycoord=data.nest.2$ycoord)

table(is.na(data.success$at_least_one_success))
success.na <- is.na(data.success$at_least_one_success)
na.success <- which(success.na==TRUE)

data.success <- data.success[-na.success,]
data.success$UnCoor <- as.factor(as.character(data.success$UnCoor))
d <- data.success



#d <- data.success[c(sample(1:nrow(data.success),8000)),]





c <- data.success[-c(1:nrow(data.success)),]

spaut.success <- data.success[,-2]
spaut.success$prop_success <- as.numeric(0)
spaut.success <- spaut.success [-c(1:nrow(spaut.success )),]

for(i in 1:nlevels(d$UnCoor)) {
  a <- subset (d, UnCoor==levels(d$UnCoor)[i])
  f <- a[,-2]
  f <- unique(f)
  b <- subset(d,xcoord<f$xcoord+100000)
  b <- subset(b, xcoord>f$xcoord-100000)
  b <- subset(b,ycoord<f$ycoord+100000)
  b <- subset(b, ycoord>f$ycoord-100000)
  
  
  prop_success <- sum(b$at_least_one_success)/nrow(b)
  f$prop_success <- prop_success
  spaut.success <- rbind(spaut.success,f)
  print(i)
}


write.csv(spaut.success,"sollman_spaut_success.csv")

data.failure <- data.frame(UnCoor=as.factor(data.nest.2$UnCoor),at_least_one_failure=data.nest.2$at_least_one_failure,xcoord=data.nest.2$xcoord,ycoord=data.nest.2$ycoord)

table(is.na(data.failure$at_least_one_failure))
failure.na <- is.na(data.failure$at_least_one_failure)
na.failure <- which(failure.na==TRUE)

data.failure <- data.failure[-na.failure,]
data.failure$UnCoor <- as.factor(as.character(data.failure$UnCoor))
d <- data.failure



#d <- data.failure[c(sample(1:nrow(data.failure),8000)),]
#c <- data.failure[-c(1:nrow(data.failure)),]

spaut.failure <- data.failure[,-2]
spaut.failure$prop_failure <- as.numeric(0)
spaut.failure <- spaut.failure [-c(1:nrow(spaut.failure )),]

for(i in 1:nlevels(d$UnCoor)) {
  a <- subset (d, UnCoor==levels(d$UnCoor)[i])
  f <- a[,-2]
  f <- unique(f)
  b <- subset(d,xcoord<f$xcoord+100000)
  b <- subset(b, xcoord>f$xcoord-100000)
  b <- subset(b,ycoord<f$ycoord+100000)
  b <- subset(b, ycoord>f$ycoord-100000)
  
  
  prop_failure <- sum(b$at_least_one_failure)/nrow(b)
  f$prop_failure <- prop_failure
  spaut.failure <- rbind(spaut.failure,f)
  print(i)
}


write.csv(spaut.failure,"sollman_spaut_failure.csv")

####Other approach


for(i in 1:nlevels(d$UnCoor)) {
  a <- subset (d, UnCoor==levels(d$UnCoor)[i])
  f <- a[,-2]
  f <- unique(f)
  for(j in 1:nrow(d)) {
    
    b <- d[j,]
    if  (f$xcoord-b$xcoord < 200000 & f$ycoord-b$ycoord< 200000) {c <- rbind (c,b)}
  }
  
  prop_success <- sum(c$at_least_one_success)/nrow(c)
  f$prop_success <- prop_success
  spaut.success <- rbind(spaut.success,f)
  c <- data.success[-c(1:nrow(data.success)),]
  print(i)
}












