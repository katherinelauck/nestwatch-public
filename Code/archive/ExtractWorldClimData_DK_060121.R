library(raster)
library(geosphere)

bio1=raster("~/nestwatch/Data/archive/Worldclim/wc2.1_30s_bio_1.tif") # Raster of temp data to extract

# Download nest dataset and build small dataset of unique sites + coordinates
load("~/nestwatch/Data/archive/raw-collapsed.RData")
dd <- nest
UnCoors=sort(unique((dd$UnCoor)))

SeparatedCoords=strsplit(as.character(UnCoors),"_")
SeparatedCoords_Unlisted=t(matrix(unlist(SeparatedCoords),nrow=2,ncol=length(UnCoors)))

dd2=as.data.frame(cbind(as.character(UnCoors), SeparatedCoords_Unlisted))
dd2[,2]=as.numeric(as.character(dd2[,2]))
dd2[,3]=as.numeric(as.character(dd2[,3]))
colnames(dd2)=c("UnCoor","Lon","Lat")

# Make spatial points dataset and extract the temperature values 
points=SpatialPointsDataFrame(coords= cbind(dd2 $Lon, dd2 $Lat),data= dd2,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
temp_values=extract(bio1,points)  

# Need to iteratively check to see which points are over water and need to be extracts after slightly moving them
dd2$UpdateLat=dd2$Lat

dd2$UpdateLat[which(is.na(temp_values)==TRUE)]=dd2$UpdateLat[which(is.na(temp_values)==TRUE)]+.05

points=SpatialPointsDataFrame(coords= cbind(dd2 $Lon, dd2 $UpdateLat),data= dd2,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
temp_values_2=extract(bio1,points)
length(which(is.na(temp_values)==TRUE))-length(which(is.na(temp_values_2)==TRUE)) # 73 points moved 0.05 degrees North

dd2$UpdateLat[which(is.na(temp_values_2)==TRUE)]=dd2$UpdateLat[which(is.na(temp_values_2)==TRUE)]-.1
points=SpatialPointsDataFrame(coords= cbind(dd2 $Lon, dd2 $UpdateLat),data= dd2,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
temp_values_3=extract(bio1,points)  
length(which(is.na(temp_values_2)==TRUE))-length(which(is.na(temp_values_3)==TRUE)) # 23 points moved 0.05 degrees South

dd2$UpdateLat[which(is.na(temp_values_3)==TRUE)]=dd2$UpdateLat[which(is.na(temp_values_3)==TRUE)]+.55
points=SpatialPointsDataFrame(coords= cbind(dd2 $Lon, dd2 $UpdateLat),data= dd2,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
temp_values_4=extract(bio1,points)  
length(which(is.na(temp_values_3)==TRUE))-length(which(is.na(temp_values_4)==TRUE)) # Last 4 points moved 0.5 degree North 

dd2$AnnualTave_World=temp_values_4

write.csv(dd2,"Data/archive/AnnualTave_WorldClim.csv")
