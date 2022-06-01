library(raster)
library(lme4)
library(scales)
library(geosphere)

climGenerator=function(months,climatetype,climate_directory,method,locations){
  setwd(climate_directory)
  climate_files=list.files(climate_directory)
  climate_files=climate_files[grep(climatetype,climate_files)]
  
  nsites=dim(locations@data)[1]
  climates=array(dim=c(nsites,0))
  
  for (i in 1:length(months)){
    clim_month_raster=raster(climate_files[grep(months[i],climate_files)])
    climates=cbind(climates,extract(clim_month_raster,locations))
  }
  
  if(method=="mean"){
    climates_summarized=apply(climates,1,mean)
  }
  if(method=="sum"){
    climates_summarized=apply(climates,1,sum)
  }
  if(method=="max"){
    climates_summarized=apply(climates,1,max)
  }
  if(method=="min"){
    climates_summarized=apply(climates,1,min)
  }
  climates_summarized
}
