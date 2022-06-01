
bounded.hpp<-function(sample, lower = 0, upper = 1, mode = TRUE, 
                      HPDcoverage = 0.95, codaHPD = TRUE, n = 1024) {
  reflectedSample <- c( if(lower > -Inf) lower - (sample-lower) else numeric(),
                        sample,
                        if(upper < Inf) upper + (upper-sample) else numeric() )
  
  griddedDensityCall <- quote(density(reflectedSample, n = n, bw = "SJ"))
  if(lower > -Inf) griddedDensityCall[['from']] <- lower
  if(upper < Inf) griddedDensityCall[['to']] <- upper
  griddedDensity <- eval(griddedDensityCall)
  
  iMax <- which.max(griddedDensity$y)
  argMaxX <- griddedDensity$x[iMax] ## this is the value of the mode, to 
  ## within precision of n (default=1024 points)
  if(is.numeric(HPDcoverage)) {
    estimatedPDF <- (griddedDensity$y / sum(griddedDensity$y))
    
    alpha <- 1-HPDcoverage
    
    funThatWeWantToSolveForZero <- function(densityLevel) {
      whichAbove <- which(estimatedPDF >= densityLevel)
      if(length(whichAbove)==0) return(1-alpha)
      if(length(whichAbove)==n) return(-alpha)
      interval <- griddedDensity$x[range(whichAbove)]
      mean(sample < interval[1] | sample > interval[2])  - alpha
    }
    
    HPDsolution <- uniroot(funThatWeWantToSolveForZero, lower = 0, upper = estimatedPDF[iMax])
    HPDdensityLevel <- HPDsolution$root
    whichAbove <- which(estimatedPDF > HPDdensityLevel)
    HPDinterval <- griddedDensity$x[range(whichAbove)]
  } else {
    HPDinterval <- NULL
  }
  
  list(mode = argMaxX, HPDinterval = HPDinterval)
}

res<-readRDS("~/Documents/nestwatch/results/Question 5/sp_analysis_jags_050721.RDS")
res<-readRDS("~/Documents/nestwatch/results/q5/sp_analysis_jags_121021.RDS")
chains<-res$BUGSoutput$sims.matrix
chains[,"sig.tmaxxLUxSp"]

##analyze the significance of sigma term for tmax x LU x Sp
bounded.hpp (chains[,"sig.tmaxxLUxSp"],lower=0,upper=Inf)
bounded.hpp (chains[,"sig.tmaxxLUxSp2"],lower=0,upper=Inf)
bounded.hpp (chains[,"sig.tmaxxLUxSp3"],lower=0,upper=Inf)
bounded.hpp (chains[,"sig.tmaxxLUxSp4"],lower=0,upper=Inf)
