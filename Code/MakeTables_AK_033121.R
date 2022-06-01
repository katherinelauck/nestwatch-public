library("lme4")
library("sjPlot")
library("lubridate")

##EXAMPLE CODE, for success~stdmax2laydate2way.AK.RDS

#read in the model result
modresult<-readRDS("~/Documents/nestwatch/results/Question 1-2/success~stdmax2laydate2way.AK.RDS")

#take a look at the summary
summary(modresult)

#check assumptions

plot(baseline)

library(arm)
binnedplot(fitted(baseline),resid(baseline))

qqnorm(resid(baseline))

install.packages("DHARMa")

library(DHARMa)

simulateResiduals(baseline) #creates scaled (quantile) residuals through a default 250 simulations (which can be modified)

plotSimulatedResiduals(baseline) #provides qqplot and residuals vs predicted plots to determine deviations from normality

#Goodness of fit tests:
testUniformity(baseline)


###make tables for likelihood ratio tests
#a. LRT and delta AIC of full model vs model without any Tmax interaction with squared terms
LRTCompare<-readRDS("~/Documents/nestwatch/results/Question 1-2/success~stdmax2laydate2way.LRT.AK.RDS")
anova(baseline, LRTCompare)

#b. LRT and delta AIC of full model vs model without any squared terms whatsoever 
LRTCompare<-readRDS("~/Documents/nestwatch/results/Question 1-2/success~stdmaxlaydate2way.AK.RDS")
anova(baseline, LRTCompare)

#c. LRT and delta AIC of model vs. model without any Tmax interaction terms (both squared and linear terms), but with singular terms
LRTCompare<-readRDS("~/Documents/nestwatch/results/Question 1-2/success~stdmax2laydate_LRT_AK.RDS")
anova(baseline, LRTCompare)


LRTCompare<-readRDS("~/Documents/nestwatch/results/Question 1-2/failure~stdmaxlaydate2way.AK.RDS")
