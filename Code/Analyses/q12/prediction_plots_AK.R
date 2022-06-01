library(gtools)
library(lme4)

maxsq<-readRDS("results/Question 1-2/success~stdmax2laydate2way.AK.RDS")

nest<-readRDS("Data/success-cleaned.rds")
nest$Tmax_std_gridmet_sq<-nest$Tmax_std_gridmet*nest$Tmax_std_gridmet

Tmax_std<-seq(-5,5,.1)

data=expand.grid(Tmax_std_gridmet = Tmax_std, NewLU1= c("Ag","Forest","Natural_open","Human"),pcpbefore_raw_gridmet = 0,NLCD_p_forest= 0, NLCD_p_human=0, NLCD_p_ag=0, substrate_binary=1,laydate_scaled=0,at_least_one_success=1)

data=cbind(data[,1],data[,1]^2,data[,2:9])
colnames(data)[1]<-"Tmax_std_gridmet"

colnames(data)[2]="Tmax_std_gridmet_sq"

mm=model.matrix(terms(maxsq), data)

data$Mean_Alpha = mm %*% fixef(maxsq) 
pvar1 <- diag(mm %*% vcov(maxsq) %*% t(mm))
y=data $Mean_Alpha
plow=data $Mean_Alpha-2*sqrt(pvar1)
phigh=data $Mean_Alpha +2*sqrt(pvar1)


#"Ag","Forest","Natural_open","Human"

plot(Tmax_std,inv.logit(y[1:101]),ylim=c(0.6,0.8),col="red")
polygon(c(Tmax_std, rev(Tmax_std)), c(inv.logit(plow[1:101]) ,rev(inv.logit(phigh[1:101]))), col = rgb(1, 0, 0,0.5))

points(Tmax_std,inv.logit(y[102:202]),col="blue")
polygon(c(Tmax_std, rev(Tmax_std)), c(inv.logit(plow[102:202]) ,rev(inv.logit(phigh[102:202]))), col = rgb(0.5, 0.5, 0.9,0.5))

points(Tmax_std,inv.logit(y[203:303]),col="yellow")
polygon(c(Tmax_std, rev(Tmax_std)), c(inv.logit(plow[203:303]) ,rev(inv.logit(phigh[203:303]))), col = rgb(0.8, 0.8, 0.1,0.5))

points(Tmax_std,inv.logit(y[304:404]),col="green")
polygon(c(Tmax_std, rev(Tmax_std)), c(inv.logit(plow[304:404]) ,rev(inv.logit(phigh[304:404]))), col = rgb(0.5, 0.8, 0.1,0.1))


summ<-data.frame(summary(maxsq)[[10]])

test<-head(nest)
test<-test[,c(9,12,30)]
test$pcpbefore_raw_gridmet<-0
test$NLCD_p_forest<-0
test$NLCD_p_human<-0
test$NLCD_p_ag<-0
test$laydate_scaled<-0
test$substrate_binary<-0.5

est.Tmax<-summ$Estimate[2]
est.Tmax_sq<-summ$Estimate[6]

est.Tmax.low<-summ$Estimate[2]-2*summ$Std..Error[2]
est.Tmax_sq.low<-summ$Estimate[6]-2*summ$Std..Error[6]
est.Tmax.high<-summ$Estimate[2]+2*summ$Std..Error[2]
est.Tmax_sq.high<-summ$Estimate[6]+2*summ$Std..Error[6]

est.Ag<-summ$Estimate[3]
est.Natural_open<-summ$Estimate[4]
est.Human<-summ$Estimate[5]

est.Ag.low<-summ$Estimate[3]-2*summ$Std..Error[3]
est.Natural_open.low<-summ$Estimate[4]-2*summ$Std..Error[4]
est.Human.low<-summ$Estimate[5]-2*summ$Std..Error[5]
est.Ag.high<-summ$Estimate[3]+2*summ$Std..Error[3]
est.Natural_open.high<-summ$Estimate[4]+2*summ$Std..Error[4]
est.Human.high<-summ$Estimate[5]+2*summ$Std..Error[5]

est.TmaxxAg<-summ$Estimate[13]
est.TmaxxNatural_open<-summ$Estimate[14]
est.TmaxxHuman<-summ$Estimate[15]

est.TmaxxAg.low<-summ$Estimate[13]-2*summ$Std..Error[13]
est.TmaxxNatural_open.low<-summ$Estimate[14]-2*summ$Std..Error[14]
est.TmaxxHuman.low<-summ$Estimate[15]-2*summ$Std..Error[15]
est.TmaxxAg.high<-summ$Estimate[13]+2*summ$Std..Error[13]
est.TmaxxNatural_open.high<-summ$Estimate[14]+2*summ$Std..Error[14]
est.TmaxxHuman.high<-summ$Estimate[15]+2*summ$Std..Error[15]

est.Tmax_sqxAg.low<-summ$Estimate[16]-2*summ$Std..Error[16]
est.Tmax_sqxNatural_open.low<-summ$Estimate[17]-2*summ$Std..Error[17]
est.Tmax_sqxHuman.low<-summ$Estimate[18]-2*summ$Std..Error[18]
est.Tmax_sqxAg.high<-summ$Estimate[16]+2*summ$Std..Error[16]
est.Tmax_sqxNatural_open.high<-summ$Estimate[17]+2*summ$Std..Error[17]
est.Tmax_sqxHuman.high<-summ$Estimate[18]+2*summ$Std..Error[18]

est.Tmax_sqxAg<-summ$Estimate[16]
est.Tmax_sqxNatural_open<-summ$Estimate[17]
est.Tmax_sqxHuman<-summ$Estimate[18]

Tmax_std<-seq(-5,5,.1)
Tmax_std_sq<-Tmax_std^2
test.pred<-data.frame(Tmax_std,Tmax_std_sq)

test.pred$Ag.pred<-
  est.Tmax*test.pred$Tmax_std +
  est.Tmax_sq*test.pred$Tmax_std_sq +
  est.Ag+
  est.TmaxxAg*test.pred$Tmax_std +
  est.Tmax_sqxAg*test.pred$Tmax_std_sq
  
test.pred$Ag.pred.low<-
  est.Tmax.low*test.pred$Tmax_std +
  est.Tmax_sq.low*test.pred$Tmax_std_sq +
  est.Ag.low+
  est.TmaxxAg.low*test.pred$Tmax_std +
  est.Tmax_sqxAg.low*test.pred$Tmax_std_sq

test.pred$Ag.pred.high<-
  est.Tmax.high*test.pred$Tmax_std +
  est.Tmax_sq.high*test.pred$Tmax_std_sq +
  est.Ag.high+
  est.TmaxxAg.high*test.pred$Tmax_std +
  est.Tmax_sqxAg.high*test.pred$Tmax_std_sq


test.pred$For.pred<-
  est.Tmax*test.pred$Tmax_std +
  est.Tmax_sq*test.pred$Tmax_std_sq 

test.pred$For.pred.low<-
  est.Tmax.low*test.pred$Tmax_std +
  est.Tmax_sq.low*test.pred$Tmax_std_sq 

test.pred$For.pred.high<-
  est.Tmax.high*test.pred$Tmax_std +
  est.Tmax_sq.high*test.pred$Tmax_std_sq 


test.pred$natop.pred<-
  est.Tmax*test.pred$Tmax_std +
  est.Tmax_sq*test.pred$Tmax_std_sq +
  est.Natural_open+
  est.TmaxxNatural_open*test.pred$Tmax_std +
  est.Tmax_sqxNatural_open*test.pred$Tmax_std_sq

test.pred$hum.pred<-
  est.Tmax*test.pred$Tmax_std +
  est.Tmax_sq*test.pred$Tmax_std_sq +
  est.Human+
  est.TmaxxHuman*test.pred$Tmax_std +
  est.Tmax_sqxHuman*test.pred$Tmax_std_sq

plot(test.pred$Tmax_std,inv.logit(test.pred$Ag.pred),col="blue")
points(test.pred$Tmax_std,inv.logit(test.pred$Ag.pred.low))
points(test.pred$Tmax_std,inv.logit(test.pred$Ag.pred.high))

plot(test.pred$Tmax_std,inv.logit(test.pred$For.pred),col="blue")
points(test.pred$Tmax_std,inv.logit(test.pred$For.pred.low))
points(test.pred$Tmax_std,inv.logit(test.pred$For.pred.high))


points(test.pred$Tmax_std,inv.logit(test.pred$For.pred))
points(test.pred$Tmax_std,inv.logit(test.pred$natop.pred),col="red")
points(test.pred$Tmax_std,inv.logit(test.pred$hum.pred),col="yellow")


library(sjPlot)
plot_model(maxsq, type="int")


