
#setwd("C:/Users/toelch/Dropbox/DMC_Europe_2016-update/")
setwd("C:/Users/ulf/Dropbox/DMC_Europe_2016-update/")
# Current working directory must be set to the top-level folder  
# containing the dmc and tutorial subfolders 
source ("tutorial/file_utils.R")
load_model ("lba_B.R")

#S#R#RT
#create data
help.data<-subset(my.data,participant==unique(my.data$participant)[5])
help.data<-subset(help.data,norm3=="incong")

S<-ifelse(help.data$correct=="left","s1","s2")
R<-ifelse(help.data$key_resp_direction.keys=="left","r1","r2")
RT<-help.data$key_resp_direction.rt
data.model<-data.frame(S=S,R=R,RT=RT)
rm(S,R,RT)
#load_data ("dmc_3_3.RData")

# NB: For the LBA one of B, mean_v or sd_v must be fixed in at least one
#     cell of the design. In this case we fix sd_v for both cells
p.map <- list(A="1",B="1",mean_v="M",sd_v="1",t0="1",st0="1")
const <- c(st0=0,sd_v=1)

# Same simple design as for previous LNR examples
model <- model.dmc(p.map,constants=const,match.map=list(M=list(s1=1,s2=2)),
                   factors=list(S=c("s1","s2")),responses=c("r1","r2"),type="norm")


# Simulate some data, with around 65% accuracy
# p.vector  <- c(A=.25,B=.35,mean_v.true=1,mean_v.false=.25,t0=.2)
# data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)
data.model <- data.model.dmc(data.model,model)
# Data distributions similar to LNR model
par(mfrow=c(1,2))
plot.cell.density(data.cell=data.model[data.model$S=="s1",],C="r1",xlim=c(0,2))
plot.cell.density(data.cell=data.model[data.model$S=="s2",],C="r2",xlim=c(0,2))

# Slow errors
crct <- (data.model$S=="s1" & data.model$R=="r1") |
  (data.model$S=="s2" & data.model$R=="r2")
round(tapply(data.model$RT,list(data.model$S,C=crct),mean),2)

# Give t0 a uniform prior from 0.1-1s, other priors normal, truncated
# below for A and B as they must be positive, unbounded for the v
p.prior <- prior.p.dmc(
  dists = c("tnorm","tnorm","tnorm","tnorm","beta"),
  p1=c(A=.3,B=.3,mean_v.true=1,mean_v.false=0,t0=1),                           
  p2=c(1,1,3,3,1),lower=c(0,0,NA,NA,.1),upper=c(NA,NA,NA,NA,1)
)

par(mfcol=c(2,3)); for (i in names(p.prior)) plot.prior(i,p.prior)


# Parameters of the LBA are more strongly correlated than those of the LNR
# hence longer burnin and more stuck chains are to be expected, so try a longer
# burnin run than for LNR.
samples <- samples.dmc(nmc=400,p.prior,data.model)
samples <- run.dmc(samples, report = 25, cores=4,p.migrate=.05)

plot.dmc(samples,layout=c(3,4))
plot.dmc(samples,layout=c(3,4),start=300,smooth=FALSE)

# Looks like burnt in, so get a longer run without migration
samples2 <- run.dmc(samples.dmc(nmc=1000,samples=samples), 
                    cores=4,report=25)
plot.dmc(samples1,layout=c(3,4),smooth=FALSE)

# R-hat shows whole series is close to converged
gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)

# Add 500 more to see if that settles things down (longer series often help)
samples2 <- run.dmc(samples.dmc(nmc=500,samples=samples1,add=TRUE), 
                    cores=4,report=50)
plot.dmc(samples2,layout=c(3,4),smooth=FALSE)

# Now reports converged
gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)

# Now have over 500 of each type
effectiveSize(theta.as.mcmc.list(samples1))

# Looking at autocorrelations
acf.dmc(samples,chain=1,par="A")

# Confirm that no chains are stuck
pick.stuck.dmc(samples1,cut=10,verbose=TRUE)


# Tabled estimates confirm some inaccuracy in estimates of A, B, and 
# mean_v.false with mean_v and t0 parameters relatively well recovered.
summary.dmc(samples2)

# Strong correlations are very evident.
pairs.dmc(samples2)

# NB: When there are more conditions in the experiment parameter correlations
#     reduce and parameters become easier to estimate (at least as long as 
#     there are a reasonable number of observations per condition) and 
#     constraints can be imposed across conditions (e.g., a bias manipulation
#     only affecting thresholds and a stimulus manipulation only affecting
#     rates).

# Sample posterior predictive to check fit
pp <- post.predict.dmc(samples2)

# Good fit to pdf
plot.pp.dmc(pp)

# And cdf
plot.pp.dmc(pp,"cdf")

# Posterior predictive p value for robust skew
ppp.dmc(samples2,plot.density=TRUE,
        fun=function(data) diff(diff(quantile(data$RT,probs=c(.25,.5,.75)))) )
# [1] 0.35   [indicative only]

# save_data (samples,samples1,samples2,pp,file="dmc_3_3.RData")


#simulate data
# Simulate some data
p.vector2  <- c(A=1.7,B=2.6,mean_v.true=3.3,mean_v.false=2.2,t0=.9)
data.model2 <- data.model.dmc(simulate.dmc(p.vector2,model,n=1e5),model)
par(mfrow=c(1,1))
plot.cell.density(data.cell=data.model2[data.model2$S=="s1",],C="r1",xlim=c(0,2))
plot.cell.density(data.cell=data.model2[data.model2$S=="s2",],C="r2",xlim=c(0,2))
get.prop.corr<-function(RT.thresh){
  get.values<-subset(data.model2,RT>RT.thresh-0.01&RT<RT.thresh+0.01)
  a<-(sum(get.values$S==ifelse(get.values$R=="r1","s1","s2"))/nrow(get.values))
  b<-(sum(get.values$S==ifelse(get.values$R=="r1","s2","s1"))/nrow(get.values))
  return(a)
}
help.data$ev<-0
for (i in 1:nrow(help.data))
help.data$ev[i]<-get.prop.corr(help.data$key_resp_direction.rt[i])

plot.cell.density(data.cell=data.model[data.model$S=="s1",],C="r1",xlim=c(0,2))
plot.cell.density(data.cell=data.model[data.model$S=="s2",],C="r2",xlim=c(0,2))
plot(data.model$RT~help.data$zConf,xlim=c(-2,1))
help.data$ev1<-(data.model$RT-0.24)*0.75+2
help.data$ev2<-(data.model$RT-0.24)*2+2

hh<-subset(help.data,PDW.response!="0.0")
hh<-help.data
as.numeric(help.data$PDW.rt)
ggplot(aes(y=ev,x=zConf,col=as.numeric(PDW.rt)),data=subset(hh,as.numeric(PDW.rt)<3))+geom_point()
plot(as.numeric(residuals(lm(hh$ev~hh$zConf)))~as.numeric(hh$PDW.rt))
plot(((data.model$RT-0.24)*0.75*help.data$key_resp_direction.corr)+((data.model$RT-0.24)*2*abs(help.data$key_resp_direction.corr-1))~help.data$zConf,xlim=c(-2,1))
plot(((data.model$RT-0.24)*2*abs(help.data$key_resp_direction.corr-1))~help.data$zConf,xlim=c(-2,1))

min()
summary(lm(((data.model$RT-0.88)*1.88*help.data$key_resp_direction.corr)+((data.model$RT-0.88)*3.04*abs(help.data$key_resp_direction.corr-1))~help.data$zConf))
summary(lm(data.model$RT~help.data$zConf))
hist(2*(data.model$RT-0.24))
hist(help.data$zConf)
help.data$key_resp_direction.corr


results<-summary.dmc(samples2)
A<-results$statistics[1,1]
B<-results$statistics[2,1]
v.true<-results$statistics[3,1]
v.false<-results$statistics[4,1]
t0<-results$statistics[5,1]
ev<-rep(0,nrow(data.model))
for(i in 1:nrow(data.model)){
  RT<-data.model$RT[i]+as.numeric(help.data$PDW.rt[i])
if(help.data$key_resp_direction.corr[i]==1){
  v1<-v.true
  v2<-v.false
  v_hat<-calc_trial_drift(A,B,v1,t0,RT)[1]
  A_hat<-calc_trial_drift(A,B,v1,t0,RT)[2]
  ev1<-(RT-t0)*v_hat+A_hat
  ev2<-(RT-t0)*v2+A/2
  ev[i]<-log(ev1/ev1+ev2)
}
if(help.data$key_resp_direction.corr[i]==0){
  v2<-v.true
  v1<-v.false
  v_hat<-calc_trial_drift(A,B,v1,t0,RT)[1]
  A_hat<-calc_trial_drift(A,B,v1,t0,RT)[2]
  ev1<-(RT-t0)*v_hat+A_hat
  ev2<-(RT-t0)*v2+A/2
  ev[i]<-log(ev1/ev1+ev2)
}
}
plot.data<-data.frame(ev=ev,conf=help.data$zConf,corr=help.data$key_resp_direction.corr,RT=help.data$key_resp_direction.rt)
plot.data<-plot.data[-which(plot.data$conf==min(plot.data$conf)),]
plot.data<-plot.data[-which(plot.data$conf>1.5),]
plot.data<-plot.data[-which(plot.data$ev>0.6),]
plot(plot.data)
m.1<-(lm(conf~ev,data=plot.data))
ggplot(aes(y=conf,x=as.numeric(help.data$PDW.rt)),data=plot.data)+geom_point()+stat_smooth(method="lm")
hist(as.numeric(help.data$PDW.rt))
summary(lm(help.data$zConf~plot.data$ev))
require(car)
qqPlot(residuals(m.1))
summary(m.1)



