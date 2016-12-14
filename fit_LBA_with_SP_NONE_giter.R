#fit LBA to incong and none trials
setwd("C:/Users/toelch/Dropbox/DMC_Europe_2016-update/")
setwd("C:/Users/ulf/Dropbox/DMC_Europe_2016-update/")
# Current working directory must be set to the top-level folder  
# containing the dmc and tutorial subfolders 
source ("tutorial/file_utils.R")
load_model ("lba_B.R")
#prepare data
help.data<-subset(my.data,participant==unique(my.data$participant)[5])
help.data<-subset(help.data,norm2=="NONE")
S<-ifelse(help.data$correct=="left","s1","s2")
SI<-ifelse(help.data$social==0,"none",
           ifelse(help.data$social3=="valid",paste(help.data$correct,"SI",sep="_"),
                  paste(ifelse(help.data$correct=="left","right","left"),"SI",sep="_")))
R<-ifelse(help.data$key_resp_direction.keys=="left","r1","r2")
RT<-help.data$key_resp_direction.rt
data.model<-data.frame(S=S,R=R,RT=RT,SI=SI)
rm(S,R,RT,SI)
# Model flags
match.map <- list(M=list(s1="r1", s2="r2"))
responses <- c("r1","r2")
#

# models and priors

## SP ----

factors <- list(S=c("s1","s2"),SI=c("none","left_SI","right_SI"))
p.map <- list(A="1",B=c("SI","R"),mean_v="M",sd_v="1",t0="1",st0="1")
const <- c(st0=0,sd_v=1)

model.1<-model.dmc(p.map,factors,responses,match.map,const)

p.prior <- prior.p.dmc(
  dists = c("tnorm","tnorm","tnorm","tnorm","tnorm","tnorm","tnorm","tnorm","tnorm","beta"),
  p1=c(A=.3,
       B.none.r1=0.3,B.none.r2=0.3,B.left_SI.r1=0.3,B.left_SI.r2=0.3,B.right_SI.r1=0.3,B.right_SI.r2=0.3,
       mean_v.true=1,mean_v.false=0,t0=1),                           
  p2=c(1,1,1,1,1,1,1,3,3,1),lower=c(0,0,0,0,0,0,0,NA,NA,.1),upper=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,1)
)
data.model <- data.model.dmc(data.model,model.1)

plot.cell.density(data.cell=data.model[data.model$S=="s1",],C="r1",xlim=c(0,2))
plot.cell.density(data.cell=data.model[data.model$S=="s2",],C="r2",xlim=c(0,2))
par(mfcol=c(2,3)); for (i in names(p.prior)) plot.prior(i,p.prior)

samples <- samples.dmc(nmc=400,p.prior,data.model)
samples <- run.dmc(samples, report = 25, cores=4,p.migrate=.05)
plot.dmc(samples,layout=c(3,4))

summary.dmc(samples)




p.vector  <- c(A=.25,B=.5,mean_v.true=1,mean_v.false=.25,t0=.2)
data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)

