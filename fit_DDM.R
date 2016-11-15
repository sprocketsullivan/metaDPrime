######################################################
#FIt DDM
######################################################

# NOTE: if you want to use more than one factor to model a parameter,
# keep in mind to write them in p.map and factors in the same order!
# AND S must always be the first!!!

#######################################################

# CLEAR WORKSPACE

rm(list=ls())

######################################################

# DEFINE FUNCTION FOR LATER
#(as writen by Folco?)

check.migration.dmc<-function(samples,cut=30, verbose=FALSE){
  # check if at least one chain has not converged with the others
  report<-FALSE
  for (i in 1:samples$n.pars){
    dist<-diff(sort(as.array(window(theta.as.mcmc.list(samples)[,i],start=samples$nmc))))
    test<-max(dist)/median(dist)
    if(test>cut) {report<-TRUE
    if(verbose)     cat(samples$p.names[i], 'parameter chains have not converged. Index value:',test,'\n')
    }
  }
  report
}

##########################################################

#######
#PRE-REQ
#####

## Import Packages
library(plyr); 
library(data.table)

## Load my.data (if it is not already present)
if (!exists("my.data")){
  load("confidenceData.RData")
  print("loading my.data")
}

## Download the DDM model 
# Should I put the DMC package also on github as well?

setwd('..\\DMC_160825') # right now links to my pernal folder
source ("tutorial/file_utils.R")
load_model ("ddm.R")
setwd('..\\metaDPrime') # right now links to my pernal folder

# convert data.frame to a better format
# do I need to do this?
# my.data<-data.table(my.data)

## PREALOCATION

# models and priors
model <- list()
p.prior <- list()

# Participants parameters
participants <- list()

# Information criterion and weights
WAIC<-list()
winner<-data.frame(stringsAsFactors = F)
tll<-list()

# Posterior-predictive
pp<-list() 

gelman<-list()
ES<-list()


######################################################

## DEFINE MODELs

# parameter type: z = starting point ; v = drift rate
# model: SP = StartingPoint ; DR = DriftRate ; SPr = StartingPointReduced ; DR = DriftRateReduced ; RE = Reduced; SPDR = Complete model

# Model flags
constants <- c(st0=0,d=0,sz=0.0) #sz=0.11
match.map <- list(M=list(s1="r1", s2="r2"))
responses <- c("r1","r2")
type <- "rd"

# Models and priors

# Starting point varies with cue, drift rate varies with norm
p.map <- list(a="1",v="N",z="C",d="1",sz = "1",sv="1",t0="1",st0="1")
factors <- list(S=c("s1","s2"), N=c('NONE', 'SAME', 'ONLY'), C=c('left','right'))

model$SPDR <- model.dmc(p.map,factors,responses,match.map,constants,type)

p1 <- c(a=1,v.NONE = 0, v.SAME = 0, v.ONLY = 0, z.left = 2, z.right = 2, sv=1,t0=1)
p2 <- c(a=1,v.NONE = 2, v.SAME = 2, v.ONLY = 2, z.left = 2, z.right = 2, sv=1,t0=1)
lower <- c(0,-5,-5,-5,NA,NA,0,NA) #lower <- c(0,-5,-5,-5,NA,NA,0,NA)
upper <- c(2, 5, 5, 5,NA,NA,2,NA) #upper <- c(2,5,5,5,NA,NA,2,NA)
dists <- c("tnorm","tnorm","tnorm","tnorm","beta","beta","tnorm","beta")

p.prior$SPDR <- prior.p.dmc(p1, p2, lower, upper, dists)


# z.Treshold varies with norm
p.map <- list(a="N",v="1",z="C",d="1",sz = "1",sv="1",t0="1",st0="1")
factors <- list(S=c("s1","s2"), N=c('NONE', 'SAME', 'ONLY'), C=c('left','right'))

model$SPDB <- model.dmc(p.map,factors,responses,match.map,constants,type)

p1 <- c(a.NONE=1, a.SAME=1, a.ONLY=1, v = 0, z.left = 2, z.right = 2, sv=1,t0=1)
p2 <- c(a.NONE=1, a.SAME=1, a.ONLY=1, v = 2, z.left = 2, z.right = 2, sv=1,t0=1)
lower <- c(0, 0, 0,-5,NA,NA,0,NA)
upper <- c(2, 2, 2, 5,NA,NA,2,NA)
dists <- c("tnorm","tnorm","tnorm","tnorm","beta","beta","tnorm","beta")

p.prior$SPDB <- prior.p.dmc(p1, p2, lower, upper, dists)


# Starting point 2 (SP varies with cue and norm)

p.map <- list(a="1",v="1",z=c("N","C"),d="1",sz = "1",sv="1",t0="1",st0="1")
factors <- list(S=c("s1","s2"), N=c('NONE', 'SAME', 'ONLY'), C=c('left','right'))

model$SPSP<- model.dmc(p.map,factors,responses,match.map,constants,type)

p1 <- c(a=1,v=0,z.NONE.left = 2, z.SAME.left = 2, z.ONLY.left = 2, z.NONE.right = 2, z.SAME.right = 2, z.ONLY.right = 2, sv=1,t0=1)
p2 <- c(a=1,v=2,z.NONE.left = 2, z.SAME.left = 2, z.ONLY.left = 2, z.NONE.right = 2, z.SAME.right = 2, z.ONLY.right = 2, sv=1,t0=1)
lower <- c(0,-5,NA,NA,NA,NA,NA,NA,0,NA)
upper <- c(2, 5,NA,NA,NA,NA,NA,NA,2,NA)
dists <- c("tnorm","tnorm","beta","beta","beta","beta","beta","beta","tnorm","beta")

p.prior$SPSP <- prior.p.dmc(p1, p2, lower, upper, dists)


# Starting point (SP varies with cue)

p.map <- list(a="1",v="1",z="C",d="1",sz = "1",sv="1",t0="1",st0="1")
factors <- list(S=c("s1","s2"), C=c('left','right'))

model$SP<- model.dmc(p.map,factors,responses,match.map,constants,type)

p1 <- c(a=1,v=0,z.left = 2, z.right = 2, sv=1,t0=1)
p2 <- c(a=1,v=2,z.left = 2, z.right = 2, sv=1,t0=1)
lower <- c(0,-5,NA,NA,0,NA)
upper <- c(2, 5,NA,NA,2,NA)
dists <- c("tnorm","tnorm","beta","beta","tnorm","beta")

p.prior$SP <- prior.p.dmc(p1, p2, lower, upper, dists)


# Reduced model (only the stimulus)

p.map <- list(a="1",v="1",z="1",d="1",sz = "1",sv="1",t0="1",st0="1")
factors <- list(S=c("s1","s2"))

model$RE <- model.dmc(p.map,factors,responses,match.map,constants,type)

p1 <- c(a=1,v=0,z = 2, sv=1,t0=1)                          
p2 <- c(a=1,v=2,z = 2, sv=1,t0=1)
lower <- c(0,-5,NA,0,NA)
upper <- c(2, 5,NA,2,NA)
dists <- c("tnorm","tnorm","beta","tnorm","beta")

p.prior$RE <- prior.p.dmc(p1, p2, lower, upper, dists)

#####################################################

## SET-UP DATA FILE (my.data.ddm)
# This file contains all the information necessary for the ddm analysis of the various models
# Has to be in a correct format

filtered.data <- subset(my.data, social!=0) # Remove trials with incongruent social information

#RT
RT <- filtered.data$key_resp_direction.rt

# RESPONSE (also ddm boundary)
R <- filtered.data$key_resp_direction.keys
R <- revalue(R, c("left" = "r1","right" = "r2")) #sets lower boundary to left and upper to right

# STIMULUS
S <- filtered.data$correct
revalue(S, c("left" = "s1", "right" = "s2"))  -> S

# NORM
N <- filtered.data$norm2 # norm

# SOCIAL CUES
C<-ifelse(filtered.data$SI_LO!=0&filtered.data$SI_LU!=0,"left","right")
C <- factor(C)#transform into factors VERY IMPORTANT for post.predict (do I need to do post predict?)

# participant ID
id <- filtered.data$participant

# CREATE my.data.ddm file
my.data.ddm <- data.frame(S, C, N, R, RT, id)

# delete rows with NA (if any)
my.data.ddm  <- na.omit(my.data.ddm)
my.data.ddm$R <- droplevels(my.data.ddm$R)


#####################################################################################################

## RUN THE ANALYSIS

select<-unique(my.data.ddm$id); # Folco had here sample(unique(...)) do I need it?

for(i in 1:length(select)){
  
  data2 <- subset(my.data.ddm,id==select[i])[1:5]
  row.names(data2) <- NULL
  
  #### SPDR model ####
  data = data.model.dmc(data2, model$SPDR)
  
  samples <- run.dmc(samples.dmc(nmc=400,p.prior$SPDR,data), report = 25, cores=4,p.migrate=.05)
  while (check.migration.dmc(samples, cut = 30) & samples$nmc<2000){
    samples <- run.dmc(samples.dmc(nmc=200,samples = samples, add = T), report = 100, cores=4,p.migrate=.05)}
  
  samples2 <- run.dmc(samples.dmc(nmc=1600,samples=samples),cores=4,report=100)
  
  participants$SPDR[[i]] <- summary.dmc(samples2)$statistics
  
  tll$SPDR[[i]] <- waic.dmc(trial_log_likes(samples2,thin_pointwise = 100),digits=2,save=TRUE)
  WAIC$SPDR[i] <- as.numeric(tll$SPDR[[i]][3])
  
  gelman$SPDR[[i]] <- gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)
  ES$SPDR[[i]] <- effectiveSize(theta.as.mcmc.list(samples2))
  #pp$SPDR[[i]] <- post.predict.dmc(samples2)
  
  
  #### SPDB model ####
  data = data.model.dmc(data2, model$SPDB)
  
  samples <- run.dmc(samples.dmc(nmc=400,p.prior$SPDB,data), report = 25, cores=4,p.migrate=.05)
  while (check.migration.dmc(samples, cut = 30) & samples$nmc<2000){
    samples <- run.dmc(samples.dmc(nmc=200,samples = samples, add = T), report = 100, cores=4,p.migrate=.05)}
  
  samples2 <- run.dmc(samples.dmc(nmc=1600,samples=samples),cores=4,report=100)
  
  participants$SPDB[[i]] <- summary.dmc(samples2)$statistics
  
  tll$SPDB[[i]] <- waic.dmc(trial_log_likes(samples2,thin_pointwise = 100),digits=2,save=TRUE)
  WAIC$SPDB[i] <- as.numeric(tll$SPDB[[i]][3])
  
  gelman$SPDB[[i]] <- gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)
  ES$SPDB[[i]] <- effectiveSize(theta.as.mcmc.list(samples2))
  #pp$SPDB[[i]] <- post.predict.dmc(samples2)
  
  #### SPSP model ####
  data = data.model.dmc(data2, model$SPSP)
  
  samples <- run.dmc(samples.dmc(nmc=400,p.prior$SPSP,data), report = 25, cores=4,p.migrate=.05)
  while (check.migration.dmc(samples, cut = 30) & samples$nmc<2000){
    samples <- run.dmc(samples.dmc(nmc=200,samples = samples, add = T), report = 100, cores=4,p.migrate=.05)}
  
  samples2 <- run.dmc(samples.dmc(nmc=1600,samples=samples),cores=4,report=100)
  
  participants$SPSP[[i]] <- summary.dmc(samples2)$statistics
  
  tll$SPSP[[i]] <- waic.dmc(trial_log_likes(samples2,thin_pointwise = 100),digits=2,save=TRUE)
  WAIC$SPSP[i] <- as.numeric(tll$SPSP[[i]][3])
  
  gelman$SPSP[[i]] <- gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)
  ES$SPSP[[i]] <- effectiveSize(theta.as.mcmc.list(samples2))
  #pp$SPSP[[i]] <- post.predict.dmc(samples2)
  
  #### SP model ####
  
  # final dataset for model fitting
  data  <- data.model.dmc(data2,model$SP)
  
  # first round with migration
  samples <- run.dmc(samples.dmc(nmc=400,p.prior$SP,data), report = 25, cores=4,p.migrate=.05)
  
  while (check.migration.dmc(samples, cut = 30) & samples$nmc<2000){
    samples <- run.dmc(samples.dmc(nmc=200,samples = samples, add = T), report = 100, cores=4,p.migrate=.05)}
  
  # second round
  samples2 <- run.dmc(samples.dmc(nmc=1600,samples=samples),cores=4,report=100)
  
  
  ## save values SP model ##
  participants$SP[[i]] <- summary.dmc(samples2)$statistics
  
  ### model comparison using WAIC and DIC 
  tll$SP[[i]] <- waic.dmc(trial_log_likes(samples2,thin_pointwise = 100),digits=2,save=TRUE)
  WAIC$SP[i] <- as.numeric(tll$SP[[i]][3])
  
  gelman$SP[[i]] <- gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)
  ES$SP[[i]] <- effectiveSize(theta.as.mcmc.list(samples2))
  
  #### Reduced model ####
  data  <- data.model.dmc(data2,model$RE)
  samples <- run.dmc(samples.dmc(nmc=400,p.prior$RE,data), report = 25, cores=4,p.migrate=.05)
  while (check.migration.dmc(samples, cut = 30) & samples$nmc<2000){
    samples <- run.dmc(samples.dmc(nmc=200,samples = samples, add = T), report = 100, cores=4,p.migrate=.05)}
  
  samples2 <- run.dmc(samples.dmc(nmc=1600,samples=samples),cores=4,report=100)
  
  participants$RE[[i]] <- summary.dmc(samples2)$statistics
  
  tll$RE[[i]] <- waic.dmc(trial_log_likes(samples2,thin_pointwise = 100),digits=2,save=TRUE)
  WAIC$RE[i] <- as.numeric(tll$RE[[i]][3])
  
  gelman$RE[[i]] <- gelman.diag(theta.as.mcmc.list(samples2),transform=TRUE)
  ES$RE[[i]] <- effectiveSize(theta.as.mcmc.list(samples2))
  #pp$RE[[i]] <- post.predict.dmc(samples2)
  
  #### Weights ####    
  models<-list(SP=tll$SP[[i]],SPDR=tll$SPDR[[i]],SPDB=tll$SPDB[[i]],SPSP=tll$SPSP[[i]],RE=tll$RE[[i]])
  WAIC$weights[[i]] <- loocompare.dmc(models,digits=3)
  
  ### count cases in which ci of WAIC differences includes 0
  win <- which.min(WAIC$weights[[i]][1,])
  winner[i,names(models[win])]<-"wins"
  
  junk<-capture.output(
    for (m in attributes(models[-win])$names) ifelse(diff(loocompare.dmc(models[[win]], models[[m]]))>0, winner[i,m]<-"ci",winner[i,m]<-NA) 
  )
  
  
}

###########################################################################
# DO SOME PLOTS

# PLOT WEIGHTS
m.WAIC <- matrix(NA,nrow=9,ncol=5)

for (ipart in 1:9){
  m.WAIC[ipart,1:5] <-WAIC$weights[[ipart]][1,1:5]
}

colnames(m.WAIC)<- c("SP","SPDR","SPDB","SPSP","RE")

m.w <- matrix(NA,nrow=9,ncol=5)

for (ipart in 1:9){
  m.w[ipart,1:5] <-WAIC$weights[[ipart]][2,1:5]
}

colnames(m.w)<- c("SP","SPDR","SPDB","SPSP","RE")

m.w<-data.frame(m.w)
m.WAIC<-data.frame(m.WAIC)

m.w$part <- pID
m.WAIC$part <-pID

m.w.p <- melt(m.w, id.vars="part")
m.WAIC.p <- melt(m.WAIC, id.vars="part")

p.WAIC = ggplot(aes(x=variable, y=value),data=m.WAIC.p) + geom_bar(stat = "identity") + xlab("model")
p.w = ggplot(aes(x=variable, y=value),data=m.w.p) + geom_bar(stat = "identity")+ xlab("model")

# do the graph for all participants and save it into a list
fig.WAIC.p <- group_by(m.WAIC.p,part) %>%
  do(plots = p.WAIC %+% .)

fig.w.p <- group_by(m.w.p,part) %>%
  do(plots = p.w %+% . )

grid.arrange(fig.w.p$plots[[1]],fig.w.p$plots[[2]],fig.w.p$plots[[3]],fig.w.p$plots[[4]],fig.w.p$plots[[5]],fig.w.p$plots[[6]],fig.w.p$plots[[7]],fig.w.p$plots[[8]],fig.w.p$plots[[9]], ncol=3, nrow=3)

# PLOT FOR SPSP NUMBERS
m.SPSP <- matrix(NA,nrow=9,ncol=10)
colnames(m.SPSP)<-c("a","v","z.NONE.left", "z.SAME.left", "z.ONLY.left", "z.NONE.right", "z.SAME.right", "z.ONLY.right", "sv","t0")

for (ipart in 1:9){
  m.SPSP[ipart,1:10] <-participant2$SPSP[[ipart]][,1]
}

m.SPSP<-data.frame(m.SPSP)

m.SPSP$part <- pID

m.SPSP.p <- melt(m.SPSP, id.vars="part")

p.SPSP = ggplot(aes(x=variable, y=value),data=m.WAIC.p) + geom_bar(stat = "identity") + xlab("variable")

# do the graph for all participants and save it into a list
fig.SPSP.p <- group_by(m.SPSP.p,part) %>%
  do(plots = p.SPSP %+% .)

grid.arrange(fig.SPSP.p$plots[[1]],fig.SPSP.p$plots[[2]],fig.SPSP.p$plots[[3]],fig.SPSP.p$plots[[4]],fig.SPSP.p$plots[[5]],fig.SPSP.p$plots[[6]],fig.SPSP.p$plots[[7]],fig.SPSP.p$plots[[8]],fig.SPSP.p$plots[[9]], ncol=3, nrow=3)

