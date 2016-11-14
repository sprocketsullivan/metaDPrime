################################################################
# FIT META D' TO DATA
################################################################

# clear the workspace
rm(list=ls())

################################################################

#### PRE-REQ

# Import Packages (not sure if I need them all)
library(tidyverse) 
library(stringr)
library(data.table)
library(dtplyr)  
library(gridExtra) # need to upload this one

# Load functions
source('meta_dprime_functions.R')

# Load my.data (if it is not already present)
if (!exists("my.data")){
  load("confidenceData.RData")
  print("loading my.data")
}

# load participants' IDs
pID <- unique(my.data$participant)
  
##### PRE-ALLOCATION
# graphs
fig.meta<- list()
fig.d<-list()
fig.d_meta <-list()

# meta d' for all conditions
m.meta <- matrix(NA, ncol = 4, nrow = length(pID))# set up so it corresponds with the number of p.
colnames(m.meta)<-c("incon","none","only","same")

# d' for all conditions
m.d <- matrix(NA, ncol = 4, nrow = length(pID))
colnames(m.d)<-c("incon","none","only","same")

#####################################################################x

# SIMPLE ANALYSIS

#### INCON
i<-0
for (iparticipant in pID){
  #iparticipant<-pID[1]
  i<-i+1
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==0&participant==iparticipant)
    model <- 
      DataMetaD(f.data)%>%
      print()%>%
      FitMetaD()
  m.meta[i,1] <- mean(as.numeric(model$meta_d))
  m.d[i,1] <- mean(as.numeric(model$d1))
}
#### NONE
#part.exc <- c()
run.v <- c(1:length(pID))
for (iparticipant in run.v){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social!=0&participant==pID[iparticipant]&norm2=="NONE")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.meta[iparticipant,2] <- mean(as.numeric(model$meta_d))
  m.d[iparticipant,2] <- mean(as.numeric(model$d1))
}

#### ONLY
part.exc <- 1# had to exclude 1 due to: RUNTIME ERROR: Failed check for discrete-valued parameters in function
run.v <- c(1:length(pID))#[-part.exc]
for (iparticipant in run.v){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social!=0&participant==pID[iparticipant]&norm2=="ONLY")
  model <- DataMetaD(f.data)%>%
    FitMetaD()
  m.meta[iparticipant,3] <- mean(as.numeric(model$meta_d))
  m.d[iparticipant,3] <- mean(as.numeric(model$d1))
}


#### SAME
#run.v <- c(1:9) 
for (iparticipant in run.v){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social!=0 &participant==pID[iparticipant]&norm2=="SAME")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.meta[iparticipant,4] <- mean(as.numeric(model$meta_d))
  m.d[iparticipant,4] <- mean(as.numeric(model$d1))
}

#### PREPARE FOR PLOTTING

#convert matrices into data.frames (for ggplot, and easier handling)
m.meta <- data.frame(m.meta)
m.d <- data.frame(m.d)

# add a column in m.meta for the difference between d' and meta-d' (metacognitive efficiency)
m.meta$diff <- m.d-m.meta

# add a column with participant info
m.meta$participant<-pID
m.d$participant<-pID

# PLOT AND PRINT
#ADD 
selector<-c("25776","36684","36685","45526","62235")
# Plot Incon
fig.meta[[1]]<-ggplot(aes(x=factor(participant),y=as.numeric(incon)),data=subset(m.meta,pID%in%selector)) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('INCONGUENT')+theme_classic()
fig.d[[1]]<-ggplot(aes(x=factor(participant),y=as.numeric(incon)),data=m.d) + geom_bar(stat = "identity")  + ylab("dPrime") + xlab("participant") + ggtitle('INCONGRUENT')+theme_classic() 
fig.d_meta[[1]]<-ggplot(aes(x=factor(participant),y=as.numeric(diff$incon)),data=subset(m.meta,pID%in%selector)) + geom_bar(stat = "identity")  + ylab("dPrime-metad") + xlab("participant") + ggtitle('INCONGUENT')+theme_classic()


# Plot None
fig.meta[[2]]<-ggplot(aes(x=factor(participant),y=as.numeric(none)),data=subset(m.meta,pID%in%selector)) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('NONE')+theme_classic()
fig.d[[2]]<-ggplot(aes(x=factor(participant),y=as.numeric(none)),data=m.d) + geom_bar(stat = "identity")  + ylab("dPrime") + xlab("participant") + ggtitle('NONE')+theme_classic()
fig.d_meta[[2]]<-ggplot(aes(x=factor(participant),y=as.numeric(diff$none)),data=subset(m.meta,pID%in%selector)) + geom_bar(stat = "identity")  + ylab("dPrime-metad") + xlab("participant") + ggtitle('NONE')+theme_classic()

# plot Only
fig.meta[[3]]<-ggplot(aes(x=factor(participant),y=as.numeric(only)),data=subset(m.meta,pID%in%selector)) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('ONLY')+theme_classic()
fig.d[[3]]<-ggplot(aes(x=factor(participant),y=as.numeric(only)),data=m.d) + geom_bar(stat = "identity")  + ylab("dPrime") + xlab("participant") + ggtitle('ONLY')+theme_classic() 
fig.d_meta[[3]]<-ggplot(aes(x=factor(participant),y=as.numeric(diff$only)),data=subset(m.meta,pID%in%selector)) + geom_bar(stat = "identity")  + ylab("dPrime-metad") + xlab("participant") + ggtitle('ONLY')+theme_classic()

# Plot SAME
fig.meta[[4]]<-ggplot(aes(x=factor(participant),y=as.numeric(same)),data=subset(m.meta,pID%in%selector)) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('SAME')+theme_classic()
fig.d[[4]]<-ggplot(aes(x=factor(participant),y=as.numeric(same)),data=m.d) + geom_bar(stat = "identity")  + ylab("dPrime") + xlab("participant") + ggtitle('SAME')+theme_classic() 
fig.d_meta[[4]]<-ggplot(aes(x=factor(participant),y=as.numeric(diff$same)),data=subset(m.meta,pID%in%selector)) + geom_bar(stat = "identity")  + ylab("dPrime-metad") + xlab("participant") + ggtitle('SAME')+theme_classic()

#print(fig.meta[1:4])
#print(fig.d[1:4])
#print(fig.d_meta[1:4])
require(gridExtra)
grid.arrange(fig.meta[[1]],fig.meta[[2]],fig.meta[[3]],fig.meta[[4]], ncol=2, nrow =2)
grid.arrange(fig.d[[1]],fig.d[[2]],fig.d[[3]],fig.d[[4]], ncol=2, nrow =2)
grid.arrange(fig.d_meta[[1]],fig.d_meta[[2]],fig.d_meta[[3]],fig.d_meta[[4]], ncol=2, nrow =2)

##########################################################################################x

# Differences between social info in a norm

#pre-allocation
fig.meta.a<-list()
fig.d.a<-list()
fig.m_d.a <-list()

m.advanced.meta <- matrix(NA, ncol = 6, nrow = 9)
colnames(m.advanced.meta)<-c("noneValid","noneInvalid","onlyValid","onlyInvalid","sameValid","sameInvalid")

m.advanced.d <- matrix(NA, ncol = 6, nrow = 9)
colnames(m.advanced.d)<-c("noneValid","noneInvalid","onlyValid","onlyInvalid","sameValid","sameInvalid")

# none with Valid social info
run.v <- c(1:length(pID))
for (iparticipant in run.v){
  f.data <-
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==1&participant==pID[iparticipant]&norm2=="NONE")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.advanced.meta[iparticipant,1] <- mean(as.numeric(model$meta_d))
  m.advanced.d[iparticipant,1] <- mean(as.numeric(model$d1))
}
# none with invalid social info
run.v <- c(1:length(pID))
for (iparticipant in run.v){
  f.data <-
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==-1 &participant==pID[iparticipant]&norm2=="NONE")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.advanced.meta[iparticipant,2] <- mean(as.numeric(model$meta_d))
  m.advanced.d[iparticipant,2] <- mean(as.numeric(model$d1))
}

# ONLY with Valid social info
run.v <- c(2:length(pID)) #HAD TO EXCLUDE P.1 -> jags.model - COMPILATION ERROR on line 83(invalid range)
for (iparticipant in run.v){
  f.data <-
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==1 &participant==pID[iparticipant]&norm2=="ONLY")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.advanced.meta[iparticipant,3] <- mean(as.numeric(model$meta_d))
  m.advanced.d[iparticipant,3] <- mean(as.numeric(model$d1))
}
# ONLY with invalid social info
run.v <- c(2:length(pID)) # see above
for (iparticipant in run.v){
  f.data <-
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==-1 &participant==pID[iparticipant]&norm2=="ONLY")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.advanced.meta[iparticipant,4] <- mean(as.numeric(model$meta_d))
  m.advanced.d[iparticipant,4] <- mean(as.numeric(model$d1))
}

# same with valid social info
part.exc <- c(7,8)# had to exclude 1 due to: RUNTIME ERROR: Failed check for discrete-valued parameters in function
run.v <- c(1:length(pID))[-part.exc]
for (iparticipant in run.v){
  f.data <-
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==1 &participant==pID[iparticipant]&norm2=="SAME")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.advanced.meta[iparticipant,5] <- mean(as.numeric(model$meta_d))
  m.advanced.d[iparticipant,5] <- mean(as.numeric(model$d1))
}

# same with invalid social info
run.v <- c(1:length(pID))
for (iparticipant in run.v){
  f.data <-
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==-1&participant==pID[iparticipant]&norm2=="SAME")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.advanced.meta[iparticipant,6] <- mean(as.numeric(model$meta_d))
  m.advanced.d[iparticipant,6] <- mean(as.numeric(model$d1))
}

#### PREPARE FOR PLOTTING

#convert matrices into data.frames (for ggplot, and easier handling)
m.advanced.meta <- data.frame(m.advanced.meta)
m.advanced.d <- data.frame(m.advanced.d)

# add a column in m.meta for the difference between d' and meta-d' (metacognitive efficiency)
m.advanced.meta$diff <- m.advanced.d-m.advanced.meta

# add a column with participant info
m.advanced.meta$participant<-pID
m.advanced.d$participant<-pID

# PLOT META INVALID, VALID
fig.meta.a[[1]]<-ggplot(aes(x=factor(participant),y=as.numeric(noneInvalid)),data=m.advanced.meta) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('NONE-Inalid')+theme_classic() + ylim(-1.5, 1.5)
fig.meta.a[[2]]<-ggplot(aes(x=factor(participant),y=as.numeric(noneValid)),data=m.advanced.meta) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('NONE-valid')+theme_classic()+ ylim(-1.5, 1.5)
grid.arrange(fig.meta.a[[1]],fig.meta.a[[2]], ncol=1, nrow =2)

fig.meta.a[[3]]<-ggplot(aes(x=factor(participant),y=as.numeric(onlyInvalid)),data=m.advanced.meta) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('ONLY-Invalid')+theme_classic()+ ylim(-1.5, 1.5)
fig.meta.a[[4]]<-ggplot(aes(x=factor(participant),y=as.numeric(onlyValid)),data=m.advanced.meta) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('ONLY-Valid')+theme_classic()+ ylim(-1.5, 1.5)
grid.arrange(fig.meta.a[[3]],fig.meta.a[[4]], ncol=1, nrow =2)

fig.meta.a[[5]]<-ggplot(aes(x=factor(participant),y=as.numeric(sameInvalid)),data=m.advanced.meta) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('SAME-Invalid')+theme_classic()+ ylim(-1.5, 1.5)
fig.meta.a[[6]]<-ggplot(aes(x=factor(participant),y=as.numeric(sameValid)),data=m.advanced.meta) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('SAME-Valid')+theme_classic()+ ylim(-1.5, 1.5)
grid.arrange(fig.meta.a[[5]],fig.meta.a[[6]], ncol=1, nrow =2)


grid.arrange(fig.meta.a[[1]],fig.meta.a[[3]],fig.meta.a[[5]],fig.meta.a[[2]],fig.meta.a[[4]],fig.meta.a[[6]], ncol=3, nrow =2)

# PLOT D INVALID, VALID
fig.d.a[[1]]<-ggplot(aes(x=factor(participant),y=as.numeric(noneInvalid)),data=m.advanced.d) + geom_bar(stat = "identity") + ylab("dPrime") + xlab("participant") + ggtitle('NONE-Invalid')+theme_classic() + ylim(-1.5, 2.5)
fig.d.a[[2]]<-ggplot(aes(x=factor(participant),y=as.numeric(noneValid)),data=m.advanced.d) + geom_bar(stat = "identity") + ylab("dPrime") + xlab("participant") + ggtitle('NONE-Valid')+theme_classic()+ ylim(-1.5, 2.5)
grid.arrange(fig.d.a[[1]],fig.d.a[[2]], ncol=1, nrow =2)

fig.d.a[[3]]<-ggplot(aes(x=factor(participant),y=as.numeric(onlyInvalid)),data=m.advanced.d) + geom_bar(stat = "identity") + ylab("dPrime") + xlab("participant") + ggtitle('ONLY-Invalid')+theme_classic()+ ylim(-1.5, 2.5)
fig.d.a[[4]]<-ggplot(aes(x=factor(participant),y=as.numeric(onlyValid)),data=m.advanced.d) + geom_bar(stat = "identity") + ylab("dPrime") + xlab("participant") + ggtitle('ONLY-Valid')+theme_classic()+ ylim(-1.6, 2.5)
grid.arrange(fig.d.a[[3]],fig.d.a[[4]], ncol=1, nrow =2)

fig.d.a[[5]]<-ggplot(aes(x=factor(participant),y=as.numeric(sameInvalid)),data=m.advanced.d) + geom_bar(stat = "identity") + ylab("dPrime") + xlab("participant") + ggtitle('SAME-Invalid')+theme_classic()+ ylim(-1.5, 2.5)
fig.d.a[[6]]<-ggplot(aes(x=factor(participant),y=as.numeric(sameValid)),data=m.advanced.d) + geom_bar(stat = "identity") + ylab("dPrime") + xlab("participant") + ggtitle('SAME-Valid')+theme_classic()+ ylim(-1.5, 2.5)
grid.arrange(fig.d.a[[5]],fig.d.a[[6]], ncol=1, nrow =2)


grid.arrange(fig.d.a[[1]],fig.d.a[[3]],fig.d.a[[5]],fig.d.a[[2]],fig.meta.a[[4]],fig.d.a[[6]], ncol=3, nrow =2)


