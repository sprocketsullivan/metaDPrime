#########
#fit data
######### 
# clear the workspace
rm(list=ls())

# Import Packages (not sure if I need them all)
require(tidyverse) #require vs library
require(stringr)
require(data.table)
require(dtplyr)  

# Load functions and data files
source('meta_dprime_functions.R')
load("myData.RData")

# preallocatoion
fig<- list()
m.in = data.frame()
m.none = data.frame()
m.only = data.frame()
m.same = data.frame()

# RUN the analysis
#participants 6 and 7 (i.e. 45524,45525) were excluded from all analyses already in the my.data. (they always chose 0 as their wager)
# if you have them in my.data add:[-c(6:7)] 


# INCON
pID <- unique(my.data$participant)
for (iparticipant in 1:length(pID)){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==0&participant==pID[iparticipant])
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.in <- rbind(m.in,mean_se(model$meta_d))
}
fig[[1]]<-ggplot(aes(x=c(1:9),y=y,fill=c(1:9)),data=m.in) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2) + ylab("meta-dPrime") + xlab("participant") + ggtitle('INCON')



# NONE
pID <- unique(my.data$participant)[-c(4,9)] # for excluded - RUNTIME ERROR: Failed check for discrete-valued parameters in function
for (iparticipant in 1:length(pID)){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==1&participant==pID[iparticipant]&norm2=="NONE")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.none <- rbind(m.none,mean_se(model$meta_d))
}

m.none <- rbind(m.none[1:3,], c(0,0,0), m.none[4:7,],c(0,0,0))
rownames(m.none)<-c(1:9)
fig[[2]]<-ggplot(aes(x=c(1:9),y=y,fill=c(1:9)),data=m.none) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2) + ylab("meta-dPrime") + xlab("participant") + ggtitle('NONE')


# ONLY
pID <- unique(my.data$participant) 
for (iparticipant in 1:length(pID)){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==1&participant==pID[iparticipant]&norm2=="ONLY")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.only <- rbind(m.only,mean_se(model$meta_d))
}

fig[[3]]<-ggplot(aes(x=c(1:9),y=y,fill=c(1:9)),data=m.only) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2) + ylab("meta-dPrime") + xlab("participant") + ggtitle('SAME')


# SAME
pID <- unique(my.data$participant)[-c(6:7)] 
for (iparticipant in 1:length(pID)){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==1&participant==pID[iparticipant]&norm2=="SAME")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.same <- rbind(m.same,mean_se(model$meta_d))
}

m.same <- rbind(m.same[1:5,], c(0,0,0),c(0,0,0), m.same[ 6:7,] )
rownames(m.same)<-c(1:9)

fig[[4]]<-ggplot(aes(x=c(1:9),y=y,fill=c(1:9)),data=m.same) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2) + ylab("meta-dPrime") + xlab("participant") + ggtitle('ONLY')
