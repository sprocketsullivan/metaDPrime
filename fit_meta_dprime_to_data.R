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
library(gridExtra)
# Load functions and data files
source('meta_dprime_functions.R')
load("myData.RData")



# preallocatoion
fig<- list()
fig.d<-list()
m.in = data.frame()
m.d = data.frame()

m.only = data.frame()
m.same = data.frame()

# RUN the analysis
#participants 6 and 7 (i.e. 45524,45525) were excluded from all analyses already in the my.data. (they always chose 0 as their wager)
# if you have them in my.data add:[-c(7:8)] 


# INCON
pID <- unique(my.data$participant)[-c(1,7,8,12)]
for (iparticipant in 1:length(pID)){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==0&participant==pID[iparticipant])
    model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.in <- rbind(m.in,mean_se(as.numeric(model$meta_d)))
  m.d<- rbind(m.d,mean_se(as.numeric(model$d1)))
}
m.in$participant<-pID
m.d$participant<-pID
fig[[1]]<-ggplot(aes(x=factor(participant),y=y),data=m.in) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2) + ylab("meta-dPrime") + xlab("participant") + ggtitle('INCON')+theme_classic()
print(fig[[1]])
fig.d[[1]]<-ggplot(aes(x=factor(participant),y=y),data=m.d) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2) + ylab("dPrime") + xlab("participant") + ggtitle('INCON')+theme_classic()
m.in$diff<-m.in$y-m.d$y
ggplot(aes(x=factor(participant),y=diff),data=m.in) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('INCON')+theme_classic()

# NONE
m.none = data.frame()
m.none.d = data.frame()
pID <- unique(my.data$participant)[-c(1,7,8,12)] # for excluded - RUNTIME ERROR: Failed check for discrete-valued parameters in function
for (iparticipant in 1:length(pID)){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(participant==pID[iparticipant]&norm2=="NONE")
  model <-DataMetaD(f.data)%>%
    FitMetaD()
  m.none <- rbind(m.none,mean_se(model$meta_d))
  m.none.d<- rbind(m.none.d,mean_se(as.numeric(model$d1)))
}
m.none$participant<-pID
m.none.d$participant<-pID
fig[[2]]<-ggplot(aes(x=factor(participant),y=y),data=m.none) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2) + ylab("meta-dPrime") + xlab("participant") + ggtitle('NONE')+theme_classic()
print(fig[[2]])
fig.d[[2]]<-ggplot(aes(x=factor(participant),y=y),data=m.none.d) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.2) + ylab("dPrime") + xlab("participant") + ggtitle('NONE')+theme_classic()
grid.arrange(fig.d[[1]],fig.d[[2]])
grid.arrange(fig[[1]],fig[[2]])
m.in$diff<-m.in$y-m.d$y
ggplot(aes(x=factor(participant),y=diff),data=m.in) + geom_bar(stat = "identity") + ylab("meta-dPrime") + xlab("participant") + ggtitle('INCON')+theme_classic()


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
