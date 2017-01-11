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
library(gridExtra)

# Load functions
source('meta_dprime_functions.R')

# Load my.data (if it is not already present)
if (!exists("my.data")){
  load("confidenceData.RData")
  print("loading my.data")
}

# load participants' IDs
pID <- unique(my.data$participant)

############
# SOCIAL INFO ONLY
###########

##### PRE-ALLOCATION
# names of columns
names <-c("Invalid", "Incon", "Valid")

# meta d'
m.meta <- matrix(NA, ncol = 3, nrow = length(pID))
colnames(m.meta)<-names

# d' 
m.d <- matrix(NA, ncol = 3, nrow = length(pID))
colnames(m.d)<-names

# graphs
fig.meta<- list()
fig.d<-list()
fig.d_meta <-list()

SI<- c(-1,0,1)
for (iSI in 1:3){
  run.v <- c(1:length(pID))
  for (iparticipant in run.v){
    f.data <- 
      filter(my.data,(!is.na(zConf)))%>%
      filter(trialType==SI[iSI]&participant==pID[iparticipant])
    model <-DataMetaD(f.data)%>%
      FitMetaD()
    m.meta[iparticipant,iSI] <- mean(as.numeric(model$meta_d))
    m.d[iparticipant,iSI] <- mean(as.numeric(model$d1))
  }
}


###########################################################################xx

#### PREPARE FOR PLOTTING

#convert matrices into data.frames (for ggplot, and easier handling)
m.meta <- data.frame(m.meta)
m.d <- data.frame(m.d)

# create a new matrix with metacognitive efficiency (meta-d' - d')
m.diff <- m.meta - m.d

# add a column with participant info
m.meta$participant<-pID
m.d$participant<-pID
m.diff$participant<-pID

# save the matrices
save(m.meta,file='m.meta.RData') 
save(m.d,file='m.d.RData') 
save(m.diff,file='m.diff.RData') 



###########################################################################

# PLOT by participant

# change the structure of the d.frame so that I can plot multiple columns
#m.meta.p<-melt(m.meta[c(2,3,6,8,9),c(1,5:10,11)],id.vars="participant")
m.meta.p<-melt(m.meta,id.vars="participant") #- use if I dont want to plot social info
m.d.p <-melt(m.d,id.vars="participant")
m.diff.p <-melt(m.diff,id.vars="participant")

# define the form of the graph
p.meta = ggplot(aes(x=variable, y=value),data=m.meta.p) + geom_bar(stat = "identity") + xlab("condition") + ylab("meta-d") + ylim(-2,2.5) + geom_abline(intercept = 0, slope = 0)
p.d = ggplot(aes(x=variable, y=value),data=m.d.p) + geom_bar(stat = "identity")+ xlab("condition") + ylab("dPrime")+ ylim(-2,2.5) + geom_abline(intercept = 0, slope = 0)
p.diff = ggplot(aes(x=variable, y=value),data=m.diff.p) + geom_bar(stat = "identity")+ xlab("condition") + ylab("metaD-dPrime")+ ylim(-2.15,0.8) + geom_abline(intercept = 0, slope = 0)

# do the graph for all participants and save it into a list
fig.meta.p <- group_by(m.meta.p,participant) %>%
  do(plots = p.meta %+% .)

fig.d.p <- group_by(m.d.p,participant) %>%
  do(plots = p.d %+% .)

fig.diff.p <- group_by(m.diff.p,participant) %>%
  do(plots = p.diff %+% .)

# Put meta and dPrime together 

for(i in 1:length(unique(m.meta.p$participant))){
  #fig.p[[i]]<- grid.arrange(fig.meta.p$plots[[i]],fig.d.p$plots[[i]], fig.diff.p$plots[[i]],ncol=1, nrow =3, top=as.character(unique(m.meta.p$participant)[i]))

  fig.p[[i]]<- grid.arrange(fig.meta.p$plots[[i]],fig.d.p$plots[[i]],ncol=1, nrow =2, top=as.character(unique(m.meta.p$participant)[i]))
}

grid.arrange(fig.p[[1]],fig.p[[2]],fig.p[[3]],fig.p[[4]],fig.p[[5]],fig.p[[6]],fig.p[[7]], ncol=4, nrow=2)

