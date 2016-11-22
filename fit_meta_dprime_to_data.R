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

##### PRE-ALLOCATION
# names of columns
names <-c("incon","none", "only", "same","noneValid", "noneInvalid", "onlyValid","onlyInvalid","sameValid", "sameInvalid")

# meta d'
m.meta <- matrix(NA, ncol = 10, nrow = length(pID))
colnames(m.meta)<-names

# d' 
m.d <- matrix(NA, ncol = 10, nrow = length(pID))
colnames(m.d)<-names

# graphs
fig.meta<- list()
fig.d<-list()
fig.d_meta <-list()
fig.p <- list()
#####################################################################

#### INCONGRUENT trials
run.v <- c(1:length(pID))
for (iparticipant in run.v){
  f.data <- 
    filter(my.data,(!is.na(zConf)))%>%
    filter(social==0&participant==pID[iparticipant])
  model <- 
    DataMetaD(f.data)%>%
    print()%>%
    FitMetaD()
  m.meta[iparticipant,1] <- mean(as.numeric(model$meta_d))
  m.d[iparticipant,1] <- mean(as.numeric(model$d1))
}

#### NORMS
# does not work if some participants have to be excluded from some (but not all) conditions
# for that need to add vector that would change everytime 

norm <- c("NONE","ONLY","SAME")
for (inorm in 1:3){
  run.v <- c(1:length(pID))
  for (iparticipant in run.v){
    f.data <- 
      filter(my.data,(!is.na(zConf)))%>%
      filter(social!=0&participant==pID[iparticipant]&norm2==norm[inorm])
    model <-DataMetaD(f.data)%>%
      FitMetaD()
    m.meta[iparticipant,inorm+1] <- mean(as.numeric(model$meta_d))
    m.d[iparticipant,inorm+1] <- mean(as.numeric(model$d1))
  }
}

#### SOCIAL INFO
# Can be eventually added to the above - once I figure how to encode the social!=0 
# Some participants unstable - their m.meta changes massively over different runs of the code 
# (1,8,6)

norm <- c("NONE","ONLY","SAME")
SI <- c(1,-1)

# type the position of part. to be excluded (if none excluded input number > # participants)
part.exc<-list(c(1,4,6,9),c(8),c(99),c(6),c(1,4,5),c(99))

i<-1
for (inorm in 1:length(norm)){
  for (iSI in 1:length(SI)) {
    
    run.v <- c(1:length(pID))[-part.exc[[i]]]
    
    for (iparticipant in run.v){
      f.data <-
        filter(my.data,(!is.na(zConf)))%>%
        filter(social==SI[iSI]&participant==pID[iparticipant]&norm2==norm[inorm])
      model <-DataMetaD(f.data)%>%
        FitMetaD()
      m.meta[iparticipant,4+i] <- mean(as.numeric(model$meta_d))
      m.d[iparticipant,4+i] <- mean(as.numeric(model$d1))
    }
    
    i<- i+1
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
m.meta.p<-melt(m.meta,id.vars="participant")
#m.meta.p<-melt(m.meta[c(1:4,11)],id.vars="participant") - use if I dont want to plot social info
m.d.p <-melt(m.d,id.vars="participant")
m.diff.p <-melt(m.diff,id.vars="participant")

# define the form of the graph
p.meta = ggplot(aes(x=variable, y=value),data=m.meta.p) + geom_bar(stat = "identity") + xlab("condition") + ylab("meta-d")
p.d = ggplot(aes(x=variable, y=value),data=m.d.p) + geom_bar(stat = "identity")+ xlab("condition") + ylab("dPrime")
p.diff = ggplot(aes(x=variable, y=value),data=m.diff.p) + geom_bar(stat = "identity")+ xlab("condition") + ylab("metaD-dPrime")

# do the graph for all participants and save it into a list
fig.meta.p <- group_by(m.meta.p,participant) %>%
  do(plots = p.meta %+% .)

fig.d.p <- group_by(m.d.p,participant) %>%
  do(plots = p.d %+% .)

fig.diff.p <- group_by(m.diff.p,participant) %>%
  do(plots = p.diff %+% .)

# Put meta and dPrime together 
for(i in 1:length(pID)){
  fig.p[[i]]<- grid.arrange(fig.meta.p$plots[[i]],fig.d.p$plots[[i]], fig.diff.p$plots[[i]],ncol=1, nrow =3, top=as.character(pID[i]))
}

grid.arrange(fig.p[[1]],fig.p[[2]],fig.p[[3]],fig.p[[4]],fig.p[[5]],fig.p[[6]],fig.p[[7]],fig.p[[8]],fig.p[[9]], ncol=3, nrow=3)