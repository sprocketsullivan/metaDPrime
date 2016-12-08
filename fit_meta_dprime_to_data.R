################################################################
# FIT META D' TO DATA
################################################################

# clear the workspace
#rm(list=ls())

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

# # Load my.data (if it is not already present)
# if (!exists("my.data")){
#   load("confidenceData.RData")
#   print("loading my.data")
# }

# load participants' IDs
pID <- unique(my.data$participant)

##### PRE-ALLOCATION
# names of columns
names <-c("incon","none", "only", "same")

# meta d'
m.meta <- matrix(NA, ncol = 4, nrow = length(pID))
colnames(m.meta)<-names

# d' 
m.d <- matrix(NA, ncol = 4, nrow = length(pID))
colnames(m.d)<-names


#####################################################################
#bin data
my.data<-
  filter(my.data,(!is.na(my.data$zConf)))
my.data$binned<-0
for(i in pID){
  my.data$binned[my.data$participant==i]<-bin_function(my.data[my.data$participant==i,])
}
my.data$binned<-factor(my.data$binned)

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

###########################################################################xx

