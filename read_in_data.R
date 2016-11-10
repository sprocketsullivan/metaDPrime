#READ IN BEHAVIOURAL DATA and ANALYSE THEM

# Input: This file needs to be in a folder that also contains a folder with the data (named data)
# Output: This script outputs:
#                  - data from all participants saved into a single file (my.data)
#                  - Reaction times
#                  - Accuracy
#                  - Confidence based on accuracy, social info or norm


#############################################################################x
# clear the workspace
rm(list=ls())

# Import Packages
library(tidyverse) 
library(stringr)
library(data.table)
library(dtplyr)  

# SET-UP variables
colorScheme<-c("#F5A503","#56D9CD", "#3AA1BF") #yellow, l.blue, d.blue

# participants to exclude
part.excl <- c(7:8,12)
#excludes pp45524,45525 (always 0 wager),62236(low accuracy - chance level), also suggested to exclude 25774 (tended to have low wager) 


# Define pathways


# Preallocation
dodge <- position_dodge(.5)
fig<- list() #container for figures

#########################################################################################

######
# Read in Data
#####
# Reads in the data, saves them into my.data, deletes trials with no response,wd and adds a couple of extra columns with labels


# Extract data from Phase2 from all participants and save them into a single file: my.data
setwd('.//data')
files<-list.files(pattern='*Phase2.csv')[-part.excl] 
my.data<-rbindlist(lapply(files, fread),use.names=TRUE,fill=TRUE) 
setwd('..\\')


# Consider removing unneccessary columns from my.data.


# Delete lines at the end of each file denotet by NA 
# (deletes the last trial, in which the outcome of the wager was caluculated)
my.data<-my.data[!is.na(my.data$ttSAME)]

# Delete trials in which participant failed to respond
number_no_response<-nrow(my.data[is.na(my.data$key_resp_direction.rt)])
my.data<-my.data[!is.na(my.data$key_resp_direction.rt)] #this leaves unequal numer of trials pPP

# Calculate the number or participants
no.part<-length(unique(my.data$participant))

# Normalize confidence ratings and put them into a new column
my.data[,zConf:=scale(as.numeric(PDW.response,na.rm=T)),by=participant]

# Add a column that states if social info was presented
my.data$si<-factor(ifelse(my.data$social==0,0,1),labels=c("no social\ninfo","social\ninfo"))

# Add another column that states whether the social info was valid or not
# Maybe exchange social2 for social 3?
my.data$social2<-factor(my.data$social,labels = c("invalid\nsocial information","none","valid\nsocial information"))
my.data$social3<-factor(my.data$social,labels=c("invalid","none","valid"))

# Add column that states what norm was presented during each trial
my.data$norm2<-ifelse(my.data$ttNONE,1,ifelse(my.data$ttSAME,2,3))
my.data$norm2<-factor(my.data$norm,labels = c("NONE","SAME","ONLY"))




#####
#Save it
###


save(my.data,file='confidenceData.RData') # need to figure where to save it so that it is private and easy to access


######################################################################################################

#Put this into a new file

######
#basic plots
#####

### reaction times density plots ###

#log RTs
# Is this even used anywhere?
my.data$rt.log<-log(my.data$key_resp_direction.rt)
#calculate mean log RT for each participant
my.data[,meanRT:=mean(rt.log,na.rm=T),by=participant]
#z transform
my.data$rt.log.z<-my.data$rt.log-my.data$meanRT
# density distribution of RT in correct vs. wrong answers under NONE, SAME and ONLY conditions

### correct answers overall ###

#calculate some more variables

summary_corr_choices<-
  group_by(my.data,participant,si,norm2)%>%
  summarise(mean_corr_l=mean(key_resp_direction.corr*100),N=length(key_resp_direction.corr))%>%
  group_by(si,norm2) %>%
  summarise(mean_corr=mean(mean_corr_l),sd_corr=sd(mean_corr_l))%>%
  mutate(se.min=mean_corr-sd_corr,se.max=mean_corr+sd_corr)

ggplot(aes(y=mean_corr,x=norm2,fill=norm2),data=summary_corr_choices)+geom_bar(stat='identity',position="dodge")+
  geom_errorbar(aes(ymin=se.min,ymax=se.max),width=0.1,position=position_dodge(.9))+
  ylab("Proportion correct \ncompared to no social info")+
  xlab("")+theme_classic()+
  theme(text=element_text(size=20),legend.position=c(0.83,0.8),legend.title=element_blank(),legend.key.size = unit(2, 'lines'),legend.text=element_text(size=8))+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=c("#F5A503","#56D9CD", "#3AA1BF"))+ # isnt this the same as colourscheme at the beginning
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 1.2))+facet_wrap(~si)




##########################
#plot
#correct choices
summary_corr_choices<-
  group_by(my.data,participant,social3,norm2)%>%
  summarise(meanCorrL=mean(key_resp_direction.corr*100))%>%
  spread(social3,meanCorrL)%>%
  mutate(meanCorrLInvalid=invalid-none,meanCorrLValid=valid-none)%>%
  gather(socialInfo,meanCorrC,meanCorrLInvalid,meanCorrLValid)%>%
  group_by(socialInfo,norm2) %>%
  summarise(meanCorr=mean(meanCorrC),sdCorr=sd(meanCorrC)/sqrt(no.part))%>%
  mutate(se.min=meanCorr-sdCorr,se.max=meanCorr+sdCorr)
summary_corr_choices$socialInfo2<-factor(summary_corr_choices$socialInfo,labels=c("invalid social\ninformation","valid social\ninformation"))
fig[[1]]<-ggplot(aes(y=meanCorr,x=norm2,fill=norm2),data=summary_corr_choices)+geom_bar(stat='identity',position="dodge")+
  geom_errorbar(aes(ymin=se.min,ymax=se.max),width=0.1,position=position_dodge(.9))+
  ylab("Proportion correct \ncompared to no social info")+
  xlab("")+theme_classic()+
  theme(text=element_text(size=20),legend.position='',legend.title=element_blank(),legend.key.size = unit(2, 'lines'),legend.text=element_text(size=8))+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=colorScheme)+
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 1.2))+facet_wrap(~socialInfo2)
print(fig[[1]])

#plot
#Reaction Time
summary_RT<-
  group_by(my.data,participant,social3,norm2)%>%
  summarise(meanCorrL=mean(key_resp_direction.rt*1000))%>%
  spread(social3,meanCorrL)%>%
  mutate(meanCorrLInvalid=invalid-none,meanCorrLValid=valid-none)%>%
  gather(socialInfo,meanCorrC,meanCorrLInvalid,meanCorrLValid)%>%
  group_by(socialInfo,norm2) %>%
  summarise(meanCorr=mean(meanCorrC),sdCorr=sd(meanCorrC)/sqrt(no.part))%>%
  mutate(se.min=meanCorr-sdCorr,se.max=meanCorr+sdCorr)
summary_RT$socialInfo2<-factor(summary_RT$socialInfo,labels=c("invalid social\ninformation","valid social\ninformation"))
fig[[2]]<-ggplot(aes(y=meanCorr,x=norm2,fill=norm2),data=summary_RT)+geom_bar(stat='identity',position="dodge")+
  geom_errorbar(aes(ymin=se.min,ymax=se.max),width=0.1,position=position_dodge(.9))+
  ylab("Reaction time [ms] \ncompared to no social info")+
  xlab("")+theme_classic()+
  theme(text=element_text(size=20),legend.position='none',legend.title=element_blank(),legend.key.size = unit(2, 'lines'),legend.text=element_text(size=8))+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=colorScheme)+
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 1.2))+facet_wrap(~socialInfo2)
print(fig[[2]])



#now confidence
#calculate mean conf pp
#first normalise per participant

summary_confidence<-
  group_by(my.data,participant,social3,norm2)%>%
  summarise(meanCorrL=mean(zConf,na.rm=T))%>%
  spread(social3,meanCorrL)%>%
  mutate(meanCorrLInvalid=invalid-none,meanCorrLValid=valid-none)%>%
  gather(socialInfo,meanCorrC,meanCorrLInvalid,meanCorrLValid)%>%
  group_by(socialInfo,norm2) %>%
  summarise(meanCorr=mean(meanCorrC),sdCorr=sd(meanCorrC)/sqrt(no.part))%>%
  mutate(se.min=meanCorr-sdCorr,se.max=meanCorr+sdCorr)
summary_confidence$socialInfo2<-factor(summary_confidence$socialInfo,labels=c("invalid social\ninformation","valid social\ninformation"))
fig[[3]]<-ggplot(aes(y=meanCorr,x=norm2,fill=norm2),data=summary_confidence)+geom_bar(stat='identity',position="dodge")+
  geom_errorbar(aes(ymin=se.min,ymax=se.max),width=0.1,position=position_dodge(.9))+
  ylab("Confidence \ncompared to no social info")+
  xlab("")+theme_classic()+
  theme(text=element_text(size=20),legend.position='none',legend.title=element_blank(),legend.key.size = unit(2, 'lines'),legend.text=element_text(size=8))+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=colorScheme)+
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 1.2))+facet_wrap(~socialInfo2)
print(fig[[3]])
####
#to DO correct or incorrect answer....
####
my.data$c_choice<-factor(my.data$key_resp_direction.corr,labels=c("incorrect","correct"))
my.data$norm3<-ifelse(my.data$social==0,'incong',my.data$norm2)
my.data$norm3<-factor(my.data$norm3,labels=c("NONE","SAME","ONLY","incong"))
summary_confidence_cor<-
  group_by(my.data,participant,c_choice,norm3)%>%
  summarise(meanCorrL=mean(zConf,na.rm=T))%>%
  group_by(c_choice,norm3) %>%
  summarise(meanCorr=mean(meanCorrL),sdCorr=sd(meanCorrL)/sqrt(no.part))%>%
  mutate(se.min=meanCorr-sdCorr,se.max=meanCorr+sdCorr)
fig[[4]]<-ggplot(aes(y=meanCorr,x=norm3,fill=norm3),data=summary_confidence_cor)+geom_bar(stat='identity',position="dodge")+
  geom_errorbar(aes(ymin=se.min,ymax=se.max),width=0.1,position=position_dodge(.9))+
  ylab("Confidence")+
  xlab("")+theme_classic()+
  theme(text=element_text(size=20),legend.position='none',legend.title=element_blank(),legend.key.size = unit(2, 'lines'),legend.text=element_text(size=8))+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=c(colorScheme,"grey"))+
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 1.2))+facet_wrap(~c_choice)
print(fig[[4]])
####################
#four panels valid/invalid SI ~ correct incorrect choice
###############
summary_confidence_cor<-
  group_by(my.data,participant,c_choice,social3,norm3)%>%
  summarise(meanCorrL=mean(zConf,na.rm=T))%>%
  group_by(c_choice,social3,norm3) %>%
  summarise(meanCorr=mean(meanCorrL),sdCorr=sd(meanCorrL)/sqrt(no.part))%>%
  mutate(se.min=meanCorr-sdCorr,se.max=meanCorr+sdCorr)
fig[[5]]<-ggplot(aes(y=meanCorr,x=norm3,fill=norm3),data=summary_confidence_cor)+geom_bar(stat='identity',position="dodge")+
  geom_errorbar(aes(ymin=se.min,ymax=se.max),width=0.1,position=position_dodge(.9))+
  ylab("Confidence")+
  xlab("")+theme_classic()+
  theme(text=element_text(size=20),legend.position='none',legend.title=element_blank(),legend.key.size = unit(2, 'lines'),legend.text=element_text(size=8))+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=c(colorScheme,"grey"))+
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 1.2))+facet_grid(social3~c_choice)
print(fig[[5]])
#do players follow social information effect on confidence
my.data$follow<-my.data$key_resp_direction.corr*my.data$social
my.data$norm3
summary_confidence_cor<-
  filter(my.data,norm3!="incong")%>%
  group_by(participant,follow,norm3)%>%
  summarise(meanCorrL=mean(zConf,na.rm=T))%>%
  group_by(follow,norm3) %>%
  summarise(meanCorr=mean(meanCorrL),sdCorr=sd(meanCorrL)/sqrt(no.part))%>%
  mutate(se.min=meanCorr-sdCorr,se.max=meanCorr+sdCorr)
fig[[6]]<-ggplot(aes(y=meanCorr,x=norm3,fill=norm3),data=summary_confidence_cor)+geom_bar(stat='identity',position="dodge")+
  geom_errorbar(aes(ymin=se.min,ymax=se.max),width=0.1,position=position_dodge(.9))+
  ylab("Confidence")+
  xlab("")+theme_classic()+
  theme(text=element_text(size=20),legend.position='none',legend.title=element_blank(),legend.key.size = unit(2, 'lines'),legend.text=element_text(size=8))+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_fill_manual(values=c(colorScheme,"grey"))+
  guides(fill = guide_legend(keywidth = 1.2, keyheight = 1.2))+facet_wrap(~follow)
print(fig[[6]])

