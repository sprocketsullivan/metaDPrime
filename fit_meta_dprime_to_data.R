#########
#fit data
######### 

##########
#bin confidence
#########
#S1 left S2 right
bins<-4
cutpoints<-quantile(my.data$zConf,(0:bins)/bins,na.rm=T)
my.data$binned <-cut(my.data$zConf,cutpoints,include.lowest=TRUE,na.rm=T)
summary(binned)
#get S1 and S2 for each participant
my.data$binned<-binned
my.data$control_var<-1
id<-unique(my.data$participant)[1]
nR_S1<-
  filter(my.data,(!is.na(zConf)))%>%
  filter(key_resp_direction.keys=="left"&social==0&participant==id)%>%
  group_by(key_resp_direction.corr,binned)%>%
  summarise(conf_count=sum(control_var))%>%
  spread(key_resp_direction.corr,value=conf_count,drop=F,fill=0)
nR_S1<-c(as.numeric(nR_S1$`1`),as.numeric(nR_S1$`0`))
nR_S2<-
  filter(my.data,(!is.na(zConf)))%>%
  filter(key_resp_direction.keys=="right"&social==0&participant==id)%>%
  group_by(key_resp_direction.corr,binned)%>%
  summarise(conf_count=sum(control_var))%>%
  spread(key_resp_direction.corr,value=conf_count,drop=F,fill=0)
nR_S2<-c(as.numeric(nR_S2$`0`),as.numeric(nR_S2$`1`))

