#########
# meta D prime functions
bin_function <- function(filtered.data,nBins=3){
  #bins confidence ratings into bins
  cutpoints <-as.numeric(quantile(filtered.data$zConf,(0:nBins)/nBins,na.rm=T))
  if(length(unique(cutpoints))!=length(cutpoints))
    cutpoints <-as.numeric(quantile(jitter(filtered.data$zConf),(0:nBins)/nBins,na.rm=T))  
  max.cut<-max(cutpoints)
  min.cut<-min(cutpoints)
  filtered.data$zConf<-ifelse(filtered.data$zConf>max.cut,max.cut,filtered.data$zConf)
  filtered.data$zConf<-ifelse(filtered.data$zConf<min.cut,max.cut,filtered.data$zConf)
  #cutpoints <-cutpoints + seq_along(cutpoints) * .Machine$double.eps # in case that two cutpoints are the same - introduce jitter to separate them
  filtered.data$binned <-cut(filtered.data$zConf,cutpoints,include.lowest=TRUE,na.rm=T) #na.rm=T - exclude mising values from the analysis - are we expecting any after all the filtering?
  #filtered.data$binned <-.bincode(filtered.data$zConf,cutpoints,include.lowest=TRUE)
  return(factor(filtered.data$binned))
}
DataMetaD<- function (filtered.data, nBins=3) {
  filtered.data$control_var<-1
  # Calculates nR_S1, nR_S2
  nR_S1<-
    filter(filtered.data,key_resp_direction.keys=="left")%>%
    group_by(key_resp_direction.corr,binned)%>%
    summarise(conf_count=sum(control_var))%>%
    spread(key_resp_direction.corr,value=conf_count,drop=F,fill=0)
  nR_S1<-c(as.numeric(nR_S1$`1`)[nBins:1],as.numeric(nR_S1$`0`))
  nR_S1[is.na(nR_S1)]<-0
  nR_S2<-
    filter(filtered.data,key_resp_direction.keys=="right")%>%
    group_by(key_resp_direction.corr,binned)%>%
    summarise(conf_count=sum(control_var))%>%
    spread(key_resp_direction.corr,value=conf_count,drop=F,fill=0)
  nR_S2<-c(as.numeric(nR_S2$`0`)[nBins:1],as.numeric(nR_S2$`1`))
  nR_S2[is.na(nR_S2)]<-0
  return(c(nR_S1,nR_S2))
}


FitMetaD <- function (count) {
  require(rjags)
  require(coda)
  #count <- c(nR_S1, nR_S2)
  nTot <- sum(count) #do we need this?
  nRating <- length(count)/4
  forJags <- list(counts=count,nratings=nRating,Tol=0.0001)
  #
  mod.1<-jags.model(file="Bayes_metad2.txt",data = forJags,n.chains=4)
  update(mod.1, 1000)
  mod.1.samp<-jags.samples(mod.1,
                           c('meta_d','d1','c1','cS1','cS2'),
                           1000)
  return(mod.1.samp)
}