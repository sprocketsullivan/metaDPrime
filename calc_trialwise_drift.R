
calc_trial_drift<-function(A,B,v,t0,RT){
  crit1<-B/v+t0
  crit2<-(B+A)/v+t0
  if(RT<=crit1){
    d_hat<-B/(RT-t0)
    a_hat<-A
  }
  if(RT>crit1&RT<crit2){
    d_hat<-v
    a_hat<-(B+A)-(RT-t0)*v
  }
  if(RT>=crit2){
    d_hat<-(B+A)/(RT-t0)
    a_hat<-0
  }
  return(c(d_hat,a_hat))
}
