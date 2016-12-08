require(truncnorm)
#simulate confidence
lba.par<-summary.dmc(samples2)
#simulation
#first draw start point from A for both BAs
get_conf<-function(n.sim,A_est,v_high_est,v_sd_high_est,v_low_est,v_sd_low_est,t0_est,B_est){
  A<-runif(n=n.sim*2,min=0,max=A_est)
  A<-matrix(A,ncol=2)
  v<-matrix(0,ncol=2,nrow=n.sim)
  #v[1]<-rtruncnorm(n=1,mean=v_high_est,sd=v_sd_high_est,a=0)
  #v[2]<-rtruncnorm(n=1,mean=v_low_est,sd=v_sd_low_est,a=0)
  v[,1]<-rtnorm(n=n.sim,mean=v_high_est,sd=v_sd_high_est,lower=0)
  v[,2]<-rtnorm(n=n.sim,mean=v_low_est,sd=v_sd_low_est,lower=0)
  t0<-t0_est
  b<-A_est+B_est
  ttf<-rep(0,n.sim*2)
  ttf<-c(t0+(b-A[,1])/v[,1],t0+(b-A[,2])/v[,2])
  ttf<-matrix(ttf,ncol=2)
  winning.lba<-apply(ttf,1,function(x)which(x==min(x)))
  ttf_win<-ifelse(winning.lba==1,ttf[,1],ttf[,2])
  evidence.diff<-A[,1]+v[,1]*ttf_win-A[,2]+v[,2]*ttf_win
  return(data.frame(conf=evidence.diff,response=winning.lba,ttf_win))
}
########
#simulate confidence

nsim=50000
conf.sim<-get_conf(n.sim=nsim,A_est=lba.par$statistics[1,1],v_high_est=lba.par$statistics[3,1],
         v_sd_high_est=lba.par$statistics[3,2],v_low_est=lba.par$statistics[4,1],
         v_sd_low_est=lba.par$statistics[4,2],t0_est=lba.par$statistics[5,1],
         B_est=lba.par$statistics[2,1])

require(ggplot2)
rt.sim<-rLBA(n=nsim, A=lba.par$statistics[1,1],
             b=(lba.par$statistics[2,1]+lba.par$statistics[1,1]),
             t0 = lba.par$statistics[5,1], 
             mean_v=c(lba.par$statistics[3,1],lba.par$statistics[4,1]), 
             sd_v=c(lba.par$statistics[3,2],lba.par$statistics[4,2]))
conf.sim$rt2<-rt.sim$rt
p.1<-ggplot(aes(x=ttf_win,fill=factor(response)),data=conf.sim)+geom_density(alpha=0.3)
p.2<-ggplot(aes(x=rt,fill=factor(response)),data=rt.sim)+geom_density(alpha=0.3)
conf.sim$response2<-ifelse(conf.sim$response==2,0,1)

require(gridExtra)
grid.arrange(p.1,p.2)
hist(help.data$zConf)
conf.sim$zconf<-scale(conf.sim$conf)

ggplot(aes(x=zConf,fill=factor(key_resp_direction.corr)),data=help.data)+geom_density()+theme_classic()+geom_density(aes(x=zconf,fill=factor(response2),alpha=0.3),data=conf.sim)

