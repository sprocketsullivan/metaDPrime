#summary of player choices
#1. accuracy

require(ggplot2)
require(gridExtra)

vintage.col<-c("#5C4B51","#8CBEB2","#F2EBBF","#F3B562","#F06060")
summary_plots<-list()
for (no in 1:length(unique(my.data$participant))){
  help.data<-subset(my.data,participant==unique(my.data$participant)[no])
  current.id<-unique(my.data$participant)[no]
  p.acc<-ggplot(aes(y=key_resp_direction.corr,x=norm3),data=help.data)+stat_summary(fun.y="mean",geom="bar")+ggtitle(paste("Accuracy",current.id))+theme_classic()+geom_abline(intercept=0.5,slope=0)
  #2. left and right answers
  help.data$keys.trans<-ifelse(help.data$key_resp_direction.keys=="left",1,0)
  p.key<-ggplot(aes(y=keys.trans,x=norm3),data=help.data)+stat_summary(fun.y="mean",geom="bar")+theme_classic()+geom_abline(intercept=0.5,slope=0)+ylim(0,1)+ggtitle(paste("Choices Left\n",current.id))
  #reaction times distributions
  p.rt<-ggplot(aes(x=key_resp_direction.rt),data=help.data)+geom_density(aes(fill=factor(key_resp_direction.corr,labels=c("wrong","correct")),alpha=0.5))+guides(alpha = "none")+theme_classic()+facet_wrap(~norm3)+scale_fill_manual(values=vintage.col[c(5,2)])+theme(legend.title=element_blank())+ggtitle("Reaction times")+theme(legend.position="bottom")+xlab("Reaction time in s")
  #confidence distribution
  p.conf<-ggplot(aes(x=zConf),data=help.data)+geom_histogram(aes(y=..density..,fill=factor(key_resp_direction.corr,labels=c("wrong","correct"))),position="dodge")+geom_density(aes(fill=factor(key_resp_direction.corr,labels=c("wrong","correct")),alpha=0.5))+guides(alpha = "none")+theme_classic()+facet_wrap(~norm3)+scale_fill_manual(values=vintage.col[c(5,2)])+theme(legend.title=element_blank())+ggtitle("Confidence")+xlab("Normalized Confidence")+theme(legend.position="bottom")
  #d prime and meta d-prime
  #make sure you fit the data in fir meta d rime to data file
  #data frame is prime.res
  plot.prime.data<-melt(prime.res,id.vars=c("participant","norm"),measure.vars=c("meta_d_prime","d_prime","prime_diff"))
  p.dprime<-ggplot(aes(y=value,x=norm),data=subset(plot.prime.data,participant==unique(my.data$participant)[no]))+stat_summary(fun.y="mean",geom="bar")+facet_wrap(~variable)+theme_classic()
  #add later on the LBA parameter
  summary_plots[[no]]<-arrangeGrob(p.acc,p.key,p.rt,p.conf,p.dprime,layout_matrix=cbind(c(1,3,3,5),c(2,4,4,5)))
}

ggsave("summary.pdf", marrangeGrob(grobs = summary_plots, nrow=1,ncol=1))

pdf("summary.pdf")
invisible(lapply(summary_plots, print))
dev.off()


dev.off()


