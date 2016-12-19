#export for python hddm fitting
#needed is response (correct/incorrect), rt, norm
#pick a suitable player...
#25776 (see summray.pdf)
tpd<-subset(my.data,participant==25776)
to.py.data<-data.frame(rt=tpd$key_resp_direction.rt,response=tpd$key_resp_direction.corr,norm=tpd$norm3,social=paste(tpd$social3,tpd$norm3,sep="_"))
write.csv(to.py.data,file="id_25776.csv",sep=",",dec=".",row.names = F)

