library(tidyr)
library(ggplot2)
library(readr)

### Read Data
dat<-read_csv("C:/Users/nstephens/Downloads/AWS OpenSearch - Price Perf Comparison - Copy of Throughput.csv")
dat$algo_name<-gsub("raft_","",dat$algo_name)

### Tidy
recall<-tapply(dat$recall,list(dat$metric,dat$algo_name,dat$n_queries),c)
value<-tapply(dat$value,list(dat$metric,dat$algo_name,dat$n_queries),c)
smooth<-tapply(dat$value,list(dat$metric,dat$algo_name,dat$n_queries),function(x) c(x)*0)
z<-seq(0.8,0.999,len=200)
for(i in 1:2) for(j in 1:2) for(k in 1:2) smooth[[i,j,k]]<-approx(recall[[i,j,k]],value[[i,j,k]],z)
appr<-do.call(rbind,lapply(smooth,data.frame))
nams<-expand.grid(append(list(as.character(seq(z))),dimnames(smooth)))
out<-data.frame(nams,appr)
names(out)<-c('id','metric','algo','batch','recall','value')

### Plot
out1<-out[out$metric=='latency',]
out2<-out[out$metric=='throughput',]
ggplot(out1,aes(recall,value, col=algo)) + 
  geom_line() +
  facet_wrap(~metric+batch)
ggplot(out2,aes(recall,value, col=algo)) + 
  geom_line() +
  facet_wrap(~metric+batch)

### Output
out_wide<-out %>%
  pivot_wider(
    id_cols = recall,
    names_from = c(metric,algo,batch),
    values_from = value
  )
out_wide<-out_wide[!apply(is.na(out_wide[,-1]),1,all),]
write.csv(out_wide,"price-perf-smooth.csv",row.names = FALSE,na = "")

