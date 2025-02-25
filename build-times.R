library(tidyr)
library(ggplot2)
library(readr)

### Plots

x <- read.csv("AWS OpenSearch - Price Perf Comparison.csv")
x$build_time<-factor(round(x$build.time))
x <- x[grep("hnsw",x$index_name),]

x %>%
  ggplot(aes(recall,throughput)) +
  geom_line(data=y,aes(x,y),col='lightgrey') +
  geom_line(col='#76b900') +
  geom_point(col="#76b900") +
  facet_wrap(~build_time,ncol=3) +
  labs(title="Throughput vs Recall",
       subtitle="By Index Build Times (Seconds)",
       caption="BIGANN-10M; batch size 10; k=10") +
  xlab("Recall") +
  ylab("Throughput (QPS)") +
  scale_y_continuous(labels = label_number(big.mark=","))


x %>%
  ggplot(aes(recall,throughput,col=build_time)) +
  geom_point() +
  labs(title="Throughput vs Recall",
       subtitle="(Seconds)",
       caption="BIGANN-10M; batch size 10; k=10") +
  xlab("Recall") +
  ylab("Throughput (QPS)") +
  guides(color = guide_legend(title = "Index Build Time",
                              nrow=1, 
                              byrow=TRUE)) +
  theme(legend.position="top") +
  scale_y_continuous(labels = label_number(big.mark=","))

