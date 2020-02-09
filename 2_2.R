library(jsonlite)
dns.list <- read_json(
  path='https://www.twnic.net.tw/dnjson.txt',
  simplifyVector = TRUE
)
dns.df <-as.data.frame(
  x=dns.list$com.tw
) 
names(dns.df) <- c('ym','count')
head(dns.df)
tail(dns.df)
y.df <- aggregate(
  formula=count~ substr(ym,1,4),
  data=dns.df[dns.df$ym<202001,],
  FUN=sum
) 
names(y.df) <- c('year','count')
library(ggplot2)
p<-ggplot(
  data=y.df,
  mapping=aes(
    x=year, y=count,
    group = 1
  ))+ 
  ggtitle('域名(com.tw)申請趨勢')+
  xlab('西元年')+ylab('申請數量')+
  geom_point()+
  geom_path()+
  scale_x_discrete(limits=y.df$year)+
  scale_y_continuous(
    breaks=seq(min(y.df$count),max(y.df$count),by=500000))+
  theme(axis.text.x = element_text(angle=60, hjust=1))
print(p)
