# 本例需先安裝 xlsx，RSQLite，qcc 等package
# 步驟1.模擬常態分布下平均數及全距管制圖在樣本大小為n的上下限因子(factor)
# 產生在常態分佈平均值=0，標準差=1，1萬筆數字做為群體觀測值
gd<-rnorm(n=1e4, mean=0, sd=1)
gd.mean<-mean(gd)
gd.sd<-sd(gd)

n=5
sample<-lapply(rep(n,each=1e4),FUN=function(x) {sample(gd, size=x)})
sample.R<-c()
for (i in 1:length(sample)){
  sample.R<- c(sample.R,range(sample[i])[2]-range(sample[i])[1])
}
sample.R.mean<-mean(sample.R)
sample.R.sd <- sd(sample.R)
d3<-sample.R.sd
print(d3)
d2<- sample.R.mean/gd.sd
print(d2)
D4<- 1+3*d3/d2
print(D4)
D3<- max(0,1-3*d3/d2)
print(D3)
A2<-3/(d2*sqrt(n))
print(A2)

# 步驟2. 蒐集數據
#  讀入 Excel 資料
library(xlsx)
xlsx.data <-read.xlsx2(
  'connector_cpk.xls',
  sheetIndex=1,
  header=TRUE,
  startRow=2,
  stringsAsFactors=FALSE)
xlsx.data <- xlsx.data[-c(21),]
# end of read Excel data ############

# 步驟3. 計算每個樣本的全距與全距平均數
s.df<- xlsx.data[,2:6]
#s.df<- as.data.frame(t(as.data.frame(sapply(s.df,as.numeric))))
min.max<- as.data.frame(apply(apply(X=s.df,MARGIN=1,FUN=range),MARGIN=2,FUN=as.numeric),stringsAsFactors=FALSE)
#min.max<- lapply(s.df,range)
range.vector<- sapply(
  min.max, 
  FUN=function(x){as.numeric(x)[2]-as.numeric(x)[1]}
)
print(range.vector)
mean.R<- mean(range.vector)

# 步驟4. 決定全距管制圖的上下管制界限
R.sd.estimated <- mean.R*d3/d2
print(R.sd.estimated)
UCL.R <- mean.R+3*R.sd.estimated
UCL.D4 <- D4*mean.R
LCL.R <- max(0,mean.R-3*R.sd.estimated)
LCL.D3 <- D3*mean.R 
print(UCL.R)
print(UCL.D4)
print(LCL.R)
print(LCL.D3)

# 步驟5.求得樣本的平均數及管制圖上下限
mean.sample <- apply(
  X=s.df,
  MARGIN=1,
  FUN=function(x){mean(as.numeric(x))})
mean.X<- mean(mean.sample)
X.sd.estimated <- mean.R/d2
UCL.X <- mean.X + 3*X.sd.estimated/sqrt(n)
UCL.A2 <- mean.X + A2*mean.R
LCL.X <- mean.X - 3*X.sd.estimated/sqrt(n)
LCL.A2 <- mean.X - A2*mean.R
print(UCL.X)
print(UCL.A2)
print(LCL.X)
print(LCL.A2)

# 步驟6.繪製管制圖
library(ggplot2)
y.scale <- c(LCL.R,mean.R,UCL.R)
gpnum<-nrow(s.df)
p.df<- as.data.frame(range.vector)
p<-ggplot(
  data=p.df,
  mapping=aes(x=as.numeric(rownames(p.df)),y=p.df[,1])
  )+ 
  ggtitle('R Chart \n (連接器尺寸)')+
  xlab('樣本組')+ylab('R(全距)值')+
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(color = "#56ABCD", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#993333", size = 12, face = "bold")
  )+
  theme(axis.text.x = element_text(size = 10))+
  geom_point()+
  geom_path()+
  geom_segment(mapping=aes(x=1,xend=gpnum,y=UCL.R,yend=UCL.R,linetype='dashed'))+
  geom_segment(mapping=aes(x=1,xend=gpnum,y=LCL.R,yend=LCL.R,linetype='dashed'))+
  geom_segment(mapping=aes(x=1,xend=gpnum,y=mean.R,yend=mean.R,linetype='solid'))+
  annotate("text", x = gpnum+1, y = UCL.R, label = "UCL")+
  annotate("text", x = gpnum+1, y = LCL.R, label = "LCL")+
  annotate("text", x = gpnum+1, y = mean.R, label = "CL")+
  scale_linetype_identity() +
  scale_x_discrete(limits=rownames(p.df))+
  scale_y_continuous(breaks=y.scale)
windows()
print(p)

library(gridExtra)
library(grid)
library(qcc)
r.chart <- qcc(
  s.df,
  type="R",
  nsigmas=3,
  ylab='R(全距)值',
  xlab='樣本組',
  plot=FALSE)
bottom.legend <- data.frame(
  col1 = c(paste0('Number of groups=',gpnum),paste0('Center=',round(mean.R,digits=5)),paste0('StdDev=',round(R.sd.estimated,digits=5))),
  col2 = c('',paste0('LCL=',round(LCL.R,digits=5)),paste0('UCL=',round(UCL.R,digits=5))),
  col3 = c('',paste0('Number beyond limits=',0),paste0('Number violating runs=',0)))
g <- tableGrob(bottom.legend, rows = NULL, cols = NULL,theme=ttheme_minimal(core = list(fg_params=list(hjust=0.02, x=0.05))))
b <- arrangeGrob(p, g, nrow = NULL, heights = unit(c(3, .5),c("null", "null","null")))
grid.newpage()
grid.draw(b)