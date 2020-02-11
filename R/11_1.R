# 方法一: 不使用外掛套件
# 步驟1.模擬常態分布下平均數及全距管制圖在樣本大小為n的上下限因子(factor)
# 產生在常態分佈平均值=0，標準差=1，1萬筆數字做為群體觀測值
gd<-rnorm(n=1e4, mean=0, sd=1)
gd.mean<-mean(gd)
gd.sd<-sd(gd)

n=4
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
s.1 <- c(0.5014,0.5022,0.5009,0.5027) # 樣本組 1 
s.2 <- c(0.5021,0.5041,0.5024,0.5020) # 樣本組 2 
s.3 <- c(0.5018,0.5026,0.5035,0.5023) # 樣本組 3
s.4 <- c(0.5008,0.5034,0.5024,0.5015) # 樣本組 4 
s.5 <- c(0.5041,0.5056,0.5034,0.5047) # 樣本組 5 
s.df<- data.frame( # 整理樣本組資料為 data frame 物件各欄
  s1=s.1,
  s2=s.2,
  s3=s.3,
  s4=s.4,
  s5=s.5
)

# 步驟3. 計算每個樣本的全距與全距平均數
min.max<- lapply(s.df,range)
range.vector<- sapply(
  min.max, 
  FUN=function(x){x[2]-x[1]}
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
mean.sample <- sapply(
  s.df,
  mean)
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
############################# end of 方法一 ################



# 方法二: 使用外掛套件qcc
library(qcc)
value <-c(
  0.5014,0.5022,0.5009,0.5027,
  0.5021,0.5041,0.5024,0.5020,
  0.5018,0.5026,0.5035,0.5023,
  0.5008,0.5034,0.5024,0.5015,
  0.5041,0.5056,0.5034,0.5047)
group <-c(
  1,1,1,1,
  2,2,2,2,
  3,3,3,3,
  4,4,4,4,
  5,5,5,5)
diameter <- qcc.groups(
  data=value,
  sample=group)
r.chart <- qcc(
  diameter,
  type="R",
  nsigmas=3,
  ylab='R(全距)值',
  xlab='樣本組',
  plot=TRUE)
x.bar <- qcc(
  diameter,
  type="xbar",
  nsigmas=3,
  ylab='平均值',
  xlab='樣本組', 
  title='X-Bar Chart \n (螺絲的直徑)', 
  plot=TRUE)


