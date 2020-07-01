##########方法一: 不使用外掛套件##################
# 步驟1.模擬常態分布下平均數及全距管制圖在樣本大小為n的上下限因子(factor)
# 產生在常態分佈平均值=0，標準差=1，1萬筆數字做為群體觀測值
gd<-rnorm(n=1e4, mean=0, sd=1)  # 函式說明如本書附錄A
gd.mean<-mean(gd)    #  計算群體平均值
gd.sd<-sd(gd) # 計算群體標準差

n=4  # 本實例的樣本大小(sample size)
# 模擬隨機抽樣size=n ，subgroup=1e4(1萬筆)
sample<-lapply(rep(n,each=1e4),FUN=function(x) {sample(gd, size=x)})
# 產生隨機抽樣全距結果vector
sample.R<-c()  # 產生隨機抽樣(size=n)全距值100萬筆
for (i in 1:length(sample)){
  sample.R<- c(sample.R,range(sample[i])[2]-range(sample[i])[1])
}
sample.R.mean<-mean(sample.R) # 模擬隨機抽樣全距平均值
sample.R.sd <- sd(sample.R) # 模擬隨機抽樣全距標準差
d3<-sample.R.sd  # 群組全距標準差
print(d3) # 印出模擬群組之全距標準差
d2<- sample.R.mean/gd.sd # 請參閱表11-1(b)公式定義及說明
print(d2) # 印出模擬群組之推估標準差之因子
D4<- 1+3*d3/d2  # 計算在樣本數n的R chart上限因子(factor)
print(D4) # 印出樣本數n的 D4 
D3<- max(0,1-3*d3/d2)  # 計算在樣本數n的R chart下限因子(factor)
print(D3) # 印出樣本數n的 D3
A2<-3/(d2*sqrt(n)) # 計算在樣本數n的X-bar chart上、下限因子(factor)
print(A2) # 印出樣本數n之下的A2

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
min.max<- lapply(s.df,range) # 以內建函式range計算各欄之最大與最小值，回傳list 
range.vector<- sapply( # 計算各欄之全距值，回傳vector
  min.max, 
  FUN=function(x){x[2]-x[1]}
)
print(range.vector)  # 5 subgroup 的全距值
mean.R<- mean(range.vector) # 求全距平均數

# 步驟4. 決定全距管制圖的上下管制界限
R.sd.estimated <- mean.R*d3/d2  # 在sample size下的推估全距標準差
print(R.sd.estimated) # 印出推估全距圖標準差
UCL.R <- mean.R+3*R.sd.estimated  # 依定義計算管制圖上限
UCL.D4 <- D4*mean.R # 同上UCL.R, UCL.D4 為解法一，採查表方式
LCL.R <- max(0,mean.R-3*R.sd.estimated)  # 依定義計算管制圖下限
LCL.D3 <- D3*mean.R  # 同上LCL.R, LCL.D3 為解法一，採查表方式, 結果是殊途同歸
print(UCL.R) # 印出本批R chart抽樣管制圖上限
print(UCL.D4) # 為解法一，採查表方式，結果是殊途同歸
print(LCL.R) # 印出本批R chart抽樣管制圖下限
print(LCL.D3) # 為解法一，採查表方式，結果是殊途同歸

# 步驟5.求得樣本的平均數及管制圖上下限
mean.sample <- sapply( # 計算各欄之平均值，回傳vector物件
  s.df,
  mean)
mean.X<- mean(mean.sample) # 求各抽樣樣本的平均數的再平均
X.sd.estimated <- mean.R/d2 # sample size=n之下推估標準差同表11-1(b)之w
UCL.X <- mean.X + 3*X.sd.estimated/sqrt(n)  # X-bar的管制圖上限
UCL.A2 <- mean.X + A2*mean.R   # 為解法一，採查表方式，結果是殊途同歸
LCL.X <- mean.X - 3*X.sd.estimated/sqrt(n)   # X-bar的管制圖下限
LCL.A2 <- mean.X - A2*mean.R   # 為解法一，採查表方式，結果是殊途同歸
print(UCL.X)  # 印出本批X-bar chart抽樣管制圖上限
print(UCL.A2)  # 為解法一，採查表方式，結果是殊途同歸
print(LCL.X)  # 印出本批X-bar chart抽樣管制圖下限
print(LCL.A2)  # 為解法一，與採查表方式，結果是殊途同歸


##########方法二: 使用外掛套件qcc##################
library(qcc) # 載入qcc 程式庫
value <-c(   # 螺絲的直徑量測值
  0.5014,0.5022,0.5009,0.5027,
  0.5021,0.5041,0.5024,0.5020,
  0.5018,0.5026,0.5035,0.5023,
  0.5008,0.5034,0.5024,0.5015,
  0.5041,0.5056,0.5034,0.5047)
group <-c(   # 將量測值依序分組
  1,1,1,1,
  2,2,2,2,
  3,3,3,3,
  4,4,4,4,
  5,5,5,5)
# qcc.groups: 根據樣本指標對數據進行分組；此一功能，可以輕鬆地將數據分組
diameter <- qcc.groups( # 分組的陣列(R物件matrix)
  data=value,   # 觀察值
  sample=group) # 分組序碼
r.chart <- qcc( # 產生管制圖的繪圖物件
  diameter,  # data為第一個參數，可省略參數指定(data=)
  type="R",  # 產生R(全距)圖
  nsigmas=3, # 管制線依據3個標準差計算
  ylab='R(全距)值',  # y軸標籤
  xlab='樣本組',     # x軸標籤
  plot=TRUE)         # 於console繪製出管制圖
x.bar <- qcc(  # 同上
  diameter,    # 同上
  type="xbar", # 產生X-bar圖
  nsigmas=3,   # 同上
  ylab='平均值',  # 同上
  xlab='樣本組',  # 同上
  title='X-Bar Chart \n (螺絲的直徑)', # 自訂管制圖標題 \n 為換行符號
  plot=TRUE)      # 同上
