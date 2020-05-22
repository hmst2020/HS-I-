# 繪出表格
# 宣告複利率計算函式
# y: 第幾年 rt: 利率 p:本金
y1.f <- function(y,rt,p){
  p*(1+rt)^y
}
prnspl <- 100  # 本金(principal)
y=c(0:20) # 投資年數組
rts= c(0.05,0.10,0.15) # 年利率組
# 表格資料依投資年數各一列(row)
f.data <- data.frame(
  year=y)
# 表格資料依年利率組各產生一行(column)
for (i in 1:length(rts)){
  # 每行資料由本金、利率與投資年數由函式y1.f分別得出
  c.data=c()
  for (j in 1:length(y)){c.data[j] <- round(y1.f(y[j],rts[i],prnspl),digits=0)}
  f.data <- cbind(f.data, c.data)  # 使用cbind將產生的行資料c.data併入f.data
}
# 依利率重新給予新的欄位名稱
c.names <- c(NULL)
for (i in 1:length(rts)){
  c.names[i] <- paste0('年利率',rts[i]*100,'%')
}
colnames(f.data) <- c('投資年數',c.names) # 置換成新的欄位名稱
library(gridExtra)
library(grid)
library(ggplot2)
grid.newpage()  # 產生Studio Plots 新頁 
grid.table(  # 繪出表格，函式說明請參閱附錄a
  d=f.data,rows=NULL,theme=ttheme_default(colhead = list(fg_params=list(cex = 0.8)),base_size = 10)
)


# 繪出曲線
# 宣告複利率計算函式
# y: 第幾年 rt: 利率 p:本金
y1.f <- function(y,rt,p){
  p*(1+rt)^y
}
# 宣告本例相關常數
prnspl <- 100 # 本金(principal)
title <- "$100 Accumulated Amount at the end of t years \n with compound interest" # 圖表表題
xy <- data.frame(x = c(1,20),y=c(0,5000)) # xy 軸範圍
x.label <- 'Year' # x軸標籤
y.label <- 'Accumulated Amount(million)' # y軸標籤
lgnd.title <- 'Interest/year' # 圖例標題
rts= c(0.05,0.10,0.15) # 年利率組
sizes= c(0.2,0.7,1.2) # 線條粗細對應
colors= c('#FF2345','#34FF45','#AD34AE') # 各利率線圖顏色順序對應
# 使用ggplot 繪圖
library(ggplot2)
j<- 1
p<-ggplot(
  data=xy,  # 繪圖資料來源
  mapping=aes(x=x,y=y)  # x、y軸在引數data 的對應行
  )+
  ggtitle(title)+ # 圖標題
  xlab(x.label)+ylab(y.label)+  # 給予xy軸標籤
  theme(   # xy軸標籤的字體、顏色、大小等
    axis.title.x = element_text(color = "#56ABCD", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#993333", size = 12, face = "bold")
  )+
  xlim(0,20)+  # 畫出x軸的範圍，本例為20年
  scale_colour_manual(lgnd.title, values =colors)  # 圖例依線圖顏色對應標示於圖右上
# 宣告ggplot 疊加線圖函式
s.f <- function(s,rt,p,size){
  s<- s+
    stat_function(  # 使用stat_function將線圖疊加於plot 物件
      fun = y1.f,   # 自動將每一 x軸值範圍(本例為c(0,20)以及args 的參數值帶入y1.f函式計算出y軸值
      n = 1000,     # 線圖依x軸計算y軸值之內插點數，本例為拋物曲線此數字影響平滑程度
      args = list(p=p,rt = rt), # y1.f函式傳入值除了第一個引數外，其它附加的引數
      mapping=aes(colour = as.character(rt)), # 線圖顏色及圖例不同線圖的文字標示
      size = size  # 線圖粗細
    )
  return(s)
}
# 利用迴圈繪出疊加曲線圖
for (i in 1:length(rts)){
  p <-s.f(    # 使用自訂函式將繪圖物件疊加各線條
    p,        # ggplot繪圖物件
    rts[i],   # 利率對應
    prnspl,   # 本金
    sizes[i]  # 線圖粗細
  )
}
# 顯示圖形
print(p)
