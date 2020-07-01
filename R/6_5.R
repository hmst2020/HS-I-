# 宣告單利率計算函式
# y: 第幾年 rt: 利率 p:本金
y1.f <- function(y,rt,p){ 
  p*(1+rt*y)
}
# 宣告本例相關常數
prnspl <- 100 # 本金(principal)
title <- paste0('100元單利率未來值') # 圖表表題
xy <- data.frame(x = c(1,20),y=c(0,5000)) # xy 軸範圍
x.label <- '年' # x軸標籤
y.label <- '本+年利' # y軸標籤
lgnd.title <- '年利率' # 圖例標題
rts= c(0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18) # 年利率組
sizes= c(0.2,0.4,0.6,0.8,1,1.2,1.4,1.8,2) # 線條粗細對應
colors= c('black','green','blue','#345678','grey62','#AD8945','#56DD94','#987654','red') # 各利率線圖顏色對應
# 使用ggplot 繪圖
library(ggplot2)
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
      n = 2,        # 線圖依x軸計算y軸值之內插點數，本例為直線至少2即可，
      args = list(p=p,rt = rt), # y1.f函式傳入值除了第一個引數外，其它附加的引數
      mapping=aes(colour = as.character(rt)), # 線圖顏色及圖例不同線圖的文字標示
      size = size  # 線圖粗細
    )
  return(s)
}
# 利用迴圈繪出疊加線圖
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
