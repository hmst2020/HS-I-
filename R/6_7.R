# 宣告等比級數和計算函式
# y: 第幾日 rt: 公比 p:本金
y1.f <- function(y,rt,p){
  reslt <- p*(1-rt^y)/(1-rt)/1000000 # 等比級數和的公式(換算以millions回傳)
}
# 宣告本例相關常數
prnspl <- 0.01  # 本金(美元)
title <- '1美分起每日翻倍之未來値' # 圖表標題
xy <- data.frame(x = c(0,30),y=c(0,0)) # xy 軸範圍
x.label <- '天' # x軸標籤
y.label <- '本利和(millions)' # y軸標籤
lgnd.title <- '' # 圖例標題
rt= 2.0  # 等比級數之公比(每日翻倍)
colors <- c('#FF2345','#000000')  # 利率顏色對應
# 使用ggplot 繪圖
library(ggplot2)
p<-ggplot(
  data=xy,  # 繪圖資料來源
  mapping=aes(x,y)  # x、y軸在引數data 的對應行
  )+
  ggtitle(title)+
  xlim(0,30)
p <- p +
  labs(x=x.label,y=y.label)+  # 給予xy軸標籤
  theme( # xy軸標籤的字體、顏色、大小等
    axis.title.x = element_text(color = "#56ABCD", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#993333", size = 12, face = "bold")
  )+
  scale_colour_manual(lgnd.title, values =colors)+
  stat_function(fun = y1.f, n = 1000, args = list(p=prnspl,rt = rt),aes(colour = '一美分未來値'))+
  stat_function(fun = function(x) (3), n = 1000,aes(colour = '3百萬')) 
print(p) # 顯示圖形
