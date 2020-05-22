# 宣告學習曲線計算函式
y1.f <- function(x,b){  # x: 累計件數
  100000*(x^b)
}
# 宣告本例相關常數
title <- "人工工時與學習曲線" # 圖表標題
xy <- data.frame(x = seq(0,100,by=10)) # xy 軸範圍
x.label <- '累計件數' # x軸標籤
y.label <- '每件人工小時' # y軸標籤
lgnd.title <- '學習率' # 圖例標題
lr <- c(0.9,0.8,0.7) # 學習率組
colors= c('#FF2345','#34FF45','#AD34AE') # 各學習率線圖顏色對應
# 宣告本例點狀圖資料
p.df <- data.frame(x=c(1,seq(2,30,by=2)))
for (i in 1:length(lr)){
  p.df[,i+1] <- y1.f( # 呼叫自訂函式計算y值
    p.df$x, # 以p.df的x欄資料依序傳入函式
    log(lr[i])/log(2) # 學習曲線指數
  ) # 讀者執行至此可於console 執行print(p.df)指令視其結果
}
# 使用ggplot 繪圖
library(ggplot2)
p<-ggplot(
  data=p.df,
  mapping=aes(x=p.df[,1],y=NULL))+ # 指定x、y軸資料(暫時為NULL)
  ggtitle(title)+ # 圖標題
  xlab(x.label)+ylab(y.label)+  # 給予xy軸標籤
  theme(   # xy軸標籤的字體、顏色、大小等
    axis.title.x = element_text(color = "#56ABCD", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#993333", size = 12, face = "bold")
  )+
  theme(axis.text.x = element_text(size = 10))+  # 給予x 軸字體大小
  scale_x_discrete(limits=p.df$x)+  # 依x軸各值標示
  scale_colour_manual(lgnd.title,values =colors)  # 圖例依線圖顏色對應標示於圖右上

# 宣告不同點狀圖及連線圖的疊加函數
s.f <- function(s,i){  # s: ggplot 物件 i: 對應學習率組
  # 使用geom_point 及 geom_path將線圖疊加於plot 物件上
  # 自動將每一 x軸值及args 的參數值帶入y1.f函式計算出y軸值
  # 產生不同線圖的圖例文字標示
  s<- s+
    geom_point( # 點狀圖疊加於plot 物件上
      data=data.frame(p.df[,i+1]), # 資料為p.df的 i+1 欄位資料
      aes(y=p.df[,i+1], # y 軸同上資料
          colour = as.character(lr[i]) # 圖例顏色對應之文字
      )
    )+ # 畫出各點點狀
    geom_path( # 線圖疊加於plot 物件上
      data=data.frame(p.df[,i+1]), #同上述
      aes(y=p.df[,i+1], #同上述
          colour = as.character(lr[i]) # 圖例顏色對應之文字
      )
    ) # 疊加畫出各點連線
}
# 利用迴圈呼叫自訂函式s.f繪出疊加線圖
for (i in 1:length(lr)){
  p <-s.f(p,i)
}
print(p)  # 印出本例繪圖物件
