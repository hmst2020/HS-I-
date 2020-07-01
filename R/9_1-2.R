#########################################
120000*100**-0.3219282 # 第10件所需完成之時間

##########方法一 : 使用內建繪圖函數繪製學習曲線#######
curve(   # R 語言內建套件graphics之曲線繪圖函式
  120000*x**-0.3219282,  # y 值的計算公式
  from=0,   # 指定x 值的起始值
  to=100,   # 指定x 值的結束值
  ylab ='小時,每件完成時間',  # y 軸標籤
  xlab ='累積件數'  # x 軸標籤
)

#####方法二 :使用套件ggplot2繪圖函式ggplot，及##########
#####自訂學習曲線計算函式指定予stat_function，
#####使曲線呈現平滑 ###########################
# 宣告學習曲線計算函式
# x:  累計件數
y1.f <- function(x){
  120000*(x^-0.3219282)
}
print(y1.f(10))  # 印出第10件所需完成之時間
# 宣告本例相關常數
title <- "學習率為80%的學習曲線" # 圖表標題
xy <- data.frame(x = c(0,100),y=c(0,0)) # xy 軸範圍
x.label <- '累計件數' # x軸標籤
y.label <- '每件完成時間' # y軸標籤
# 使用ggplot 繪圖
library(ggplot2)
p<-ggplot(  # 產生繪圖物件
  data=xy,  # 繪圖資料
  mapping=aes(
    x=x,y=y) # 指定x、y軸資料
)+ 
  ggtitle(title)+ # 圖標題
  xlab(x.label)+ylab(y.label)+  # 給予xy軸標籤
  theme(   # xy軸標籤的字體、顏色、大小等
    axis.title.x = element_text(color = "#56ABCD", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#993333", size = 12, face = "bold")
  )+
  xlim(1,100)+  # 畫出x軸的範圍，本例為第1至第100件
  stat_function( # 使用統計函式stat_function令每一x軸上的值分別傳入fun指定函式計算其y軸數值
    fun = y1.f,  # fun 對應上述宣告之自訂函式
    n = 1000     # 於x 軸的資料範圍插入細分之點數
  )
print(p) # 印出本例學習曲線

######方法三 :使用geom_point及geom_path來構成點狀圖及其連線圖#####
# 宣告學習曲線計算函式
# x:  累計件數
y1.f <- function(x){
  120000*(x^-0.3219282)
}
# 宣告本例相關常數
title <- "學習率為80%的學習曲線" # 圖表標題
x.label <- '累計件數' # x軸標籤
y.label <- '每件完成時間' # y軸標籤
# 宣告本例點狀圖資料
p.df <- data.frame( # 點狀繪圖資料為data frame 物件
  x=c(1,seq(2,10,by=2),20,40,60,80,100) # x 軸標示點
)
p.df$y <- y1.f( # 呼叫自訂函式計算y值
  p.df$x  # 以p.df的x欄資料依序傳入函式
) # 讀者執行至此可於console 執行print(p.df)指令視其結果

# 使用ggplot 繪圖
library(ggplot2)
p<-ggplot( # 產生繪圖物件
  data=p.df, # 繪圖資料
  mapping=aes(x=x,y=y) # 指定x、y軸資料
)+ 
  ggtitle(title)+ # 圖標題
  xlab(x.label)+ylab(y.label)+  # 給予xy軸標籤
  theme(   # xy軸標籤的字體、顏色、大小等
    axis.title.x = element_text(color = "#56ABCD", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#993333", size = 12, face = "bold")
  )+
  theme(axis.text.x = element_text(size = 10))+  # 給予x 軸字體大小
  geom_point()+ # 畫出各點點狀圖
  geom_path()+  # 疊加畫出各點連線
  scale_x_continuous(breaks=p.df$x)+  # 依x軸各值標示
  scale_y_continuous(breaks=p.df$y)   # 依y軸各值標示
print(p)  # 印出本例學習曲線

########多學習率的曲線比較##########################
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
  scale_x_continuous(breaks=p.df$x)+  # 依x軸各值標示
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
