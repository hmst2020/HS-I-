# 列印學習曲線
# 心臟移植手術與死亡率模式(Model) Yi = B0+B1*EXP(-B2*X)
death_rate <- function(B0,B1,B2,X){  # 宣告計算死亡率函式B0,B1,B2同下述b0,b1,b2，X 第幾次移植
  return (B0+B1*exp(-B2*X)) # 函式回傳值
}
# 實際死亡率資料, 根據原作者的在1984年 - 1987年搜集的「移植次序與死亡率的記錄」資料
death.a <- c(1,1,0,0,0,1,0,0,0,1,
             0,0,0,1,0,0,0,0,0,0,
             1,0,1,0,0,0,0,0,0,0,
             0,0,1,0,1,0,0,0,0,1,
             0,0,0,0,0,0,1,0,0,0,
             0,0,0,0,0,0,0,0,0,0,
             0,0)

# 宣告本例常數
b0 <- 0.2329  # Model 的 B0 值
b1 <- 0.8815  # Model 的 B1 值
b2 <- 0.2362  # Model 的 B2 值
xs <- c(seq(1,62,by=1)) # 依序第1次至第62次心臟移植
x.scale <- c(1,seq(5,62,by=5)) # x座標標示
y.scale <- c(0,seq(0.15,1,by=0.2))　# ｙ座標標示
title <- "心臟移植手術死亡率的學習曲線" # 圖表標題
x.label <- '累計心臟移植次數' # x軸標籤
y.label <- '一年內死亡率' # y軸標籤
lgnd.title <- '死亡率' # 圖例標題
colors= c(實際='#FF2345',模式預測='#34FF45') # 各學習率線圖顏色命名對應
ys.m <- death_rate(b0,b1,b2,xs) # model 下的死亡率
ys.a <- cumsum(death.a)/xs # 實際累計死亡率
p.df <- data.frame(x=xs,y=ys.m,actual=ys.a)　#繪圖資料源
library(ggplot2)  # 載入繪圖函式庫
p<-ggplot(
  data=p.df,  # 繪圖資料
  mapping=aes(x=p.df$x,y=p.df$y))+ # 指定x、y軸資料
  ggtitle(title)+ # 圖標題
  xlab(x.label)+ylab(y.label)+  # 給予xy軸標籤
  theme(   # xy軸標籤的字體、顏色、大小等
    axis.title.x = element_text(color = "#56ABCD", size = 12, face = "bold"),
    axis.title.y = element_text(color = "#993333", size = 12, face = "bold")
  )+
  theme(axis.text.x = element_text(size = 10))+  # 給予x 軸字體大小
  geom_point( # 疊加畫出Model各點點狀圖
    data=data.frame(p.df$y), # 點狀圖資料
    mapping=aes(y=p.df$y,  # y 軸資料
                colour = '模式預測'  # 點狀圖圖例文字
    ) 
  )+ 
  geom_path( # 疊加畫出Model各點連線
    data=data.frame(p.df$y), # 各點連線資料
    mapping=aes(y=p.df$y,colour = '模式預測') # 連線圖圖例文字
  )+ 
  geom_point(data=data.frame(p.df$actual), # 疊加畫出實際各點點狀圖
             mapping=aes(y=p.df$actual,colour = '實際'))+  # 同上
  geom_path(data=data.frame(p.df$actual),  # 疊加畫出實際各點連線
            mapping=aes(y=p.df$actual,colour = '實際'))+   # 同上
  scale_colour_manual(lgnd.title,values =colors)+ # 圖例標題及顏色
  scale_x_discrete(limits=x.scale)+  # x軸依x.scale各值標示
  scale_y_continuous(breaks=y.scale)   # y軸y.scale各值標示
print(p)  # 印出本例學習曲線


# 列印學習曲線內手術後一年內的死亡率
library(gridExtra) # 載入表格函式庫
library(grid)      # 同上
grid.newpage()
d <- data.frame(x1=xs[c(1:31)],y1=ys.a[c(1:31)],x2=xs[c(32:62)],y2=ys.a[c(32:62)]) # 使用上述資料分成多欄
colnames(d) <- c( 
  '累計心臟移植次數\n(1~31)',
  '一年內死亡率\n(1~31)',
  '累計心臟移植次數\n(32~62)',
  '一年內死亡率\n(32~62)')
# 繪出表格，rows=NULL 表示不顯示row number，format 可給予數字的小數點及千位數等符號 ，theme 可給予標題及行列資料的字體大小等
t <- tableGrob( # 繪出表格
  d=format(
    d,  # 繪表資料
    decimal.mark = ".",  # 給予數字的小數點符號
    big.mark = ","       # 給予數字的千位數符號
  ),
  rows=NULL , #表示不顯示row number
  theme=ttheme_default( # 表格主題設定
    padding = unit(c(1, 1), "mm"), # 資料方格邊界與文字間之間隔
    colhead = list( # 欄位標題，fg_params 參數用來指定字型、顏色及大小等
      fg_params=list(cex = 0.8) # col:表顏色, fontface: 字型, cex: 大小(相對於base_size)
    ), # 欄位標題
    base_size = 8 # 預設字體大小(pixls)
  )
)
grid.draw(t) # 印出表格，函式說明請參閱附錄a
