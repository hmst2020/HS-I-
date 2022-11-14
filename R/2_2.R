library(jsonlite) # 載入一般json處理套件
dns.list <- read_json(
  path='https://www.twnic.net.tw/dnjson.txt',
  simplifyVector = TRUE # 將多層list資料簡化為vector及data frame
)
dns.df <-as.data.frame(# 將dns.list轉成data frame資料物件
  x=dns.list$com.tw # 本例針對com.tw分析
) 
names(dns.df) <- c('ym','count') # 重新命名dns.df
head(dns.df) # 列出前幾筆
tail(dns.df) # 列出最後幾筆
if (packageVersion('stats')<'4.2.0'){   # R 版本4.2.0以後引數formula 改為x
  y.df <- aggregate( # 使用聚集函式
    formula=count~ substr(ym,1,4), # 依年度的數量處理
    data=dns.df[dns.df$ym<202001,], # 截至本書出版年之前資料
    FUN=sum # 依年度加總
  ) 
}else{
  y.df <- aggregate( # 使用聚集函式
    x=count~ substr(ym,1,4), # 依年度的數量處理
    data=dns.df[dns.df$ym<202001,], # 截至本書出版年之前資料
    FUN=sum # 依年度加總
  ) 
}
names(y.df) <- c('year','count')
library(ggplot2)
p<-ggplot( # 產生繪圖物件
  data=y.df, # 繪圖資料
  mapping=aes(
    x=year, y=count, # 指定x、y軸對應data(y.df)資料
    group = 1 # geom_path 之group為必要欄位
  ))+ 
  ggtitle('域名(com.tw)申請趨勢')+ # 圖標題
  xlab('西元年')+ylab('申請數量')+  # 給予xy軸標籤
  geom_point()+ # 畫出各點點狀圖
  geom_path()+  # 疊加畫出各點連線
  scale_x_discrete(limits=y.df$year)+  # 指定 x軸各值標示
  scale_y_continuous(  # y軸為計量值之尺規標示
    breaks=seq(min(y.df$count), # 指定尺規標示值
               max(y.df$count),by=500000))+  
  theme( # 調整x軸標示文字旋轉及橫向位移
    axis.text.x=element_text(angle=60,hjust=1)) 
print(p)  #將軌跡圖印出
