#	圓餅圖(Pie chart)
complain.frequency <- c(  # 建立抱怨次數資料
  12,42,5,20,10)
complain.item <- c(   # 建立抱怨項目
  "服務生粗魯" , "服務緩慢",  "冷餐點" , "餐桌狹窄", "氣氛不佳")
pie( 
  x = complain.frequency,  # 給予抱怨次數資料
  labels = complain.item , # 抱怨項目
  main="PIE Chart" # 圖標題
)

# 直立長條圖(Vertical Bar chart)
complain.frequency <- c( # 建立抱怨次數資料
  12,42,5,20,10)
complain.item <- c( # 建立抱怨項目
  "服務生粗魯" , "服務緩慢",  "冷餐點" , "餐桌狹窄", "氣氛不佳")
barplot( # 產生長條圖
  complain.frequency, # 給予繪圖資料 
  names.arg = complain.item  # 分析類別名稱
)

# 橫向長條圖(Horizontal Bar chart)
complain.frequency <- c( # 建立抱怨次數資料
  12,42,5,20,10)
complain.item <- c( # 建立抱怨項目
  "態度差" , "服務慢",  "冷餐點" , "餐桌窄", "氣氛差")
barplot( # 產生長條圖
  complain.frequency,  # 給予繪圖資料 
  names.arg = complain.item, # 分析類別名稱
  horiz = TRUE,  # 橫式長條圖
  las = 2,  # 座標軸文字方向 (0~3)，讀者可自行嘗試
  cex.names = 1.0
)

# 柏拉圖(Pareto chart)
library (qcc)  # 載入qcc套件
complain.freq <- c( # 建立抱怨次數資料
  12,42,5,20,10)
complain.item <- c( # 建立抱怨項目
  "服務生粗魯" , "服務緩慢",  "冷餐點" , "餐桌狹窄", "氣氛不佳")
complain.df<- data.frame(freq=complain.freq,item=complain.item)
row.names(complain.df) <- complain.item
complain.df <- complain.df[ # complain.df排序後回存
  order(  # 使用排序函式，函式說明請參閱附錄a
    complain.df$freq, # 排序欄位依據
    decreasing=TRUE   # 降冪排序
  ),
  ]
par<-pareto.chart (  # 柏拉圖分析圖函式，函式說明請參閱附錄a
  complain.df$freq,  # 給予繪圖資料
  ylab ="缺點數", # y 軸標籤
  names =rownames(complain.df), # x 軸標籤
  las=2 # 座標軸文字方向整數(0~3)
)

# 客戶抱怨柏拉圖分析表
complain.freq <- c( # 建立抱怨次數資料
  12,42,5,20,10)
complain.item <- c( # 建立抱怨項目
  "服務生粗魯" , "服務緩慢",  "冷餐點" , "餐桌狹窄", "氣氛不佳")
complain.df <- data.frame( # 建立data frame 物件
  item=complain.item,  # 給予欄位資料
  freq=complain.freq,  # 給予欄位資料
  percentage=complain.freq/sum(complain.freq)*100 # 使用sum函數以計算百分佔比
) 
complain.df <- complain.df[ # complain.df排序後回存
  order(  # 使用排序函式
    complain.df$freq, # 排序欄位依據
    decreasing=TRUE   # 降冪排序
  ),
  ]
# 增加cum.freq欄位於complain.df
complain.df$cum.freq <- cumsum( # 使用累計函數
  complain.df$freq # 對象欄位
)
# 增加cum.percentage欄位於complain.df
complain.df$cum.percentage <- round( # 數字
  cumsum(complain.df$percentage), # 同上
  digits=2 # 取小數兩位
)
print(complain.df) # 列印結果

