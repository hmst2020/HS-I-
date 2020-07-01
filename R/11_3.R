library(qcc) # 載入qcc 程式庫
cc <- c(16,21,17,22,24,5) #每捲缺點數
c.chart <-qcc( # 產生管制圖的繪圖物件
  cc,type="c", # 產生 c chart
  nsigmas =2,  # 以2個標準差建立c chart 管制線
  center=20,  # 依本題給定的平均數為基準
  ylab = '缺點數', # y軸標籤
  xlab = '樣本(捲筒紙編號)', # x軸標籤
  title='c Chart \n (缺點管制圖)'  # 自訂管制圖標題 \n 為換行符號
)
