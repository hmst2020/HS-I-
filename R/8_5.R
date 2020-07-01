########### 核密度估計圖 #################
f<-round(     # 依浮點運算捨入標準轉換為秒單位
  faithful$eruptions*60,   # 來源資料為分鐘
  digits=0    # 取整數
)
head(f)  # 列印前5筆資料
d <- density(  #  核心密度估計
  x=f,    # 資料
  kernel='gaussian',   # 使用高斯趨近法使曲線圓滑
  bw='SJ'  # 使用導引貼近實際資料變異的bandwidth method
)
plot( # 列印核心密度估計曲線
  x=d,
  main='老實噴泉噴發持續時間核心密度估計'
)
