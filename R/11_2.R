library(qcc) # 載入qcc 程式庫
nod <- c(      # 各週(共12週)錯誤帳號數量
  15,12,19,2,19,4,24,07,10,17,15,3)
group <- rep(  # 各週(共12週)樣本數，重複12週每週抽樣數2500
  2500:2500,each=12) 
p.chart <-qcc(
  nod,               # 觀察值
  type='p',          # 產生 p chart
  sizes=group,       # 各樣本組數量
  #nsigmas=3,      # 此參數不設定，同預設值3
  ylab = '錯誤率',   # y軸標籤
  xlab = '樣本(週別)',     # x軸標籤
  title='p Chart \n (帳號錯誤)'  # 自訂管制圖標題 \n 為換行符號
)
