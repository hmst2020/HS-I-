library(qcc) # 載入qcc 程式庫
cc <- c(2,0,3,1,2,3,1,0,0,  #每天缺點數
        4,2,1,2,3,1,3,2,0,
        1,1,2,1,0,3,0,1) 
c.chart <-qcc( # 產生管制圖的繪圖物件
  data= cc,   # 觀察值 vector物件
  type="c", # 產生 c chart
  nsigmas =3,  # 以3個標準差建立c chart 管制線
  # center= 1.5,  #若省略則函式依data自動算出
  # limits= c(0,3),  # 若省略則函式依data自動算出
  ylab = '缺點數', # y軸標籤
  xlab = '日期', # x軸標籤
  title='c Chart \n (缺點管制圖)' # 自訂管制圖標題 \n 為換行符號
)
print(c.chart)  # 印出qcc物件之數據，也是繪圖過程資料
