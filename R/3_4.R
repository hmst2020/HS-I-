####### 解法一 使用R內建套件 ############
year <- c(0,1,2,3,4,5) # 自2013年起為第一年，依此類推
expense <- c(2.91,3.23,3.42,3.63,3.85,4.08) # 各年費用
least.sqr <-lm(  # 叫用lm內建函式建構線性回歸模型
  formula=expense ~ year  # 公式依據每年費用
) 
intercept <- least.sqr$coefficients[1] # 線之截距
slope <- least.sqr$coefficients[2]     # 線之斜率
print(summary(least.sqr)) # 列印完整的回歸分析, 其分析表上，也可以看到線之截距與斜率
plot(  # 使用內建函式plot 繪製本例之散佈圖與最小平方缐
  x=year,   # x軸為年度順序
  y=expense, # y軸為年度費用
  type='p',  # 指定繪點狀圖
  xlab="年度", # x軸標籤
  ylab="健康照護花費(兆元) ",  # y軸標籤     
  main="年度與健康照護費用支付的散佈圖" # 圖標題
)
abline( # 將plot繪出的圖疊加直線圖,即最小平方缐
  coef=c(intercept,slope) # 直線係數的截距及斜率
) 
text( # 將plot繪出的圖疊加文字
  x=3, # 文字對應x軸位置 
  y=3.5, # 文字對應y軸位置 
  paste0('Y =',    # paste將參數轉換為字符串，並將其連接
         round(intercept,digits=5),
         '+',round(slope,digits=7),
         't') # 文字內容
)
print(paste0('forecast Y2020 : ', # 印出依線性模型推估2020年的可能費用
             intercept+slope*7)
)

####### 解法二 使用forecast(預測)外掛套件##########
df <- data.frame( # 建立data frame物件
  ym=c(2013,2014,2015,2016,2017,2018), # 實際年度資料
  expense=c(2.91,3.23,3.42,3.63,3.85,4.08)) # 各年費用
ts.year <- ts(   # 叫用內建函式ts建構時間序列物件
  data=df$expense,  # 資料引用df 的expense(費用)欄
  start=c(2013),    # 時間序列啟始年
  end=c(2018),      # 時間序列截止年
  frequency=1       # 每年幾筆資料(費用)
)
library(forecast)   # 載入函式庫forecast
least.sqr <-tslm(   # 叫用forcast::tslm 時間序列線性回歸模型
  formula=ts.year ~ trend   # 公式依據每年費用及簡單之時間趨勢(time trend)影響
)
intercept <- least.sqr$coefficients[1] # 線之截距
slope <- least.sqr$coefficients[2] # 線之斜率
print(summary(least.sqr))   # 列印完整的回歸分析
fcst <- forecast( # 叫用forecast函式建構預測物件
  object=ts.year,  # 預測之實際資料依據
  level = c(80,95), # 給予不同的信心水準範圍
  h=2        # 往後預估2年
) 
print(fcst) # 印出預測物件
plot( # 使用內建函式plot 繪圖
  x=fcst,  # 序列預測資料
  type='o',  # 指定繪點狀圖及線圖
  xlab="Year", # x軸標籤
  ylab='Expense', # y軸標籤
  main="年度與健康照護費用支付的散佈圖" # 圖標題
)
print(paste0('forecast Y2020 : ', # 印出依線性模式推估2020年的可能費用
             intercept+slope*8)
)
