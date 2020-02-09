# 台南地圖繪製 請參考本章實例一將台北改換成台南其餘相同
library(geojsonio)  # 載入處理GeoJSON資料套件
geojson.sp <- geojson_read( # 將下載之縣市GeoJSON圖資讀入變數
  x= 'https://raw.githubusercontent.com/g0v/twgeojson/master/json/twCounty2010.geo.json',
  what = "sp" # 指定回傳Spatial class 之物件
)
print(geojson.sp@data[c('COUNTYSN','COUNTYNAME')]) # 列印縣市代碼對照
geojson.sp.tainan <- geojson.sp[  # 過濾geojson.sp(SpatialPolygonsDataFrame 物件) 的臺南市資料
  geojson.sp@data$COUNTYNAME  %in% c('台南市'),
  ]
library(broom) # 載入轉換tibble(data.frame 的擴充物件)資料套件
twn.map <- tidy( # 將sp物件轉換為data.frame物件
  geojson.sp.tainan,  # 資料對象
  region = "COUNTYSN" # 群(group)欄位的依據
) 
library(ggplot2) # 載入繪圖套件
g <- ggplot(  # 使用繪圖函式產生繪圖物件
  data=twn.map,  # 符合data.frame格式的繪圖資料
  mapping=aes(   # 指定data 的欄位
    x = long,  # x軸為經度
    y = lat    # y軸為緯度
  )) +
  labs(title='台南市縣界地圖')+
  geom_path(show.legend=FALSE)  # 繪出座標路徑線
print(g) # 印出台南縣市界地圖

# 登革熱病例分布繪圖
library(jsonlite) # 載入讀取一般json處理套件
tainan.dengue <- read_json(
  #path='https://data.tainan.gov.tw/api/3/action/datastore_search?resource_id=7617bfcd-20e2-4f8d-a83b-6f6b479367f9&limit=50000',
  path='https://data.tainan.gov.tw/api/3/action/datastore_search?resource_id=108cddee-f951-4a9a-9a03-678d48c69102&limit=50000',
  simplifyVector = TRUE # 將多層list資料簡化為vector(主要為經緯度等資料)
)
map.title <- '108年臺南市本土登革熱分布--截至2019/10' # 圖表標題
head(tainan.dengue[["result"]][["records"]]) # 顯示6筆例示資料內容
point.df <-tainan.dengue[["result"]][["records"]] # 分布經緯資料
g<-g+ # 將上述的g物件累加多邊形的土壤潛勢資料
  geom_point( # 繪出點狀資料
    data = point.df, # 繪圖資料
    mapping=aes( # 指定data 的欄位
      x = 經度座標,  # x軸的經度欄位
      y = 緯度座標,   # y軸的緯度欄位
    ),
    size= 1, # 點狀大小
    show.legend = FALSE, # 不顯示圖例
    color='red' # 點狀顏色
  )+
  xlim(120,120.7)+
  ylim(22.8,23.5)+
  labs(title=map.title, x ="經度", y = "緯度")+ # 設定圖表名稱及xy各軸標籤
  theme_bw() # 繪圖主題使用黑白
print(g) # 印出分布圖
