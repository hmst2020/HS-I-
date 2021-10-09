####### 繪製台北市界地圖解法一###################
library(geojsonio)  # 載入處理GeoJSON資料套件
geojson.sp <- geojson_read( # 將下載之縣市GeoJSON圖資讀入變數
  x= 'https://raw.githubusercontent.com/g0v/twgeojson/master/json/twCounty2010.geo.json',
  what = "sp" # 指定回傳Spatial class 之物件
)
print(geojson.sp@data[c('COUNTYSN','COUNTYNAME')]) # 列印縣市代碼對照
sp.taipei <- geojson.sp[  # 過濾geojson.sp(SpatialPolygonsDataFrame 物件) 的臺北市資料
  geojson.sp@data$COUNTYNAME  %in% c('台北市'),
]
library(broom) # 載入轉換tibble(data.frame 的擴充物件)資料套件
twn.map <- tidy( # 將sp物件轉換為data.frame物件
  sp.taipei,  # 資料對象
  region = "COUNTYSN" # 群(group)欄位的依據
) 
library(ggplot2) # 載入繪圖套件
g <- ggplot(  # 使用繪圖函式產生繪圖物件
  data=twn.map,  # 符合data.frame格式的繪圖資料
  mapping=aes(   # 指定data 的欄位
    x = long,  # x軸為經度
    y = lat    # y軸為緯度
  )) +
  labs(title='台北市縣界地圖(解方一)')+
  geom_path(show.legend=FALSE)  # 繪出座標路徑線
print(g) # 印出台北縣市界地圖


####### 繪製台北市界地圖解法二###################
library(rgdal) # 載入rgdal套件讀取shp地圖資料
twn.shp <- rgdal::readOGR( # 回傳Spatial class 之物件
  dsn='data/mapdata201911261001', # 目前工作目錄下(或路徑)圖資目錄
  use_iconv = TRUE, # 依據encoding參數給予內碼轉碼
  encoding='UTF-8'  # 轉為UTF-8 內碼
)
twn.shp.taipei <- twn.shp[  # 過濾出twn.shp(sp 物件) 的臺北市資料
  twn.shp@data$COUNTYNAME  %in% c('臺北市'),  #  %in% 運算子在附錄下可以找到說明
  ]
library(broom) # 載入轉換tibble(data.frame 的擴充物件)資料套件
twn.map <- tidy( # 將sp物件轉換為data.frame物件
  x=twn.shp.taipei, # 資料對象
  region = "COUNTYCODE" # 群(group)欄位的依據
) 
library(ggplot2) # 載入繪圖套件
g <- ggplot(  # 使用繪圖函式產生繪圖物件
  data=twn.map,  # 符合data.frame格式的繪圖資料
  mapping=aes(   # 指定data 的欄位
    x = long,  # x軸為經度
    y = lat,   # y軸為緯度
    group=group
  )) +
  labs(title='台北市縣界地圖(解方二)')+
  geom_path(show.legend=FALSE)  # 繪出座標路徑線
print(g) # 印出台北縣市界地圖

############土壤液化分布標示於地圖上######################
taipei.js <- geojson_read( # 將下載之縣市GeoJSON圖資讀入變數
  x='data/taipei_soil.geojson',
  what = "sp" # 指定回傳Spatial class 之物件
)
soil.df <- tidy( # 將sp物件轉換為data.frame物件
  taipei.js)  # 資料對象
xclass<- taipei.js$class
sev<-function(i){xclass[i]}
library(dplyr)
soil.df<-soil.df %>%
  mutate(severity=
           sev(as.integer(id)))
head(soil.df)
map.title <- "台北市土壤液化潛勢分布" # 圖表標題
lgnd.title <- '嚴重程度' # 圖例標題
class.name <- c('High','Medium','Low') # 圖例顏色對應(嚴重度)
colors= c('red','#FFFF00','green') # 填色顏色對應
g<-g+ # 將上述的g物件累加多邊形的土壤潛勢資料
  geom_polygon( # 繪出多邊形資料
    data = soil.df, # 繪圖資料
    mapping=aes( # 指定data 的欄位
      x = long,  # x軸的經度欄位
      y = lat,   # y軸的緯度欄位
      group = group,  # 封閉區塊(polygon)同群欄位
      fill = severity # 填色依據欄位
    ),
    show.legend = TRUE # 指定需將圖例繪出
  )+
  scale_fill_manual( # 圖例指定內容
    name=lgnd.title, # 圖例標題名
    values =colors,  # 圖例顏色依據colors這vector
    labels=class.name # 圖例值依據class.name這vector
  )+
  labs(title=map.title,  # 設定圖表名稱及xy各軸標籤
       x ="經度", y = "緯度")+ 
  theme_bw() # 繪圖主題使用黑白
print(g) # 印出圖
##############  end of 2_1_v01.R ##########
