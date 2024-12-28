#  注意 !  配合broom版本變更，請改用2_1_v03.R
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

library(FRK)
opts_FRK$set("parallel",0L)
twn.map<-SpatialPolygonsDataFrame_to_df(sp.taipei, vars = names(sp.taipei))
colnames(twn.map)<-c('long','lat','id','COUNTYSN','COUNTYNAME','name')
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


############土壤液化分布標示於地圖上######################
taipei.js <- geojson_read( # 將下載之縣市GeoJSON圖資讀入變數
  x='data/taipei_soil.geojson',
  what = "sp"   # 指定回傳SpatialPolygonsDataFrame 之物件
)
library(FRK)
opts_FRK$set("parallel",0L)
soil.df<-SpatialPolygonsDataFrame_to_df(taipei.js, vars = names(taipei.js))
#soil.df <- tidy( # 將sp物件轉換為data.frame物件
#  taipei.js)  # 資料對象
xclass<- taipei.js$class
sev<-function(i){xclass[i]}
library(dplyr)
soil.df<-soil.df %>%
  mutate(severity=
           sev(as.integer(id)))
colnames(soil.df)<-c('long','lat','group','class','severity')
head(soil.df)
map.title <- "台北市土壤液化潛勢分布" # 圖表標題
lgnd.title <- '嚴重程度' # 圖例標題
class.name <- c('High','Medium','Low') # 圖例顏色對應(嚴重度)
colors= c('red','#FFFF00','green') # 填色顏色對應
p<-g+ # 將上述的g物件累加多邊形的土壤潛勢資料
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
print(p) # 印出圖
##############  end of 2_1_v03.R ##########
