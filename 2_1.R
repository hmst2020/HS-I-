# 台北地圖解方 一
library(geojsonio)  # 載入處理GeoJSON資料套件
geojson.sp <- geojson_read( # 將下載之縣市GeoJSON圖資讀入變數
  x= 'https://raw.githubusercontent.com/g0v/twgeojson/master/json/twCounty2010.geo.json',
  what = "sp" # 指定回傳Spatial class 之物件
)
print(geojson.sp@data[c('COUNTYSN','COUNTYNAME')]) # 列印縣市代碼對照
geojson.sp.taipei <- geojson.sp[  # 過濾geojson.sp(SpatialPolygonsDataFrame 物件) 的臺北市資料
  geojson.sp@data$COUNTYNAME  %in% c('台北市'),
  ]
library(broom) # 載入轉換tibble(data.frame 的擴充物件)資料套件
twn.map <- tidy( # 將sp物件轉換為data.frame物件
  geojson.sp.taipei,  # 資料對象
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

# 台北地圖解方 二
library(rgdal) # 載入rgdal套件讀取shp地圖資料
twn.shp <- rgdal::readOGR( # 回傳Spatial class 之物件
  dsn='mapdata201911261001', # 工作目錄下圖資目錄
  use_iconv = TRUE, # 依據encoding參數給予內碼轉碼
  encoding='UTF-8'  # 轉為UTF-8 內碼
)
twn.shp.taipei <- twn.shp[  # 過濾出twn.shp(sp 物件) 的臺北市資料
  twn.shp@data$COUNTYNAME  %in% c('臺北市'),
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

# 土地液化分布繪圖
library(jsonlite) # 載入讀取一般json處理套件
taipei.soil <- read_json(
  path='https://soil.taipei/Taipei/Main/pages/TPLiquid_84.GeoJSON', # 開放資料源
  simplifyVector = TRUE # 將多層list資料簡化為vector(主要為經緯度等資料)
)
class.name <- taipei.soil[["features"]][["properties"]][["Name"]] # 圖例顏色對應(嚴重度)
class.id <- taipei.soil[["features"]][["properties"]][["class"]]  # 嚴重類別id

map.title <- "台北市土壤液化潛勢分布" # 圖表標題
lgnd.title <- '嚴重程度' # 圖例標題
colors= c('red','#FFFF00','green') # 填色顏色對應
rshp.f <- function(result,cls,i,soil,color){ # 宣告處裡繪圖data.frame圖資之自訂函式
  long<-c() # 初始經度vector 
  lat<-c()  # 初始緯度vector
  if (class(soil)=='list'){ # 處理傳入soil之資料為list格式
    for (j in 1:length(soil)){ # 迴圈list內容
      result <- rshp.f(result,paste0(cls,'.',i),j,soil[[j]],color) # 遞迴壘加至result
    }
    return (result) # 回傳result
  }else if(class(soil)=='matrix'){ # 處理matrix格式
    long<-soil[,1]  # 經度資料
    lat<-soil[,2]   # 緯度資料
  }else if(class(soil)=='array'){ # 處理array格式
    long<-soil[,,1]  # 經度資料
    lat<-soil[,,2]   # 緯度資料
  }
  new.df <- data.frame( # 產生新的data.frame物件
    long=long, # 指定經度欄位vector資料
    lat=lat,   # 指定緯度欄位vector資料
    group=paste0(cls,'.',i), # 指定封閉區塊(polygon)的歸群(group)資料
    severity=color # 指定填(fill)色之顏色代表嚴重程度
  )
  result <- rbind(result,new.df) # 回傳併入new.df之後的result
}
soil.df<-data.frame() # 初始soil.df為data.frame物件
for (i in as.numeric(class.id)){ # 迴圈嚴重類別
  length.coord <-length( # 經緯座標群(group)數
    taipei.soil[["features"]][["geometry"]][["coordinates"]][[i]]
  )
  for(j in 1:length.coord){ # 迴圈每一經緯座標群(group)
    soil<-taipei.soil[["features"]][["geometry"]][["coordinates"]][[i]][[j]]
    soil.df <- rshp.f(soil.df,i,j,soil,colors[i]) # 將soil.df經由自訂函式壘加
  }
}
head(soil.df)
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
print(g) # 印出分布圖
