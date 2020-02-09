# 台南地圖繪製 請參考本章實例一將台北改換成台南其餘相同
library(geojsonio)
geojson.sp <- geojson_read(
  x= 'https://raw.githubusercontent.com/g0v/twgeojson/master/json/twCounty2010.geo.json',
  what = "sp"
)
print(geojson.sp@data[c('COUNTYSN','COUNTYNAME')])
geojson.sp.tainan <- geojson.sp[
  geojson.sp@data$COUNTYNAME  %in% c('台南市'),
  ]
library(broom)
twn.map <- tidy(
  geojson.sp.tainan,
  region = "COUNTYSN"
) 
library(ggplot2)
g <- ggplot(
  data=twn.map,
  mapping=aes(
    x = long,
    y = lat
  )) +
  labs(title='台南市縣界地圖')+
  geom_path(show.legend=FALSE)
print(g)

# 登革熱病例分布繪圖
library(jsonlite)
tainan.dengue <- read_json(  
  path='https://data.tainan.gov.tw/api/3/action/datastore_search?resource_id=108cddee-f951-4a9a-9a03-678d48c69102&limit=50000',
  simplifyVector = TRUE
)
map.title <- '108年臺南市本土登革熱分布--截至2019/10'
head(tainan.dengue[["result"]][["records"]])
point.df <-tainan.dengue[["result"]][["records"]]
g<-g+
  geom_point(
    data = point.df,
    mapping=aes(
      x = 經度座標,
      y = 緯度座標,
    ),
    size= 1,
    show.legend = FALSE,
    color='red'
  )+
  xlim(120,120.7)+
  ylim(22.8,23.5)+
  labs(title=map.title, x ="經度", y = "緯度")+
  theme_bw()
print(g)
