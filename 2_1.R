library(geojsonio)
geojson.sp <- geojson_read(
  x= 'https://raw.githubusercontent.com/g0v/twgeojson/master/json/twCounty2010.geo.json',
  what = "sp"
)
print(geojson.sp@data[c('COUNTYSN','COUNTYNAME')]) 
geojson.sp.taipei <- geojson.sp[
  geojson.sp@data$COUNTYNAME  %in% c('台北市'),
  ]
library(broom)
twn.map <- tidy(
  geojson.sp.taipei,
  region = "COUNTYSN"
) 
library(ggplot2)
g <- ggplot(
  data=twn.map,
  mapping=aes(
    x = long,
    y = lat
  )) +
  labs(title='台北市縣界地圖(解方一)')+
  geom_path(show.legend=FALSE)  
print(g)

# 台北地圖解方 二
library(rgdal)
twn.shp <- rgdal::readOGR(
  dsn='mapdata201911261001',
  use_iconv = TRUE,
  encoding='UTF-8'
)
twn.shp.taipei <- twn.shp[
  twn.shp@data$COUNTYNAME  %in% c('臺北市'),
  ]
library(broom)
twn.map <- tidy(
  x=twn.shp.taipei,
  region = "COUNTYCODE"
) 
library(ggplot2)
g <- ggplot(
  data=twn.map,
  mapping=aes(
    x = long,
    y = lat,
    group=group
  )) +
  labs(title='台北市縣界地圖(解方二)')+
  geom_path(show.legend=FALSE)
print(g)

# 土地液化分布繪圖
library(jsonlite)
taipei.soil <- read_json(
  path='https://soil.taipei/Taipei/Main/pages/TPLiquid_84.GeoJSON',
  simplifyVector = TRUE
)
class.name <- taipei.soil[["features"]][["properties"]][["Name"]]
class.id <- taipei.soil[["features"]][["properties"]][["class"]]

map.title <- "台北市土壤液化潛勢分布"
lgnd.title <- '嚴重程度'
colors= c('red','#FFFF00','green')
rshp.f <- function(result,cls,i,soil,color){
  long<-c()
  lat<-c()
  if (class(soil)=='list'){
    for (j in 1:length(soil)){
      result <- rshp.f(result,paste0(cls,'.',i),j,soil[[j]],color)
    }
    return (result)
  }else if(class(soil)=='matrix'){
    long<-soil[,1] 
    lat<-soil[,2]
  }else if(class(soil)=='array'){
    long<-soil[,,1]
    lat<-soil[,,2]
  }
  new.df <- data.frame(
    long=long,
    lat=lat,
    group=paste0(cls,'.',i),
    severity=color
  )
  result <- rbind(result,new.df)
}
soil.df<-data.frame()
for (i in as.numeric(class.id)){
  length.coord <-length(
    taipei.soil[["features"]][["geometry"]][["coordinates"]][[i]]
  )
  for(j in 1:length.coord){
    soil<-taipei.soil[["features"]][["geometry"]][["coordinates"]][[i]][[j]]
    soil.df <- rshp.f(soil.df,i,j,soil,colors[i])
  }
}
head(soil.df)
g<-g+ 
  geom_polygon(
    data = soil.df,
    mapping=aes(
      x = long,
      y = lat,
      group = group,
      fill = severity
    ),
    show.legend = TRUE
  )+
  scale_fill_manual(
    name=lgnd.title,
    values =colors,
    labels=class.name
  )+
  labs(title=map.title,
       x ="經度", y = "緯度")+ 
  theme_bw()
print(g)
