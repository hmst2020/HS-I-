# 直方圖(histogram)
# 解法一 : 用R內建的hist函式繪圖
diameter <- c(  # 抽驗100件檢測值
  1.38,1.377,1.376,1.376,1.377,1.372,1.371,1.377,1.372,1.373,
  1.376,1.379,1.382,1.37,1.376,1.375,1.379,1.38,1.37,1.376,
  1.375,1.381,1.379,1.376,1.382,1.382,1.385,1.374,1.377,1.376,
  1.381,1.38,1.378,1.374,1.376,1.372,1.373,1.369,1.371,1.375,
  1.37,1.381,1.377,1.379,1.378,1.381,1.378,1.379,1.378,1.378,
  1.375,1.375,1.377,1.377,1.376,1.378,1.375,1.386,1.373,1.384,
  1.377,1.373,1.378,1.374,1.381,1.379,1.371,1.375,1.376,1.377,
  1.385,1.383,1.372,1.382,1.376,1.384,1.379,1.367,1.372,1.372,
  1.371,1.38,1.375,1.375,1.37,1.37,1.384,1.378,1.372,1.385,
  1.377,1.378,1.38,1.369,1.382,1.374,1.383,1.375,1.375,1.378)
h <- hist(  # 用R內建的hist函式繪圖
  diameter,  # 給予檢測資料
  breaks='Sturges', # 組距依據
  main= "直方圖(hist)" , # 圖標題
  xlab= "直徑",   # x軸標籤
  ylab= "個數",   # y軸標籤
  ylim=c(0,30)    # 指定y軸的範圍
)
print(h) # 列出這list之各component(於下列引用)
text(  # 新增文字疊加於圖(利用histogram 這list物件之各值)
  x = h$mids,  # 於座標 x 軸位置
  y = h$counts,    # 於座標 y 軸位置
  labels = h$counts, # 標示文字
  adj=c(0.5, -0.3) # 標示位置調整(對於x、y軸)
)

# 解法二: 用R外掛套件ggplot2 的ggplot函式繪圖
diam.df <- data.frame(diameter) # 將diameter轉換成 data frame物件
breaks <- base::pretty( # 將diameter分組
  range(diam.df$diameter),  # 分組依據的範圍(最大值與最小值)
  n = nclass.Sturges(diam.df$diameter), # 用Sturges公式計算間隔數
  min.n = 1 #最小間隔
)
library(ggplot2) # 載入ggplot2程式庫
p <- ggplot(  # 產生繪圖物件
  diam.df,    # 繪圖資料
  aes(x=diam.df$diameter)) +  # 指定x軸資料
  geom_histogram( # 繪製直方圖
    color="black", # 直方圖塊外框顏色
    fill="white",  # 直方圖塊填入顏色
    breaks=breaks  # x軸組距各邊界點
  )+
  ggtitle('直方圖(ggplot)')+ # 圖標題
  xlab('直徑')+ # x軸標籤
  ylab('個數')+ # y軸標籤
  ylim(0,30)  # 指定y軸的範圍
pg <- ggplot_build(p) # 將繪圖物件轉成ggplot_built繪圖資料物件
pg.df <- pg$data[[1]] # 從繪圖資料物件取其一圖層(layer)data frame
p+   
  geom_text( # 疊加文字標示於直方圖塊上
    data=pg.df, # 文字資料來源
    mapping = aes(x=x,y=y, # xy軸對應於文字資料來源
                  label=y  # 標示的文字
    ), 
    color= 'blue', # 文字的顏色
    size =4,       # 文字的大小(mm)
    vjust = -1    # 文字位置垂直向上調整幾個字高
  )+
  scale_x_continuous(breaks=pg.df$xmax) # x軸標示各組邊界

