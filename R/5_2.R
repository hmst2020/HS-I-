######## 方法一:lpSolve 使用解決線性/整數規劃的套件######
# 題目，求目標函數最大值(最佳解):
# P = 34x1 + 40x2
# 4x1 + 6x2 <= 48
# 2x1 + 2x2 <= 18
# 2x1 + x2 <= 16
# x1 >= 0 
# x2 >= 0
library(lpSolve)  # 載入線性規劃函式庫lpSolve
f.obj <- c(34,40) # 定義目標函數之各係數
f.con <- matrix(  # 建立限制條件之矩陣
  c(4,6,  # 第一限制式之係數
    2,2,  # 第二限制式之係數
    2,1,  # 第三限制式之係數
    1,0,  # 第四限制式之係數
    0,1   # 第五限制式之係數
  ), 
  nrow = 5,  # 矩陣列數
  byrow=TRUE # 每列填滿再換列
)
f.dir <- c("<=", "<=", "<=",">=",">=") # 限制條件方向(<= 小於等於， >= 大於等於)
f.rhs <- c(48,18,16,0,0) # 限制條件計算式之右側數字
result <- lp(  # 使用線性規劃函式 lp求解，函式說明請參閱附錄A
  direction ="max",     # 目標函數取最大值之解
  objective.in =f.obj,  # 給予上述目標函數之各係數
  const.mat =f.con,     # 給予上述限制條件之矩陣
  const.dir =f.dir,     # 給予上述限制條件方向陣
  const.rhs =f.rhs)     # 給予上述限制條件計算式之右側數字
print(result)           # 將結果印出
print(result$solution)  # 印出目標函數之各變數(即求解的x1與x2))
print(result$objval)    # 印出目標值(本例取最大值)

###### 二維圖形解 ##################
# 題目，求目標函數最大值(最佳解):
# P = 34x1 + 40x2
# 4x1 + 6x2 <= 48
# 2x1 + 2x2 <= 18
# 2x1 + x2 <= 16
# x1 >= 0 
# x2 >= 0
#繪其圖解(圖形法)
library(ggplot2) # 載入繪圖套件(函式庫)，以下相關ggplot2各函式說明請參閱附錄B
aes <- data.frame(x1 = c(0,10),x2=c(0,200)) 
p<-ggplot(  # 產生繪圖物件
  data=aes, # 繪圖資料
  mapping=aes(x1,x2))+ # 指定x、y軸資料
  xlab('第一型塑膠管')+ylab('第二型塑膠管')  # 給予xy軸標籤
x21.f <- function(x1){(48-4*x1)/6} # 第一限制條件繪線函式
x22.f <- function(x1){(18-2*x1)/2} # 第二限制條件繪線函式
x23.f <- function(x1){16-2*x1}     # 第三限制條件繪線函式
p<- p +
  stat_function( # 為每一x 軸上給予的值傳入fun指定的函式計算y值並據以繪出線圖
    fun=x21.f, # 指定函式x21.f
    geom='line', # 繪製直線圖
    colour = "blue" # 線圖顏色為blue
  )+
  stat_function( # 同上
    fun=x22.f,    # 指定函式x22.f
    geom='line', # 同上
    colour = "green" # 線圖顏色為green
  )+
  stat_function( # 同上
    fun=x23.f,    # 指定函式x23.f
    geom='line', # 同上
    colour = "orange" # 線圖顏色為orange
  )+
  scale_x_continuous(breaks=seq(0,10,by=1))  # x軸依指定各值標示
print(p)
# 求各線交點
intersect.f <- function(eq,rhs){  # 自定方程式求解函式
  solve(eq,rhs)  # 直線交叉點求解，請參閱第3章第1節，函式說明請參閱附錄A
}
i12 <- intersect.f(  # 呼叫自定函式intersect.f
  matrix(c(4,6,2,2), # 第一、第二限制條件之係數陣列
         nrow=2,     # 兩列
         byrow=TRUE  # 依列順序填入
  ),
  matrix(c(48,18),   # 等號右側(rhs)陣列
         nrow=2,     # 兩列
         byrow=TRUE  # 依列順序填入
  )
)
i23 <- intersect.f(  # 同上
  matrix(c(2,2,2,1), # 第二、第三限制條件之係數陣列
         nrow=2,     # 同上
         byrow=TRUE  # 同上
  ),
  matrix(c(18,16),   # 同上
         nrow=2,     # 同上
         byrow=TRUE  # 同上
  )
)
f1x1 <- approxfun( # 產生符合線性函數4x1 + 6x2 = 48 的內插函數，函式說明清參閱附錄 A
  x=c(0,12), # x 軸座標值
  y=c(8,0) # y 軸座標值
)
f2x1 <- approxfun( # 產生符合線性函數2x1 + 2x2 = 18 的內插函數
  c(0,9), # 同上f1x1
  c(9,0)  # 同上f1x1
)
f3x1 <- approxfun( # 產生符合線性函數2x1 + x2 = 16 的內插函數
  c(0,8),  # 同上f1x1
  c(16,0)  # 同上f1x1
)
f1x2 <- approxfun(  # 同上f1x(注意y值在前)
  c(8,0), # y 軸座標值
  c(0,12)   # x 軸座標值
) 
f2x2 <- approxfun(  # 同上f2x(注意y值在前)
  c(9,0), # f1x2
  c(0,9)  # f1x2
)
f3x2 <- approxfun(  # 同上f3x(注意y值在前)
  c(16,0), # f1x2
  c(0,8)   # f1x2
)
max.x2 <- min(f1x1(0),f2x1(0),f3x1(0)) #  y軸最大截距
max.x1 <- min(f1x2(0),f2x2(0),f3x2(0)) #  x軸最大截距
intersect.df <- data.frame( # 將限制條件各線交點右上所交集的區域順時鐘建立端點資料
  x1=c(0,0,     i12[1,1],i23[1,1],max.x1), # 各端點依序之x值
  x2=c(0,max.x2,i12[2,1],i23[2,1],0    ), # 各端點依序之y值
  group=c(0,0,0,0,0)  # 將各點歸於同一組，以俾下述geom_polygon連線各點
)
p<- p+ # 繪出疊加頂點、頂點座標、可行解之多邊區塊
  geom_point( # 繪點狀圖
    data=intersect.df, # 繪圖資料
    aes(x=x1,y=x2), # 各軸資料對應intersect.df欄位
    size=2,color='red' # 點的大小及顏色
  )+
  geom_text( # 疊加文字於圖
    data=intersect.df, # 文字資料來源
    aes(label=paste0('(',round(x1,digits=0),',',round(x2,digits=0),')')), # 文字構成
    hjust=-0.2, # 文字位置水平向右幾個字寬
    vjust=-1    # 文字位置垂直向上調整幾個字高
  )+
  geom_polygon( # 繪出多邊形
    data=intersect.df, # 繪圖資料
    aes(x=x1,y=x2,group=group), # 多邊路徑資料與intersect.df的欄位對應
    fill="burlywood2", # 封閉的多邊圖填滿顏色burlywood2
    alpha=0.3 # 顏色透明狀態代碼(0~1)
  )
print(p)
obj.values <- 34*intersect.df$x1+40*intersect.df$x2 # 目標函數各頂點得到值
target.val <- max(obj.values) # 使用max函式求最大值

p.f <- function(x1){(target.val-34*x1)/40} # 目標函數繪線函式(通過最大值所代表之頂點)
p <- p+  # 疊加目標函數的直線
  stat_function( # 同上
    fun=p.f,     # 指定函式p.f(目標函數)
    geom='line', # 同上
    size = 1.2, # 線條粗細比例
    colour = "red" # 線圖顏色為red
  )
print(p)
max.idx <- which.max(obj.values)  # 從intersect.df找出構成最大值的位置
obj.x1 <- intersect.df[max.idx,]$x1  # 滿足目標函數最大值之x1 
obj.x2 <- intersect.df[max.idx,]$x2  # 滿足目標函數最大值之x2
print(c(obj.x1,obj.x2)) # 列印 x1,x2

