# 方法一:lpSolve
# 題目，求目標函數最小值(最佳解):
# C = 6x + 8y
# 40x + 10y ≥ 2400
# 10x + 15y ≥ 2100
# 5x +15y ≥ 1500
# x >= 0 
# y >= 0
library(lpSolve)  # 載入線性規劃函式庫lpSolve
f.obj <- c(6,8) # 定義目標函數之各係數
f.con <- matrix(  # 建立限制條件之矩陣
  c(40, 10,  # 第一限制式之係數
    10, 15,  # 第二限制式之係數
    5, 15,   # 第三限制式之係數
    1,0,     # 第四限制式之係數
    0,1      # 第五限制式之係數
  ), 
  nrow = 5,  # 矩陣列數
  byrow=TRUE # 每列填滿再換列
)
f.dir <- c(">=", ">=", ">=",">=",">=") # 限制條件方向(<= 小於等於， >= 大於等於)
f.rhs <- c(2400,2100,1500,0,0) # 限制條件計算式之右側數字
result <- lp(  # 使用線性規劃函式 lp求解，函式說明請參閱附錄A
  direction ="min",     # 目標函數取最小值之解
  objective.in =f.obj,  # 給予上述目標函數之各係數
  const.mat =f.con,     # 給予上述限制條件之矩陣
  const.dir =f.dir,     # 給予上述限制條件方向陣
  const.rhs =f.rhs)     # 給予上述限制條件計算式之右側數字
print(result)           # 將結果印出
print(result$solution)  # 印出目標函數之各變數(即求解的x與y))
print(result$objval)    # 印出目標值(本例取最小值)


# 方法二:lpSolveAPI
# 題目，求目標函數最小值(最佳解):
# C = 6x + 8y
# 40x + 10y ≥ 2400
# 10x + 15y ≥ 2100
# 5x +15y ≥ 1500
# x >= 0 
# y >= 0
library(lpSolveAPI) # 載入線性規劃函式庫lpSolveAPI
lprec <- make.lp(  # 建立一新線性規劃model物件，函式說明請參閱附錄A
  5, 2  # 此model具5個限制條件2個維度求解
)
# 程式執行此可先print(lprec) 初步檢查model內容
lp.control(      # 線性規劃模式，設定其相關控制參數，函式說明請參閱附錄A
  lprec=lprec,   # 對象線性規劃model物件
  sense='min'   # 設定此model取最小值
)
set.column( # 設定model欄限制條件各係數，函式說明請參閱附錄A
  lprec,    # 此model物件
  column=1, # 此model第一欄
  x=c(40,10,5,1,0) # 此欄各限制條件值(對應上述5個條件)
) 
set.column( # 同上
  lprec,    # 同上
  column=2, # 此model第二欄
  x=c(10,15,15,0,1) # 同上
) 
set.objfn(  # 設定model的目標函數，函式說明請參閱附錄A
  lprec,    # 此model物件
  c(6,8)    # 目標函數各係數
)
# 給予各條件名稱
rownames <- c('Iron', 'B1','B2','A_brand','B_brand')
colnames <- c("A_brand", "B_brand") # 給予各係數名稱
dimnames(lprec) <- list( # 將model變數欄及條件欄重新命名，方便閱讀
  rownames,   
  colnames
)
set.constr.value( # 設定限制值(Right Hand Side)，函式說明請參閱附錄A
  lprec,  # 此model物件 
  rhs=c(2400,2100,1500,0,0), # 限制值(Right Hand Side)
  constraints=1:5 # 五個限制條件
)
set.constr.type( # 設定限制型態(方向)，函式說明請參閱附錄A
  lprec,  # 此model物件
  types=c(">=", ">=", ">=",">=",">="),  # 限制型態(方向)
  constraints=1:5 # 五個限制條件
)
# 程式執行此可先print(lprec) 檢查model完整內容
print(lprec)  # 將model變數欄及條件欄已重新命名
solve(lprec)  # 將此model 求解，函式說明請參閱附錄A
get.objective(lprec) # 讀出目標函數最佳解
get.variables(lprec) # 讀出目標函數最佳解之各係數(依欄順序顯示)
get.constraints(lprec) # 讀出其各限制條件式右側(rhs)之結果(依列順序顯示)


# 圖型驗證: 
# 題目，求目標函數最大值(最佳解):
# C = 6x + 8y
# 40x + 10y ≥ 2400
# 10x + 15y ≥ 2100
# 5x +15y ≥ 1500
# x >= 0 
# y >= 0
#繪其圖解(圖形法)
library(ggplot2) # 載入繪圖函式庫，以下相關ggplot2各函式說明請參閱附錄B
aes <- data.frame(x = c(0,300),y=c(0,300)) 
p<-ggplot(  # 產生繪圖物件，函式說明請參閱附錄B
  data=aes, # 繪圖資料
  mapping=aes(x,y))+ # 指定x、y軸資料
  xlab('品牌A')+ylab('品牌B')  # 給予xy軸標籤
y1.f <- function(x){(2400-40*x)/10} # 第一限制條件繪線函式
y2.f <- function(x){(2100-10*x)/15} # 第二限制條件繪線函式
y3.f <- function(x){(1500-5*x)/15} # 第三限制條件繪線函式
p.f <- function(x){(objval-6*x)/8} # 目標函數繪線函式
p<-p +
  stat_function( # 為每一x 軸上給予的值傳入fun指定的函式計算y值並據以繪出線圖，函式說明請參閱附錄A
    fun=y1.f, # 指定函式y1.f
    geom='line', # 繪製直線圖
    colour = "blue" # 線圖顏色為blue
  )+
  stat_function( # 同上
    fun=y2.f,    # 指定函式y2.f
    geom='line', # 同上
    colour = "green" # 線圖顏色為green
  )+
  stat_function( # 同上
    fun=y3.f,    # 指定函式y3.f
    geom='line', # 同上
    colour = "orange" # 線圖顏色為green
  )+
  xlim(c(0,300))+ # 限制x軸尺規範圍，函式說明請參閱附錄B
  ylim(c(0,300))  # 限制y軸尺規範圍，函式說明請參閱附錄B
print(p) # 印出繪圖物件
# 求各線交點
intersect.f <- function(eq,rhs){  # 自定方程式求解函式
  solve(eq,rhs)  # 直線交叉點求解，請參閱第3章第1節，，函式說明請參閱附錄A
}
i12 <- intersect.f(  # 呼叫自定函式intersect.f
  matrix(c(40,10,10,15), # 第一、第二限制條件之係數陣列
         nrow=2,     # 兩列
         byrow=TRUE  # 依列順序填入
  ),
  matrix(c(2400,2100),   # 等號右側(rhs)陣列
         nrow=2,     # 兩列
         byrow=TRUE  # 依列順序填入
  )
)
i23 <- intersect.f(  # 同上
  matrix(c(10,15,5,15), # 第二、第三限制條件之係數陣列
         nrow=2,     # 同上
         byrow=TRUE  # 同上
  ),
  matrix(c(2100,1500),   # 同上
         nrow=2,     # 同上
         byrow=TRUE  # 同上
  )
)
f1x <- approxfun( # 產生符合線性函數40x + 10y = 2400 的內插函數
  x=c(0,60), # x 軸座標值
  y=c(240,0) # y 軸座標值
)
f2x <- approxfun( # 產生符合線性函數10x + 15y = 2100 的內插函數
  c(0,210), # 同上f1
  c(140,0)  # 同上f1
)
f3x <- approxfun( # 產生符合線性函數5x +15y = 1500 的內插函數
  c(0,300), # 同上f1
  c(100,0)  # 同上f1
)
f1y <- approxfun(  # 同上f1x(注意y值在前)
  c(240,0), # y 軸座標值
  c(0,60)   # x 軸座標值
) 
f2y <- approxfun(  # 同上f2x(注意y值在前)
  c(140,0),c(0,210)  # 同上f1y
)
f3y <- approxfun(  # 同上f3x(注意y值在前)
  c(100,0),c(0,300)  # 同上f1y
)
max.y <- max(f1x(0),f2x(0),f3x(0)) #  y軸最大截距
max.x <- max(f1y(0),f2y(0),f3y(0)) #  x軸最大截距
intersect.df <- data.frame( # 將限制條件各線交點右上所交集的區域順時鐘建立端點資料
  x=c(i12[1],0    ,max.x,max.x,i23[1]), # 各端點依序x值
  y=c(i12[2],max.y,max.y,0    ,i23[2]), # 各端點依序y值
  group=c(0,0,0,0,0)  # 將各點歸於同一組，以俾下述geom_polygon連線各點
)
p<- p+
  geom_point( # 繪點狀圖
    data=intersect.df, # 繪圖資料
    aes(x=x,y=y), # 各軸資料對應intersect.df欄位
    size=2,color='red' # 點的大小及顏色
  )+
  geom_text( # 疊加文字於圖
    data=intersect.df, # 文字資料來源
    aes(label=paste0('(',round(x,digits=0),',',round(y,digits=0),')')), # 文字構成
    hjust=0.5, # 文字位置水平向右幾個字寬
    vjust=-1    # 文字位置垂直向上調整幾個字高
  )+
  geom_polygon( # 繪出多邊形
    data=intersect.df, # 繪圖資料
    aes(x=x,y=y,group=group), # 多邊路徑資料與intersect.df的欄位對應
    fill="burlywood2", # 封閉的多邊圖填滿顏色burlywood2
    alpha=0.3 # 顏色透明狀態代碼(0~1)
  )
print(p)  # 印出繪圖物件

obj.values <- 6*intersect.df$x+8*intersect.df$y # 目標函數各頂點得到值
target.val <- min(obj.values) # 使用min函式求最大值
p.f <- function(x){(target.val-6*x)/8} # 目標函數繪線函式(通過最大值所代表之頂點)
p <- p+  # 疊加目標函數的直線
  stat_function( # 同上
    fun=p.f,     # 指定函式p.f(目標函數)
    geom='line', # 同上
    size = 1.2, # 線條粗細比例
    colour = "red" # 線圖顏色為red
  )
print(p)

min.idx <- which.min(obj.values)  # 從intersect.df找出構成最大值的位置
obj.x <- intersect.df[min.idx,]$x  # 滿足目標函數最大值之x 
obj.y <- intersect.df[min.idx,]$y  # 滿足目標函數最大值之y
print(c(obj.x,obj.y)) # 列印 x,y
