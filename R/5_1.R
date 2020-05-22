# 方法一:lpSolve   # 解決線性/整數規劃的套件
# 題目，求目標函數最大值(最佳解):
# P = x + 1.2y
# 2x + y <= 180
# x + 3y <= 300
# x >= 0 
# y >= 0
library(lpSolve)  # 載入線性規劃函式庫lpSolve
f.obj <- c(1,1.2) # 定義目標函數之各係數
f.con <- matrix(  # 建立限制條件之矩陣
  c(2, 1, # 第一限制式之係數
    1,3,  # 第二限制式之係數
    1,0,  # 第三限制式之係數
    0,1   # 第四限制式之係數
  ), 
  nrow = 4,  # 矩陣列數
  byrow=TRUE # 每列填滿再換列
)
f.dir <- c("<=", "<=", ">=",">=") # 限制條件方向(<= 小於等於， >= 大於等於)
f.rhs <- c(180,300,0,0) # 限制條件計算式之右側數字
result <- lp(  # 使用線性規劃函式 lp求解，函式說明請參閱附錄a
  direction ="max",     # 目標函數取最大值之解
  objective.in =f.obj,  # 給予上述目標函數之各係數
  const.mat =f.con,     # 給予上述限制條件之矩陣
  const.dir =f.dir,     # 給予上述限制條件方向之向量物件
  const.rhs =f.rhs)     # 給予上述限制條件計算式之右側數字之向量物件
print(result)           # 將結果印出
print(result$solution)  # 印出目標函數之各變數(即求解的x與y))
print(result$objval)    # 印出目標值(本例取最大值)

# 方法二:lpSolveAPI
# 題目，求目標函數最大值(最佳解):
# P = x + 1.2y
# 2x + y <= 180
# x + 3y <= 300
# x >= 0
# y >= 0
library(lpSolveAPI) # 載入線性規劃函式庫lpSolveAPI
lprec <- make.lp(  # 建立一新線性規劃model物件，函式說明請參閱附錄a
  4, 2     # 此model具4個限制條件2個維度求解
)
# 程式執行此可先print(lprec) 初步檢查model內容
lp.control(      # 線性規劃模式，設定其相關控制參數，函式說明請參閱附錄a
  lprec=lprec,   # 對象線性規劃model物件
  sense='max'   # 設定此model取最大值
)  
set.column( # 設定model欄限制條件各係數，函式說明請參閱附錄a
  lprec,    # 此model物件
  column=1, # 此model第一欄
  x=c(2,1,1,0) # 此欄各限制條件值(對應上述四個條件)
) 
set.column( # 同上
  lprec,    # 同上
  column=2, # 此model第二欄
  x=c(1,3,0,1) # 同上
) 
set.objfn(  # 設定model的目標函數，函式說明請參閱附錄a
  lprec,    # 此model物件
  c(1,1.2)  # 目標函數各係數
)
# 給予各條件名稱
rownames <- c('machine1', 'machine2','productA','productB')
colnames <- c("productA", "productB") # 給予各係數名稱
dimnames(lprec) <- list( # 將model變數欄及條件欄重新命名，方便閱讀
  rownames,   
  colnames
)
set.constr.value( # 設定限制值(Right Hand Side)，函式說明請參閱附錄a
  lprec, # 此model物件
  rhs=c(180,300,0,0), # 限制值(Right Hand Side)
  constraints=1:4     # 四個限制條件
)
set.constr.type( # 設定限制型態(方向)，函式說明請參閱附錄a
  lprec,  # 此model物件
  types=c("<=", "<=", ">=",">="), # 限制型態(方向)
  constraints=1:4   # 四個限制條件
)
# 程式執行此可先print(lprec) 檢查model完整內容
print(lprec)
solve(lprec) # 將此model 求解
get.objective(lprec) # 讀出目標函數最佳解
get.variables(lprec) # 讀出目標函數最佳解之各係數(依欄順序顯示)
get.constraints(lprec) # 讀出其各限制條件式右側(rhs)之結果(依列順序顯示)

# 圖形解
# 題目，求目標函數最大值(最佳解):
# P = x + 1.2y
# 2x + y <= 180
# x + 3y <= 300
# x >= 0
# y >= 0
# 找出截距及其端點
f1 <- approxfun( # 產生符合線性函數2x + y = 180 的內插函數，函式說明請參閱附錄a
  x=c(0,90), # x 軸座標值
  y=c(180,0) # y 軸座標值
)
f2 <- approxfun( # 產生符合線性函數x + 3y = 300 的內插函數
  c(0,300),c(100,0) # 同上f1
)
f3 <- approxfun(  # 同上f1(注意y值在前)
  c(180,0), # y 軸座標值
  c(0,90)   # x 軸座標值
) 
f4 <- approxfun(  # 同上f2(注意y值在前)
  c(100,0),c(0,300)  # 同上f3
)
max.y <- min(f1(0),f2(0)) #  y軸最小截距，函式說明請參閱附錄a
max.x <- min(f3(0),f4(0)) #  x軸最小截距，函式說明請參閱附錄a
intersect.x = result$solution[1] # 上述方法一得知的最佳解x值(即限制條件線之交會)
intersect.y = result$solution[2] # 上述方法一得知的最佳解y值(即限制條件線之交會)
objval <- result$objval # 上述得知的最佳解(最大值P)

#繪其圖解(圖形法)，以下相關ggplot2各函式說明請參閱附錄b
library(ggplot2) # 載入繪圖套件(函式庫)
aes <- data.frame(x = c(0,100),y=c(0,200)) 
p<-ggplot(  # 產生繪圖物件
  data=aes, # 繪圖資料
  mapping=aes(x,y))+ # 指定x、y軸資料
  xlab('A紀念品')+ylab('B紀念品')  # 給予xy軸標籤
y1.f <- function(x){180-2*x} # 第一限制條件繪線函式
y2.f <- function(x){(300-x)/3} # 第二限制條件繪線函式
p.f <- function(x){(objval-x)/1.2} # 目標函數繪線函式
intersect.df <- data.frame( # 將限制條件各線與原點圍成的區域順時鐘建立端點資料
  seq=c('A','D','C','B'),  # 交叉點代碼
  x=c(0,0,intersect.x,max.x), # 各端點依序x值
  y=c(0,max.y,intersect.y,0), # 各端點依序y值
  group=c(0,0,0,0)  # 將各點歸於同一組，以俾下述geom_polygon連線各點
)
p<-p +
  stat_function( # 為每一x 軸上給予的值傳入fun指定的函式計算y值並據以繪出線圖
    fun=y1.f, # 指定函式y1.f
    geom='line', # 繪製直線圖
    size=0.8,
    colour = "blue" # 線圖顏色為blue
  )+
  stat_function( # 同上
    fun=y2.f,    # 指定函式y2.f
    geom='line', # 同上
    size=0.4,
    colour = "green" # 線圖顏色為green
  )+
  stat_function( # 同上
    fun=p.f,     # 指定函式y2.f
    geom='line', # 同上
    size=1.2,
    colour = "red" # 線圖顏色為red
  )+
  scale_x_discrete(limits=seq(0,100,by=10))+  # x軸依指定各值標示
  geom_point( # 繪點狀圖
    data=intersect.df, # 繪圖資料
    aes(x=x,y=y), # 各軸資料對應intersect.df欄位
    size=2,color='red' # 點的大小及顏色
  )+
  geom_text( # 疊加文字於圖
    data=intersect.df, # 文字資料來源
    aes(label=paste0(seq,'(',x,',',y,')')), # 文字構成
    hjust=-0.2, # 文字位置水平向右幾個字寬
    vjust=-1    # 文字位置垂直向上調整幾個字高
  )+
  geom_polygon( # 繪出多邊形
    data=intersect.df, # 繪圖資料
    aes(x=x,y=y,group=group), # 多邊路徑資料與intersect.df的欄位對應
    fill="burlywood2",  # 封閉的多邊圖填滿顏色burlywood2
    alpha=0.3  # 顏色透明狀態代碼(0~1)
  )
