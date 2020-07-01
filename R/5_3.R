# 方法一:lpSolve
# 題目，求目標函數最大值(最佳解):
# p = 6x + 5 y + 4z
# 2x + y + z ≤ 180
# X + 3y + 2z ≤ 300
# 2X + y + 2z ≤ 240
# X ≥ 0 
# y ≥ 0 
# z ≥ 0
library(lpSolve)  # 載入線性規劃函式庫lpSolve
f.obj <- c(6,5,4) # 定義目標函數之各係數
f.con <- matrix(  # 建立限制條件之矩陣
  c(2, 1, 1, # 第一限制式之係數
    1, 3, 2, # 第二限制式之係數
    2, 1,2,  # 第三限制式之係數
    1,0,0,   # 第四限制式之係數
    0,1,0,   # 第五限制式之係數
    0,0,1    # 第六限制式之係數
  ), 
  nrow = 6,  # 矩陣列數
  byrow=TRUE # 每列填滿再換列
)
f.dir <- c("<=","<=","<=",">=",">=",">=") # 限制條件方向(<= 小於等於， >= 大於等於)
f.rhs <- c(180,300,240,0, 0, 0) # 限制條件計算式之右側數字
result <- lp(  # 使用線性規劃函式 lp求解，函式說明請參閱附錄A
  direction ="max",     # 目標函數取最大值之解
  objective.in =f.obj,  # 給予上述目標函數之各係數
  const.mat =f.con,     # 給予上述限制條件之矩陣
  const.dir =f.dir,     # 給予上述限制條件方向陣
  const.rhs =f.rhs)     # 給予上述限制條件計算式之右側數字
print(result)           # 將結果印出
print(result$solution)  # 印出目標函數之各變數(即求解的x與y))
print(result$objval)    # 印出目標值(本例取最大值)
