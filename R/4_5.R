A <- matrix( # 建立輸出入分析矩陣
  c(0.278,0.125,0.333,
    0.111,0.188,0.167,
    0.167,0.125,0.167
  ),
  nrow = 3, 
  byrow=TRUE
)
I <- matrix( # 定義與上述A同為3x3的單位矩陣
  c(1,0,0,
    0,1,0,
    0,0,1
  ),
  nrow = 3, 
  byrow= TRUE
)
D <- matrix(   # 定義三年後的需求矩陣
  c(60,110,60),
  ncol = 1, 
  byrow=TRUE) 
solve(I -A)%*% D   # 代入公式，求滿足預估的需求下R、S、T的產值
