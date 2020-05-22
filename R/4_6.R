A <- matrix( # 建立輸出入分析矩陣
  c(0.2,0.2,0.1,
    0.2,0.4,0.1,
    0.1,0.2,0.3
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
D <- matrix(  # 定義消費者的需求
  c(100,80,50),
  ncol = 1, 
  byrow=TRUE) 
x<- solve(I -A)%*% D # 代入公式，求滿足消費者的需求下的A、M、S產出
print(x) #印出本例結果
