A  <- matrix(  # 建構方程式(去掉redundant)各系數之矩陣
  c(1,1,1,      # 3*3 矩陣資料(向量物件)
    3,-7,3,
    1,3,-6),
  nrow=3,
  byrow=TRUE
)
print(A)  # 印出A 矩陣
B <- matrix(c(1,0,0),nrow=3,byrow=TRUE)  # 方程式等號右邊常數
A1 <-solve(A)   # 求 A 之反矩陣，函式說明請參閱附錄 a
X<- A1 %*% B    # 求 本例X值  
print(X)        # 印出本例結果
