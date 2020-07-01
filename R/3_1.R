A <- matrix(  # 產生矩陣物件函式
  c(5,3,      # 需求方程式各係數
    52,-30    # 供給方程式各係數
  ),
  nrow = 2,   # 依序排兩列之矩陣
  byrow=TRUE  # 依列排滿換列之順序
)
B <- matrix(  # 同上
  c(30,-45),  # 矩陣代表式AX=B 之rhs(right hand side)
  ncol=1      # 依序排一行之矩陣
)
inverse.A <-solve(A) # 用solve函式解A反矩陣
result <- inverse.A %*% B # A反矩陣 X B矩陣 
print(inverse.A)  # 印出A反矩陣
print(result)   # 印出本例結果
