A <- matrix(  # 矩陣函式
  c(2,1,1,      # 第一方程式各係數
    3,2,1,      # 第二方程式各係數
    2,1,2       # 第三方程式各係數
  ),
  nrow = 3,   # 依序排兩列之矩陣
  byrow=TRUE  # 依列排滿換列之順序
)
B <- matrix(  # 矩陣函式
  c(1,2,-1),  # 矩陣代表式AX=B 之rhs
  ncol=1       # 依序排一行之矩陣
)
result <-solve(A,B) # 用solve函式解本例結果
print(result)        # 印出本例結果
