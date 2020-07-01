A <- matrix( # 叫用內建函式建構3x3矩陣物件
  c(2,1,1, 
    3,2,1,
    2,1,2
  ),
  nrow = 3, 
  byrow=TRUE
) 
inverse1<- solve(# 叫用solve解矩陣等式a %*% x = b 其中的x
  a=A,
  b=matrix(   # b參數 即為反矩陣定義中的I
    c(1,0,0,
      0,1,0,
      0,0,1
    ),
    nrow = 3, 
    byrow=TRUE)
) 
print(inverse1)
inverse2<- solve(a=A) # 可省略b 參數即代表解反矩陣
print(inverse2)
