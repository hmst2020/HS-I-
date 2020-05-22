A <- matrix( # 建立上述矩陣A
  c(1,2,3,4,5,6,
    1,1,1,1,1,1
  ),
  nrow = 6,
  byrow=FALSE
)
Y <- matrix( # 建立上述矩陣Y
  c(2.91,3.23,3.42,3.63,3.85,4.08),
  nrow = 6, 
  byrow=TRUE
)
AT <- t(A)   # 求矩陣A的轉置(transpose)矩陣，即上述的AT
C <- AT%*%A   # 求上述 CX=D 式其中的C
D <- AT%*% Y  # 求上述 CX=D 式其中的D
solve(a=C,b=D) # 叫用solve解矩陣等式C %*% X = D 其中的X
