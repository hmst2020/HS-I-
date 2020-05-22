# 方法一 未使用函式的寫法
T <- matrix( # 建構遞移矩陣物件，函式說明請參閱附錄 a
  c(0.97,0.03,
    0.06,0.94),
  nrow=2, 
  byrow = FALSE, 
  dimnames=list(c('City', 'Suburbs'),c('City', 'Suburbs'))
)
X0 <- matrix(  # 建構初始人口分布狀態
  c(0.65,0.35),
  nrow=2,
  byrow = FALSE
)
rownames(X0) <- c('City','uburbs')  #  將X0的列的重新命名

print(T)
print(X0)
X1 <- T%*% X0
X2 <- T%*% X1
X3 <- T%*% X2
X4 <- T%*% X3
X5 <- T%*% X4
X6 <- T%*% X5  
X7 <- T%*% X6
X8 <- T%*% X7
X9 <- T%*% X8
X10 <- T%*% X9
print(X10)

# 方法二: 宣告分布向量(distribution vector)函式
dist_vector <- function(X0,T,year){
  X<- X0  #給予y 初始值
  for (i in 1:year){
    X <- T %*% X
  }
  return(X)
}
T <- matrix( # 建構遞移矩陣物件，函式說明請參閱附錄 a
  c(0.97,0.03,
    0.06,0.94),
  nrow=2, 
  byrow = FALSE, 
  dimnames=list(c('City', 'Suburbs'),c('City', 'Suburbs'))
)
X0 <- matrix(  # 建構初始人口分布狀態
  c(0.65,0.35),
  nrow=2,
  byrow = FALSE
)
print(dist_vector(X0,T,10))

# 方法三: 使用expm套件的寫法
library(expm) 
X0 <- matrix(c(0.65,0.35),nrow=2, byrow = FALSE) # 同上
T <-matrix(c(0.97,0.06,0.03,0.94),nrow=2, byrow = TRUE) # 同上
TM <- T %^% 10  # 使用expm套件之運算子(operator)，計算矩陣之自乘10次
TM %*% X0   #  本例結果

