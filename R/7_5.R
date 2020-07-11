####### 求穩定分布解###############
####方法一########
A  <- matrix(  # 建構方程式(去掉redundant)各系數之矩陣
  c(1,1,1,      # 3*3 矩陣資料(向量物件)
    3,-7,3,
    1,3,-6),
  nrow=3,
  byrow=TRUE
)
print(A)  # 印出A矩陣
B <- matrix(  # 方程式等號右邊常數
  c(1,0,0),nrow=3,byrow=TRUE
)  
A1 <-solve(A)   # 求 A 之反矩陣，函式說明請參閱附錄 A
X<- A1 %*% B    # 求 本例X值  
print(X)        # 印出本例結果

####方法二########
library(markovchain)
statesNames <- c("1", "2", "3")  # 各初始狀態名稱
T  <- matrix(  # 3*3 遞移矩陣資料(向量物件)
  c(0.6,0.4,0.3,      
    0.3,0.3,0.3,
    0.1,0.3,0.4),
  nrow=3,
  byrow=TRUE,
  dimnames=list(statesNames,statesNames) # 給予行列名稱
)

markovB <- new(    # 建構一新的物件
  'markovchain',   # 物件類別
  states=statesNames,  # 狀態各名稱
  byrow=FALSE,  # 轉移機率逐列否
  transitionMatrix=T,  # 指定遞移矩陣
  name='A markovchain Object'  # 給予物件名稱
)
steadyStates(markovB)  # 計算穩定分布解