######### (1)	經過十代後 ###########
########方法一 自訂馬可夫遞移函式##############
dist_vector <- function(X0,T,year){
  X<- X0  #給予y 初始值
  for (i in 1:year){
    X <- T %*% X
  }
  return(X)
}

rownames = c("2年以上大專教育", "2年以下大專教育")
colnames = c("2年以上大專教育", "2年以下大專教育")
# T 每年人口變化(比率)
T <- matrix(  # 建構遞移矩陣物件，函式說明請參閱附錄 A
  c(0.7,0.3,
    0.2,0.8),
  nrow=2, 
  byrow = FALSE,
  dimnames = list(rownames, colnames)
)
print(T)   # 遞移矩陣
X0 <- matrix(  # 建構初始教育人口分布狀態
  c(0.2,0.8),
  nrow =2,
  byrow =FALSE,
  dimnames = list(rownames,c('population'))
)
print(X0)  # 初始狀態
result <- dist_vector(X0,T,10) # 10年後教育人口比率
print(result)  # 印出10代後結果

######### 十代後 方法二 使用expm套件之運算子##############
library(expm)  # 載入expm套件，若尚未安裝請參閱第一篇套件安裝之說明
rownames = c("2年以上大專教育", "2年以下大專教育")
colnames = c("2年以上大專教育", "2年以下大專教育")
X0 <- matrix(  # 同方法一
  c(0.2,0.8),
  nrow =2,
  byrow =FALSE,
  dimnames = list(rownames, c('population'))
)
T <- matrix( # 同方法一
  c(0.7,0.3,0.2,0.8),
  nrow=2, 
  byrow = FALSE,
  dimnames = list(rownames, colnames)
)
TM <- T %^% 10  # 使用expm套件之運算子(operator)，計算矩陣之自乘
TM %*% X0   #  本例10代後結果

####### 求穩定分布解###############
library(markovchain)
statesNames = c("2年以上大專教育", "2年以下大專教育")
T <- matrix(  # 建構遞移矩陣物件，函式說明請參閱附錄 A
  c(0.7,0.3,
    0.2,0.8),
  nrow=2, 
  byrow = FALSE,
  dimnames = list(statesNames, statesNames)
)

markovB <- new(    # 建構一新的物件
  'markovchain',   # 物件類別
  states=statesNames,  # 狀態各名稱
  byrow=FALSE,  # 轉移機率逐列否
  transitionMatrix=T,  # 指定遞移矩陣
  name='A markovchain Object'  # 給予物件名稱
)
steadyStates(markovB)  # 計算穩定分布解
