# 解法一
P<- 120000  #  貸款金額(現值)
i<- 0.0045  #  每期利率
n <- 360  #  期數
Numerator <- 1-(1+ i)^(-n)
ca.factor <- Numerator/i # ca.factor <- [(1+ i)^ (n -1)]/i
R <-P / ca.factor
print(R)  # 印出本例結果


# 解法二
P<- 120000  #  貸款金額(現值)
i<- 0.0045  #  每期利率
n <- 360  #  期數
S <- P*((1+i)^n)  # 求複利下之終值
print(S)  # 印出複利下之終值
factor <- sum(  # 加總向量元素，函式說明請參閱附錄a
  (1+i)^(0:(n-1)) # 將0 至 n-1 期的複利加總 
)
R<-S/factor  # 以 S 求算每期金額
print(R)  # 印出本例結果