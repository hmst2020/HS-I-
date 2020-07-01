#####使用R 基本運算元(operator)計算#####
R <- 100  #  每期金額
n <- 12  #  期數
i <- 0.01  #  每期利率
Numerator <-(1+ i)^ n -1
ca.factor <- Numerator/i
S <-R * ca.factor
print(S) # 印出本例結果

##########運用sum函式對vector便捷的處理#########
R <- 100  #  每期金額
n <- 12  #  期數
i <- 0.01  #  每期利率
S <- sum(  # 加總向量元素，函式說明請參閱附錄A
  100*(1+i)^(0:(n-1)) # 將0 至 n-1 期的複利加總 
)  
print(S) # 印出本例結果

