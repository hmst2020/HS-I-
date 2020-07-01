salary.edu <- c(74560,54904, 40368,32629,24398,19666,18737,12809)

mean (salary.edu) #平均數
median(salary.edu) #中位數
sd(salary.edu) #標準差
var(salary.edu) #變異數
sd(salary.edu)^2 #變異數
100 * sd(salary.edu) / mean (salary.edu) #變異係數 
range(salary.edu)[2] - range(salary.edu)[1] #全距(最大值減最小值)

#四分位:把資料切分為四等分，中間的三條線就是四分位，Q1=P25,Q2=P50,Q3=P75 
quantile(salary.edu, 1/4) # 25% 小於此年收入
quantile(salary.edu, 2/4) # 50% 同中位數
quantile(salary.edu, 3/4) # 75% 小於此年收入
Q3 - Q1      #IQR = Q3-Q1

quantile(salary.edu)  # 上述四分位加前後0%(即最小值)與100%(即最大值)
