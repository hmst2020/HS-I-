salary.edu <- c(74560,54904,40368,32629,24398,19666,18737,12809)
mean(salary.edu) #平均數

median(salary.edu) #中位數

sd(salary.edu) #標準差

var(salary.edu) #變異數
sd(salary.edu)^2 #變異數

cv <- 100 * sd(salary.edu) / mean (salary.edu) #變異係數

range(salary.edu)[2] - range(salary.edu)[1]  # 全距(最大值減最小值)

Q1 <- quantile(x=salary.edu, probs=1/4)

Q2 <- quantile(x=salary.edu,probs= 2/4)

Q3 <- quantile(x=salary.edu, probs=3/4)

Q3 - Q1  #IQR = Q3-Q1

quantile(x=salary.edu)