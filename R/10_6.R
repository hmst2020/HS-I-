#########方法一: 使用常態累計分布函式##############
1-pnorm(500,460,21.44761) # 1 減去500通以下之機率


#########方法二： 使用卜瓦松累計分布函式###########
p <- ppois(q=500,lambda=460) # 500通以下之機率，函式說明請參閱附件A
1- p   # 500通以上之機率
