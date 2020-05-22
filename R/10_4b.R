# 情境一，n=20，p=0.2 時的機率分配
n =20
p = 0.2
x = 1:20
plot(   #  內建繪圖函數
  x,    #  x 軸資料
  dbinom(x=x,size=n,prob=p),  # 藉二項分配下的機率密度函數產生y軸資料，函式說明請參閱附錄a 
  type="h",  #  繪出直方圖
  lwd =2,  # 直方圖粗細，參閱?par之HELP說明
  ylab="probability"  # y 軸文字標籤
)

# 情境二，n=20，p=0.8 時的機率分配
n =20
p = 0.8
x = 1:20
plot(x,dbinom(x,n,p),type="h",lwd =2,ylab="probability")

# 情境三，n=20，p=0.5 時的機率分配
n =20
p = 0.5 #
x = 1:20
plot(x,dbinom(x,n,p),type="h",lwd =2,ylab="probability")


