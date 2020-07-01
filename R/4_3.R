target.str <- "SEPTEMBER IS OKAY" # 加密目標字串
A <- matrix( # 叫用內建函式建構3x3 的加密矩陣物件
  data=c(1,0,0, 
         3,1,5,
         -2,0,1
  ),
  nrow = 3,
  byrow=TRUE
) 
dim.A <- ncol(A) # A矩陣行數
str.a <- gsub(  # 去除字串中空白字元
  pattern=" ", replacement="",  #以空字串代替空白字元
  x= target.str # 串對象
)
encode.f<- function(data){ # 宣告自訂轉碼加密函式 參數data為目標字串
  B<- matrix( # 目標字串之對應數字 dim.A x 1矩陣
    data= match( # 叫用match函式傳回各字母對應之位置數字
      x= unlist( # 轉換list物件為vector物件
        strsplit( # 將傳入的字串物件分離各字母，回傳list的結果
          data,  # 目標字串
          split="" # 空字串表示無分隔符號地分割字母
        )
      ), 
      table=LETTERS # R內建大寫字母vector物件
    ),
    nrow = dim.A, #  同A列數
    ncol=1,   # 1行
    byrow=TRUE
  )
  result <- A %*% B  # 本例加密法
  return (result)
}
sec.pos <- seq( # dim.A個字母一組位置
  1, # 啟始位置
  nchar(str.a), # str.a 字串長度(byte數)
  by=dim.A # 每隔dim.A個數
)
str.vector <- sapply( # 叫用內建sapply函式執行自訂之一次函式function(pos)
  sec.pos, # 依上述sec.pos切割位置function(pos)的傳入參數
  function(pos) { 
    substr(str.a, pos, pos+dim.A-1) # 擷取str.a dim.A個字元
  }
)
result <- c() # 宣告本例加密結果初始值
for (i in 1:length(str.vector)){ # 將分組之目標字串vector依序加密
  result<- c(result,encode.f(str.vector[i])[,1]) # 加密後併入本例加密結果
}
print(result) # 印出本例加密結果
