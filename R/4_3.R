# 轉碼加密(encode)
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
        x= unlist( # 5轉換list物件為vector物件
          strsplit( # 將SEP 分離各字母
            data,  # 目標字串
            split="" # 空字串表示單獨字母分離
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

# 解碼(decode)
decode.f<- function(B){ # 宣告自訂解碼函式 參數B為加密vector
  inverse.A <- solve(A) # 上述A 之反矩陣
  LETTERS[(inverse.A %*%B)[,1]]  # 反矩陣與B相乘結果對應大寫字母
}

resultd <- c() # 宣告本例解碼結果初始值
for(i in 1:length(sec.pos)){ 
  x <- result[ # 擷取前述加密結果一組dim.A 個數字
    sec.pos[i]:(sec.pos[i]+dim.A-1)
    ] 
  resultd<- c(resultd,decode.f(matrix(x))) # 解碼後併入本例解碼結果
}
print(paste0(resultd, collapse = '')) # 將結果的vector 併成文字字串