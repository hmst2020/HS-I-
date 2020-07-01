# 宣告本例常數
lot_id <- '0001' # 檢驗批號
gpnum <- 20 # 群組數
diameter <- c(  # 抽驗100件檢測值
  1.38,1.377,1.376,1.376,1.377,
  1.372,1.371,1.377,1.372,1.373,
  1.376,1.379,1.382,1.37,1.376,
  1.375,1.379,1.38,1.37,1.376,
  1.375,1.381,1.379,1.376,1.382,
  1.382,1.385,1.374,1.377,1.376,
  1.381,1.38,1.378,1.374,1.376,
  1.372,1.373,1.369,1.371,1.375,
  1.37,1.381,1.377,1.379,1.378,
  1.381,1.378,1.379,1.378,1.378,
  1.375,1.375,1.377,1.377,1.376,
  1.378,1.375,1.386,1.373,1.384,
  1.377,1.373,1.378,1.374,1.381,
  1.379,1.371,1.375,1.376,1.377,
  1.385,1.383,1.372,1.382,1.376,
  1.384,1.379,1.367,1.372,1.372,
  1.371,1.38,1.375,1.375,1.37,
  1.37,1.384,1.378,1.372,1.385,
  1.377,1.378,1.38,1.369,1.382,
  1.374,1.383,1.375,1.375,1.378
)
group <- rep(1:20,each=5) # 產生20組每組size為5的vector 
group.sample <- as.data.frame(  # 將分組後之matrix轉成data frame 物件
  qcc.groups(  # 此函式將觀察值分為20組
    data=diameter,
    sample=group
  )
)
group.sample$lot_id<-rep(lot_id,20) # 在data frame物件上增加一lot_id欄位
library(DBI) # 載入DBI函式庫
con <- DBI::dbConnect( # 透過DBI 函式庫的dbConnect函式建立資料庫連結
  RSQLite::SQLite(),  # 連線的資料庫為本機的RSQLite 函式庫的SQLite 資料庫
  dbname = ":memory:" # 以暫存資料庫示範本例，若給予實際名稱則屬永久資料庫
)
dbWriteTable( # 透過DBI 函式庫的dbWriteTable函式依資料建立資料表及資料
  conn=con,   # 利用前述建立的連線物件con
  name='insp_lot_data',  # 命名建立的資料表
  value=group.sample     # 資料表內容資料
)
read.sql <- dbSendQuery( # 透過DBI 函式庫的dbSendQueryt產生讀取資料的SQL指令物件
  con,  # 同前述
  paste0(   # 依lot_id讀取之SQL 指令
    'SELECT * FROM insp_lot_data
     WHERE lot_id = \'',
    lot_id,'\''
  ) 
)
lot.df <- dbFetch(read.sql) # 執行讀出資料表之資料存入指定變數
dbDisconnect(con) #關閉資料庫連結(暫存資料庫也隨即消失)
gp.data <- rowMeans(  # data frame依列平均得出vector，函式說明請參閱附錄A
  x = lot.df[,1:5]
)
library(qcc) # 載入qcc 函式庫
# 繪出xbar Chart
q <- qcc(            # 產生管制圖的繪圖物件
  data=gp.data,      # 資料來自平均後之資料
  type='xbar.one'    # 模擬平均值即為唯一個抽樣(one-at-time)的xbar chart
)
# 繪出Process Capability Analysis
p <- process.capability( # 產生製程能力的繪圖物件
  q,   # xbar 的qcc物件 
  spec.limits=range(1.36,1.40) # 產品規格高低範圍限制值
)
