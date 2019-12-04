## WRITE R TO SQL TABLES
library(RPostgreSQL)

# set up the connection
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password='postgres', dbname='U01')

# drop table if it already exists
if (dbExistsTable(con, c("u01_tom_jhou","jhou_wfu_master")))
  dbRemoveTable(con, c("u01_tom_jhou","jhou_wfu_master"))

# write data frame to database
dbWriteTable(con, c("u01_tom_jhou","jhou_wfu_master"), value = WFU_Jhou_test_df, row.names = FALSE)

sql <- " 
    select sp.ticker, sp.date, sp.price
    from stock_prices sp
    join temp_tickers tt on sp.ticker = tt.ticker
    where date between '2000-01-01' and '2015-07-08'
"

results <- dbGetQuery(con, sql)
