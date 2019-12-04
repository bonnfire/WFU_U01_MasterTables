## WRITE R TO SQL TABLES
library(RPostgreSQL)

# set up the connection
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password='postgres', dbname='U01')

# drop table if it already exists
if (dbExistsTable(con, c("u01_tom_jhou","jhou_wfu_master")))
  dbRemoveTable(con, c("u01_tom_jhou","jhou_wfu_master"))

# write data frame to database
dbWriteTable(con, c("u01_tom_jhou","jhou_wfu_master"), value = shipments[["Jhou"]], row.names = FALSE)

# trial query, joining two tables
sql <- " 
    select u01
    from u01_tom_jhou.jhou_wfu_master 
    where cohort='01'
"
results <- dbGetQuery(con, sql)


dbDisconnect(con)

# 
con <- dbConnect(
  drv,
  dbname='U01',
  user='postgres',
  password='postgres',
  options="-c search_path=u01_peter_kalivas"
)

dbWriteTable(con, c("u01_peter_kalivas","kalivas_wfu_master"), value = WFU_Kalivas_test_df, row.names = FALSE)
