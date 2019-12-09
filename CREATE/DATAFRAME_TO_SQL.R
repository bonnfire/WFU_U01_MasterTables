## WRITE R TO SQL TABLES
library(RPostgreSQL)

# set up the connection
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password='postgres', dbname='U01')

# drop table if it already exists
if (dbExistsTable(con, c("u01_tom_jhou","jhou_wfu_master")))
  dbRemoveTable(con, c("u01_tom_jhou","jhou_wfu_master"))

# write data frame to database
dbWriteTable(con, c("u01_peter_kalivas","kalivas_wfu_master"), value = shipments[["Kalivas"]], row.names = FALSE)
dbWriteTable(con, c("u01_peter_kalivas_italy","kalivas_italy_wfu_master"), value = shipments[["Kalivas_Italy"]], row.names = FALSE)
dbWriteTable(con, c("u01_tom_jhou","jhou_wfu_master"), value = shipments[["Jhou"]], row.names = FALSE)
dbWriteTable(con, c("u01_suzanne_mitchell","mitchell_wfu_master"), value = shipments[["Mitchell"]], row.names = FALSE)
dbWriteTable(con, c("u01_olivier_george_cocaine","olivier_cocaine_wfu_master"), value = shipments[["Olivier_Co"]], row.names = FALSE)
dbWriteTable(con, c("u01_olivier_george_oxycodone","olivier_oxyc_wfu_master"), value = shipments[["Olivier_Oxy"]], row.names = FALSE)

dbDisconnect(con)

# 
# trial query, joining two tables
# sql <- " 
#     select u01
#     from u01_tom_jhou.jhou_wfu_master 
#     where cohort='01'
# "
# results <- dbGetQuery(con, sql)
