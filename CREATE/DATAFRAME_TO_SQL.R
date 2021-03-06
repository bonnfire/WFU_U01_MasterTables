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


# solution1: update function to update the table from r 



update <- function(i) {
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user='postgres', password='postgres', dbname='U01')
  txt <- paste("UPDATE data SET column_one=",data$column_one[i],",column_two=",data$column_two[i]," where id=",data$id[i])
  dbGetQuery(con, txt)
  dbDisconnect(con)
}


registerDoMC()

foreach(i = 1:length(data$column_one), .inorder=FALSE,.packages="RPostgreSQL")%dopar%{
  update(i)
}


# solution2: using RPostgreSQL::CopyInDataframe() function
dbSendQuery(con, "copy foo from stdin")
postgresqlCopyInDataframe(con, df)


# solution3: using dbWriteTable
dbWriteTable(con, c("u01_olivier_george_cocaine","olivier_cocaine_wfu_master"), shipments$Olivier_Co[which(shipments$Olivier_Co$cohort == 10 & is.na(shipments$Olivier_Co$comment)),] , row.names=FALSE, append=TRUE) # adding non scrubs for now until i hear further notice
