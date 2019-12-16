# shipments <- list("Kalivas_Italy" = glimpse(WFU_Kalivas_Italy_test_df), 
#                     "Kalivas" = glimpse(WFU_Kalivas_test_df),
#                     "Jhou" = glimpse(WFU_Jhou_test_df),
#                     "Mitchell" = glimpse(WFU_Mitchell_test_df),
#                     "Olivier_Co" = glimpse(WFU_Olivier_co_test_df), 
#                     "Olivier_Oxy" = glimpse(WFU_Olivier_ox_test_df))

shipments <- list("Kalivas_Italy" = WFU_Kalivas_Italy_test_df, 
                  "Kalivas" = WFU_Kalivas_test_df,
                  "Jhou" = WFU_Jhou_test_df,
                  "Mitchell" = WFU_Mitchell_test_df,
                  "Olivier_Co" = WFU_Olivier_co_test_df, 
                  "Olivier_Oxy" = WFU_Olivier_ox_test_df)


shipments <- lapply(shipments, function(x){
  x$cohort <- ifelse(grepl("#", x$cohort), stringr::str_match(x$cohort, "#(\\d+).*?")[,2], x$cohort)
  x$cohort <- ifelse(nchar(x$cohort) > 1, x$cohort, gsub('([[:digit:]]{1})$', '0\\1', x$cohort)) # add leading zeroes when necessary
  x$litternumber = as.numeric(x$litternumber)
  x$littersize = as.numeric(x$littersize)
  return(x)
})
shipments_df <- rbindlist(shipments, id = "u01", fill = T)
sapply(shipments_df, unique) # check if all formats are consistent # NOT YET!!

shipments_df <- shipments_df %>% dplyr::filter(nchar(rfid) == 15) # XXXX excluding 5 cases for now, all from olivier's labs 
# experiments <- mapply(append, experiments, "master_table") # by apurva's request
shipments <- split(shipments_df, as.factor(shipments_df$U01))

save(shipments, file = "shipments.RData")

openxlsx::write.xlsx(shipments$Mitchell[2,], file = "u01_masterfile_template.xlsx")

save(shipments_df, file = "shipmentswithU01col.RData")
write.csv(shipments_df, file = "WFU_U01_shipments.csv")



