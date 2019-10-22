shipments <- list("Kalivas_Italy" = glimpse(WFU_Kalivas_Italy_test_df), 
                    "Kalivas" = glimpse(WFU_Kalivas_test_df),
                    "Jhou" = glimpse(WFU_Jhou_test_df),
                    "Mitchell" = glimpse(WFU_Mitchell_test_df),
                    "Olivier_Co" = glimpse(WFU_Olivier_co_test_df), 
                    "Olivier_Oxy" = glimpse(WFU_Olivier_ox_test_df))
shipments <- lapply(shipments, function(x){
  x$cohort <- stringr::str_match(x$cohort, "#\\d+")
  x$cohort <- gsub("#", "Cohort0", x$cohort)
  return(x)
})
shipments_df <- rbindlist(shipments, id = "U01", fill = T)

# experiments <- mapply(append, experiments, "master_table") # by apurva's request


save(shipments, file = "shipments.RData")
save(shipments_df, file = "shipmentswithU01col.RData")
shipments_df <- rbindlist(shipments, id = "U01", fill = T)

