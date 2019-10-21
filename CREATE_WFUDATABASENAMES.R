shipments <- list("Kalivas_Italy" = glimpse(WFU_Kalivas_Italy_test_df), 
                    "Kalivas" = glimpse(WFU_Kalivas_test_df),
                    "Jhou" = glimpse(WFU_Jhou_test_df),
                    "Mitchell" = glimpse(WFU_Mitchell_test_df),
                    "Olivier_Co" = glimpse(WFU_Olivier_co_test_df), 
                    "Olivier_Oxy" = glimpse(WFU_Olivier_ox_test_df))
shipments_df <- rbindlist(shipments, id = "U01", fill = T)

# experiments <- mapply(append, experiments, "master_table") # by apurva's request


save(shipments, file = "shipments.RData")
