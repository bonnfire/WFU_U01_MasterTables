### PROCESSING PALMER LAB DATASETS 
# EXTRACTION AND FLOW CELL TABLES FROM HANNAH AND KHAI

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues")


extractions_khai_original <- u01.importxlsx("U01 spleen extraction database.xlsx") # 9 tables
extractions_hannah_original <- u01.importxlsx("High_Throughput_DNA_&_spleen_info.xlsx") # 33 tables 
flow_cell_original <- u01.importxlsx("2020-01-16-Flowcell Sample-Barcode list-Riptide-UMich2-Riptide03_NovaSeq01.xlsx") # 1 table

## are there cocaine spleens submitted in the flow cell submitted to the sequencing core? 

######################## 
## FLOW CELL TABLE 
######################## 
flow_cell_original_rip <- flow_cell_original$Sheet1 %>% 
  dplyr::filter(grepl("Riptide", Library))
flow_cell_original_rip %>% mutate_at(vars(one_of("Library", "Flow cell lane")), as.factor) %>% summary()

extractions_khai_df <- lapply(extractions_khai_original, function(x){
  x <- x %>% mutate_at(vars(contains('Date')), ~lubridate::ymd(.))
  return(x)
  }) %>% rbindlist(idcol = "u01", fill = T, use.names = T) 
names(extractions_khai_df) <- mgsub::mgsub(tolower(names(extractions_khai_df)), 
                                           c("[#]", "[[:space:]]|[.]|[[:punct:]]$", "[[:punct:]]"), 
                                           c("num", "", "_"))
extractions_khai_df %>% 
  subset(`sampleid_barcode` %in% flow_cell_original_rip$`Sample ID`) %>% 
  mutate_at(vars(one_of("u01", "comments")), as.factor) %>% 
  summary()

extractions_khai_df %>% mutate_at(vars(one_of("u01", "comments")), as.factor) %>% 
  summary()

## library in flow cell "Riptide-"01 to 03 seem to be line with the "Olivier"01-03 counts
