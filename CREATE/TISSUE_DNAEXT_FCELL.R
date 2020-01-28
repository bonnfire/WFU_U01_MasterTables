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


######################## 
## KHAI EXTRACTION TABLE 
######################## 
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

extractions_flowcell <- extractions_khai_df %>% 
  subset(`sampleid_barcode` %in% flow_cell_original_rip$`Sample ID`)

extractions_khai_df %>% mutate_at(vars(one_of("u01", "comments")), as.factor) %>% 
  summary()

extractions_khai_df <- extractions_khai_df %>% dplyr::filter(u01 != "Template")
# extractions_khai_df %>% dplyr::filter(u01 != "Template") %>% dplyr::filter(u01!=dnaplatecode) # note that olivier c06 shares with mitchell c01


extractions_flowcell %>% 
  ggplot(aes(x = `260_280`)) + 
  geom_histogram() + 
  facet_grid(~ u01) +
  labs(title = paste0("260_280 values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% 
  ggplot(aes(x = `u01`, y = `260_280`)) + 
  geom_boxplot() + 
  labs(title = paste0("260_280 values by U01")) + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% 
    ggplot(aes(x = `260_230`)) + 
    geom_histogram() + 
    facet_grid(~ u01) +
    labs(title = paste0("260_230 values by U01")) + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))


extractions_flowcell %>% 
  ggplot(aes(x = `u01`, y = `260_230`)) + 
  geom_boxplot() + 
  labs(title = paste0("260_230 values by U01"))  + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

# extractions_flowcell %>% 
#   ggplot(aes(x = u01, y = `260_230`, color = comments)) + 
#   geom_boxplot() + 
#   facet_grid(~ userid) +
#   labs(title = paste0("260_230 values by U01 and tech"))

########################### 
# HANNAH EXTRACTION TABLE #
###########################

## NONE OF THESE ARE OVERLAPPING WITH THE RIPTIDE ONES 
extractions_hannah_df <- lapply(extractions_hannah_original, function(x){
  x <- x %>% mutate_at(vars(contains('Date')), ~lubridate::ymd(.))
  return(x)
}) %>% rbindlist(fill = T, use.names = T) 
names(extractions_hannah_df) <- mgsub::mgsub(tolower(names(extractions_hannah_df)), 
                                           c("[#]", "[[:space:]]|[.]|[[:punct:]]$", "[[:punct:]]"), 
                                           c("num", "", "_"))

extractions_hannah_df <- extractions_hannah_df %>% 
  mutate(sampleid_barcode = coalesce(sampleid, sampleid_barcode)) %>% 
  select(-sampleid)

extractions_hannah_df %>% 
  subset(sampleid_barcode %in% flow_cell_original_rip$`Sample ID`) %>% 
  dim()

## library in flow cell "Riptide-"01 to 03 seem to be line with the "Olivier"01-03 counts
