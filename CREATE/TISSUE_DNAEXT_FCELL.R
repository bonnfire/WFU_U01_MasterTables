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
## FLOW CELL TABLE (RIYAN)
######################## 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues")

flow_cell_original_riyan <- u01.importxlsx("2020-01-16-Flowcell Sample-Barcode list-Riptide-UMich2-Riptide03_NovaSeq01 (copy for Riyan).xlsx") # 1 table
dataframe = list()
for(i in 1:10){
  dataframe[[i]] <- flow_cell_original_riyan$Sheet1 %>% group_by(Barcode) %>% slice(i)
}
write.xlsx(dataframe, file='Flowcell_Sample_Sheet.xlsx')



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

extractions_khai_df <- extractions_khai_df %>%
  dplyr::filter(u01 != "Template") %>% 
  mutate(rfid = paste0("933000", sampleid_barcode)) %>% 
  mutate(u01_rfid_verified = case_when(
    rfid %in%  WFU_OlivierCocaine_test_df$rfid ~ "u01_olivier_cocaine",
    # rfid == "933000120117313" ~ "u01_olivier_cocaine",
    rfid %in%  WFU_OlivierOxycodone_test_df$rfid ~ "u01_olivier_oxycodone",
    rfid %in%  WFU_Jhou_test_df$rfid ~ "u01_jhou",
    rfid %in%  WFU_Mitchell_test_df$rfid ~ "u01_mitchell",
    TRUE ~ "NA")) %>% 
  select(-u01) %>%  
  left_join(., shipments_df[,c("rfid", "cohort", "u01")], by = c("rfid")) %>% 
  mutate(u01 = paste0(u01, "_", cohort)) %>% 
  select(-cohort)
# origin is not cohort
# WFU_OlivierCocaine_test_df %>% subset(rfid %in% c("933000320047386", "933000320046848"))

extractions_khai_df$u01_rfid_verified %>% table()
extractions_khai_df$u01 %>% table() ## fix the origin cells? also cocaine_oxy 2 cases?

# extractions_khai_df %>% mutate_at(vars(one_of("u01", "comments")), as.factor) %>% 
#   summary()
# extractions_khai_df %>% dplyr::filter(u01 != "Template") %>% dplyr::filter(u01!=dnaplatecode) # note that olivier c06 shares with mitchell c01

## INVESTIGATE THIS
# extractions_flowcell %>% dim
# extractions_flowcell %>% subset(transponder %in% WFU_OlivierCocaine_test_df$rfid) %>% dim
# extractions_flowcell %>% subset(transponder %in% WFU_OlivierOxycodone_test_df$rfid) %>% dim

#### SENT TO SEQUENCING CORE
extractions_flowcell <- extractions_khai_df %>% 
  subset(`sampleid_barcode` %in% flow_cell_original_rip$`Sample ID`) # 288

text(barplot(table(extractions_flowcell$u01), beside = T), 0, table(extractions_flowcell$u01))
table(extractions_flowcell$u01, extractions_flowcell$userid)

# using origin will get you the wrong cohorts?

extractions_flowcell$u01 %>% table() 
extractions_flowcell$u01_rfid_verified %>% table() 

# extractions_flowcell$origin %>% table()

olivier_spleen_list_df %>% subset(rfid %in% extractions_flowcell[which(extractions_flowcell$comments == "mismatch"),]$transponder)
extractions_flowcell[which(extractions_flowcell$comments == "mismatch"),]
extractions_flowcell %>% subset(comments == "mismatch")
# searching for duplicate entries in the sampleid barcode column FALSE FOR BOTH extractions_flowcell %>% select(transponder AND sampleid_barcode) %>% duplicated() %>% any() 
agrep("933000120117342", extractions_flowcell$transponder, value = T)
"933000120138331"
"933000120138561"



## GRAPHS 

extractions_flowcell %>% 
  ggplot(aes(x = `nanodropng_ul`)) + 
  geom_histogram() + 
  facet_grid(~ u01) +
  labs(title = paste0("Nanodrop(ng/ul) values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% 
  ggplot(aes(x = `u01`, y = `nanodropng_ul`)) + 
  geom_boxplot() + 
  labs(title = paste0("Nanodrop(ng/ul) values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))


extractions_flowcell %>% dplyr::filter(u01 == "Olivier_Co_03") %>% 
  ggplot(aes(x = `u01`, y = `nanodropng_ul`, fill = userid)) + 
  geom_boxplot() + 
  labs(title = paste0("Nanodrop(ng/ul) values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% 
  ggplot(aes(x = `260_280`, color = u01)) + 
  geom_density() + 
  # facet_grid(~ u01) +
  labs(title = paste0("260_280 values by U01")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        legend.text=element_text(size=20))

extractions_flowcell %>% 
  ggplot(aes(x = `260_280`, color = u01)) + 
  geom_density() + 
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

extractions_flowcell %>% dplyr::filter(u01 == "Olivier_Co_03") %>% 
  ggplot(aes(x = `u01`, y = `260_280`, fill = userid)) + 
  geom_boxplot() + 
  labs(title = paste0("260_280 values by U01")) + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))
  
extractions_flowcell %>% 
    ggplot(aes(x = `260_230`, color = u01)) + 
    geom_density() + 
    # facet_grid(~ u01) +
    labs(title = paste0("260_230 values by U01")) + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        legend.text=element_text(size=20))


extractions_flowcell %>% 
  ggplot(aes(x = `u01`, y = `260_230`)) + 
  geom_boxplot() + 
  labs(title = paste0("260_230 values by U01"))  + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

extractions_flowcell %>% dplyr::filter(u01 == "Olivier_Co_03") %>% 
  ggplot(aes(x = `u01`, y = `260_230`, fill = userid)) + 
  geom_boxplot() + 
  labs(title = paste0("260_280 values by U01")) + 
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




#### KHAI DNA EXTRACTION USING THE FLIPAPI

names(khai_spleenextraction_df) <- mgsub::mgsub(tolower(names(khai_spleenextraction_df)), 
                                           c("[#]", "[[:space:]]|[.]|[[:punct:]]$", "[[:punct:]]"), 
                                           c("num", "", "_"))


khai_spleenextraction_df <- khai_spleenextraction_df %>%
  mutate_all(as.character) %>% 
  mutate(rfid = paste0("933000", sampleid_barcode)) %>% 
  mutate(u01_rfid_verified = case_when(
    rfid %in%  WFU_OlivierCocaine_test_df$rfid ~ "u01_olivier_cocaine",
    # rfid == "933000120117313" ~ "u01_olivier_cocaine",
    rfid %in%  WFU_OlivierOxycodone_test_df$rfid ~ "u01_olivier_oxycodone",
    rfid %in%  WFU_Jhou_test_df$rfid ~ "u01_jhou",
    rfid %in%  WFU_Mitchell_test_df$rfid ~ "u01_mitchell",
    TRUE ~ "NA")) %>% 
  left_join(., shipments_df[,c("rfid", "cohort", "u01")], by = c("rfid")) %>% 
  mutate(u01 = paste0(u01, "_", cohort)) %>% 
  select(-cohort)

# origin is not cohort
# WFU_OlivierCocaine_test_df %>% subset(rfid %in% c("933000320047386", "933000320046848"))