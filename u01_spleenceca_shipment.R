# process the ceca and spleen shipment data

library(dplyr)
library(data.table)
library(tidyverse)
library(readxl)
library(tidyxl)

# install.packages('splitstackshape')
# install.packages('janitor')
library(splitstackshape)
library(janitor)
library(stringr)


###########################
###### OLIVIER ############
###########################

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_WFU_U01_ShippingMaster")

olivier_spleen_raw <- read_excel(path = "Olivier Spleens Oxy and Coc 91319.xlsx", col_names = F)
olivier_spleen_cells_raw <- tidyxl::xlsx_cells(path = "Olivier Spleens Oxy and Coc 91319.xlsx")
olivier_spleen_formats_raw <- xlsx_formats(path = "Olivier Spleens Oxy and Coc 91319.xlsx")

# parse through indices using colors, couldn't work because of two observations 

# using olivier_spleen_formats_raw$local$fill$patternFill$fgColor$rgb %>% unique()
# why is the result of this only 6 when there are 9 categories? olivier_spleen_formats_raw$local$fill$patternFill$fgColor$rgb %>% na.omit %>% unique() %>% length()

# test <- olivier_spleen_cells_raw %>% 
#   filter(grepl("Cohort", olivier_spleen_cells_raw$character)) 
# test$character_formatted is all NA 

# parse through the data using location and order 
# create dataframe with three cols: cohort, rfid, and labanimalid

# assign colnames 
names(olivier_spleen_raw)[seq(1, ncol(olivier_spleen_raw)-1, 2)] <- paste0("rfid")
names(olivier_spleen_raw)[seq(2, ncol(olivier_spleen_raw)-1, 2)] <- paste0("labanimalid")

# make list of dfs
olivier_spleen_list <- list()

for(i in seq(1, ncol(olivier_spleen_raw), 2)){
  j = (i/2)+0.5
  k = i + 1
  olivier_spleen_list[[j]] <- olivier_spleen_raw[, (i:k)]
}

# add cohort information
for(i in 1:9){
  olivier_spleen_list[[i]]$cohort = as.character(olivier_spleen_list[[10]][i,2])
} # replacement has 40 rows, data has 39 so it has to be outside the lapply function
olivier_spleen_list[[10]] <- NULL

# remove na rows and split the cohort into two
olivier_spleen_list_df <- lapply(olivier_spleen_list, function(df){
  df <- df %>% 
    dplyr::filter(!grepl("^(?!\\d)", rfid, perl = T)) %>% 
    na.omit(rfid) %>%
    tidyr::separate(col = cohort, into = c("cohort", "experiment"), sep = " (?=[^ ]+$)") %>% 
    mutate(sex = substring(labanimalid, 1, 1))
return(df)
}) %>% rbindlist()

# QC: 
# number of counts as the raw files: all numbers match cohorts 1:8 for cocaine, no cohort 6, and only 3 and 4 for oxy
##  olivier_spleen_list_df %>% group_by(cohort, experiment) %>% count

# sex count ## bring this to apurva's attention
olivier_spleen_sex <- olivier_spleen_list_df %>% group_by(cohort, experiment, sex) %>% summarize(sexcount = n())
ggplot(olivier_spleen_list_df, aes(x = sex, fill = sex)) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~cohort + experiment) + 
  labs(title = "Spleens sent to UCSD for Oxy and Coc 9/13/19")

# unique id's, no duplicate id's 
## olivier_spleen_list_df[duplicated(olivier_spleen_list_df$rfid)]

# are any of these found in naive 
## extract from the u01_qc file  
# list <- list(Olivier_co_naive <- rbindlist(WFU_Olivier_co_naive_test, use.names = T, idcol = "cohort"), 
#              Olivier_ox_naive <- rbindlist(WFU_Olivier_ox_naive_test, use.names = T, idcol = "cohort")) 
WFU_Olivier_co_naive_df <- rbindlist(WFU_Olivier_co_naive_test, use.names = T)
olivier_spleen_list_df %>% 
  mutate(Cocaine_Naive = ifelse(experiment == "Cocaine" && rfid %in% WFU_Olivier_co_naive_df$rfid, "Naive", "Not Naive"),
         Oxycodone_Naive = ifelse(experiment == "Oxycodone" && rfid %in% WFU_Olivier_co_naive_df$rfid, "Naive", "Not Naive")) %>% 
  group_by(Cocaine_Naive, Oxycodone_Naive) %>% 
  count() # All Not Naive

# are any of the shipped spleens from the naive dataset 
olivier_spleen_list_df %>% 
  dplyr::filter(experiment == "Cocaine", rfid %in% WFU_Olivier_co_naive_df$rfid)
# are any of the shipped spleens from the collected data 
olivier_spleen_list_df %>% 
  dplyr::filter(experiment == "Cocaine", rfid %in% $rfid)

# are all of these found in the experiments 
## extract from the u01_qc file  
## merge the two olivier Rdata files and compare the long rfid 
phenotyped_vs_spleens <- do.call("rbind", list(olivier_spleen_list_df$rfid, WFU_Olivier_ox_test_df$rfid, WFU_Olivier_co_test_df$rfid))
any(duplicated(do.call("rbind", list(olivier_spleen_list_df$rfid, WFU_Olivier_ox_test_df$rfid, WFU_Olivier_co_test_df$rfid))))

olivier_spleen_list_df %>% 
  mutate(Cocaine_Data = ifelse(experiment == "Cocaine" && rfid %in% selfadmin_df$rfid, "Self admin data", "No self admin data")) %>% 
         # ,
         # Oxycodone_Data = ifelse(experiment == "Oxycodone" && rfid %in% WFU_Olivier_ox_naive_test$rfid, "Naive", "Not Naive")) %>% 
  group_by(Cocaine_Data) %>% 
           #, Oxycodone_Data) %>% 
  count() # All 411 spleens have self admin data

# check the number of characters in the rfid
olivier_spleen_list_df %>% mutate(rfid_digits = nchar(rfid)) %>% filter(rfid_digits != 15) # all id's are 15 digits here
WFU_Olivier_ox_test_df %>% mutate(rfid_digits = nchar(rfid)) %>% filter(rfid_digits != 15) # one case isn't 15 digits
WFU_Olivier_co_test_df %>% mutate(rfid_digits = nchar(rfid)) %>% filter(rfid_digits != 15) # four cases aren't 15 digits


