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
    filter(!grepl("^(?!\\d)", rfid, perl = T)) %>% 
    na.omit(rfid) %>%
    separate(col = cohort, into = c("cohort", "experiment"), sep = " (?=[^ ]+$)") %>% 
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

# are all of these found in the experiments 
## extract from the u01_qc file  
## merge the two olivier Rdata files and compare the long rfid 
phenotyped_vs_spleens <- do.call("rbind", list(olivier_spleen_list_df$rfid, WFU_Olivier_ox_test_df$rfid, WFU_Olivier_co_test_df$rfid))
any(duplicated(do.call("rbind", list(olivier_spleen_list_df$rfid, WFU_Olivier_ox_test_df$rfid, WFU_Olivier_co_test_df$rfid))))


pd.merge(df1, df2, on=['Name'], how='inner')


# check the number of characters in the rfid
olivier_spleen_list_df %>% mutate(rfid_digits = nchar(rfid)) %>% filter(rfid_digits != 15) # all id's are 15 digits here
WFU_Olivier_ox_test_df %>% mutate(rfid_digits = nchar(rfid)) %>% filter(rfid_digits != 15) # one case isn't 15 digits
WFU_Olivier_co_test_df %>% mutate(rfid_digits = nchar(rfid)) %>% filter(rfid_digits != 15) # four cases aren't 15 digits


