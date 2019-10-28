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

# remove na rows
olivier_spleen_list_split <- lapply(olivier_spleen_list, function(df){
  df <- df %>% 
    filter(!grepl("^(?!\\d)", rfid, perl = T)) %>% 
    na.omit(rfid) 
return(df)
})
olivier_spleen_list_split[[10]] <- NULL

# add cohort information
for(i in 1:9){
    olivier_spleen_list_split[[i]]$cohort = as.character(olivier_spleen_list[[10]][i,2])
} # replacement has 40 rows, data has 39 so it has to be outside the lapply function

olivier_spleen_list_df <- rbindlist(olivier_spleen_list_split)
  