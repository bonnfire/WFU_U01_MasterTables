setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster")

# Load libraries
library(tidyverse) #loads dplyr, tidyr, ggplot2, purrr, etc
require(mgsub)
library(readxl)
library(lubridate)
library(openxlsx)
library(stringr)
library(data.table)

# QC Wake Forest University Shipment 

## all user-defined functions for consistent reformatting ## 
# ordered by appearance 

u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 

uniform.var.names.testingu01 <- function(df){
  lapply(seq_along(df), function(i) {
    if(grepl("Parent", names(df[[i]])) %>% any()){
      names(df[[i]])[1:2] <- df[[i]][1,][1:2] %>% as.character()
      df[[i]] <- df[[i]][-1, ]
    }
    names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
                                   c(" |\\.", "#", "Transponder ID", "Date of Wean|Wean Date","Animal", "Shipping|Ship", "Dams"),
                                   c("", "Number", "RFID", "DOW","LabAnimal", "Shipment", "Dames"))
    # names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
    #                                c("DateofShipment", "LabAnimalID"), 
    #                                c("ShipmentDate", "LabAnimalNumber"))
    names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
                                   c("DateofShipment", "LabAnimalNumber"), 
                                   c("ShipmentDate", "LabAnimalID")) # actually keep the column named lab animal number
    names(df[[i]]) <- tolower(names(df[[i]]))
    df[[i]]
  })
}

# XX: write code that verifies and sets the correct variable types 

uniform.coatcolors <- function(df){
  lapply(seq_along(df), function(i) {
    df[[i]]$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(df[[i]]$coatcolor, 
                                                                        c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                        c("BROWN", "BLACK", "HOOD", "ALBINO"))) 
      
    #   mgsub::mgsub(df[[i]]$coatcolor, 
    #                                   c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD\\.|[H|h]ood", "[A|a]lbino"), 
    #                                   c("BROWN", "BLACK", "HOOD", "ALBINO"))
    # df[[i]]$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", df[[i]]$coatcolor)
    #df[[i]]$coatcolor <- toupper(df[[i]]$coatcolor)
    df[[i]]
  })
} # function should be used for other cases


uniform.date.testingu01 <- function(df){
  lapply(seq_along(df), function(i) {
    datecols <- c("dob", "dow", "shipmentdate")
    datefunction <- function(x){
      if(is.POSIXct(x) == F){
        as.POSIXct(as.numeric(x) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")
      } else x
    }
    df[[i]] <- df[[i]] %>% 
      mutate_at(.vars = vars(datecols), .funs = datefunction)
    return(df[[i]])
  })
} # function should be used for other cases (testing)

unique.values.length.by.col <- function(df, var){
  lapply(df, function(df){
    uniquevarcount <- sapply(df[var], function(x){
      unique(x) %>% length()})
      print(paste("for", var, "the number of unique values (observed count) is", uniquevarcount, "and number of rows (expected count) is", nrow(df)))})
} # function should be used for identification variables, and for other cases (testing)

unique.values.by.col <- function(df, var){
  lapply(df, function(df){
    sapply(df[var], unique)
  })
} # function should be used to view all unique values in a column (like coat color) and for other experiments (testing )

remove.scrubs.and.narows <- function(df){
  lapply(seq_along(df), function(i) {
    # rownumber <- apply(df[[i]], MARGIN = 1, function(r){any(r %in% c("Scrubs", "Scrub", "ITALY EXTRA 15 RATS"))}) %>% which()
    # if(is.integer(rownumber) && length(rownumber) != 0){
    #   df[[i]] <- df[[i]][-(rownumber:nrow(df[[i]])),]
    # }
    rownumber <- which(is.na(df[[i]]$rfid))[1]
    if(is.na(rownumber) == F){
      df[[i]] <- df[[i]][-(rownumber:nrow(df[[i]])),]
    }
    
    # df[[i]] <- ifelse(which(is.na(df[[i]]$rfid))[1] > 1, df[[i]][- c(which(is.na(df[[i]]$rfid))[1] : nrow(df[[i]])), ],  df[[i]])
    # df[[i]] <- df[[i]][rowSums(is.na(df[[i]])) != ncol(df[[i]]), ] #remove rows that have all na
    # df[[i]] <- df[[i]][ , colSums(is.na(df[[i]])) == 0] # remove columns that have any na
    return(df[[i]])
  })
}

uniform.boxformat <- function(df){
  lapply(seq_along(df), function(i){
    df[[i]]$shipmentbox <- str_extract(df[[i]]$shipmentbox, "\\d+")
    return(df[[i]])
  })
}

master_df <- function(x){
  x$cohort <- ifelse(grepl("#", x$cohort), stringr::str_match(x$cohort, "#(\\d+).*?")[,2], x$cohort)
  x$cohort <- ifelse(nchar(x$cohort) > 1, x$cohort, gsub('([[:digit:]]{1})$', '0\\1', x$cohort)) # add leading zeroes when necessary
  x$litternumber = as.numeric(x$litternumber)
  x$littersize = as.numeric(x$littersize)
  return(x)
  }

## all user-defined functions for quality check and validation ## 
# QC <- function(df){
#   lapply(seq_along(df), function(i){
#     dur.wean.summary = list()
#     dur.ship.summary = list()
#     dur.wean.summary[[i]] <- interval(df[[i]]$'D.O.B', df[[i]]$'D.O.W') %>% 
#       as.duration() %>% summary() 
#     dur.ship.summary[[i]] <- interval(df[[i]]$'D.O.B', df[[i]]$'Shipment_Date') %>% 
#       as.duration() %>% summary() 
#     list <- list("WeaningAge" = dur.wean.summary[[i]], "ShipmentAge" = dur.ship.summary[[i]])
#     list
#   })
# }

## note: which experiment/lab required a specified function
# 1. flagel: has misinputted year of shipment in the second sheet
# 2. kalivas: some sheets have labanimalid and others have labanimalnumber (FIXED ALL)
# 3. kalivas (italy) : includes age at shipment column that we are omitting and cross checking, doesn't have "labanimalnumber", and addtional rats rows (FIXED ALL)
# 4. jhou: doesn't have "labanimalnumber" but there seems to be issue (FIXED)
# 5. mitchell: first table labanimalnumber looks like TJ001 vs second and third tables MIT101 (noted 10/17) no action until necessary, needed to add first table shipping date, added the pregnant cases to the first table (FIXED)
# 6. olivier (cocaine): rfid was numeric rather than character (FIXED) and requires removal of scrub cases (FIXED) and FIRST TABLE NEEDS A LOT OF WORK
# 7. olivier (oxycodone): requires removal of scrub cases, added shipment dates (sheet 2), extracted box info, removed trailing date  (FIXED ALL)


######################
####### ALL DF #######
######################


######################
## IGNORE Flagel #####
######################
# WFU_Flagel <- u01.importxlsx("Flagel Master Shipping Sheet.xlsx")
# WFU_Flagel_test <- uniform.var.names.testingu01(WFU_Flagel)
# WFU_Flagel_test <- remove.scrubs.and.narowsolivier(WFU_Flagel_test) # temporarily borrowing olivier scrubs function bc it also removes columns with na and in the first sheet, additional column that notes cost to add per rat in first table
# 
# # # checking id vars
# idcols <- c("labanimalnumber", "accessid", "rfid")
# unique.values.length.by.col(WFU_Flagel_test, idcols) ## XX unique.values.length.by.col needs minor tweaking for cleaner output 
# 
# # # checking date consistency 
# WFU_Flagel_test2 <- uniform.date.testingu01(WFU_Flagel_test)
# 
# # # add age of shipment and check consistency
# WFU_Flagel_test <- lapply(WFU_Flagel_test, transform, shipmentage = as.numeric(shipmentdate - dob))
# lapply(WFU_Flagel_test, function(x) summary(x$shipmentage))
# subset(WFU_Flagel_test[[2]], shipmentage < 0) ## XX include this in Dropbox, notify Apurva and Oksana
# 
# # # checking the number of rfid digits 
# 
# lapply(WFU_Flagel_test, function(x){
#   x %>% 
#     mutate(rfid_digits = nchar(rfid)) %>% 
#     filter(rfid_digits != 15)
# })
# 
# # # checking coat color consistency
# WFU_Flagel_test <- uniform.coatcolors(WFU_Flagel_test)
# lapply(WFU_Flagel_test, function(df)
#   sapply(df["coatcolor"], unique))
# 
# # # return clean sheet names 
# names(WFU_Flagel_test) <- names(WFU_Flagel)

######################
### Kalivas(ITALY) ###
######################
WFU_KalivasItaly <- u01.importxlsx("(Italy) Master Shipping.xlsx")
WFU_KalivasItaly[[4]] <- u01.importxlsx("Italy #4 Shipping sheet.xlsx")$Italy
WFU_KalivasItaly[[5]] <- u01.importxlsx("Italy #5 Shipping sheet.xlsx")$Italy
WFU_KalivasItaly_test <- uniform.var.names.testingu01(WFU_KalivasItaly)

# create the naive dataset before removing it and clean up naive dataset (pilot 15 rats in cohort 2)
WFU_KalivasItaly_naive_test <- lapply(WFU_KalivasItaly_test, function(df) {
  rownumber <- apply(df, MARGIN = 1, function(r){any(r %in% c("Scrubs", "Scrub", "ITALY EXTRA 15 RATS"))}) %>% which()
  if(length(rownumber) != 0){
    subset(df[rownumber:nrow(df),], grepl("^\\d+.+$", rfid))
  } else NULL
  })
# to do: add the shipment and wean ages either as separate function or integrate into the function above
  
# experiment specific: second cohort requires remove additional 15 rats sent to italy note because they are pilot rats 

# 12/6 unsure about removing scrubs now; just comment

# WFU_KalivasItaly_test <- remove.scrubs.and.narows(WFU_KalivasItaly_test) # get row number for which italy is shown and then remove all rows after that 

# removeallnarow <- function(df){
#   ind <- apply(df, 1, function(x) all(is.na(x)))
#   df <- df[ !ind, ]
# } # remove rows with all na
# WFU_KalivasItaly_test[[2]] <- removeallnarow(WFU_KalivasItaly_test[[2]]) # might not actually need this

# # checking date consistency 
WFU_KalivasItaly_test <- uniform.date.testingu01(WFU_KalivasItaly_test)

# # add age of shipment and check consistency
WFU_KalivasItaly_test <- lapply(WFU_KalivasItaly_test, transform, shipmentage = as.numeric(shipmentdate - dob))
lapply(WFU_KalivasItaly_test, function(x) summary(x$shipmentage))

# # add age of wean and check consistency
WFU_KalivasItaly_test <- lapply(WFU_KalivasItaly_test, transform, weanage = as.numeric(dow - dob))
lapply(WFU_KalivasItaly_test, function(x) summary(x$weanage))

# # checking the number of rfid digits 

lapply(WFU_KalivasItaly_test, function(x){
  x %>% 
    mutate(rfid_digits = nchar(rfid)) %>% 
    dplyr::filter(rfid_digits != 15)
})

# # checking coat color consistency
lapply(WFU_KalivasItaly_test, function(df)
  sapply(df["coatcolor"], unique)) ## XX Address [[2]] data 

WFU_KalivasItaly_test <- uniform.coatcolors(WFU_KalivasItaly_test)

# add comment section (esp for delayed shipping day and to add scrub note for 15 animals in co2)
WFU_KalivasItaly_test <- mapply(cbind, WFU_KalivasItaly_test, comment = NA, resolution = NA) 
WFU_KalivasItaly_test <- lapply(WFU_KalivasItaly_test, function(x){
  x <- x %>% 
    mutate(comment = ifelse(rfid %in% rbindlist(WFU_KalivasItaly_naive_test, fill = T)$rfid, "Scrub", comment))
  return(x)
})

# currently ok, but change to paste in case any scrubs need additional comments
WFU_KalivasItaly_test[[1]] <- WFU_KalivasItaly_test[[1]] %>%
  mutate(comment = "Original shipment date 2019-01-29 (held 1 week due to heat)") %>%
  select(-pairnumber)
WFU_KalivasItaly_test[[3]] <- WFU_KalivasItaly_test[[3]][, -c(16:19)] %>% # XX remove three empty columns, age@ship, and bottom rows 
  mutate(comment = "Original shipment 2019-08-05 (held 2 weeks due to heat)") 
WFU_KalivasItaly_test[[4]] <- WFU_KalivasItaly_test[[4]][, -c(16:19)]
WFU_KalivasItaly_test[[5]] <- WFU_KalivasItaly_test[[5]][, -c(16:19)]

# # remove non-digit rfid checking id vars
WFU_KalivasItaly_test <- lapply(WFU_KalivasItaly_test, function(x){
 x <- x %>% 
   dplyr::filter(grepl("^\\d{2,}", rfid))
   return(x)
}) 
idcols <- c("accessid", "rfid")
unique.values.length.by.col(WFU_KalivasItaly_test, idcols) 

# cleared out the scrubs and comments 
names(WFU_KalivasItaly_test) <- str_pad(seq(1:length(WFU_KalivasItaly_test)), 2, pad = "0")

WFU_KalivasItaly_test_df <- rbindlist(WFU_KalivasItaly_test, id = "cohort", fill = T)


## check no siblings from prev cohort 
WFU_KalivasItaly_test_df %>% mutate(U01 = "KalivasItaly") %>% dplyr::filter(cohort == "04") %>% 
  group_by(sires, dames, cohort, U01) %>% 
  add_count() %>% 
  select(U01, sires, dames, cohort, n) %>% 
  ungroup() %>% 
  rename("siredamepair_in_cohort"="n") %>% 
  group_by(sires,dames, U01) %>% 
  add_count() %>% 
  rename("siredamepair_in_u01"="n") %>% 
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01) %>% 
  unique() %>% 
  arrange(U01, sires) %>%
  data.frame() 

## check no same sex siblings (diff litter)
WFU_KalivasItaly_test_df %>% dplyr::filter(cohort == "04") %>% janitor::get_dupes(sires, dames, sex)

## check no same sex littermates (same litter)
WFU_KalivasItaly_test_df %>% dplyr::filter(cohort == "04") %>% janitor::get_dupes(sires, dames, litternumber, sex)

## check number of same sex rats in each rack and get number of rat sexes in each rack
WFU_KalivasItaly_test_df %>% dplyr::filter(cohort == "04") %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
WFU_KalivasItaly_test_df %>% mutate(U01 = "KalivasItaly") %>% dplyr::filter(cohort == "04") %>% group_by(rack) %>% count(sex) 

# add resolution section XX can I assume that these are ignorable??? 

######################
## Kalivas(Heroine) ##
######################
WFU_Kalivas <- u01.importxlsx("MUSC (Kalivas) Master Shipping.xlsx")
# As per email chain from August and September, the shipment from the third cohort of animals needs to be merged into Kalivas master sheets.
WFU_Kalivas[[3]] <-  u01.importxlsx("MUSC (Kalivas) #3 Shipping sheet.xlsx")$Kalivas
WFU_Kalivas[[4]] <- u01.importxlsx("MUSC (Kalivas) Shipping sheet #4.xlsx")$Kalivas
WFU_Kalivas[[5]] <-  u01.importxlsx("MUSC (Kalivas)#5 Shipping Sheet.xlsx")$Kalivas
WFU_Kalivas_test <- uniform.var.names.testingu01(WFU_Kalivas)
WFU_Kalivas_test[[1]] <- WFU_Kalivas_test[[1]] %>% 
  select(-pairnumber)

# # checking id vars
idcols <- c("labanimalid", "accessid", "rfid")
unique.values.length.by.col(WFU_Kalivas_test, idcols) ## XX generalize the test so it is able to skip the test if the column doesn't exist

# # checking date consistency 
WFU_Kalivas_test <- uniform.date.testingu01(WFU_Kalivas_test)

# # add age of shipment and check consistency
WFU_Kalivas_test <- lapply(WFU_Kalivas_test, transform, shipmentage = as.numeric(shipmentdate - dob))
lapply(WFU_Kalivas_test, function(x) summary(x$shipmentage))

# # add age of wean and check consistency
WFU_Kalivas_test <- lapply(WFU_Kalivas_test, transform, weanage = as.numeric(dow - dob))
lapply(WFU_Kalivas_test, function(x) summary(x$weanage))

# # add comment and resolution and check consistency
WFU_Kalivas_test <- lapply(WFU_Kalivas_test, cbind, comment = NA, resolution = NA)

# # checking the number of rfid digits 

lapply(WFU_Kalivas_test, function(x){
  x %>% 
    mutate(rfid_digits = nchar(rfid)) %>% 
    dplyr::filter(rfid_digits != 15)
})

# # checking coat color consistency
unique.values.by.col(WFU_Kalivas_test, "coatcolor")
WFU_Kalivas_test <- uniform.coatcolors(WFU_Kalivas_test)

# assign cohort numbers to list and create df
names(WFU_Kalivas_test) <-  str_pad(seq(1:length(WFU_Kalivas_test)), 2, pad = "0")

WFU_Kalivas_test_df <- rbindlist(WFU_Kalivas_test, id = "cohort", fill = T)


## check no siblings from prev cohort 
WFU_Kalivas_test_df %>% mutate(U01 = "Kalivas") %>% 
  group_by(sires, dames, cohort, U01) %>% 
  add_count() %>% 
  select(U01, sires, dames, cohort, n) %>% 
  ungroup() %>% 
  rename("siredamepair_in_cohort"="n") %>% 
  group_by(sires,dames, U01) %>% 
  add_count() %>% 
  rename("siredamepair_in_u01"="n") %>% 
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01) %>% 
  dplyr::filter(cohort == "04") %>%
  unique() %>% 
  arrange(U01, sires) %>%
  data.frame() 

## check no same sex siblings (diff litter)
WFU_Kalivas_test_df %>% dplyr::filter(cohort == "04") %>% janitor::get_dupes(sires, dames, sex)

## check no same sex littermates (same litter)
WFU_Kalivas_test_df %>% dplyr::filter(cohort == "04") %>% janitor::get_dupes(sires, dames, litternumber, sex)

## check number of same sex rats in each rack and get number of rat sexes in each rack
WFU_Kalivas_test_df %>% dplyr::filter(cohort == "04") %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
WFU_Kalivas_test_df %>% dplyr::filter(cohort == "04") %>% group_by(rack) %>% count(sex) 


######################
######## JHOU ########
######################
WFU_Jhou <- u01.importxlsx("Jhou Master Shipping Sheet.xlsx")

# 12/10 Realized that I hadn't included Jhou #12
WFU_Jhou[[12]] <- u01.importxlsx("Jhou #12 shipping sheet.xlsx")[["JHOU"]] # from WFU (Angela) email "Please refer to the tab named JHOU. Let me know if there are any questions. ARP- The tab labeled ARP is for the convenience of shipping."
names(WFU_Jhou)[12] <- "#12(10-08-2019)" 

## 11/18 
WFU_Jhou[[13]] <- u01.importxlsx("Jhou #13 Shipping sheet.xlsx")[["Jhou"]] # from WFU (Angela) email "Please refer to the tab named JHOU. Let me know if there are any questions. ARP- The tab labeled ARP is for the convenience of shipping."
names(WFU_Jhou)[13] <- "#13(11-19-2019)" 

## 1/9 add cohort
WFU_Jhou[[14]] <- u01.importxlsx("Jhou #14 Shipping Sheet.xlsx")[["Jhou"]] # from WFU (Angela) email "Please refer to the tab named JHOU. Let me know if there are any questions. ARP- The tab labeled ARP is for the convenience of shipping."

# # make within-df variable names consistent and fix sires/dames column name issue
WFU_Jhou_test <- uniform.var.names.testingu01(WFU_Jhou)
WFU_Jhou_test[[14]]$rfid = as.character(WFU_Jhou_test[[14]]$rfid)

# experiment/table specific: remove empty columns 16-18
WFU_Jhou_test[[1]] <- WFU_Jhou_test[[1]][, -c(16:18), drop = F] # drop = F retains data type
WFU_Jhou_test <- lapply(WFU_Jhou_test, janitor::remove_empty, which = "rows")

###  add Shipment Date into Jhou data *** EXPERIMENTER SPECIFIC *** 
WFU_Jhou_test[[2]] <- WFU_Jhou_test[[2]] %>% mutate("shipmentdate" = as.POSIXct("2018-07-05", format="%Y-%m-%d", tz = "UTC"))
# WFU_Jhou_test[[2]] <- WFU_Jhou_test[[2]] %>% mutate("shipmentdate" = ymd("2018-07-05"))

# # checking id vars
idcols <- c("accessid", "rfid")
unique.values.length.by.col(WFU_Jhou_test, idcols) 
# length(unique(WFU_Jhou_test[[x]]$idcols)) == nrow(WFU_Jhou_test[[x]]) 
# WFU_Jhou_test[[12]][duplicated(WFU_Jhou_test[[12]]$accessid),] # returns two
WFU_Jhou_test[[12]][which(WFU_Jhou_test[[12]]$rfid == "933000320186901"),]$accessid <- "75010_2" # based on WFU (Angela) email from 11/18/2019 10:05 AM: "Sorry, it was a copy and paste error. They are the same except the last digit. 933000320186901 is actually 75010_2. I have corrected it on this end so all you need to do is change the last digit from 1 to 2." 


# # checking date consistency 
WFU_Jhou_test <- uniform.date.testingu01(WFU_Jhou_test)

# # add age of shipment and check consistency
WFU_Jhou_test <- lapply(WFU_Jhou_test, transform, shipmentage = as.numeric(shipmentdate - dob))
lapply(WFU_Jhou_test, function(x) summary(x$shipmentage))

# # add age of wean and check consistency
WFU_Jhou_test <- lapply(WFU_Jhou_test, transform, weanage = as.numeric(dow - dob))
lapply(WFU_Jhou_test, function(x) summary(x$weanage))

# # add comment and resolution and check consistency
WFU_Jhou_test <- lapply(WFU_Jhou_test, cbind, comment = NA, resolution = NA)
# # checking the number of rfid digits 

# check number of rfid digits
lapply(WFU_Jhou_test, function(x){
  x %>% 
    mutate(rfid_digits = nchar(rfid)) %>% 
    dplyr::filter(rfid_digits != 15)
})


# # checking coat color consistency
unique.values.by.col(WFU_Jhou_test, "coatcolor")
WFU_Jhou_test <- uniform.coatcolors(WFU_Jhou_test)

# final touches: clean up names 
names(WFU_Jhou_test) <- names(WFU_Jhou)

WFU_Jhou_test_df <- bind_rows(WFU_Jhou_test, .id = "cohort") # create this format for rfidandcohort data that are joined with raw data
WFU_Jhou_test_df %<>% mutate(cohort = ifelse(nchar(cohort) > 1, cohort, gsub('([[:digit:]]{1})$', '0\\1', cohort)))

# check number of rat siblings in each cohort # cohort 13 has all two siblings
# and 
# make sure that cohort pairs don't spill into another # no new cases from cohort 13

## check no siblings from prev cohort 
WFU_Jhou_test_df %>% mutate(U01 = "Jhou") %>% 
  group_by(sires, dames, cohort, U01) %>% 
  add_count() %>% 
  select(U01, sires, dames, cohort, n) %>% 
  ungroup() %>% 
  rename("siredamepair_in_cohort"="n") %>% 
  group_by(sires,dames, U01) %>% 
  add_count() %>% 
  rename("siredamepair_in_u01"="n") %>% 
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01) %>% 
  dplyr::filter(cohort == "14") %>%
  unique() %>% 
  arrange(U01, sires) %>%
  data.frame() 

## check no same sex siblings (diff litter)
WFU_Jhou_test_df %>% dplyr::filter(cohort == "14") %>% janitor::get_dupes(sires, dames, sex)

## check no same sex littermates (same litter)
WFU_Jhou_test_df %>% dplyr::filter(cohort == "14") %>% janitor::get_dupes(sires, dames, litternumber, sex)

## check number of same sex rats in each rack and get number of rat sexes in each rack
WFU_Jhou_test_df %>% dplyr::filter(cohort == "14") %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
WFU_Jhou_test_df %>% dplyr::filter(cohort == "14") %>% group_by(rack) %>% count(sex) 



######################
######## MITCHELL ####
######################
WFU_Mitchell <- u01.importxlsx("Mitchell Master Shipping Sheet.xlsx")
WFU_Mitchell_test <- uniform.var.names.testingu01(WFU_Mitchell)

# fix first table to account for: no ship date data(DONE), formatting of first row/sires and dames (DONE), highlight (add comment that the highlighted should be excluded)
pregnant <- WFU_Mitchell_test[[1]] %>% 
  dplyr::filter(is.na(`15`)==F) %>% 
  select(rfid) %>% 
  unlist %>% 
  as.vector # extract the pregnant cases to include as comment

WFU_Mitchell_test[[1]] <- WFU_Mitchell_test[[1]][ , colSums(is.na(WFU_Mitchell_test[[1]])) == 0] # remove columns with any na's, checked for no na rows 

WFU_Mitchell_test[[1]] <- WFU_Mitchell_test[[1]] %>% 
  mutate(shipmentdate = as.POSIXct("2018-10-30", format="%Y-%m-%d"))

# # checking id vars
idcols <- c("labanimalid", "accessid", "rfid")
unique.values.length.by.col(WFU_Mitchell_test, idcols)

# # checking date consistency 
WFU_Mitchell_test <- uniform.date.testingu01(WFU_Mitchell_test)

# # add age of shipment and check consistency
WFU_Mitchell_test <- lapply(WFU_Mitchell_test, transform, shipmentage = as.numeric(shipmentdate - dob) %>% round)
lapply(WFU_Mitchell_test, function(x) summary(x$shipmentage)) # cohort 3 is slightly older

# # add age of wean and check consistency
WFU_Mitchell_test <- lapply(WFU_Mitchell_test, transform, weanage = as.numeric(dow - dob))
lapply(WFU_Mitchell_test, function(x) summary(x$weanage))

# # add comment and resolution and check consistency (NO NEED BECAUSE IT ALREADY EXISTS)
#WFU_Mitchell_test <- lapply(WFU_Mitchell_test, cbind, comment = NA, resolution = NA)

# add comment and resolution columns **EXPERIMENTER SPECIFIC** 
WFU_Mitchell_test <- lapply(WFU_Mitchell_test, cbind, comment = NA, resolution = NA) ## XX PICK UP FROM HERE
WFU_Mitchell_test[[1]]$comment <- ifelse(WFU_Mitchell_test[[1]]$rfid %in% pregnant, "Pregnant female", NA)
WFU_Mitchell_test[[1]]$resolution <- ifelse(WFU_Mitchell_test[[1]]$rfid %in% pregnant, "REMOVE_FROM_EXCLUSION_AND_REPLACE", NA)


# # checking the number of rfid digits 

lapply(WFU_Mitchell_test, function(x){
  x %>% 
    mutate(rfid_digits = nchar(rfid)) %>% 
    filter(rfid_digits != 15)
})

# # checking coat color consistency
# before unique.values.by.col(WFU_Mitchell_test, "coatcolor")

WFU_Mitchell_test <- uniform.coatcolors(WFU_Mitchell_test)
# after unique.values.by.col(WFU_Mitchell_test, "coatcolor")

# return original names of all sheets 
names(WFU_Mitchell_test) <- names(WFU_Mitchell) 

WFU_Mitchell_test_df <- rbindlist(WFU_Mitchell_test, id = "cohort", fill = T)
WFU_Mitchell_test_df <- master_df(WFU_Mitchell_test_df)

## XX INCOMPLETE  
# # # validate dates 
# QC(WFU_Mitchell_test)
# 
# # # check for outliers
# 
# # # collect counts for each column
# cols <- c("Dames", "Sires", grep("ID|Lab_Animal", names(WFU_Mitchell_test[[1]]), value = T) %>% unlist())
# table(WFU_Mitchell_test[[1]][c("Shipment_Box", "Ear_Punch")])
# 
# map(WFU_Mitchell_test, ~ count(., Sex)) # replace sex with any of the variables, but figure out a way to output all interested variables
# map(WFU_Mitchell_test, ~ count(., Dames, Sires, Litter_Number))
# 
# map(WFU_Mitchell_test, ~ count(., Litter_Number, Litter_Size))
# 
# map(WFU_Mitchell_test, ~ subset(., Dames==Sires))
# 
# map_interestedvars <- function(df, vector){
#   for(i in 1:length(vector))
#   map(WFU_Mitchell_test, ~ count(., Sex))
# }
# 
# map(WFU_Mitchell_test, ~ count(., Dames, Sires, Sex) %>% 
#                      subset(n != 1))
# 
# # graphics for email
# Mitchell_SameSexSiblings <- map(WFU_Mitchell_test, ~ count(., Dames, Sires, Sex) %>%
#                               subset(n!=1) %>% 
#                               nrow)
# All_Mitchell <- bind_rows(WFU_Mitchell_test) %>% count(Dames, Sires) %>% subset(is.na(Dames) == F)
# ggplot(All_Mitchell, aes(n)) + geom_bar() + ggtitle("All Mitchell Shipments") +
#   xlab("Number of siblings in one shipment") + ylab("Count") + 
#   geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.5),size=4)
# 
# # # subset original dataset to extract cases
# # WFU_Mitchell_problemsubset<- lapply(seq_along(WFU_Mitchell_test), 
# #                                 function(i) subset(WFU_Mitchell_test[[i]], Dames %in% conds_mitch[[i]]$Dames & Sires %in% conds_mitch[[i]]$Sires))
# WFU_Mitchell_problemsubset <- map(WFU_Mitchell_test, ~ group_by(.x, Dames, Sires, Sex) %>% 
#       filter(n() > 1))
# names(WFU_Mitchell_problemsubset) <- names(WFU_Mitchell_test)
# Mitchell_listDF <- list("#1(10-30-18)"=WFU_Mitchell_problemsubset[["#1(10-30-18)"]][,-(15:18)], 
#                     "#2(2-26-19)"=WFU_Mitchell_problemsubset[["#2(2-26-19)"]],
#                     "#3(6-17-19)"=WFU_Mitchell_problemsubset[["#3(6-17-19)"]])  
# 
# write.xlsx(Mitchell_listDF, file = "Mitchell_SiblingSubset.xlsx", append = T)
# 
# # # subset original dataset to extract one sib cases
# WFU_Mitchell_problem2subset <- map(WFU_Mitchell_test, ~ group_by(.x, Dames, Sires) %>% 
#                                     filter(n() == 1))
# names(WFU_Mitchell_problem2subset) <- names(WFU_Mitchell_test)
# Mitchell_list2DF <- list("#1(10-30-18)"=WFU_Mitchell_problem2subset[["#1(10-30-18)"]][,-(15:18)], 
#                         "#2(2-26-19)"=WFU_Mitchell_problem2subset[["#2(2-26-19)"]],
#                         "#3(6-17-19)"=WFU_Mitchell_problem2subset[["#3(6-17-19)"]])  
# write.xlsx(Mitchell_list2DF, file = "Mitchell_OneSiblingSubset.xlsx", append = T)
# 
# table(WFU_Mitchell_test[[3]]$Dames, WFU_Mitchell_test[[3]]$Sires) %>% knitr::kable()
# # WFU_Mitchell_test %>% 
# #   dplyr::select(-one_of(cols))  %>% 
# #   map(table)
# # str_extract_all(names(WFU_Mitchell_test, "ID") %>% unlist()))
# # %>% map(table)
# 
# # # collect counts for each experiment 
# ## plan: link with mitchell data to know which experiment 
# 
# 
# # # check litter size 
# map(WFU_Mitchell_test, ~ count(., Litter_Number))
# map(WFU_Mitchell_test, ~ count(., Litter_Size))
# 
# 
# map(WFU_Mitchell_test, ~ group_by(., Shipment_Box, Sex) %>%
#       summarise(Count = n()))
# 
# map(WFU_Mitchell_test, ~ group_by(., Dames, Sires, Litter_Size) %>%
#       summarise(Count = n()))
# 
# map(WFU_Mitchell_test, ~ group_by(., Dames, Sires) %>%
#       summarise(Count = n()))
# 
# map(WFU_Mitchell_test, ~ group_by(., Dames, Sires) %>% 
#       summarise(n = n()) %>%
#       tidyr::spread(Sires, n)) %>%
#   na.omit() %>%
#       knitr::kable()
# # # parents and number of offspring
# UniqueParents <- map(WFU_Mitchell_test, ~ count(., Dames, Sires)) 
# knitr::kable(UniqueParents)
# 
# # # summary for family size
# options(digits = 2) 
# lapply(UniqueParents, function(df)
#   sapply(df["n"], table))
# lapply(UniqueParents, function(df)
#   sapply(df["n"], summary))
# 
# map(UniqueParents, ggplot(.) + geom_histogram(aes(n)))
# 
# ggplot(UniqueParents[[1]]) + geom_histogram(aes(n))
# ggplot(UniqueParents[[2]]) + geom_histogram(aes(n))
# ggplot(UniqueParents[[3]]) + geom_histogram(aes(n))
# ############################## map(~ggplot(mtcars, aes_string(x = .)) + geom_histogram())
# 
# map(WFU_Mitchell_test, ~ count(., Dames))
# map(WFU_Mitchell_test, ~ count(., Sires))
# 
# # # tabulate by sex and parents
# 
# # highlight <- createStyle()
# # conditionalFormatting(WFU_Mitchell_1, )
# 
# # check coatcolor 
# lapply(WFU_Mitchell_test, function(df)
#   sapply(df["coatcolor"], unique))
# WFU_Mitchell_test <- uniform.coatcolors(WFU_Mitchell_test)
# # check coatcolor consistency with
# # lapply(WFU_Mitchell_test, function(df)
# # sapply(df["coatcolor"], unique))
# 
# # # check data type 
# str(WFU_Mitchell_test)
# 
# # # create log table to be imported into sql
# 
# # 

######################
## Olivier(Cocaine) ##
######################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster")
WFU_OlivierCocaine <- u01.importxlsx("UCSD(SCRIPPS) Cocaine Master Shipping sheet.xlsx")

# following instructions from Angela Beeson at WFU :
# "first two were the beginning of my lab managers recording and were done a little different"
# "the left side were all sires and the right side were all dams"(email: 9/25)
WFU_OlivierCocaine[1] <- lapply(WFU_OlivierCocaine[1], separate, col = Parents, into = c("sires", "dames"), sep = "[[:space:]][x|X][[:space:]]") # col argument draws from tidyverse::vars_pull, which only returns only one column name
WFU_OlivierCocaine[2] <- lapply(WFU_OlivierCocaine[2], separate, col = 'Parent ID\'s', into = c("sires", "dames"), sep = "[[:space:]]*[x|X][[:space:]]*") # some entries do not have the wrapping spaces i.e. rows [14, 45, 46] 

# change irregular data types (case by case)
WFU_OlivierCocaine[[1]]$`Transponder ID` <- as.character(WFU_OlivierCocaine[[1]]$`Transponder ID`) # from numeric to character
WFU_OlivierCocaine[[3]]$`Animal #` <- as.character(WFU_OlivierCocaine[[3]]$`Animal #`)
WFU_OlivierCocaine[[4]]$`Animal #` <- as.character(WFU_OlivierCocaine[[4]]$`Animal #`)


# 12/10 add shipment 10
WFU_OlivierCocaine[10:11] <- u01.importxlsx("UCSD #10 SHIPPING SHEET.xlsx")[c(3, 1)] # scrubs last

#1/3 add shipment 11 
WFU_OlivierCocaine[12:13] <- u01.importxlsx("UCSD #11 shipping sheet.xlsx")[c(2, 1)] # scrubs last


# set column names without "Cocaine and date" header
WFU_OlivierCocaine[5:13] <- lapply(WFU_OlivierCocaine[5:13], 
                              function(x){
                                names(x) <- x[1,] %>% as.character()
                                x <- x[-1, ]
                                return(x)})

WFU_OlivierCocaine[c(5:7)] <- lapply(WFU_OlivierCocaine[c(5:7)], 
                              function(x){
                                names(x)[1:2] <- x[1,1:2] %>% as.character()
                                x <- x[-1, ]
                                return(x)})

names(WFU_OlivierCocaine[[10]])[1:3] <- WFU_OlivierCocaine[[10]][1,1:3] %>% as.character()
WFU_OlivierCocaine[[10]] <- WFU_OlivierCocaine[[10]][, -(16:17) ]
WFU_OlivierCocaine[[10]] %<>% 
  mutate(Sires = lead(Sires), 
         Dames = lead(Dames),
         `Animal ID` = lead(`Animal ID`))

# clean first table to prevent code from confusing the dup shipment date columns
WFU_OlivierCocaine[[1]] <- WFU_OlivierCocaine[[1]][, -c(which(names(WFU_OlivierCocaine[[1]])== "Cage Pair"):ncol(WFU_OlivierCocaine[[1]]))] # column 16 to end 
# clean shipment 11 (tables 12 and 13, non scrubs and scrubs respectively) to prevent code confusion about "ageindays" experiment
WFU_OlivierCocaine[[12]] <- WFU_OlivierCocaine[[12]][, -c(which(names(WFU_OlivierCocaine[[12]])== "NA"):ncol(WFU_OlivierCocaine[[12]]))] # column 16 to end 
WFU_OlivierCocaine[[13]] <- WFU_OlivierCocaine[[13]][, -c(which(names(WFU_OlivierCocaine[[13]])== "NA"):ncol(WFU_OlivierCocaine[[13]]))] # column 16 to end 


# # make variable names consistent
WFU_OlivierCocaine_test <- uniform.var.names.testingu01(WFU_OlivierCocaine)


# create the naive/scrubs dataset before removing the na rows and clean up naive dataset 
names(WFU_OlivierCocaine_test) <- append(names(WFU_OlivierCocaine)[1:9], c("#10(10-28-2019)", "#10(Scrubs)", "#11(1-13-2020)", "#11(Scrubs)"))
WFU_OlivierCocaine_naive_test <- lapply(WFU_OlivierCocaine_test, function(df) {
  rownumber <- apply(df, MARGIN = 1, function(r){any(r %in% c("Scrubs", "Scrub", "ITALY EXTRA 15 RATS"))}) %>% which()
  if(length(rownumber) != 0){
    subset(df[rownumber:nrow(df),], grepl("^\\d+.+$", rfid))
  } else NULL
}) %>% rbindlist(use.names=T, idcol = "cohort") 

# bc lapply cannot access list element names, we need to separately add the naive tables from the later cohorts 
WFU_OlivierCocaine_naive_test <- WFU_OlivierCocaine_test$`#10(Scrubs)` %>%  
  mutate(cohort = "#10(10-28-2019)") %>%
  rbind(WFU_OlivierCocaine_naive_test,.) %>% 
  rbind(., WFU_OlivierCocaine_test$`#11(Scrubs)` %>%  
          mutate(cohort = "#11(1-13-2020)")) # order to preserve natural order # add scrubs from shipment #10 and #11

# remove from olivier dataframe
WFU_OlivierCocaine_test[[10]] <- rbind(WFU_OlivierCocaine_test[[10]], WFU_OlivierCocaine_test[["#10(Scrubs)"]])
WFU_OlivierCocaine_test[["#10(Scrubs)"]] <- NULL

WFU_OlivierCocaine_test[[11]] <- rbind(WFU_OlivierCocaine_test[[11]], WFU_OlivierCocaine_test[["#11(Scrubs)"]])
WFU_OlivierCocaine_test[["#11(Scrubs)"]] <- NULL

# # remove all entries after 'scrubs' ** EXPERIMENTER SPECIFIC **
# see remove.scrubs.and.narows documentation

# 11/7 rather than removing the scrubs, we will be commenting scrub status to each animal
# WFU_OlivierCocaine_test <- remove.scrubs.and.narows(WFU_OlivierCocaine_test) # XX changed the function, test if more efficient 

# change date type
# placed in this order to prevent excessive nas being introduced by coercion
WFU_OlivierCocaine_test[[10]][which(WFU_OlivierCocaine_test[[10]]$dow == "10/31/20169"),]$dow <- "43769"
WFU_OlivierCocaine_test <- uniform.date.testingu01(WFU_OlivierCocaine_test)

# change box
WFU_OlivierCocaine_test <- uniform.boxformat(WFU_OlivierCocaine_test)

# change coat colors
WFU_OlivierCocaine_test <- uniform.coatcolors(WFU_OlivierCocaine_test)

# # add age of shipment and check consistency
WFU_OlivierCocaine_test <- lapply(WFU_OlivierCocaine_test, transform, shipmentage = as.numeric(shipmentdate - dob) %>% round)
 # cohort 3 is slightly older

counter = 0
purrr::walk(WFU_OlivierCocaine_test, function(x){
  counter <<- counter + 1
  if(any(na.exclude(x$shipmentage) > 65)){
    print(paste0("Cohort ", counter, " has animals that were too old to ship"))
  }
  summary <- summary(x$shipmentage)
  print(paste0("Cohort ", counter))
  invisible(print(summary))
})

# # add age of wean and check consistency
### notes from Angela at WFU at 11/14/2019 11:32 AM  "The 4 from cocaine should have had a wean date of 9-17-17. "
WFU_OlivierCocaine_test <- lapply(WFU_OlivierCocaine_test, function(x) {
  x$dow[which(x$rfid %in% c("933000120138309", "933000120138318", "933000120138326", "933000120138317"))] <- as.POSIXct("2017-09-17", tz = "UTC")
  return(x)})

WFU_OlivierCocaine_test <- lapply(WFU_OlivierCocaine_test, transform, weanage = as.numeric(difftime(dow, dob, units = "days")))
counter = 0
purrr::walk(WFU_OlivierCocaine_test, function(x){
  counter <<- counter + 1
  if(any(na.exclude(x$weanage) > 25)){
    print(paste0("Cohort ", counter, " has animals that weaned too old"))
  }
  summary <- summary(x$weanage)
  print(paste0("Cohort ", counter))
  invisible(print(summary))
})

WFU_OlivierCocaine_test[[10]] %>% dplyr::filter(weanage > 25) # XX note to Oksana

# # checking id vars
idcols <- c("labanimalid", "accessid", "rfid")
unique.values.length.by.col(WFU_OlivierCocaine_test, idcols) %>% invisible()

# # add comment and resolution and check consistency 
WFU_OlivierCocaine_test <- lapply(WFU_OlivierCocaine_test, cbind, comment = NA, resolution = NA)

# # checking the number of rfid digits

lapply(WFU_OlivierCocaine_test, function(x){
 x %>%
    dplyr::mutate(rfid_digits = nchar(rfid)) %>%
    dplyr::filter(rfid_digits != 15)
})

# rename all sheets (with correct cohort format)
names(WFU_OlivierCocaine_test) <- append(names(WFU_OlivierCocaine)[1:9], c("#10(10-28-2019)", "#11(1-13-2020)"))
WFU_OlivierCocaine_test_df <- rbindlist(WFU_OlivierCocaine_test, id = "cohort", fill = T)
WFU_OlivierCocaine_test_df %<>% mutate(cohort = stringr::str_match(cohort, "#(\\d+).*?")[,2],
                             cohort = ifelse(nchar(cohort) > 1, cohort, gsub('([[:digit:]]{1})$', '0\\1', cohort)),
                             rfid = replace(rfid, rfid == "93300120117313", "933000120117313"))

# append naive/scrub comment to dataframe
# # add comment of scrubs for matching rfid 
WFU_OlivierCocaine_test_df <- WFU_OlivierCocaine_test_df %>% 
  dplyr::mutate(comment = ifelse(rfid %in% WFU_OlivierCocaine_naive_test$rfid, "Scrub", WFU_OlivierCocaine_test_df$comment)) %>% 
  dplyr::filter(grepl("^(?=\\d)", rfid, perl = T))
# to check if all naive cases were identified: all 15 in each cohort (5-9) were found
# WFU_OlivierCocaine_test_df_withnaive %>% dplyr::filter(!is.na(comment)) %>% group_by(cohort) %>% count()

# check the sexes # this qc can only be applied to some animals because of the format of their lab animal id's
WFU_OlivierCocaine_test_df %>%
  dplyr::filter(grepl("^HS", labanimalid)) %>% 
  mutate(sex_fromid = substr(labanimalid, 3, 3)) %>% 
  dplyr::filter(sex_fromid != sex ) # nothing wrong 
# Outstanding issues: 
# between #6 and #7, it goes from 419 to TJ420 in labanimalnumber
# diff bw labanimalid vs labanimalnumber 
# id is the letter followed by numbers and number should all be number (currently mistranslated -- should be fixed now)

## check # siblings from prev cohort 
WFU_OlivierCocaine_test_df %>% mutate(U01 = "OlivierCocaine") %>% 
  group_by(sires, dames, cohort, U01) %>% 
  add_count() %>% 
  select(U01, sires, dames, cohort, n) %>% 
  ungroup() %>% 
  rename("siredamepair_in_cohort"="n") %>% 
  group_by(sires,dames, U01) %>% 
  add_count() %>% 
  rename("siredamepair_in_u01"="n") %>% 
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01) %>% 
  dplyr::filter(cohort == "11") %>%
  unique() %>% 
  arrange(U01, sires) %>%
  data.frame() 

sibs_matches_cocaine <- WFU_OlivierCocaine_test_df %>% mutate(U01 = "OlivierCocaine") %>% 
  group_by(sires, dames, cohort, U01) %>% 
  add_count() %>% 
  select(U01, sires, dames, cohort, n) %>% 
  ungroup() %>% 
  rename("siredamepair_in_cohort"="n") %>% 
  group_by(sires,dames, U01) %>% 
  add_count() %>% 
  rename("siredamepair_in_u01"="n") %>% 
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01) %>% 
  dplyr::filter(cohort == "11") %>%
  unique() %>% 
  arrange(U01, sires) %>%
  data.frame() %>% 
  select(sires, dames)
WFU_OlivierCocaine_test_df %>% dplyr::filter(sires %in% sibs_matches_cocaine$sires, dames %in% sibs_matches_cocaine$dames)

## check # of same sex siblings (diff litter)
WFU_OlivierCocaine_test_df %>% dplyr::filter(cohort == "11") %>% janitor::get_dupes(sires, dames, sex)

## check # of same sex littermates (same litter)
WFU_OlivierCocaine_test_df %>% dplyr::filter(cohort == "11") %>% janitor::get_dupes(sires, dames, litternumber, sex)

## check number of same sex rats in each rack and get number of rat sexes in each rack
WFU_OlivierCocaine_test_df %>% dplyr::filter(cohort == "11") %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
WFU_OlivierCocaine_test_df %>% dplyr::filter(cohort == "11") %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table()

######################
# Olivier(Oxycodone) #
######################
WFU_OlivierOxycodone <- u01.importxlsx("UCSD(SCRIPPS) Oxycodone Master Shipping Sheet.xlsx")

WFU_Olivier_sheetnames <- excel_sheets("UCSD(SCRIPPS) Oxycodone Master Shipping Sheet.xlsx")

WFU_OlivierOxycodone[6:7] <- u01.importxlsx("UCSD #10 SHIPPING SHEET.xlsx")[c(2, 1)] # scrubs last
#1/3 add shipment 17
WFU_OlivierOxycodone[8:9] <- u01.importxlsx("UCSD #11 shipping sheet.xlsx")[c(3, 1)] # scrubs last

WFU_OlivierOxycodone[c(1:5, 7:9)] <- lapply(WFU_OlivierOxycodone[c(1:5, 7:9)], function(x){
  names(x) <- x[1, ] %>% as.character
  x <- x[-1, ]
}) # remove first row of all tables prep for uniform variable name fxn

# remove excessive columns to prevent confusion about values that we'll calculate on our own
WFU_OlivierOxycodone[[1]] <- WFU_OlivierOxycodone[[1]][, -c(16:18), drop = F]
WFU_OlivierOxycodone[[8]] <- WFU_OlivierOxycodone[[8]][, -c(16:17), drop = F]
WFU_OlivierOxycodone[[9]] <- WFU_OlivierOxycodone[[9]][, -c(16:17), drop = F]

WFU_OlivierOxycodone_test <- uniform.var.names.testingu01(WFU_OlivierOxycodone)

# create the naive/scrubs dataset before removing it and clean up naive dataset 
names(WFU_OlivierOxycodone_test) <- append(names(WFU_OlivierOxycodone)[1:5], c("#6(10-28-2019)", "#6(Scrubs)","#7(1-13-2020)", "#7(Scrubs)"))
WFU_OlivierOxycodone_naive_test <- lapply(WFU_OlivierOxycodone_test, function(df) {
  rownumber <- apply(df, MARGIN = 1, function(r){any(r %in% c("Scrubs", "Scrub", "ITALY EXTRA 15 RATS"))}) %>% which()
  if(length(rownumber) != 0){
    subset(df[rownumber:nrow(df),], grepl("^\\d+.+$", rfid))
  } else NULL
}) %>% rbindlist(fill=T, idcol = "cohort") 


## bind back to the dataframe to clean it all up at the same time 
WFU_OlivierOxycodone_naive_test <- WFU_OlivierOxycodone_test$`#6(Scrubs)` %>%  
  mutate(cohort = "#6(10-28-2019)") %>%
  rbind(WFU_OlivierOxycodone_naive_test,.) %>%  # order to preserve natural order # add scrubs from shipment #16 
  rbind(., WFU_OlivierOxycodone_test$`#7(Scrubs)` %>% mutate(cohort = "#7(1-13-2020)"))
# # remove all entries after 'scrubs' ** EXPERIMENTER SPECIFIC **
# see remove.scrubs.and.narows documentation
# WFU_OlivierOxycodone_test <- remove.scrubs.and.narows(WFU_OlivierOxycodone_test) # remove rows that don't have rfid to include the trailing date case in sheet 5

# remove from olivier dataframe
WFU_OlivierOxycodone_test[[6]] <- rbind(WFU_OlivierOxycodone_test[[6]], WFU_OlivierOxycodone_test[["#6(Scrubs)"]])
WFU_OlivierOxycodone_test[["#6(Scrubs)"]] <- NULL

WFU_OlivierOxycodone_test[[7]] <- rbind(WFU_OlivierOxycodone_test[[7]], WFU_OlivierOxycodone_test[["#7(Scrubs)"]])
WFU_OlivierOxycodone_test[["#7(Scrubs)"]] <- NULL

# change date type
WFU_OlivierOxycodone_test[[2]]$shipmentdate <- as.POSIXct("2018-09-11", tz = "UTC") # must add shipment date to sheet 2 
WFU_OlivierOxycodone_test[[6]][which(WFU_OlivierOxycodone_test[[6]]$dow == "10/31/20169"),]$dow <- "43769"
WFU_OlivierOxycodone_test <- uniform.date.testingu01(WFU_OlivierOxycodone_test)

# because of inconsistent date types, remove from olivier dataframe
# WFU_OlivierOxycodone_test[[6]] <- rbind(WFU_OlivierOxycodone_test[[6]], WFU_OlivierOxycodone_test[[7]])
# WFU_OlivierOxycodone_test[[7]] <- NULL # turned into comment 1/31 uncertain about why this is here


# make shipment box uniform ** EXPERIMENTER SPECIFIC ** BOX 3  to 3
WFU_OlivierOxycodone_test <- lapply(WFU_OlivierOxycodone_test, function(x){
  x$shipmentbox <- stringr::str_extract(x$shipmentbox, "\\d+")
  return(x)
})

# check id values 
unique.values.length.by.col(WFU_OlivierOxycodone_test, idcols)

# # checking the number of rfid digits

lapply(WFU_OlivierOxycodone_test, function(x){
  x %>%
    mutate(rfid_digits = nchar(rfid)) %>%
    dplyr::filter(rfid_digits != 15)
})

# change coat colors
WFU_OlivierOxycodone_test <- uniform.coatcolors(WFU_OlivierOxycodone_test)

# # add age of shipment and check consistency
WFU_OlivierOxycodone_test <- lapply(WFU_OlivierOxycodone_test, transform, shipmentage = as.numeric(shipmentdate - dob) %>% round)

counter = 0
purrr::walk(WFU_OlivierOxycodone_test, function(x){
  counter <<- counter + 1
  if(any(na.exclude(x$shipmentage) > 65)){
    print(paste0("Cohort ", counter, " has animals that were too old to ship"))
  }
  summary <- summary(x$shipmentage)
  print(paste0("Cohort ", counter))
  invisible(print(summary))
}) #all seem okay; slightly older cohort 2

# # add age of wean and check consistency
### notes from Angela at WFU at 11/14/2019 11:32 AM "The one from Oxycodone had a wean date of 8-24-18."
WFU_OlivierOxycodone_test <- lapply(WFU_OlivierOxycodone_test, function(x) {
  x$dow[which(x$rfid == "933000320046005")] <- as.POSIXct("2018-08-24", tz = "UTC")
  return(x)})

WFU_OlivierOxycodone_test <- lapply(WFU_OlivierOxycodone_test, transform, weanage = as.numeric(difftime(dow, dob, units = "days")))
counter = 0
purrr::walk(WFU_OlivierOxycodone_test, function(x){
  counter <<- counter + 1
  if(any(na.exclude(x$weanage) > 25)){
    print(paste0("Cohort ", counter, " has animals that weaned too old"))
  }
  summary <- summary(x$weanage)
  print(paste0("Cohort ", counter))
  invisible(print(summary))
})

WFU_OlivierOxycodone_test[[6]] %>% dplyr::filter(weanage > 25) %>% select(labanimalid) %>% unlist() %>% as.character() # XX note to Oksana


# # add comment and resolution and check consistency (NO NEED BECAUSE IT ALREADY EXISTS)
WFU_OlivierOxycodone_test <- lapply(WFU_OlivierOxycodone_test, cbind, comment = NA, resolution = NA)

# rename all sheets 
names(WFU_OlivierOxycodone_test) <- append(WFU_Olivier_sheetnames, c("#6(10-28-19)", "#7(1-13-2020)") )

WFU_OlivierOxycodone_test_df <- rbindlist(WFU_OlivierOxycodone_test, id = "cohort", fill = T)
WFU_OlivierOxycodone_test_df <- WFU_OlivierOxycodone_test_df %>% 
  dplyr::mutate(comment = ifelse(rfid %in% WFU_OlivierOxycodone_naive_test$rfid, "Scrub", comment),
                cohort = stringr::str_match(cohort, "#(\\d+).*?")[,2],
                cohort = ifelse(nchar(cohort) > 1, cohort, gsub('([[:digit:]]{1})$', '0\\1', cohort))) %>% 
  dplyr::filter(grepl("^(?=\\d)", rfid, perl = T)) 

# rbindlist(WFU_OlivierOxycodone_naive_test, use.names=T)$rfid %>% length() and WFU_OlivierOxycodone_test_df %>% dplyr::filter(!is.na(comment)) %>% dim() 
# BOTH EQUAL 75 SO THE CODE WORKS

# check the sexes # this qc can only be applied to some animals because of the format of their lab animal id's
WFU_OlivierOxycodone_test_df %>%
  dplyr::filter(grepl("^HS", labanimalid)) %>% 
  mutate(sex_fromid = substr(labanimalid, 3, 3)) %>% 
  dplyr::filter(sex_fromid != sex ) # nothing wrong 

## check # siblings from prev cohort 
WFU_OlivierOxycodone_test_df %>% mutate(U01 = "OlivierOxycodone") %>% 
  group_by(sires, dames, cohort, U01) %>% 
  add_count() %>% 
  select(U01, sires, dames, cohort, n) %>% 
  ungroup() %>% 
  rename("siredamepair_in_cohort"="n") %>% 
  group_by(sires,dames, U01) %>% 
  add_count() %>% 
  rename("siredamepair_in_u01"="n") %>% 
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01) %>% 
  dplyr::filter(cohort == "07") %>%
  unique() %>% 
  arrange(U01, sires) %>%
  data.frame() 

shareparents_oxy <- WFU_OlivierOxycodone_test_df %>% mutate(U01 = "OlivierOxycodoney") %>%
  group_by(sires, dames, cohort, U01) %>%
  add_count() %>%
  select(U01, sires, dames, cohort, n) %>%
  ungroup() %>%
  rename("siredamepair_in_cohort"="n") %>%
  group_by(sires,dames, U01) %>%
  add_count() %>%
  rename("siredamepair_in_u01"="n") %>%
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01) %>%
  dplyr::filter(cohort == "07") %>%
  unique() %>%
  arrange(U01, sires) %>%
  data.frame() %>% select(sires, dames)
WFU_OlivierOxycodone_test_df %>% dplyr::filter(sires %in% shareparents_oxy$sires, dames %in% shareparents_oxy$dames) 
# %>% select(labanimalid) %>% unlist() %>% as.character()

## check # of same sex siblings (diff litter)
WFU_OlivierOxycodone_test_df %>% dplyr::filter(cohort == "07") %>% janitor::get_dupes(sires, dames, sex) # if scrubs is not important, this code works, otherwise, add is.na(comment)

## check # of same sex littermates (same litter)
WFU_OlivierOxycodone_test_df %>% dplyr::filter(cohort == "07", is.na(comment)) %>% janitor::get_dupes(sires, dames, litternumber, sex)%>% dim #12 pairs

## check number of same sex rats in each rack and get number of rat sexes in each rack
WFU_OlivierOxycodone_test_df %>% dplyr::filter(cohort == "07") %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
WFU_OlivierCocaine_test_df %>% dplyr::filter(cohort == "07") %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table




################# MISC 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Misc Data")
# 1/31 OKSANA PAUL #2 SHIPPING SHEET 
## NY (he's at the University at Buffalo)

NY_excel_orig <- u01.importxlsx("Paul #2 shipping sheet.xlsx")
NY_excel_orig_test <- uniform.var.names.testingu01(NY_excel_orig)[[1]]


## shareparents_oxy <- WFU_OlivierOxycodone_test_df %>% mutate(U01 = "OlivierOxycodoney") %>%
group_by(sires, dames, cohort, U01) %>%
  add_count() %>%
  select(U01, sires, dames, cohort, n) %>%
  ungroup() %>%
  rename("siredamepair_in_cohort"="n") %>%
  group_by(sires,dames, U01) %>%
  add_count() %>%
  rename("siredamepair_in_u01"="n") %>%
  dplyr::filter(siredamepair_in_cohort != siredamepair_in_u01) %>%
  dplyr::filter(cohort == "07") %>%
  unique() %>%
  arrange(U01, sires) %>%
  data.frame() %>% select(sires, dames)
WFU_OlivierOxycodone_test_df %>% dplyr::filter(sires %in% shareparents_oxy$sires, dames %in% shareparents_oxy$dames) 
# %>% select(labanimalid) %>% unlist() %>% as.character()

## check # of same sex siblings (diff litter)
WFU_OlivierOxycodone_test_df %>% dplyr::filter(cohort == "07") %>% janitor::get_dupes(sires, dames, sex) # if scrubs is not important, this code works, otherwise, add is.na(comment)

## check # of same sex littermates (same litter)
WFU_OlivierOxycodone_test_df %>% dplyr::filter(cohort == "07", is.na(comment)) %>% janitor::get_dupes(sires, dames, litternumber, sex)%>% dim #12 pairs

## check number of same sex rats in each rack and get number of rat sexes in each rack
WFU_OlivierOxycodone_test_df %>% dplyr::filter(cohort == "07") %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
WFU_OlivierCocaine_test_df %>% dplyr::filter(cohort == "07") %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table



