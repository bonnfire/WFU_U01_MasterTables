setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_WFU_U01_ShippingMaster")
rm(list = ls())

# Load libraries
library(tidyverse) #loads dplyr, tidyr, ggplot2, purrr, etc
require(mgsub)
library(readxl)
library(lubridate)
library(openxlsx)
library(stringr)

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
    names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
                                   c("DateofShipment", "LabAnimalID"), 
                                   c("ShipmentDate", "LabAnimalNumber"))
    names(df[[i]]) <- tolower(names(df[[i]]))
    df[[i]]
  })
}

# XX: write code that verifies and sets the correct variable types 

uniform.coatcolors <- function(df){
  lapply(seq_along(df), function(i) {
    df[[i]]$coatcolor <- mgsub::mgsub(df[[i]]$coatcolor, 
                                      c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood", "[A|a]lbino"), 
                                      c("BROWN", "BLACK", "HOOD", "ALBINO"))
    df[[i]]$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", df[[i]]$coatcolor)
    df[[i]]$coatcolor <- toupper(df[[i]]$coatcolor)
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
###### Flagel ########
######################
WFU_Flagel <- u01.importxlsx("Flagel Master Shipping Sheet.xlsx")
WFU_Flagel_test <- uniform.var.names.testingu01(WFU_Flagel)
WFU_Flagel_test <- remove.scrubs.and.narowsolivier(WFU_Flagel_test) # temporarily borrowing olivier scrubs function bc it also removes columns with na and in the first sheet, additional column that notes cost to add per rat in first table

# # checking id vars
idcols <- c("labanimalnumber", "accessid", "rfid")
unique.values.length.by.col(WFU_Flagel_test, idcols) ## XX unique.values.length.by.col needs minor tweaking for cleaner output 

# # checking date consistency 
WFU_Flagel_test2 <- uniform.date.testingu01(WFU_Flagel_test)

# # add age of shipment and check consistency
WFU_Flagel_test <- lapply(WFU_Flagel_test, transform, shipmentage = as.numeric(shipmentdate - dob))
lapply(WFU_Flagel_test, function(x) summary(x$shipmentage))
subset(WFU_Flagel_test[[2]], shipmentage < 0) ## XX include this in Dropbox, notify Apurva and Oksana

# # checking coat color consistency
WFU_Flagel_test <- uniform.coatcolors(WFU_Flagel_test)
lapply(WFU_Flagel_test, function(df)
  sapply(df["coatcolor"], unique))

# # return clean sheet names 
names(WFU_Flagel_test) <- names(WFU_Flagel)

######################
### Kalivas(ITALY) ###
######################
WFU_Kalivas_Italy <- u01.importxlsx("(Italy) Master Shipping.xlsx")
WFU_Kalivas_Italy_test <- uniform.var.names.testingu01(WFU_Kalivas_Italy)

# add comment section (esp for delayed shipping day)
WFU_Kalivas_Italy_test <- mapply(cbind, WFU_Kalivas_Italy_test, comment = NA, resolution = NA) 
WFU_Kalivas_Italy_test[[1]] <- WFU_Kalivas_Italy_test[[1]][-c(41:nrow(WFU_Kalivas_Italy_test[[1]])),]
WFU_Kalivas_Italy_test[[1]] <- WFU_Kalivas_Italy_test[[1]] %>%
  mutate(comment = "Original shipment date 2019-01-29 (held 1 week due to heat)")
WFU_Kalivas_Italy_test[[3]] <- WFU_Kalivas_Italy_test[[3]][-c(41:nrow(WFU_Kalivas_Italy_test[[3]])), -c(16:19)] # XX remove three empty columns, age@ship, and bottom rows 
WFU_Kalivas_Italy_test[[3]] <- WFU_Kalivas_Italy_test[[3]] %>%
  mutate(comment = "Original shipment 2019-08-05 (held 2 weeks due to heat)") 

# add resolution section XX can I assume that these are ignorable??? 
 
# experiment specific: second cohort requires remove additional 15 rats sent to italy note because they are pilot rats 
WFU_Kalivas_Italy_test <- remove.scrubs.and.narows(WFU_Kalivas_Italy_test) # get row number for which italy is shown and then remove all rows after that 
removeallnarow <- function(df){
  ind <- apply(df, 1, function(x) all(is.na(x)))
  df <- df[ !ind, ]
} # remove rows with all na
WFU_Kalivas_Italy_test[[2]] <- removeallnarow(WFU_Kalivas_Italy_test[[2]])

# # checking id vars
idcols <- c("accessid", "rfid")
unique.values.length.by.col(WFU_Kalivas_Italy_test, idcols) 
## doesn't have labanimalnumber
WFU_Kalivas_Italy_test

# # checking date consistency 
WFU_Kalivas_Italy_test <- uniform.date.testingu01(WFU_Kalivas_Italy_test)

# # add age of shipment and check consistency
WFU_Kalivas_Italy_test <- lapply(WFU_Kalivas_Italy_test, transform, shipmentage = as.numeric(shipmentdate - dob))
lapply(WFU_Kalivas_Italy_test, function(x) summary(x$shipmentage))

# # checking coat color consistency
lapply(WFU_Kalivas_Italy_test, function(df)
  sapply(df["coatcolor"], unique)) ## XX Address [[2]] data 
WFU_Kalivas_Italy_test <- uniform.coatcolors(WFU_Kalivas_Italy_test)
names(WFU_Kalivas_Italy_test) <- names(WFU_Kalivas_Italy)

WFU_Kalivas_Italy_test_df <- rbindlist(WFU_Kalivas_Italy_test, id = "cohort", fill = T)

######################
## Kalivas(Heroine) ##
######################
WFU_Kalivas <- u01.importxlsx("MUSC (Kalivas) Master Shipping.xlsx")
WFU_Kalivas_updated3 <- u01.importxlsx("MUSC (Kalivas) #3 Shipping sheet.xlsx")
# As per email chain from August and September, the shipment from the third cohort of animals needs to be merged into Kalivas master sheets.
WFU_Kalivas[[3]] <- WFU_Kalivas_updated3[[1]]

WFU_Kalivas_test <- uniform.var.names.testingu01(WFU_Kalivas)

# # checking id vars
idcols <- c("labanimalnumber", "accessid", "rfid")
unique.values.length.by.col(WFU_Kalivas_test, idcols) ## XX generalize the test so it is able to skip the test if the column doesn't exist

# # checking date consistency 
WFU_Kalivas_test <- uniform.date.testingu01(WFU_Kalivas_test)

# # add age of shipment and check consistency
WFU_Kalivas_test <- lapply(WFU_Kalivas_test, transform, shipmentage = as.numeric(shipmentdate - dob))
lapply(WFU_Kalivas_test, function(x) summary(x$shipmentage))

# # checking coat color consistency
unique.values.by.col(WFU_Kalivas_test, "coatcolor")
WFU_Kalivas_test <- uniform.coatcolors(WFU_Kalivas_test)

names(WFU_Kalivas_test) <- names(WFU_Kalivas)

WFU_Kalivas_test_df <- rbindlist(WFU_Kalivas_test, id = "cohort", fill = T)


## XX how do we want to use this data? 
# # parent pairs
map(WFU_Kalivas_test, ~ count(., dames, sires, sex) %>%
      subset(n!=1 && is.na(dames) == F))

# # number of parent pairs

######################
######## JHOU ########
######################
WFU_Jhou <- u01.importxlsx("Jhou Master Shipping Sheet.xlsx")

# # make within-df variable names consistent and fix sires/dames column name issue
WFU_Jhou_test <- uniform.var.names.testingu01(WFU_Jhou)

# experiment/table specific: remove empty columns 16-18
WFU_Jhou_test[[1]] <- WFU_Jhou_test[[1]][, -c(16:18), drop = F] # drop = F retains data type
WFU_Jhou_test <- lapply(WFU_Jhou_test, janitor::remove_empty, which = "rows")

###  add Shipment Date into Jhou data *** EXPERIMENTER SPECIFIC *** 
WFU_Jhou_test[[2]] <- WFU_Jhou_test[[2]] %>% mutate("shipmentdate" = as.POSIXct("2018-07-05", format="%Y-%m-%d", tz = "UTC"))
# WFU_Jhou_test[[2]] <- WFU_Jhou_test[[2]] %>% mutate("shipmentdate" = ymd("2018-07-05"))

# # checking id vars
idcols <- c("accessid", "rfid")
unique.values.length.by.col(WFU_Jhou_test, idcols) 

# # checking date consistency 
WFU_Jhou_test <- uniform.date.testingu01(WFU_Jhou_test)

# # add age of shipment and check consistency
WFU_Jhou_test <- lapply(WFU_Jhou_test, transform, shipmentage = as.numeric(shipmentdate - dob))
lapply(WFU_Jhou_test, function(x) summary(x$shipmentage))

# # checking coat color consistency
unique.values.by.col(WFU_Jhou_test, "coatcolor")
WFU_Jhou_test <- uniform.coatcolors(WFU_Jhou_test)
names(WFU_Jhou_test) <- names(WFU_Jhou)

WFU_Jhou_test_df <- bind_rows(WFU_Jhou_test, .id = "cohort") # create this format for rfidandcohort data that are joined with raw data


## XX INCOMPLETE [PICK UP]
# # validate date 
QC(WFU_Jhou_test)

# # automate the counts with purrr:map() 
# # itirate through all designated columns in a list of df's
# # validate pairings 
map(WFU_Jhou_test, ~ count(., Sex))
map(WFU_Jhou_test, ~ count(., Dames))
map(WFU_Jhou_test, ~ count(., Sires))

# # validate pairings (consider unique gender?)
conds <- map(WFU_Jhou_test, ~ count(., Dames, Sires, Sex) %>%
               subset(n!=1)) 

# # subset the original data for these cases with gender
# WFU_Jhou_problemsubset<- lapply(seq_along(WFU_Jhou_test), 
#        function(i) subset(WFU_Jhou_test[[i]], Dames %in% conds[[i]]$Dames & Sires %in% conds[[i]]$Sires))
WFU_Jhou_problemsubset <- map(WFU_Jhou_test, ~ group_by(.x, Dames, Sires, Sex) %>% 
      filter(n() > 1))
names(WFU_Jhou_problemsubset) <- names(WFU_Jhou_test)
Jhou_listDF <- list("#1(6-5-18)"=WFU_Jhou_problemsubset[["#1(6-5-18)"]][,-(16:18)], 
                    "#2(7-5-18)"=WFU_Jhou_problemsubset[["#2(7-5-18)"]],
                    "#3(7-24-18)"=WFU_Jhou_problemsubset[["#3(7-24-18)"]],
                    "#4(10-9-18)"=WFU_Jhou_problemsubset[["#4(10-9-18)"]],
                    "#5(11-27-18)"=WFU_Jhou_problemsubset[["#5(11-27-18)"]],
                    "#6(1-15-19)"=WFU_Jhou_problemsubset[["#6(1-15-19)"]],
                    "#7(2-26-19)"=WFU_Jhou_problemsubset[["#7(2-26-19)"]],
                    "#8(4-9-19)"=WFU_Jhou_problemsubset[["#8(4-9-19)"]],
                    "#9(6-4-19)"=WFU_Jhou_problemsubset[["#9(6-4-19)"]],
                    "#10(7-16-19)"=WFU_Jhou_problemsubset[["#10(7-16-19)"]],
                    "#11(8-27-19)"=WFU_Jhou_problemsubset[["#11(8-27-19)"]])  
     
write.xlsx(Jhou_listDF, file = "Jhou_SiblingSubset.xlsx", append = T)

# graphics for email
Jhou_SameSexSiblings <- map(WFU_Jhou_test, ~ count(., Dames, Sires, Sex) %>%
               subset(n!=1 & is.na(Dames) == F) %>% 
               nrow)
All_Jhou <- bind_rows(WFU_Jhou_test) %>% count(Dames, Sires) %>% subset(is.na(Dames) == F)
ggplot(All_Jhou, aes(n)) + geom_bar() + ggtitle("All Jhou Shipments") +
  xlab("Number of siblings in one shipment") + ylab("Count") + 
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.5),size=4)

Jhou_UniqueParentPairs <- map(WFU_Jhou_test, ~ count(., Dames, Sires)) 
ggplot(UniqueParents[[1]]) + geom_histogram(aes(n))

Jhou_UniqueParentPairs %>% 
  names() %>%
  map(~ ggplot(Jhou_UniqueParentPairs) + geom_histogram(n))

  df %>% gather(var,value) %>% 
    ggplot(aes(x = value))+
    geom_histogram()+
    facet_wrap(~var)
  
# wo sex variable
conds2 <- map(WFU_Jhou_test, ~ count(., Dames, Sires))
map(conds2, ~ subset(., n > 2))

# # validate litter count 

# # validate dames and sires id
map(WFU_Jhou_test, ~ subset(., Dames==Sires))

######################
######## MITCHELL ####
######################
WFU_Mitchell <- u01.importxlsx("Mitchell Master Shipping Sheet.xlsx")
WFU_Mitchell_test <- uniform.var.names.testingu01(WFU_Mitchell)

# fix first table to account for: no ship date data(DONE), formatting of first row/sires and dames (DONE), highlight (add comment that the highlighted should be excluded)
pregnant <- WFU_Mitchell_test[[1]] %>% 
  filter(is.na(`15`)==F) %>% 
  select(rfid) %>% 
  unlist %>% 
  as.vector # extract the pregnant cases to include as comment

WFU_Mitchell_test[[1]] <- WFU_Mitchell_test[[1]][ , colSums(is.na(WFU_Mitchell_test[[1]])) == 0] # remove columns with any na's, checked for no na rows 

WFU_Mitchell_test[[1]] <- WFU_Mitchell_test[[1]] %>% 
  mutate(shipmentdate = as.POSIXct("2018-10-30", format="%Y-%m-%d"))

# add comment and resolution columns **EXPERIMENTER SPECIFIC** 
WFU_Mitchell_test <- lapply(WFU_Mitchell_test, cbind, comment = NA, resolution = NA) ## XX PICK UP FROM HERE
WFU_Mitchell_test[[1]]$comment <- ifelse(WFU_Mitchell_test[[1]]$rfid %in% pregnant, "Pregnant female", NA)
WFU_Mitchell_test[[1]]$resolution <- ifelse(WFU_Mitchell_test[[1]]$rfid %in% pregnant, "REMOVE_FROM_EXCLUSION_AND_REPLACE", NA)

# # checking id vars
idcols <- c("labanimalnumber", "accessid", "rfid")
unique.values.length.by.col(WFU_Mitchell_test, idcols)

# # checking date consistency 
WFU_Mitchell_test <- uniform.date.testingu01(WFU_Mitchell_test)

# # add age of shipment and check consistency
WFU_Mitchell_test <- lapply(WFU_Mitchell_test, transform, shipmentage = as.numeric(shipmentdate - dob) %>% round)
lapply(WFU_Mitchell_test, function(x) summary(x$shipmentage)) # cohort 3 is slightly older

# # checking coat color consistency
# before unique.values.by.col(WFU_Mitchell_test, "coatcolor")
WFU_Mitchell_test <- uniform.coatcolors(WFU_Mitchell_test)
# after unique.values.by.col(WFU_Mitchell_test, "coatcolor")

# return original names of all sheets 
names(WFU_Mitchell_test) <- names(WFU_Mitchell) 

WFU_Mitchell_test_df <- rbindlist(WFU_Mitchell_test, id = "cohort", fill = T)

## XX INCOMPLETE  
# # validate dates 
QC(WFU_Mitchell_test)

# # check for outliers

# # collect counts for each column
cols <- c("Dames", "Sires", grep("ID|Lab_Animal", names(WFU_Mitchell_test[[1]]), value = T) %>% unlist())
table(WFU_Mitchell_test[[1]][c("Shipment_Box", "Ear_Punch")])

map(WFU_Mitchell_test, ~ count(., Sex)) # replace sex with any of the variables, but figure out a way to output all interested variables
map(WFU_Mitchell_test, ~ count(., Dames, Sires, Litter_Number))

map(WFU_Mitchell_test, ~ count(., Litter_Number, Litter_Size))

map(WFU_Mitchell_test, ~ subset(., Dames==Sires))

map_interestedvars <- function(df, vector){
  for(i in 1:length(vector))
  map(WFU_Mitchell_test, ~ count(., Sex))
}

map(WFU_Mitchell_test, ~ count(., Dames, Sires, Sex) %>% 
                     subset(n != 1))

# graphics for email
Mitchell_SameSexSiblings <- map(WFU_Mitchell_test, ~ count(., Dames, Sires, Sex) %>%
                              subset(n!=1) %>% 
                              nrow)
All_Mitchell <- bind_rows(WFU_Mitchell_test) %>% count(Dames, Sires) %>% subset(is.na(Dames) == F)
ggplot(All_Mitchell, aes(n)) + geom_bar() + ggtitle("All Mitchell Shipments") +
  xlab("Number of siblings in one shipment") + ylab("Count") + 
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.5),size=4)

# # subset original dataset to extract cases
# WFU_Mitchell_problemsubset<- lapply(seq_along(WFU_Mitchell_test), 
#                                 function(i) subset(WFU_Mitchell_test[[i]], Dames %in% conds_mitch[[i]]$Dames & Sires %in% conds_mitch[[i]]$Sires))
WFU_Mitchell_problemsubset <- map(WFU_Mitchell_test, ~ group_by(.x, Dames, Sires, Sex) %>% 
      filter(n() > 1))
names(WFU_Mitchell_problemsubset) <- names(WFU_Mitchell_test)
Mitchell_listDF <- list("#1(10-30-18)"=WFU_Mitchell_problemsubset[["#1(10-30-18)"]][,-(15:18)], 
                    "#2(2-26-19)"=WFU_Mitchell_problemsubset[["#2(2-26-19)"]],
                    "#3(6-17-19)"=WFU_Mitchell_problemsubset[["#3(6-17-19)"]])  

write.xlsx(Mitchell_listDF, file = "Mitchell_SiblingSubset.xlsx", append = T)

# # subset original dataset to extract one sib cases
WFU_Mitchell_problem2subset <- map(WFU_Mitchell_test, ~ group_by(.x, Dames, Sires) %>% 
                                    filter(n() == 1))
names(WFU_Mitchell_problem2subset) <- names(WFU_Mitchell_test)
Mitchell_list2DF <- list("#1(10-30-18)"=WFU_Mitchell_problem2subset[["#1(10-30-18)"]][,-(15:18)], 
                        "#2(2-26-19)"=WFU_Mitchell_problem2subset[["#2(2-26-19)"]],
                        "#3(6-17-19)"=WFU_Mitchell_problem2subset[["#3(6-17-19)"]])  
write.xlsx(Mitchell_list2DF, file = "Mitchell_OneSiblingSubset.xlsx", append = T)

table(WFU_Mitchell_test[[3]]$Dames, WFU_Mitchell_test[[3]]$Sires) %>% knitr::kable()
# WFU_Mitchell_test %>% 
#   dplyr::select(-one_of(cols))  %>% 
#   map(table)
# str_extract_all(names(WFU_Mitchell_test, "ID") %>% unlist()))
# %>% map(table)

# # collect counts for each experiment 
## plan: link with mitchell data to know which experiment 


# # check litter size 
map(WFU_Mitchell_test, ~ count(., Litter_Number))
map(WFU_Mitchell_test, ~ count(., Litter_Size))


map(WFU_Mitchell_test, ~ group_by(., Shipment_Box, Sex) %>%
      summarise(Count = n()))

map(WFU_Mitchell_test, ~ group_by(., Dames, Sires, Litter_Size) %>%
      summarise(Count = n()))

map(WFU_Mitchell_test, ~ group_by(., Dames, Sires) %>%
      summarise(Count = n()))

map(WFU_Mitchell_test, ~ group_by(., Dames, Sires) %>% 
      summarise(n = n()) %>%
      tidyr::spread(Sires, n)) %>%
  na.omit() %>%
      knitr::kable()
# # parents and number of offspring
UniqueParents <- map(WFU_Mitchell_test, ~ count(., Dames, Sires)) 
knitr::kable(UniqueParents)

# # summary for family size
options(digits = 2) 
lapply(UniqueParents, function(df)
  sapply(df["n"], table))
lapply(UniqueParents, function(df)
  sapply(df["n"], summary))

map(UniqueParents, ggplot(.) + geom_histogram(aes(n)))

ggplot(UniqueParents[[1]]) + geom_histogram(aes(n))
ggplot(UniqueParents[[2]]) + geom_histogram(aes(n))
ggplot(UniqueParents[[3]]) + geom_histogram(aes(n))
############################## map(~ggplot(mtcars, aes_string(x = .)) + geom_histogram())

map(WFU_Mitchell_test, ~ count(., Dames))
map(WFU_Mitchell_test, ~ count(., Sires))

# # tabulate by sex and parents

# highlight <- createStyle()
# conditionalFormatting(WFU_Mitchell_1, )

# check coatcolor 
lapply(WFU_Mitchell_test, function(df)
  sapply(df["coatcolor"], unique))
uniform.coatcolors <- function(df){
  lapply(seq_along(df), function(i) {
    df[[i]]$coatcolor <- mgsub::mgsub(df[[i]]$coatcolor, 
                                   c("BRN", "BLK", "HHOD"), 
                                   c("BROWN", "BLACK", "HOOD"))
    df[[i]]
  })
} 
WFU_Mitchell_test <- uniform.coatcolors(WFU_Mitchell_test)
# check coatcolor consistency with
# lapply(WFU_Mitchell_test, function(df)
# sapply(df["coatcolor"], unique))

# # check data type 
str(WFU_Mitchell_test)

# # create log table to be imported into sql

# 

######################
## Olivier(Cocaine) ##
######################
WFU_Olivier_co <- u01.importxlsx("UCSD(SCRIPPS) Cocaine Master Shipping sheet.xlsx")

# following instructions from Angela Beeson at WFU :
# "first two were the beginning of my lab managers recording and were done a little different"
# "the left side were all sires and the right side were all dams"(email: 9/25)
WFU_Olivier_co[1] <- lapply(WFU_Olivier_co[1], separate, col = Parents, into = c("sires", "dames"), sep = "[[:space:]][x|X][[:space:]]") # col argument draws from tidyverse::vars_pull, which only returns only one column name
WFU_Olivier_co[2] <- lapply(WFU_Olivier_co[2], separate, col = 'Parent ID\'s', into = c("sires", "dames"), sep = "[[:space:]]*[x|X][[:space:]]*") # some entries do not have the wrapping spaces i.e. rows [14, 45, 46] 

# change irregular data types (case by case)
WFU_Olivier_co[[1]]$`Transponder ID` <- as.character(WFU_Olivier_co[[1]]$`Transponder ID`) # from numeric to character
WFU_Olivier_co[[3]]$`Animal #` <- as.character(WFU_Olivier_co[[3]]$`Animal #`)
WFU_Olivier_co[[4]]$`Animal #` <- as.character(WFU_Olivier_co[[4]]$`Animal #`)


# set column names without "Cocaine and date" header
WFU_Olivier_co[5:9] <- lapply(WFU_Olivier_co[5:9], 
                              function(x){
                                names(x) <- x[1,] %>% as.character()
                                x <- x[-1, ]
                                return(x)})

# clean first table to prevent code from confusing the dup shipment date columns
WFU_Olivier_co[[1]] <- WFU_Olivier_co[[1]][, -c(which(names(WFU_Olivier_co[[1]])== "Cage Pair"):ncol(WFU_Olivier_co[[1]]))] # column 16 to end 

# # make variable names consistent
WFU_Olivier_co_test <- uniform.var.names.testingu01(WFU_Olivier_co)

# # remove all entries after 'scrubs' ** EXPERIMENTER SPECIFIC **
# see remove.scrubs.and.narows documentation

WFU_Olivier_co_test <- remove.scrubs.and.narows(WFU_Olivier_co_test) # XX changed the function, test if more efficient 

# change date type
# placed in this order to prevent excessive nas being introduced by coercion
WFU_Olivier_co_test2 <- uniform.date.testingu01(WFU_Olivier_co_test)

# change coat colors
WFU_Olivier_co_test <- uniform.coatcolors(WFU_Olivier_co_test)

lapply(WFU_Olivier_co_test, str)

# # checking date consistency 
WFU_Olivier_co_test <- uniform.date.testingu01(WFU_Olivier_co_test)

# # add age of shipment and check consistency
WFU_Olivier_co_test <- lapply(WFU_Olivier_co_test, transform, shipmentage = as.numeric(shipmentdate - dob) %>% round)
lapply(WFU_Olivier_co_test, function(x) summary(x$shipmentage)) # cohort 3 is slightly older

# rename all sheets 
names(WFU_Olivier_co_test) <- names(WFU_Olivier_co)

WFU_Olivier_co_test_df <- rbindlist(WFU_Olivier_co_test, id = "cohort", fill = T)

# Outstanding issues: 
# between #6 and #7, it goes from 419 to TJ420 in labanimalnumber
# diff bw labanimalid vs labanimalnumber 
# id is the letter followed by numbers and number should all be number (currently mistranslated)

######################
# Olivier(Oxycodone) #
######################
WFU_Olivier_ox <- u01.importxlsx("UCSD(SCRIPPS) Oxycodone Master Shipping Sheet.xlsx")
WFU_Olivier_sheetnames <- excel_sheets("UCSD(SCRIPPS) Oxycodone Master Shipping Sheet.xlsx")
WFU_Olivier_ox_test <- lapply(WFU_Olivier_ox, function(x){
  names(x) <- x[1, ] %>% as.character
  x <- x[-1, ]
}) # remove first row of all tables prep for uniform variable name fxn

WFU_Olivier_ox_test[[1]] <- WFU_Olivier_ox_test[[1]][, -c(16:18), drop = F]
WFU_Olivier_ox_test <- uniform.var.names.testingu01(WFU_Olivier_ox_test)

# # remove all entries after 'scrubs' ** EXPERIMENTER SPECIFIC **
# see remove.scrubs.and.narows documentation
WFU_Olivier_ox_test <- remove.scrubs.and.narows(WFU_Olivier_ox_test) # remove rows that don't have rfid to include the trailing date case in sheet 5

# change date type
WFU_Olivier_ox_test[[2]]$shipmentdate <- as.POSIXct("2018-09-11", tz = "UTC") # must add shipment date to sheet 2 
WFU_Olivier_ox_test <- uniform.date.testingu01(WFU_Olivier_ox_test)

# make shipment box uniform ** EXPERIMENTER SPECIFIC **
WFU_Olivier_ox_test <- lapply(WFU_Olivier_ox_test, function(x){
  x$shipmentbox <- stringr::str_extract(x$shipmentbox, "\\d+")
  return(x)
})

# check id values 
unique.values.length.by.col(WFU_Olivier_ox_test, idcols)

# change coat colors
WFU_Olivier_ox_test <- uniform.coatcolors(WFU_Olivier_ox_test)

# validate age
WFU_Olivier_ox_test <- lapply(WFU_Olivier_ox_test, transform, shipmentage = as.numeric(shipmentdate - dob))
lapply(WFU_Olivier_ox_test, function(x) summary(x$shipmentage)) #all seem okay; slightly older cohort 2  

# rename all sheets 
names(WFU_Olivier_ox_test) <- WFU_Olivier_sheetnames

WFU_Olivier_ox_test_df <- rbindlist(WFU_Olivier_ox_test, id = "cohort", fill = T)
