setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_WFU_U01_ShippingMaster")

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
                                   c(" |\\.", "#", "Transponder ", "Date of Wean|Wean Date","Animal", "Shipping|Ship"),
                                   c("", "Number", "RF", "DOW","LabAnimal", "Shipment"))
    names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
                                   c("DateofShipment"), 
                                   c("ShipmentDate"))
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
    df[[i]]
  })
} # function should be used for other cases


## all user-defined functions for quality check and validation ## 
QC <- function(df){
  lapply(seq_along(df), function(i){
    dur.wean.summary = list()
    dur.ship.summary = list()
    dur.wean.summary[[i]] <- interval(df[[i]]$'D.O.B', df[[i]]$'D.O.W') %>% 
      as.duration() %>% summary() 
    dur.ship.summary[[i]] <- interval(df[[i]]$'D.O.B', df[[i]]$'Shipment_Date') %>% 
      as.duration() %>% summary() 
    list <- list("WeaningAge" = dur.wean.summary[[i]], "ShipmentAge" = dur.ship.summary[[i]])
    list
  })
}

## note: which experiment/lab required a specified function



######################
####### ALL DF #######
######################


######################
######## JHOU ########
######################
# Import XLSX
Jhou_path = "Jhou Master Shipping Sheet.xlsx"
Jhou_sheetnames <- excel_sheets(Jhou_path)
WFU_Jhou <- lapply(excel_sheets(Jhou_path), read_excel, path = Jhou_path)
names(WFU_Jhou) <- Jhou_sheetnames

# # make within-df variable names consistent and fix sires/dames column name issue
WFU_Jhou_test <- uniform.var.names(WFU_Jhou)
'%ni%' <- Negate('%in%')

reformat.siredames.cols <- function(df){
  lapply(seq_along(df), function(i){
    if("Sires" %ni% names(df[[i]])){
      names(df[[i]])[1:2] <- df[[i]][1,1:2] %>% as.character()
      df[[i]] <- df[[i]][-1, ]
      df[[i]]
    } else{
     df[[i]] 
    }
    })
}
WFU_Jhou_test <- reformat.siredames.cols(WFU_Jhou_test)
names(WFU_Jhou_test) <- names(WFU_Jhou) # df names = shipping dates 

# # validate date 
### add Shipment Date into Jhou data
WFU_Jhou_test[[2]] <- WFU_Jhou_test[[2]] %>% mutate("Shipment_Date" = ymd("2018-08-05"))
QC(WFU_Jhou_test)
## XX INCOMPLETE [PICK UP]

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
# Import XLSX
Mitchell_path = "Mitchell Master Shipping Sheet.xlsx"
Mitchell_sheetnames <- excel_sheets(Mitchell_path)
WFU_Mitchell <- lapply(excel_sheets(Mitchell_path), read_excel, path = Mitchell_path)
names(WFU_Mitchell) <- Mitchell_sheetnames
# # make variable names consistent
uniform.var.names<- function(df){
  lapply(seq_along(df), function(i) {
    names(df[[i]]) <- mgsub::mgsub(names(df[[i]]), 
                                   c(" |\\.", "#", "Transponder ", "Date of Wean|Wean Date","Animal", "Shipping|Ship"), 
                                   c("", "Number", "RF", "DOW","LabAnimal", "Shipment"))
    names(df[[i]]) <- tolower(names(df[[i]]))
    df[[i]]
    })
} 

WFU_Mitchell_test <- uniform.var.names(WFU_Mitchell)
# fix first table to account for: no ship date data(DONE), formatting of first row/sires and dames (DONE), highlight (XX waiting for feedback)
WFU_Mitchell_test[[1]] <- WFU_Mitchell_test[[1]] %>% 
  mutate(shipmentdate = as.POSIXct("2018-10-30", format="%Y-%m-%d"))
names(WFU_Mitchell_test[[1]])[1:2] <- WFU_Mitchell_test[[1]][1,1:2] %>% as.character() %>% tolower()
WFU_Mitchell_test[[1]] <- WFU_Mitchell_test[[1]][-1, ]
# rename all sheets 
names(WFU_Mitchell_test) <- Mitchell_sheetnames

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
# Import XLSX
Olivier_co_path = "UCSD(SCRIPPS) Cocaine Master Shipping sheet.xlsx"
Olivier_co_sheetnames <- excel_sheets(Olivier_co_path)
WFU_Olivier_co <- lapply(excel_sheets(Olivier_co_path), read_excel, path = Olivier_co_path)
names(WFU_Olivier_co) <- Olivier_co_sheetnames

# following instructions from Angela Beeson at WFU :
# "first two were the beginning of my lab managers recording and were done a little different"
# "the left side were all sires and the right side were all dams"(email: 9/25)
WFU_Olivier_co[1] <- lapply(WFU_Olivier_co[1], separate, col = Parents, into = c("sires", "dames"), sep = "[[:space:]][x|X][[:space:]]") # col argument draws from tidyverse::vars_pull, which only returns only one column name
WFU_Olivier_co[2] <- lapply(WFU_Olivier_co[2], separate, col = 'Parent ID\'s', into = c("sires", "dames"), sep = "[[:space:]]*[x|X][[:space:]]*") # some entries do not have the wrapping spaces i.e. rows [14, 45, 46] 

# change irregular data types (case by case)
WFU_Olivier_co[[1]]$`Transponder ID` <- as.character(WFU_Olivier_co[[1]]$`Transponder ID`) # from numeric to character
# set column names without "Cocaine and date" header
WFU_Olivier_co[5:9] <- lapply(WFU_Olivier_co[5:9], 
                              function(x){
                                names(x) <- x[1,] %>% as.character()
                                x <- x[-1, ]
                                return(x)})
# # make variable names consistent

WFU_Olivier_co_test <- uniform.var.names.testingu01(WFU_Olivier_co)

# # remove all entries after 'scrubs'
remove.scrubs.and.narowsolivier <- function(df){
  lapply(seq_along(df), function(i) {
    rownumber <- apply(df[[i]], MARGIN = 1, function(r){any(r %in% c("Scrubs", "Scrub"))}) %>% which() 
    if(is.integer(rownumber) && length(rownumber) != 0){
      df[[i]] <- df[[i]][-(rownumber:nrow(df[[i]])),]
    }
    df[[i]] <- df[[i]][rowSums(is.na(df[[i]])) != ncol(df[[i]]), ] #remove rows that have all na
    df[[i]] <- df[[i]][ , colSums(is.na(df[[i]])) == 0] # remove columns that have any na
    return(df[[i]])
    })
  }
WFU_Olivier_co_test <- remove.scrubs.and.narowsolivier(WFU_Olivier_co_test)

# fix first table to account for: no ship date data(DONE)
# WFU_Olivier_co_test[[1]] <- WFU_Olivier_co_test[[1]] %>% 
#   mutate(shipmentdate = as.POSIXct("2018-10-30", format="%Y-%m-%d"))

# why do dames and sires switch in [[[7]]]
# diff bw labanimalid vs labanimalnumber 
# id is the letter followed by numbers and number should all be number (currently mistranslated)

WFU_Olivier_co_test <- uniform.coatcolors(WFU_Olivier_co_test)

# change date type
uniform.date.testingu01<- function(df){
  lapply(seq_along(df), function(i){
    df[[i]] <- df[[i]] %>% mutate_at(.vars = vars(dob, dow, shipmentdate),
                         .funs = convertToDate)
    df[[i]]
  })
} # currently working but adding numbers to already okay columns

uniform.date.testingu01<- function(df){
  lapply(seq_along(df), function(i){
    safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes))
    datecols <- c("dob", "dow", "shipmentdate")
    df[[i]][, datecols] <- sapply(df[[i]][, datecols], 
                                 function(col) safe.ifelse(is.POSIXct(df[[i]]$col) == F, 
                                                      as.POSIXct(as.numeric(df[[i]]$col) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d"),
                                                      df[[i]]$col))
    df[[i]]
    })
} # not working : Error: Can't subset with `[` using an object of class POSIXct.

WFU_Kalivas_Italy_test2 <- uniform.date.testingu01(WFU_Kalivas_Italy_test)

safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes)) # because ifelse makes factors lose levels and Dates lose their class and only their mode ("numeric") is restored
# function above is from Hadley

uniform.date.testingu01<- function(df){
  lapply(seq_along(df), function(i){
    df[[i]]$dob <- safe.ifelse(is.POSIXct(df[[i]]$dob) == F, as.POSIXct(as.numeric(df[[i]]$dob) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d"), df[[i]]$dob)
    df[[i]]$dow <- safe.ifelse(is.POSIXct(df[[i]]$dow) == F, as.POSIXct(as.numeric(df[[i]]$dow) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d"), df[[i]]$dow)
    df[[i]]$shipmentdate <- safe.ifelse(is.POSIXct(df[[i]]$shipmentdate) == F, as.POSIXct(as.numeric(df[[i]]$shipmentdate) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d"), df[[i]]$shipmentdate)
    return(df[[i]])
  })
} # not working : Error: Can't subset with `[` using an object of class POSIXct.

uniform.date.testingu01<- function(df){
  lapply(seq_along(df), function(i){
    df[[i]][, col] <- mapply(function(col){
                                   if(is.POSIXct(df[[i]]$col) == F){
                                     as.POSIXct(x = as.numeric(df[[i]]$col) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")
                                   }
                                 }, df[[i]]$dob, df[[i]]$dow, df[[i]]$shipment)
    return(df[[i]])
  })
} # not working : Error: Can't subset with `[` using an object of class POSIXct.


# as.POSIXct(as.numeric(WFU_Kalivas_Italy_test[[2]]$dob) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")

WFU_Kalivas_Italy_test
WFU_Kalivas_Italy_test2 <- uniform.date.testingu01(WFU_Kalivas_Italy_test)


WFU_Olivier_co_test_2 <- uniform.date.testingu01(WFU_Olivier_co_test)

# rename all sheets 
names(WFU_Olivier_co_test_2) <- Olivier_co_sheetnames

######################
## Kalivas(Heroine) ##
######################

# Import XLSX
WFU_Kalivas <- u01.importxlsx("MUSC (Kalivas) Master Shipping.xlsx")
WFU_Kalivas_updated3 <- u01.importxlsx("MUSC (Kalivas) #3 Shipping sheet.xlsx")

# As per email chain from August and September, the shipment from the third cohort of animals needs to be merged into Kalivas master sheets.
WFU_Kalivas[[3]] <- WFU_Kalivas_updated3[[1]]

# WFU_Kalivas_test <- uniform.var.names(WFU_Kalivas)
WFU_Kalivas_test <- uniform.var.names.testingu01(WFU_Kalivas)

# variable quality check 
# # parent pairs
map(WFU_Kalivas_test, ~ count(., dames, sires, sex) %>%
                              subset(n!=1 && is.na(dames) == F))

# # number of parent pairs
unique.values.by.col <- function(df, var){
  lapply(df, function(df){
  sapply(df[var], unique)
  })
}
unique.values.by.col(WFU_Kalivas_test, "coatcolor")

unique.values.length.by.col <- function(df, var){
  lapply(df, function(df){
    sapply(df[var], function(x){
      uniquevarcount <- unique(x) %>% length()
      print(paste("number of unique values (observed count) is", uniquevarcount, "and number of rows (expected count) is", nrow(df)))})
  })
}
unique.values.length.by.col(WFU_Kalivas_test, "accessid") #working 

WFU_Kalivas_test <- uniform.coatcolors(WFU_Kalivas_test)
lapply(WFU_Kalivas_test, function(df)
  sapply(df["coatcolor"], unique))
# everything is consistent

######################
### Kalivas(ITALY) ###
######################
WFU_Kalivas_Italy <- u01.importxlsx("(Italy) Master Shipping.xlsx")
WFU_Kalivas_Italy_test <- uniform.var.names.testingu01(WFU_Kalivas_Italy)
# WFU_Kalivas_Italy_test2 <- uniform.date.testingu01(WFU_Kalivas_Italy_test) #Error: Can't subset with `[` using an object of class POSIXct.
lapply(WFU_Kalivas_Italy_test, function(df)
  sapply(df["coatcolor"], unique)) # Address [[2]] data 

######################
###### Flagel ########
######################
WFU_Flagel <- u01.importxlsx("Flagel Master Shipping Sheet.xlsx")
WFU_Flagel_test <- uniform.var.names.testingu01(WFU_Flagel)
# lapply(WFU_Flagel_test, function(df)
#   sapply(df["coatcolor"], unique))
WFU_Flagel_test <- uniform.coatcolors(WFU_Flagel_test)
