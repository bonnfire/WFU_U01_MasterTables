setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_WFU_U01_ShippingMaster")

# Load libraries
library(tidyverse) #loads dplyr, tidyr, ggplot2, purrr, etc
require(mgsub)
library(readxl)
library(lubridate)
library(openxlsx)
library(stringr)

# QC Wake Forest University Shipment 
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
## XXX figure out the pattern from the original dataset to include in the else if statement
uniform.var.names<- function(df){
  lapply(seq_along(df), function(i) {
    if(grepl("Parent", names(df[[i]])) %>% any()){
      names(df[[i]])[1:2] <- c("sires", "dames")
      df[[i]] <- df[[i]][-1, ]
      }
      names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
                                     c(" |\\.", "#", "Transponder ", "Date of Wean|Wean Date","Animal", "Shipping|Ship"),
                                     c("", "Number", "RF", "DOW","LabAnimal", "Shipment"))
      names(df[[i]]) <- tolower(names(df[[i]]))
      df[[i]]
      })
  }

WFU_Olivier_co_test <- uniform.var.names(WFU_Olivier_co)

# fix first table to account for: no ship date data(DONE), formatting of first row/sires and dames (DONE), highlight (XX waiting for feedback)
WFU_Olivier_co_test[[1]] <- WFU_Olivier_co_test[[1]] %>% 
  mutate(shipmentdate = as.POSIXct("2018-10-30", format="%Y-%m-%d"))
names(WFU_Olivier_co_test[[1]])[1:2] <- WFU_Olivier_co_test[[1]][1,1:2] %>% as.character() %>% tolower()
WFU_Olivier_co_test[[1]] <- WFU_Olivier_co_test[[1]][-1, ]
# rename all sheets 
names(WFU_Olivier_co_test) <- Olivier_co_sheetnames


######################
## Kalivas(Heroine) ##
######################


# Import XLSX
u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 

WFU_Kalivas <- u01.importxlsx("MUSC (Kalivas) Master Shipping.xlsx")
WFU_Kalivas_test <- uniform.var.names(WFU_Kalivas)

lapply(WFU_Kalivas_test, function(df)
  sapply(df["coatcolor"], unique))
# everything is consistent