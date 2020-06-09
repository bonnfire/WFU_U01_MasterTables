# FOR DROPBOX STRUCTURE

# upload the cleaned versions as csv 


# and upload the raw files as excel 
# u01s <- ls(pattern = "WFU[^_]*_[^_]*$") # only the ones that have one underscore
# for(i in seq_along(u01s)){
#   for(j in 1:length(u01s[i])){
#     data <- get(u01s[i])
#     openxlsx::write.xlsx(data[j], file = paste0("cohort", j, "_", data, ".xlsx"))
#   }
# }
# 

## 



u01s_raw <- ls(pattern = "WFU[^_]*_[^_]*$")

setwd("~/Dropbox (Palmer Lab)/PalmerLab_Datasets")
u01_dirs <- grep("u01", list.dirs(recursive = F), value = T)
for(i in 1:length(u01s_raw)){
 # go to the directory that the data should go
  u01_find_fromdata <- sub('.*_', '', u01s_raw)
  dirs <- sub("./", "", u01_dirs)
  dir <- dirs[which(grepl(paste0(u01_find_fromdata[i], "$"), u01_find_fromdata, ignore.case = T)==T)]
  setwd(file.path(paste0("~/Dropbox (Palmer Lab)/PalmerLab_Datasets/", dir, "/database/master_table" )))
  

  data <- get(u01s_raw[i])

  for (j in 1:length(data[i])){
    dir.create(file.path(paste0('cohort', str_pad(i, 2, "left", "0"))), recursive = F)
    openxlsx::write.xlsx(data[[i]], file = paste0("/home/bonnie/Dropbox (Palmer Lab)/PalmerLab_Datasets/u01_tom_jhou/database/master_table/cohort", str_pad(i, 2, "left", "0"), "/raw_cohort", str_pad(i, 2, "left", "0"),  "_", u01s_raw[2], ".xlsx"))
    write.csv(data_processed[[i]], file = paste0("/home/bonnie/Dropbox (Palmer Lab)/PalmerLab_Datasets/u01_tom_jhou/database/master_table/cohort", str_pad(i, 2, "left", "0"), "/processed_cohort", str_pad(i, 2, "left", "0"),  "_", u01s_raw[2], ".csv"))
  }
}

data <- get(u01s_raw[grep("jhou", u01s_raw, ignore.case = T)])
openxlsx::write.xlsx(data[2], file = paste0("/home/bonnie/Dropbox (Palmer Lab)/PalmerLab_Datasets/u01_tom_jhou/database/master_table/cohort", j, "_", data, ".xlsx"))

u01s_processed <- grep("(?>naive)", ls(pattern = "WFU.*test$"), perl = T, value = T, invert = T)
data_processed <- get(u01s_processed[grep("jhou", u01s_processed, ignore.case = T)])
#  create the directories depending on how many cohorts each u01 has
setwd("~/Dropbox (Palmer Lab)/PalmerLab_Datasets/u01_tom_jhou/database/master_table")
for(i in 1:length(data)){
  dir.create(file.path(paste0('cohort', str_pad(i, 2, "left", "0"))), recursive = F)
  openxlsx::write.xlsx(data[[i]], file = paste0("/home/bonnie/Dropbox (Palmer Lab)/PalmerLab_Datasets/u01_tom_jhou/database/master_table/cohort", str_pad(i, 2, "left", "0"), "/raw_cohort", str_pad(i, 2, "left", "0"),  "_", u01s_raw[2], ".xlsx"))
  write.csv(data_processed[[i]], file = paste0("/home/bonnie/Dropbox (Palmer Lab)/PalmerLab_Datasets/u01_tom_jhou/database/master_table/cohort", str_pad(i, 2, "left", "0"), "/processed_cohort", str_pad(i, 2, "left", "0"),  "_", u01s_raw[2], ".csv"))
  }


write.csv(data_processed[[1]], "test.csv")
