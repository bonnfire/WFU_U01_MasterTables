# FOR DROPBOX STRUCTURE

# upload the cleaned versions as csv 


# and upload the raw files as excel 
u01s <- ls(pattern = "WFU[^_]*_[^_]*$") # only the ones that have one underscore
for(i in seq_along(u01s)){
  for(j in 1:length(u01s[i])){
    data <- get(u01s[i])
    openxlsx::write.xlsx(data[j], file = paste0("cohort", j, "_", data, ".xlsx"))
  }
}


dir.create(paste0("~/home/bonnie/Dropbox (Palmer Lab)/PalmerLab_Datasets/u01_tom_jhou/database/master_table/cohort", 1:10))
u01s
data <- get(u01s[2])
openxlsx::write.xlsx(data[2], file = paste0("/home/bonnie/Dropbox (Palmer Lab)/PalmerLab_Datasets/u01_tom_jhou/database/master_table/cohort", j, "_", data, ".xlsx"))

/home/bonnie/Dropbox (Palmer Lab)/PalmerLab_Datasets/u01_tom_jhou/database/master_table