## Sequencing Run Log
library(lubridate)


# extract the spreadsheet notebook (first sheet; sheet = 1, "sequencing")
sequencing_run_log_IGM <- flipAPI::DownloadXLSX("https://www.dropbox.com/s/8pqkjfib37fric8/sequencing%20run%20log%20at%20IGM%20post%2004-2017.xlsx?dl=0", sheet = 1) 

# modify our copy 
sequencing_run_log_IGM_df <- sequencing_run_log_IGM %>%
  clean_names %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(matches("date", ignore.case = T)), 
            ~ openxlsx::convertToDate(as.numeric(.))) %>% 
  mutate_at(vars(matches("number_of_pools")), as.numeric)


## XX need to fix the project name before uploading
## columns include date samples submitted, date sequenced, date returned, run ID#, type of run, type of lib prep, #of pools

## upload into the dropbox 