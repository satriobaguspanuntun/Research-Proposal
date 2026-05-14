library(tidyverse)
library(openxlsx)
library(readxl)

apo_path <- "~/Research-Proposal/data/APO-Productivity-Database-2025v1-1.xlsx"

apo_sheetnames <- getSheetNames(apo_path)[-1]

clean_apo_database <- function(path, country_id) {
  
  apo_data <- openxlsx::read.xlsx(xlsxFile = apo_path, sheet = country_id)
  
  # pull cols name from the second row
  cols_name <- unname(unlist(apo_data[2, ]))
  
  # current colnames
  country_name <- colnames(apo_data)[1]
  
  # grab the data
  apo_data$na_count <- rowSums(is.na(apo_data))
  
  apo_data_final <- apo_data[4:nrow(apo_data),] %>% 
    fill(colnames(apo_data)[1], .direction = "down") %>% 
    filter(na_count != 58) %>% 
    select(-na_count)
  
  colnames(apo_data_final) <- tolower(cols_name)
  
  apo_data_final <- apo_data_final %>% 
    mutate(variable = str_squish(str_remove_all(variable, "^[.…]+|[.…]+$")),
           group = str_squish(str_remove_all(group, "^[0-9]+[ab]?\\.")),
           country = recode(country_id,
                            "CHN" = "China",
                            "IRN" = "Iran",
                            "ROC" = "Taiwan",
                            "KOR" = "Korea",
                            .default = country_name),
           country_id = country_id) %>% 
    relocate(country, country_id)
  
  return(apo_data_final)
}

# loop over the sheets
apo_database <- map(apo_sheetnames, function(x) {
  
  df <- clean_apo_database(apo_path, country_id = x) 
  
}) %>% list_rbind()


