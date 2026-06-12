library(rsdmx)
library(tidyverse)
library(ggplot2)

# This script is for interacting with IMF database through API call
# I am going to pull Monetary policy rates.

# Notes on SDMX dates
# 2000: Year (1SO 8601)
# 2000-01: Month (ISO 8601)
# 2000-01-01: Date (ISO 8601)
# 2000-Q1: Quarter
# 2000-W01: Week
# 2000-S1: Semester
# 2000-D001 : Day

database <- "IMF.STA"

dataflow <- "MFS_IR"

indicator_vec <- c("MFS135_FC_RT_PT_A_PT", "MFS135_RT_PT_A_PT", "MFS162_FC_RT_PT_A_PT",
                   "MFS162_RT_PT_A_PT", "MFS166_RT_PT_A_P", "MMRT_RT_PT_A_PT")

indicator_name <- c("Deposit rate, Foreign Currency, Rate, Percent per annum", "Deposit Rate, Percent per annum", "Lending rate, Foreign Currency, Rate, Percent per annum",
                    "Lending Rate, Percent per annum", "Monetary policy-related, Rate, Percent per annum", "Money market Rate, Percent per annum")

country_target <- c("IDN", "MYS", "THA", "KOR", "PHL")

start <- "1970-01"
end <- "2025-12"
freq <- "M"

imf_api_wrapper <- function(database_id, dataflow_id, indicator, indicator_name, country, start_period, end_period, freq) {
  
  period_checker <- function(period) {
    
    sdmx_period_format <- data.frame(period = c("2000", "2000-01", "2000-01-01", "Q1"),
                                     pattern = c("^[0-9]{4}$", "^[0-9]{4}-[0-9]{2}$", "^[0-9]{4}-[0-9]{2}-[0-9]{2}$", 
                                                 "^[0-9]{4}-Q[0-9]{1}$")) 
    pattern <- sdmx_period_format$pattern
    check_true <- c()
    
    for (date_pattern in pattern) {
      
      check <- grepl(pattern[pattern == date_pattern], x = period)
      check_true[date_pattern] <- check
      
    }
    
    if (all(check_true == FALSE)) stop("Please enter the period in the following format:
                                       Year = 2000,
                                       Month = 2000-01, 
                                       Date = 2000-01-01,
                                       Quarter = 2000-Q1")
    
  }
  
  period_checker(start_period)
  period_checker(end_period)
  
  if (!is.character(database_id)) stop("Please submit database_id in character format")
  if (!is.character(dataflow_id)) stop("Please submit dataflow_id in character format")
  if (!is.character(freq))        stop("Please submit freq in character format")
  if (!grepl("^(A|Q|M)$", freq)) stop("Please submit frequency as 'A', 'Q', or 'M'")
  
  flowref_filter <- paste(database_id, dataflow_id, sep = ",")
  indicator_filter <- paste(indicator, collapse = "+")
  country_filter <- paste(country, collapse = "+")
  filter <- paste(country_filter, indicator_filter,  freq, sep = ".")
  indicator_base <- data.frame(indicator_code = indicator,
                               indicator_name = indicator_name)
  
  
  df_raw <- as.data.frame(
    readSDMX(
      providerId = "IMF_DATA",
      resource = "data",
      flowRef = flowref_filter,
      key = filter,
      start = start_period,
      end = end_period
    )
  )
  
  colnames(df_raw) <- tolower(colnames(df_raw))
  
  df_raw <- df_raw %>% left_join(indicator_base, by = join_by(indicator == indicator_code))
  
  return(df_raw)
}

imf_data <- imf_api_wrapper(database_id = database,
                            dataflow_id = dataflow,
                            indicator = indicator_vec,
                            indicator_name = indicator_name,
                            country = country_target, 
                            start_period = start, 
                            end_period = end, 
                            freq = freq)


