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
  
  transform_checker <- function(indicator_vec) {
    
    transform_detect <- str_split(indicator_vec, pattern = "\\.")
    
    
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
  # indicator_base <- data.frame(indicator_code = indicator,
  #                              indicator_name = indicator_name)
  
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
  
  # if (any(colnames(df_raw) == "type_of_transformation")) {
  #   
  #   indicator_base <- indicator_base %>% 
  #     separate_wider_delim(indicator_code, delim = ".", names = c("code", "transform")) %>% 
  #     mutate(code = str_squish(code),
  #            transform = str_squish(transform)) %>% 
  #     rename("indicator_code" = code)
  # }
  
  # df_raw %>% left_join(indicator_base, by = join_by(indicator == indicator_code))
  
  
  return(df_raw)
}

indicator_df <- data.frame(indicator = indicator_vec,
                           indicator_name = indicator_name)

imf_data <- imf_api_wrapper(database_id = database,
                            dataflow_id = dataflow,
                            indicator = indicator_vec,
                            indicator_name = indicator_name,
                            country = country_target, 
                            start_period = start, 
                            end_period = end, 
                            freq = freq) %>% 
  left_join(indicator_df)

library(tidyverse)
library(fredr)

# interbank rates Indonesia
indo_interest <- fredr(
  series_id = "IRSTCI01IDM156N",
  observation_start = as.Date("1980-01-01"),
  observation_end = as.Date("2026-01-01"),
  units = "lin",
  frequency = "m"
) %>% 
  mutate(country = "IDN",
         time_period = ymd(date),
         indicator_name = "")%>% 
  select(-date, -realtime_start, -realtime_end)

# Exchange rate
database <- "IMF.STA"

dataflow <- "ER"

indicator_vec <- c("XDC_USD", "USD_XDC", ".PA_RT")

indicator_name <- c("Domestic currency per US Dollar", "US Dollar per DOmestic currency")

country_target <- c("IDN", "MYS", "THA", "KOR", "PHL")

start <- "1993-01"
end <- "2026-05"
freq <- "M"


indicator_df <- data.frame(indicator_code = indicator_vec[1:2],
                           type_of_transformation = indicator_vec[3],
                           indicator_name = indicator_name)

imf_data_exchange <- imf_api_wrapper(database_id = database,
                            dataflow_id = dataflow,
                            indicator = indicator_vec,
                            indicator_name = indicator_name,
                            country = country_target, 
                            start_period = start, 
                            end_period = end, 
                            freq = freq) %>% 
  left_join(indicator_df)


# inflation
database <- "IMF.STA"

dataflow <- "CPI"

indicator_vec <- c("CPI._T.YOY_PCH_PA_PT")

indicator_name <- c("Inflation")

country_target <- c("IDN", "MYS", "THA", "KOR", "PHL")

start <- "1993-01"
end <- "2026-05"
freq <- "A"

indicator_df <- data.frame(index_type = "CPI",
                           indicator_name = indicator_name)


imf_data_inflation <- imf_api_wrapper(database_id = database,
                                     dataflow_id = dataflow,
                                     indicator = indicator_vec,
                                     indicator_name = indicator_name,
                                     country = country_target, 
                                     start_period = start, 
                                     end_period = end, 
                                     freq = freq) %>% 
  left_join(indicator_df)


# unemployment & GDP
database <- "IMF.RES"

dataflow <- "WEO"

indicator_vec <- c("LUR", "NGDP_RPCH")

indicator_name <- c("Unemployment rate", "Real GDP YoY")

country_target <- c("IDN", "MYS", "THA", "KOR", "PHL")

start <- "1993-01"
end <- "2026-05"
freq <- "A"

indicator_df <- data.frame(indicator = indicator_vec,
                           indicator_name = indicator_name)


imf_data_unemp <- imf_api_wrapper(database_id = database,
                                      dataflow_id = dataflow,
                                      indicator = indicator_vec,
                                      indicator_name = indicator_name,
                                      country = country_target, 
                                      start_period = start, 
                                      end_period = end, 
                                      freq = freq) %>% 
  left_join(indicator_df)

