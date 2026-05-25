library(comtradr)
library(tidyverse)

# set comtrade api key
set_primary_comtrade_key("e4aec86321394cd59b9583109faf9e9a")

# Download UNcoMTRADE data
# it will be function 
# can be call for one country or multiple countries
# apply Lall technology clasification
# calculate primary and secondary trade

# check year date function
year_check <- function(x) {
  grepl("^\\d{4}$", x)
}
# check year month combination function
year_month_check <- function(x) {
  grepl("^\\d{4}-\\d{2}$", x)
}

# SITC codes
sitc_3 <- ct_get_ref_table("S3") %>% 
  mutate(count_code_length = nchar(id)) %>% 
  filter(count_code_length == 3)

# HS  2022 to SITC V.3 conversion
hs_to_sitc_table <- openxlsx::read.xlsx(xlsxFile = "~/Research-Proposal/data/HS2022toSITC3ConversionAndCorrelationTables.xlsx", sheet = 1) %>% 
  rename("hs2022" = From.HS.2022,
         "sitc_v3_2022" = To.SITC.Rev..3)

# HS 2017 to SITC v3 conversion
hs17_to_sitc_table <- openxlsx::read.xlsx(xlsxFile = "~/Research-Proposal/data/HS2017toSITC3ConversionAndCorrelationTables.xlsx", sheet = 1) %>% 
  rename("hs2017" = From.HS.2017,
         "sitc_v3_2017" = To.SITC.Rev..3)

# HS 2012 to SITC v3 converion
hs12_to_sitc_table <- openxlsx::read.xlsx(xlsxFile = "~/Research-Proposal/data/HS 2012 to SITC3 Correlation and conversion tables.xlsx", sheet = 3) %>% 
  rename("hs2012" = HS.2012,
         "sitc_v3_2012" = SITC3)

# HS 2002 to SITC v3 conversion
hs02_to_sitc_table <- openxlsx::read.xlsx(xlsxFile = "~/Research-Proposal/data/HS2002 to SITC3 Conversion and Correlation Tables.xlsx", sheet = 3) %>% 
  rename("hs2002" = HS02,
         "sitc_v3_2002" = S3)

# HS 1996 to SITC v3 conversion
hs96_to_sitc_table <- openxlsx::read.xlsx(xlsxFile = "~/Research-Proposal/data/HS1996 to SITC3 Conversion and Correlation Tables.xlsx", sheet = 3) %>% 
  rename("hs1996" = HS96,
         "sitc_v3_1996" = S3)

# HS 1992 to SITC v3 conversion
hs92_to_sitc_table <- openxlsx::read.xlsx(xlsxFile = "~/Research-Proposal/data/HS1992 to SITC3 Conversion and Correlation Tables.xlsx", sheet = 3) %>% 
  rename("hs1992" = HS92,
         "sitc_v3_1992" = S3)

# HS 1988 to SITC v3 conversion
hs88_to_sitc_table <- openxlsx::read.xlsx(xlsxFile = "~/Research-Proposal/data/HS88R3.xlsx", sheet = 4) %>% 
  rename("hs1988" = HS88,
         "sitc_v3_1988" = SITC3) %>% 
  mutate(hs1988 = gsub("\\.",x = hs1988,replacement = ""),
         sitc_v3_1988 = gsub("\\.", x = sitc_v3_1988, replacement = ""))

## lall (2000) classification concordance
lall_classification_path <- "~/Research-Proposal/data/DimSitcRev3Products_Ldc_Hierarchy.xlsx"

lall_classification <-  openxlsx::read.xlsx(xlsxFile = lall_classification_path , sheet = 2)

# Refactored unified trade data pulling function
pull_trade <- function(reporter, 
                       partner = "everything", 
                       direction, 
                       commod_code, 
                       freq = "A", 
                       start, 
                       end,
                       chunk_by_year = TRUE) {
  
  # Sense check
  if ("everything" %in% reporter && "everthing" %in% partner) {
    warning("Using all_countries for both reporter and partner may hit the 100K row limit")
  }
  
  # Validate and create date range based on frequency
  range <- create_date_range(freq, start, end, chunk_by_year)
  
  # Pull data for all reporters
  output_goods_list <- list()
  
  for (i in reporter) {
    country_data <- list()
    cli::cli_h1(paste0("Downloading ", tolower(freq_name(freq)), " data for ", i))
    
    for (j in names(range)) {
      goods_data <- tryCatch({
        cli::cli_bullets(paste0("Pulling data for: ", j))
        
        # Get start and end dates for this iteration
        dates <- range[[j]]
        
        data <- ct_get_data(
          type = "goods",
          frequency = freq,
          commodity_classification = "HS",
          commodity_code = commod_code,
          flow_direction = direction,
          reporter = i,
          partner = partner,
          start_date = dates$start,
          end_date = dates$end
        )
        
        # Process the data
        process_trade_data(data, i, j)
        
      }, error = function(e) {
        message("Error for country: ", i, ", period: ", j, ": ", e$message)
        create_empty_row(i, j)
      })
      
      country_data[[j]] <- goods_data
      Sys.sleep(0.5)  # Avoid API rate limit issues
    }
    
    # Combine all data for this reporter
    if (length(country_data) > 0) {
      output_goods_list[[i]] <- do.call(rbind, country_data)
    }
  }
  
  # Combine all reporters
  goods_output <- as.data.frame(do.call(rbind, output_goods_list))
  rownames(goods_output) <- 1:nrow(goods_output)
  
  return(list(goods = goods_output))
}


# Helper function to create date ranges
create_date_range <- function(freq, start, end, chunk_by_year = TRUE) {
  
  if (freq == "A") {
    # Annual frequency
    if (!year_check(start) || !year_check(end)) {
      stop("For annual data, please use year format 'YYYY'")
    }
    
    years <- seq.Date(
      from = as.Date(paste(start, "01", "01", sep = "-")),
      to = as.Date(paste(end, "01", "01", sep = "-")),
      by = "1 year"
    )
    
    years <- substr(as.character(years), 1, 4)
    
    # Return as list with start/end dates
    range_list <- lapply(years, function(y) {
      list(start = y, end = y)
    })
    names(range_list) <- years
    
  } else if (freq == "M") {
    # Monthly frequency
    if (chunk_by_year && year_check(start) && year_check(end)) {
      # Pull monthly data in yearly chunks (more efficient)
      years <- seq.Date(
        from = as.Date(paste(start, "01", "01", sep = "-")),
        to = as.Date(paste(end, "01", "01", sep = "-")),
        by = "1 year"
      )
      years <- substr(as.character(years), 1, 4)
      
      range_list <- lapply(years, function(y) {
        list(start = paste0(y, "-01"), end = paste0(y, "-12"))
      })
      names(range_list) <- years
      
    } else if (year_month_check(start) && year_month_check(end)) {
      # Pull monthly data month by month
      months <- seq.Date(
        from = as.Date(paste(start, "01", sep = "-")),
        to = as.Date(paste(end, "01", sep = "-")),
        by = "1 month"
      )
      months <- substr(as.character(months), 1, 7)
      
      range_list <- lapply(months, function(m) {
        list(start = m, end = m)
      })
      names(range_list) <- months
      
    } else {
      stop("For monthly data, please use year format 'YYYY' or year-month format 'YYYY-MM'")
    }
    
  } else {
    stop("Frequency must be 'A' (Annual) or 'M' (Monthly)")
  }
  
  return(range_list)
}


# Helper function to process trade data
process_trade_data <- function(data, reporter_iso, period) {
  
  
  # Process valid data
  processed <- data %>% 
    select(
      freq_code, 
      ref_period_id,
      ref_year, 
      ref_month,
      period,
      reporter_iso, 
      reporter_desc, 
      flow_code, 
      flow_desc,
      partner_iso, 
      partner2desc, 
      classification_code,
      cmd_code, 
      cmd_desc, 
      aggr_level,
      customs_code,
      customs_desc,
      cifvalue,
      fobvalue,
      primary_value
    ) %>% 
    mutate(
      # Fix Taiwan ISO code
      partner_iso = if_else(partner_iso == "S19", "TWN", partner_iso)
    )
  
  return(processed)
}


# Helper function to create empty row
create_empty_row <- function(reporter_iso, period) {
  data.frame(
    freq_code = NA, 
    ref_period_id = NA,
    ref_year = NA, 
    ref_month = NA,
    period = period,
    reporter_iso = reporter_iso, 
    reporter_desc = NA, 
    flow_code = NA, 
    flow_desc = NA,
    partner_iso = NA, 
    partner2desc = NA, 
    classification_code = NA,
    cmd_code = NA, 
    cmd_desc = NA, 
    aggr_level = NA,
    customs_code = NA,
    customs_desc = NA,
    cifvalue = NA,
    fobvalue = NA,
    primary_value = NA
  )
}


# Helper function for frequency name
freq_name <- function(freq) {
  switch(freq,
         "A" = "annual",
         "M" = "monthly",
         "unknown")
}


# Wrapper functions for backward compatibility (optional)
pull_monthly_trade <- function(reporter, 
                               partner, 
                               direction, 
                               commod_code, 
                               start, 
                               end) {
  result <- pull_trade(
    reporter = reporter,
    partner = partner,
    direction = direction,
    commod_code = commod_code,
    freq = "M",
    start = start,
    end = end,
    chunk_by_year = TRUE
  )
  
  # Return just the data frame for backward compatibility
  return(result$goods)
}
  
# function to wrap trade data
trade_data_wrap <- function(country_vector, start, end) {
  
  data <- pull_trade(
    reporter = country_vector,
    partner = "World",
    direction = c("export", "import"),
    commod_code = "everything",
    freq = "A",
    start = start,
    end = end
  ) %>% 
    bind_rows() 
  
  trade_data <- data %>% 
    mutate(count_code = nchar(cmd_code)) %>% 
    filter(count_code == 6) %>% 
    left_join(hs_to_sitc_table, join_by(cmd_code == hs2022)) %>% 
    left_join(hs17_to_sitc_table, join_by(cmd_code == hs2017)) %>% 
    left_join(hs12_to_sitc_table, join_by(cmd_code == hs2012)) %>% 
    left_join(hs02_to_sitc_table, join_by(cmd_code == hs2002)) %>% 
    left_join(hs96_to_sitc_table, join_by(cmd_code == hs1996)) %>%
    left_join(hs92_to_sitc_table, join_by(cmd_code == hs1992)) %>% 
    left_join(hs88_to_sitc_table, join_by(cmd_code == hs1988)) %>% 
    mutate(row_id = row_number()) %>% 
    pivot_longer(starts_with("sitc_v3_"),
                 names_to = "conv_col",
                 values_to = "sitc_val") %>% 
    mutate(conv_year = as.integer(str_extract(conv_col, "\\d{4}"))) %>% 
    filter(is.na(conv_year) | conv_year <= ref_year) %>% 
    mutate(is_valid = !is.na(sitc_val)) %>% 
    arrange(row_id, desc(conv_year)) %>% 
    group_by(row_id) %>%
    slice_head(n = 1) %>%
    ungroup() %>% 
    mutate(sitc3 = substr(sitc_val, 1, 3)) %>% 
    left_join(lall_classification, join_by(sitc3 == Code))
  
  return(trade_data)
}

inter_trade_data <- trade_data_wrap(country_vector = c("IDN", "THA", "MYS", "VNM", "KOR", "CHN"), 
                                    start = "1990", 
                                    end = "2010")

inter_trade_data2 <- trade_data_wrap(country_vector = c("IDN", "THA", "MYS", "VNM", "KOR", "CHN"), 
                                     start = "2011", 
                                     end = "2025")

trade_data_asean <- inter_trade_data %>% bind_rows(inter_trade_data2)
rm(inter_trade_data)
rm(inter_trade_data2)

# find the share export and import by lall classification
# Calculate total export and import 
trade_data_asea_total <- trade_data_asean %>% 
  group_by(ref_year, reporter_desc, flow_desc, partner2desc) %>% 
  summarise(
    trade_total = sum(primary_value, na.rm = TRUE)/1e9,
  )

# calculate total export and import by lall classification
trade_data_asea_lall <- trade_data_asean %>% 
  group_by(ref_year, reporter_desc, flow_desc, partner2desc, sitc3, Label, Technology) %>% 
  summarise(sitc_total = sum(primary_value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(ref_year, reporter_desc, flow_desc, partner2desc, Technology) %>% 
  summarise(lall_total = sum(sitc_total, na.rm = TRUE)/1e9) %>% 
  drop_na() %>% 
  left_join(trade_data_asea_total, join_by(ref_year, reporter_desc, flow_desc, partner2desc)) %>% 
  ungroup() %>% 
  mutate(share = lall_total/trade_total * 100) %>% 
  group_by(ref_year, reporter_desc, flow_desc, partner2desc) %>% 
  mutate(check_total = sum(lall_total),
         discrepancy = round(trade_total - check_total, 4),
         broad_category = case_when(
           str_detect(Technology, "^High technology") ~ "High Technology",
           str_detect(Technology, "^Medium technology") ~ "Medium Technology",
           str_detect(Technology, "^Low technology") ~ "Low Technology",
           str_detect(Technology, "^Resource-based") ~ "Resource-based",
           str_detect(Technology, "^Primary products") ~ "Primary products",
           TRUE ~ "Other/Unclassified" # Catches "Unclassified products" or anything else
         ))

trade_data_asea_lall_broad <- trade_data_asea_lall %>% 
  ungroup() %>% 
  group_by(ref_year, reporter_desc, flow_desc, partner2desc, broad_category) %>% 
  summarise(total_broad = sum(lall_total, na.rm = TRUE),
            .groups = "drop") %>% 
  left_join(trade_data_asea_total, join_by(ref_year, reporter_desc, flow_desc, partner2desc)) %>% 
  mutate(broad_share = total_broad/trade_total * 100)

# growth export by grouped periods
periods_trade <- list(
  "2000-2007" = 2000:2007,
  "2008-2014" = 2008:2014,
  "2015-2019" = 2015:2019,
  "2020-2024" = 2020:2024
)

countries <- c("Indonesia", "Malaysia", "Thailand", "Viet Nam", "Rep. of Korea", "China")

country_period <- expand_grid(country_name = countries,
                              period_vector = names(periods_trade))

period_trade_summary <- pmap(country_period, function(country_name, period_vector){
  
  # Extract the boundary years for the current period loop
  years_to_filter <- periods_trade[[period_vector]]
  start_yr <- min(years_to_filter)
  end_yr   <- max(years_to_filter)
  n_years  <- end_yr - start_yr
  
  trade_data_asea_lall %>% 
    filter(ref_year %in% c(start_yr, end_yr), 
           flow_desc == "Export", 
           reporter_desc == country_name) %>% 
    # Group by your newly created broad categories
    group_by(broad_category) %>% 
    summarise(
      # Grab values exactly at the start and end boundaries of the period
      export_start = sum(lall_total[ref_year == start_yr], na.rm = TRUE),
      export_end   = sum(lall_total[ref_year == end_yr], na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    # Calculate period-specific growth metrics
    mutate(
      country = country_name,
      period  = period_vector,
      
      # 1. Absolute Growth (in billions, matching your lall_total unit)
      abs_growth = export_end - export_start,
      
      # 2. Percentage Growth over the whole period
      pct_growth = (abs_growth / export_start) * 100,
      
      # 3. Compound Annual Growth Rate (CAGR)
      cagr = ((export_end / export_start)^(1 / n_years) - 1) * 100,
      
      # 4. Growth Share Contribution (This tier's growth / Total country growth)
      growth_share_contrib = (abs_growth / sum(abs_growth, na.rm = TRUE)) * 100
    )
}) %>% 
  # Bind the list of dataframes into one single, clean summary table
  bind_rows()

