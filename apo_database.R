library(tidyverse)
library(openxlsx)
library(readxl)
library(gt)

#APO data
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

#UNSD data

unsd_na_path <- "~/Research-Proposal/data/Download-GDPcurrent-USD-countries.xlsx"

unsd_na_data <-  openxlsx::read.xlsx(xlsxFile = unsd_na_path, sheet = 1)

cols_unsd <- tolower(unname(unlist(unsd_na_data[1, ])))

colnames(unsd_na_data) <- cols_unsd

unsd_na_data <- unsd_na_data[2:nrow(unsd_na_data), ]

# new category columns : approach and isic_level
unsd_na_data <- unsd_na_data %>% 
  group_by(country) %>% 
  mutate(
    approach = case_when(
    indicatorname == "Final consumption expenditure" ~ "Expenditure",
    indicatorname == "Household consumption expenditure (including Non-profit institutions serving households)" ~ "Expenditure",
    indicatorname == "General government final consumption expenditure" ~ "Expenditure",
    indicatorname == "Gross capital formation" ~ "Expenditure",
    indicatorname == "Gross fixed capital formation (including Acquisitions less disposals of valuables)" ~ "Expenditure",
    indicatorname == "Exports of goods and services" ~ "Expenditure",
    indicatorname == "Imports of goods and services" ~ "Expenditure",
    indicatorname == "Gross Domestic Product (GDP)" ~ "Expenditure",
    indicatorname == "Changes in inventories" ~ "Expenditure",
    .default = "Production"
  ),
  isic_level = str_extract(indicatorname, pattern = "ISIC [A-Z0-9-]+")
  ) %>% 
  relocate(countryid, country, indicatorname, approach, isic_level)



idn_data <- unsd_na_data %>%
  filter(country == "Indonesia", approach == "Production") %>% 
  pivot_longer(cols = matches("^[0-9]{4}$"),
               names_to = "year",
               values_to = "values") %>% 
  group_by(indicatorname) %>% 
  mutate(growth = (1-lag(values)/values) * 100)


ind_data_apo <- apo_database %>% 
  filter(group == "GDP by industry at constant prices", country == "Indonesia") %>% 
  pivot_longer(cols = matches("^[0-9]{4}$"),
               names_to = "year",
               values_to = "values") %>% 
  group_by(variable)

apo_industry_codes <- data.frame(variable = unique(ind_data_apo$variable),
                                 code = unique(ind_data_apo$code))

# table for Indonesia
period_selection <- list(
  "1980-1992" = c(1980, 1992),
  "1993-1996" = c(1993, 1996),
  "1997-1999" = c(1997, 1999),
  "2000-2007" = c(2000, 2007),
  "2008-2014" = c(2008, 2014),
  "2015-2019" = c(2015, 2019),
  "2020-2023" = c(2020, 2023))

period_selection <- lapply(period_selection, function(x) as.character(x))
period_idn_industry <- list()

for (period in 1:length(period_selection)) {
  
  years_filter <- period_selection[[period]]
  years_num    <- as.numeric(years_filter)
  count_year   <- years_num[2] - years_num[1]
  
  df <- ind_data_apo %>% 
    filter(year %in% years_filter) %>%
    arrange(country, variable, year) %>%   
    group_by(country, variable) %>% 
    summarise(
      annual_growth = ((values / lag(values))^(1 / count_year) - 1) * 100,
      year_period   = names(period_selection[period]),
      .groups = "drop")
  
  period_idn_industry[[period]] <- df
}

df <- period_idn_industry %>%
  bind_rows() %>% 
  group_by(variable, year_period) %>% 
  mutate(count_row = row_number()) %>% 
  ungroup() %>% 
  filter(count_row == 2) %>% 
  select(-count_row) %>% 
  pivot_wider(names_from = year_period,
              values_from = annual_growth) %>% 
  inner_join(apo_industry_codes) %>% 
  relocate(country, code)

hierarchy <- tibble(
  code = c("30300", "30310", "30320", "30330", "30340", "30350",
           "30360", "30370", "30380", "30381", "30382", "30383",
           "30384", "30390", "30400", "30500", "30600", "30700",
           "30800", "30810", "30820", "30900", "31000", "31100",
           "30100", "30200"),
  level = c(1, 2, 2, 2, 2, 2,
            2, 2, 2, 3, 3, 3,
            3, 2, 1, 1, 1, 1,
            1, 2, 2, 1, 1, 1,
            1, 1)
)

df_display <- df %>%
  left_join(hierarchy, by = "code") %>%
  arrange(code) %>% 
  rename("Sector" = variable) %>% 
  filter(!code %in% c("31000", "31100")) %>% 
  filter(code %in% c("30100", "30200", "30300", "30400", "30500", "30600", 
                     "30700", "30800", "30810", "30820", "30900"))

# row indices for each level
rows_l1 <- which(df_display$level == 1)
rows_l2 <- which(df_display$level == 2)
rows_l3 <- which(df_display$level == 3)

ft <- df_display %>% 
  select(-code, -level, -country) %>%
  gt() %>%
  
  tab_header(
    title = "Industrial Growth by Sector",
    subtitle = "Real value added, annualised growth rate %"
  ) %>%
  
  opt_align_table_header(align = "left") %>% 
  
  cols_width(
    matches("^[0-9]{4}-[0-9]{4}$") ~ px(100),
    1 ~ px(150)
  ) %>% 
  
  cols_align(
    align = "center",
    columns = matches("^[0-9]{4}-[0-9]{4}$")
  ) %>% 
  
  # format numbers
  fmt_number(
    columns = 2:8,
    decimals = 2
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "—"
  ) %>%
  
  # level 1 — bold, no indent
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = rows_l1)
  ) %>%
  
  # header styling
  tab_style(
    style = cell_fill(color = "#2C3E50"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(size = "large"),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = cell_text(size = "medium"),
    locations = cells_title(groups = "subtitle")
  ) %>% 
  
  tab_source_note(source_note = "Source: Author Calculation, APO Database 2025") %>% 
  
  # table options
  tab_options(
    table.font.size = px(12),
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#F5F5F5",
    table.border.top.color          = "#1F4E79",
    table.border.top.width          = px(3)
  )

ft %>%  gtsave("tab_1.png")

df_display_2 <- df %>%
  left_join(hierarchy, by = "code") %>%
  arrange(code) %>% 
  rename("Sector" = variable) %>% 
  filter(!code %in% c("31000", "31100")) %>% 
  filter(code %in% c("30300", "30310", "30320", "30330", "30340", "30350", "30360",
                     "30370", "30380", "30381", "30382", "30383", "30384", "30390"))

rows_l1 <- which(df_display_2$level == 1)
rows_l2 <- which(df_display_2$level == 2)
rows_l3 <- which(df_display_2$level == 3)

ft_2 <- df_display_2 %>%
  select(-code, -level, -country) %>%
  gt() %>%
  
  tab_header(
    title = "Manufacturing Sector Growth and Its Components",
    subtitle = "Real value added, annualised growth rate %"
  ) %>%
  
  opt_align_table_header(align = "left") %>% 
  
  cols_width(
    matches("^[0-9]{4}-[0-9]{4}$") ~ px(100),
    1 ~ px(150)
  ) %>% 
  
  cols_align(
    align = "center",
    columns = matches("^[0-9]{4}-[0-9]{4}$")
  ) %>% 
  
  # format numbers
  fmt_number(
    columns = 2:8,
    decimals = 2
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "—"
  ) %>%
  
  # level 1 — bold, no indent
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = rows_l1)
  ) %>%
  
  # level 2 — indent 15px
  tab_style(
    style = list(
      cell_text(indent = px(15))
    ),
    locations = cells_body(columns = "Sector", rows = rows_l2)
  ) %>%
  
  # level 3 — indent 30px + italic
  tab_style(
    style = list(
      cell_text(indent = px(30), style = "italic")
    ),
    locations = cells_body(columns = "Sector", rows = rows_l3)
  ) %>%
  
  # header styling
  tab_style(
    style = cell_fill(color = "#2C3E50"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(size = "large"),
    locations = cells_title(groups = "title")
  ) %>% 
  tab_style(
    style = cell_text(size = "medium"),
    locations = cells_title(groups = "subtitle")
  ) %>% 
  
  tab_source_note(source_note = "Source: Author Calculation, APO Database 2025") %>% 
  
  # table options
  tab_options(
    table.font.size = px(12),
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#F5F5F5",
    table.border.top.color          = "#1F4E79",
    table.border.top.width          = px(3)
  )

ft_2 %>%  gtsave("tab_2.png")


df_series <- ind_data_apo %>% 
  mutate(growth = ((values - lag(values))/lag(values)) * 100) %>% 
  filter(code %in% c("30300", "30310", "30320", "30330", "30340", "30350", "30360",
              "30370", "30380", "30381", "30382", "30383", "30384", "30390")) %>% 
  filter(year >= 1994) %>% 
  ggplot(aes(x = year, y = growth, colour = variable, group = variable)) +
  geom_line()



