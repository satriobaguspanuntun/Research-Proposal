library(tidyverse)
library(openxlsx)
library(readxl)
library(gt)
library(scales)

# Indonesia Investment realisation
invest_path <- "~/Research-Proposal/data/Tahunan.xlsx"

invest_data <-  openxlsx::read.xlsx(xlsxFile = invest_path, sheet = 2) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "year",
               values_to = "values") %>% 
  filter(Sector != "Total") %>% 
  group_by(Investment, year) %>% 
  mutate(total_invest = sum(values, na.rm = TRUE),
         share_invest = values/total_invest * 100,
         year = as.numeric(year)) %>% 
  ungroup()

# filter every 5 year
years_5 <- invest_data %>% 
  distinct(year) %>% 
  arrange(year) %>% 
  filter(year %% 5 == 0 | year == min(year) | year == max(year)) %>% 
  pull(year)

invest_5yr <- invest_data %>% 
  filter(year %in% years_5)

# calculate growth between 5 year intervals
invest_growth <- invest_5yr %>% 
  group_by(Investment, Sector) %>% 
  mutate(growth = (values/lag(values) - 1)* 100)

# ── 4. Pivot wide for GT ───────────────────────────────────────────────────────
# Build separate value, share, and growth columns per year
table_data <- invest_growth %>%
  select(Investment, Sector, year, values, share_invest, growth) %>%
  mutate(values = round(values/1e6, 2)) %>% 
  pivot_wider(
    names_from  = year,
    values_from = c(values, share_invest, growth),
    names_glue  = "{.value}_{year}"
  )

# ── 5. Split by Investment type & build GT ────────────────────────────────────
make_gt_table <- function(inv_type, data, yr_cols) {
  
  df <- data %>%
    filter(Investment == inv_type) %>%
    select(-Investment) %>%
    ungroup()
  
  val_cols   <- paste0("values_",       yr_cols)
  share_cols <- paste0("share_invest_", yr_cols)
  
  df %>%
    # ── Drop growth columns entirely ───────────────────────────────────────────
    select(Sector, all_of(val_cols), all_of(share_cols)) %>%
    rename(Sector_label = Sector) %>%
    gt(rowname_col = "Sector_label", groupname_col = "Investment") %>%
    
    # ── Title ──────────────────────────────────────────────────────────────────
    tab_header(
      title    = md(paste0("**Indonesia Investment Realisation — ", inv_type, "**")),
      subtitle = md("*Value (USD Million) · Share of Total (%)*")
    ) %>%
    
    # ── Spanners ───────────────────────────────────────────────────────────────
    { tbl <- .
    for (yr in yr_cols) {
      tbl <- tbl %>%
        tab_spanner(
          label   = as.character(yr),
          columns = matches(paste0("_", yr, "$"))
        )
    }
    tbl
    } %>%
    
    # ── Rename columns ─────────────────────────────────────────────────────────
    # replace the cols_label(.list = ...) chunk with this:
    cols_label(
      values_2010 = "Value",  share_invest_2010 = "Share %",
      values_2015 = "Value",  share_invest_2015 = "Share %",
      values_2020 = "Value",  share_invest_2020 = "Share %",
      values_2025 = "Value",  share_invest_2025 = "Share %"
    ) %>%
    
    # ── Format numbers ─────────────────────────────────────────────────────────
    fmt_number(
      columns  = all_of(val_cols),
      decimals = 1,
      use_seps = TRUE,
      suffixing = FALSE,
      pattern  = "${x} M"
    ) %>% 
    fmt_number(columns = all_of(share_cols), decimals = 1) %>%
    sub_missing(columns = everything(), missing_text = "—") %>%
    grand_summary_rows(
      fns = list(label = "Totals:", id = "totals", fn = "sum"),
      side = "bottom",
      fmt = list(
        ~ fmt_number(.,
          columns  = all_of(val_cols),
          decimals = 1,
          use_seps = TRUE,
          suffixing = FALSE,
          pattern  = "${x} M"
        ),
        ~ fmt_number(., columns = all_of(share_cols), decimals = 1) 
      )
    ) %>% 
    
    # ── Color: share (blue gradient) ───────────────────────────────────────────
    data_color(
      columns  = all_of(share_cols),
      palette  = ifelse(inv_type == "Foreign", "Blues", "Greens"),
      na_color = "white"
    ) %>%
    
    # ── Style ──────────────────────────────────────────────────────────────────
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_stub()
    ) %>%
    tab_style(
      style     = cell_fill(color = "#f5f5f5"),
      locations = cells_column_spanners()
    ) %>%
    tab_style(
      style     = cell_text(size = px(12)),
      locations = cells_body()
    ) %>%
    tab_style(
      style     = list(
        cell_text(size = px(12), weight = "bold"),
        cell_fill(color = adjust_luminance("lightblue", steps = +1))
      ),
      locations = cells_grand_summary()
    ) %>% 
    
    tab_source_note(
      source_note = md("*Source: BKPM Investment Realisation Data.*")
    ) %>%
    
    tab_options(
      table.font.names                = "Arial",
      heading.align                   = "left",
      column_labels.font.weight       = "bold",
      stub.font.weight                = "bold",
      row.striping.include_table_body = TRUE,
      table.border.top.style          = "solid",
      table.border.top.color          = ifelse(inv_type == "Foreign", "#1F4E79", "#198519"),
      table.border.top.width          = px(3)
    )
}
# ── 6. Render both tables ──────────────────────────────────────────────────────
tbl_domestic <- make_gt_table("Domestic", table_data, years_5)
tbl_foreign  <- make_gt_table("Foreign",  table_data, years_5)
gt_group(tbl_domestic, tbl_foreign)

tbl_domestic
tbl_foreign


