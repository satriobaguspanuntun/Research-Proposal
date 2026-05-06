# Indonesia Premature Deindustrialisation
# first phase: Historical context and stylesed facts
# output: Graphs and Table
# Manufacturing share GDP
# Indonesia's labour force composition (formal vs informal vs non-manuf vs manuf)
# FDI movements and to which sector
# Manufacturing exports (by skill)
# GDP per capita
# Commoditiy prices and exports 

library(tidyverse)
library(ggplot2)
library(openxlsx)
library(readr)
library(WDI)
library(comtradr)
library(haven)
library(patchwork)

# WDI data
wdi_data <- WDI(
  country = "ID",
  indicator = c(
    # Manufacturing
    "NV.IND.MANF.ZS", "NV.IND.MANF.KD.ZG", "NV.IND.MANF.CD",
    "NV.IND.TOTL.ZS", "NV.SRV.TOTL.ZS", "NV.AGR.TOTL.ZS",
    # Labour
    "SL.IND.EMPL.ZS", "SL.AGR.EMPL.ZS", "SL.SRV.EMPL.ZS",
    "SL.EMP.VULN.ZS", "SL.TLF.TOTL.IN", "SL.UEM.TOTL.ZS",
    # FDI & Investment
    "BX.KLT.DINV.WD.GD.ZS", "BX.KLT.DINV.CD.WD", "NE.GDI.FTOT.ZS",
    # Trade & Exports
    "TX.VAL.MANF.ZS.UN", "TX.VAL.TECH.MF.ZS",
    "TX.VAL.FUEL.ZS.UN", "TX.VAL.MMTL.ZS.UN", "TT.PRI.MRCH.XD.WD",
    # Income & Productivity
    "NY.GDP.PCAP.KD", "NY.GDP.PCAP.KD.ZG", "SL.GDP.PCAP.EM.KD"
  ),
  start = 1970, end = 2025,
  extra = TRUE
) %>% 
  rename("mva_gdp" = NV.IND.MANF.ZS,
         "mva_growth" = NV.IND.MANF.KD.ZG,
         "manuf_exp" = TX.VAL.MANF.ZS.UN,
         "med_high_manuf_exp" = TX.MNF.TECH.ZS.UN)

# productivity data (Penn world table)
productivity <- haven::read_dta(file = "~/Research-Proposal/data/pwt110.dta")

# productivity ASEAN 5 + China + Japan + Korea
asean_5 <- c("Indonesia", "Thailand", "Malaysia", "Philippines", "Viet Nam", "Japan", "China", "Republic of Korea")

prod_asean_5 <- productivity %>% filter(country %in% asean_5)

prod_aseas_5_desc <- data.frame(column_names = names(prod_asean_5),
                                column_description = sapply(prod_asean_5,
                                                            function(x) ifelse(is.null(attr(x, "label")), 
                                                                               "No description", attr(x, "label"))))

# Real GDP productivity composition by Capital, Labour and TFP 
# Solow Growth Decomposition: ΔlnY = α·ΔlnK + (1−α)·ΔlnL + ΔTFP
# where  L = emp × avh × hc 

# Solow Growth Decomposition
solow_growth_decomp <- function(data, country_pick, start, end, return_fig = TRUE) {
  
  date_sequence <- seq(as.numeric(start), as.numeric(end), by = 1)
  
  # filter data by country and select variables
  solow_data <- data %>% 
    filter(country %in% country_pick, year %in% date_sequence) %>% 
    select(country, countrycode, year, rgdpna, emp, labsh, avh, hc, delta, rtfpna, rnna) %>% 
    drop_na() %>% 
    mutate(
      # 1. log difference to approximate annual growth rates
      gY = log(rgdpna) - lag(log(rgdpna)),
      gK = log(rnna) - lag(log(rnna)),
      
      # 2. calculate human-capital labour growth
      L_star = emp * avh,
      gL = log(L_star) - lag(log(L_star)),
      
      # 3. Capital share (alpha) is 1 minus the labour share
      alpha = 1 - labsh,
      
      # 4. calculate contribution
      contr_K = alpha * gK,
      contr_L = labsh * gL,
      
      # 5. calculate TFP growth
      contr_TFP = gY - contr_K - contr_L,
      
      across(c(gY, contr_K, contr_L, contr_TFP), ~ . * 100)
    )
    
  # ── Period Averages (sub-periods of interest) ──────────────
  periods <- list(
    "Pre-Crisis (1970–1996)"      = 1970:1996,
    "Crisis (1997–1999)"          = 1997:1999,
    "Recovery (2000–2007)"        = 2000:2007,
    "Commodity Boom (2008–2014)"  = 2008:2014,
    "Post-Boom (2015–2019)"       = 2015:2019,
    "COVID & Recovery (2020–)"    = 2020:2023
  )
  
  period_summary <- map_dfr(names(periods), function(p) {
    solow_data %>%
      filter(year %in% periods[[p]]) %>%
      summarise(
        Period      = p,
        GDP_growth  = mean(gY, na.rm = TRUE),
        Capital     = mean(contr_K, na.rm = TRUE),
        Labour      = mean(contr_L, na.rm = TRUE),
        TFP         = mean(contr_TFP, na.rm = TRUE)
      )
  })
  
  if (return_fig == TRUE) {
    
    # ── Period Averages chart (sub-periods of interest) ──────────────
    plot_periods <- period_summary %>%
      pivot_longer(cols = c(Capital, Labour, TFP),
                   names_to = "Component",
                   values_to = "Contribution") %>%
      mutate(
        Component = factor(Component, levels = c("TFP", "Labour", "Capital")),
        Period    = factor(Period, levels = period_summary$Period)
      )
    
    p1 <- ggplot(plot_periods, aes(x = Period, y = Contribution, fill = Component)) +
      geom_col(position = "stack", width = 0.65) +
      geom_point(data = period_summary,
                 aes(x = factor(Period, levels = period_summary$Period),
                     y = GDP_growth),
                 inherit.aes = FALSE,
                 shape = 18, size = 3.5, color = "black") +
      geom_line(data = period_summary,
                aes(x = factor(Period, levels = period_summary$Period),
                    y = GDP_growth, group = 1),
                inherit.aes = FALSE,
                linetype = "dashed", color = "black", linewidth = 0.7) +
      scale_fill_manual(
        values = c("Capital" = "#2166ac",
                   "Labour"  = "#58BA14",
                   "TFP"     = "#F75836"),
        name   = "Source of Growth"
      ) +
      labs(
        title    = paste0("Sources of Real GDP Growth —", country_pick),
        subtitle = "Growth accounting decomposition by sub-period (PWT 11.0)",
        x        = NULL,
        y        = "Average Annual Contribution (pp)",
        caption  = "◆ = Total GDP growth. Decomposition: ΔlnY = αΔlnK + (1–α)ΔlnL + TFP\nSource: Penn World Tables 11.0"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x      = element_text(angle = 25, hjust = 1),
        legend.position  = "bottom",
        plot.title       = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank()
      )
    
    # ── Time-series Area Chart ─────────────────────────
    plot_ts <- solow_data %>%
      select(year, Capital = contr_K, Labour = contr_L, TFP = contr_TFP) %>%
      pivot_longer(-year, names_to = "Component", values_to = "Contribution") %>%
      mutate(Component = factor(Component, levels = c("Capital", "Labour", "TFP")))
    
    p2 <- ggplot(plot_ts, aes(x = year, y = Contribution, fill = Component)) +
      geom_col(position = "stack", width = 0.85, alpha = 0.9) +
      geom_line(
        data = t,
        aes(x = year, y = gY), inherit.aes = FALSE,
        color = "black", linewidth = 0.8, linetype = "solid"
      ) +
      scale_fill_manual(
        values = c("Capital" = "#2166ac",
                   "Labour"  = "#58BA14",
                   "TFP"     = "#F75836"),
        name = NULL
      ) +
      scale_x_continuous(breaks = seq(1970, 2025, 5)) +
      geom_hline(yintercept = 0, linewidth = 0.5, color = "grey40") +
      # Shade crisis periods
      annotate("rect", xmin = 1997, xmax = 1999.5,
               ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "red") +
      annotate("rect", xmin = 2019.5, xmax = 2021,
               ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "orange") +
      labs(
        title    = paste0("Annual Sources of Real GDP Growth —", country_pick," (1970–2023)"),
        subtitle = "Black line = total GDP growth; shaded = Asian Financial Crisis & COVID",
        x        = NULL,
        y        = "Contribution to Growth (pp)",
        caption  = "Source: Penn World Tables 11.0 L = emp × avh × hc"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position    = "bottom",
        plot.title         = element_text(face = "bold", size = 13),
        panel.grid.minor   = element_blank()
      )
    
    # ── TFP Level Trajectory ───────────────────────────
    p3 <- solow_data %>%
      ggplot(aes(x = year, y = rtfpna)) +
      geom_line(color = "#2166ac", linewidth = 1.2) +
      geom_point(size = 1.5, color = "#2166ac") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
      scale_x_continuous(breaks = seq(1970, 2025, 5)) +
      labs(
        title    = paste0("TFP Level — ", country_pick, " (2021 = 1)"),
        subtitle = "Relative to own 2021 national prices baseline",
        x        = NULL,
        y        = "TFP Index (2021 = 1)",
        caption  = "Source: Penn World Tables 10.01 (rtfpna)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title       = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank()
      )
    
    # ── 7. Combine & Save ─────────────────────────────────────────
    combined <- (p2 / p1)
    plot_final <- combined + plot_layout(widths = c(2, 1))
    
    return(plot_final)
    
  } else {
    
    return(solow_decomp_data = list(solow_data, period_summary))
    
  }
  
}

# Real GDP decomposition by Industry Origins

# TFP by comparison 
# ── Period Averages (sub-periods of interest) ──────────────
periods <- list(
  "Pre-Crisis (1970–1996)"      = 1970:1996,
  "Crisis (1997–1999)"          = 1997:1999,
  "Recovery (2000–2007)"        = 2000:2007,
  "Commodity Boom (2008–2014)"  = 2008:2014,
  "Post-Boom (2015–2019)"       = 2015:2019,
  "COVID & Recovery (2020–)"    = 2020:2023
)

countries <- list("China", "Indonesia","Japan", "Republic of Korea", "Malaysia","Philippines", "Thailand", "Viet Nam")

tasks <- expand_grid(
  country_name = countries,
  period_vector = names(periods)
)

solow_data_comp <- prod_asean_5 %>% 
  select(country, countrycode, year, rgdpna, emp, labsh, avh, hc, delta, rtfpna, rnna) %>% 
  drop_na() %>% 
  group_by(country) %>% 
  mutate(
    # 1. log difference to approximate annual growth rates
    gY = log(rgdpna) - lag(log(rgdpna)),
    gK = log(rnna) - lag(log(rnna)),
    
    # 2. calculate human-capital labour growth
    L_star = emp * avh,
    gL = log(L_star) - lag(log(L_star)),
    
    # 3. Capital share (alpha) is 1 minus the labour share
    alpha = 1 - labsh,
    
    # 4. calculate contribution
    contr_K = alpha * gK,
    contr_L = labsh * gL,
    
    # 5. calculate TFP growth
    contr_TFP = gY - contr_K - contr_L,
    
    across(c(gY, contr_K, contr_L, contr_TFP), ~ . * 100)
  )

period_summary_multiple <- pmap(tasks, function(country_name, period_vector) {
  
  # filter dataframe by year and country 
  years_to_filter <- periods[[period_vector]]
  
  subset_df <- solow_data_comp %>% 
    filter(country == country_name,
           year %in% years_to_filter) %>% 
    summarise(
      period = period_vector,
      GDP_growth = mean(gY, na.rm = TRUE),
      Capital = mean(contr_K, na.rm = TRUE),
      Labour = mean(contr_L, na.rm = TRUE),
      TFP = mean(contr_TFP, na.rm = TRUE)
    )
  
  return(subset_df)
}) %>% 
  list_rbind()

