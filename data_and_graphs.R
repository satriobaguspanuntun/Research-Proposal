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
library(grid)

manuf_va_asean <- WDI(
  country = c("ID", "MY", "TH", "PH", "CN", "JP", "VN", "KR", "SG"),
  indicator = c(
    # Manufacturing
    "NV.IND.MANF.ZS", "NV.IND.MANF.KD.ZG", "NV.IND.MANF.CD",
    "NV.IND.TOTL.ZS", "NV.SRV.TOTL.ZS", "NV.AGR.TOTL.ZS","NV.IND.MANF.KD","NV.IND.MANF.KN",
    # Trade & Exports
    "TX.VAL.MANF.ZS.UN", "TX.VAL.TECH.MF.ZS",
    "TX.VAL.FUEL.ZS.UN", "TX.VAL.MMTL.ZS.UN", "TT.PRI.MRCH.XD.WD",
    "PA.NUS.FCRF",       # Official exchange rate (LCU per USD)
    "NY.GDP.DEFL.ZS"),    # GDP deflator Indonesia
  start = 1970,
  end = 2025,
  extra = TRUE)

# productivity data (Penn world table)
productivity <- haven::read_dta(file = "~/Research-Proposal/data/pwt110.dta")

# productivity ASEAN 5 + China + Japan + Korea
asean_5 <- c("China", "Indonesia","Japan", "Republic of Korea", "Malaysia",
             "Philippines", "Thailand", "Viet Nam", "Singapore")

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
    "Pre-Crisis\n(1970–1996)"      = 1970:1996,
    "Crisis\n(1997–1999)"          = 1997:1999,
    "Recovery\n(2000–2007)"        = 2000:2007,
    "Commodity Boom\n(2008–2014)"  = 2008:2014,
    "Post-Boom\n(2015–2019)"       = 2015:2019,
    "COVID &\nRecovery (2020–)"    = 2020:2023
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
    
    # ── Better color palette ────────────────────────────────────
    growth_colors <- c(
      "Capital" = "#1E3A8A",  # Deep blue
      "Labour"  = "#3B82F6",  # Medium blue
      "TFP"     = "#60A5FA"   # Light blue
    )
    
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
      
      # ── Stacked bars with rounded corners ────────────────────
      geom_col(position = "stack", width = 0.7, alpha = 0.92) +
      
      # ── Zero line ─────────────────────────────────────────────
      geom_hline(yintercept = 0, linewidth = 0.5, color = "grey30") +
      
      # ── GDP growth overlay ────────────────────────────────────
      geom_point(data = period_summary,
                 aes(x = factor(Period, levels = period_summary$Period),
                     y = GDP_growth),
                 inherit.aes = FALSE,
                 shape = 18, size = 3, color = "black") +
      geom_line(data = period_summary,
                aes(x = factor(Period, levels = period_summary$Period),
                    y = GDP_growth, group = 1),
                inherit.aes = FALSE,
                linetype = "dashed", color = "black", linewidth = 0.6) +
      
      # ── Scales ────────────────────────────────────────────────
      scale_fill_manual(
        values = growth_colors,
        name   = "Source of Growth",
        guide  = guide_legend(reverse = TRUE)
      ) +
      scale_y_continuous(n.breaks = 6) +
      
      # ── Labels ────────────────────────────────────────────────
      labs(
        title    = paste0("Sources of Real GDP Growth — ", country_pick),
        subtitle = "Stacked bars = factor contributions (pp) · ◆ dashed = total GDP growth · PWT 11.0",
        x        = NULL,
        y        = "Average Annual Contribution (pp)",
        caption  = "Sub-periods: Pre-Crisis (1970–96), Crisis (1997–99), Recovery (2000–07),\nCommodity Boom (2008–14), Post-Boom (2015–19), COVID & After (2020–23)\nSource: Penn World Table 11.0"
      ) +
      
      # ── Theme ─────────────────────────────────────────────────
      theme_minimal(base_size = 11) +
      theme(
        # Panel
        panel.background   = element_rect(fill = "#FAFAFA", color = NA),
        plot.background    = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
        
        # Axis
        axis.text.x        = element_text(angle = 0, hjust = 0.5, size = 8.5,
                                          color = "grey20", lineheight = 0.9),
        axis.text.y        = element_text(size = 9, color = "grey20"),
        axis.title.y       = element_text(size = 9.5, margin = margin(r = 10)),
        axis.line.x        = element_line(color = "grey30", linewidth = 0.4),
        
        # Legend
        legend.position    = "bottom",
        legend.direction   = "horizontal",
        legend.title       = element_text(size = 9, face = "bold"),
        legend.text        = element_text(size = 9),
        legend.key.size    = unit(0.45, "cm"),
        legend.margin      = margin(t = 6),
        
        # Titles
        plot.title         = element_text(face = "bold", size = 13.5,
                                          color = "grey10"),
        plot.subtitle      = element_text(size = 9, color = "grey40",
                                          margin = margin(b = 10)),
        plot.caption       = element_text(size = 7.5, color = "grey50",
                                          hjust = 0, margin = margin(t = 10),
                                          lineheight = 1.3),
        plot.margin        = margin(12, 14, 10, 12)
      )
    
    # ── Time-series Area Chart ─────────────────────────
    plot_ts <- solow_data %>%
      select(year, Capital = contr_K, Labour = contr_L, TFP = contr_TFP) %>%
      pivot_longer(-year, names_to = "Component", values_to = "Contribution") %>%
      mutate(Component = factor(Component, levels = c("Capital", "Labour", "TFP")))
    
    p2 <- ggplot(plot_ts, aes(x = year, y = Contribution, fill = Component)) +
      
      # ── Crisis period shading ─────────────────────────────────
      annotate("rect", xmin = 1997, xmax = 1999.5,
               ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#EF4444") +
      annotate("rect", xmin = 2019.5, xmax = 2021,
               ymin = -Inf, ymax = Inf, alpha = 0.15, fill = "#F59E0B") +
      
      # ── Stacked bars ──────────────────────────────────────────
      geom_col(position = "stack", width = 0.9, alpha = 0.92) +
      
      # ── Zero line ─────────────────────────────────────────────
      geom_hline(yintercept = 0, linewidth = 0.5, color = "grey30") +
      
      # ── Total GDP growth line ─────────────────────────────────
      geom_line(
        data = solow_data,
        aes(x = year, y = gY), 
        inherit.aes = FALSE,
        color = "grey10", 
        linewidth = 0.9, 
        linetype = "solid",
        lineend = "round",
        linejoin = "round"
      ) +
      
      # ── Scales ────────────────────────────────────────────────
      scale_fill_manual(
        values = growth_colors,
        name = NULL,
        guide = guide_legend(reverse = TRUE)
      ) +
      scale_x_continuous(
        breaks = seq(1970, 2023, 5),
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      scale_y_continuous(n.breaks = 8) +
      
      # ── Labels ────────────────────────────────────────────────
      labs(
        title    = paste0("Annual Sources of Real GDP Growth — ", country_pick, " (1970–2023)"),
        subtitle = "Black line = total GDP growth · Shaded areas = major economic crises",
        x        = NULL,
        y        = "Contribution to Growth (pp)",
        caption  = "Note: Red shading = Asian Financial Crisis (1997–99) · Orange = COVID-19 (2020–21)\nSource: Penn World Tables 11.0"
      ) +
      
      # ── Theme ─────────────────────────────────────────────────
      theme_minimal(base_size = 11) +
      theme(
        # Panel
        panel.background   = element_rect(fill = "#FAFAFA", color = NA),
        plot.background    = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
        
        # Axis
        axis.text.x        = element_text(size = 9, color = "grey20"),
        axis.text.y        = element_text(size = 9, color = "grey20"),
        axis.title.y       = element_text(size = 9.5, margin = margin(r = 10)),
        axis.line.x        = element_line(color = "grey30", linewidth = 0.4),
        
        # Legend
        legend.position    = "bottom",
        legend.direction   = "horizontal",
        legend.text        = element_text(size = 9),
        legend.key.size    = unit(0.45, "cm"),
        legend.margin      = margin(t = 6),
        
        # Titles
        plot.title         = element_text(face = "bold", size = 13.5,
                                          color = "grey10"),
        plot.subtitle      = element_text(size = 9, color = "grey40",
                                          margin = margin(b = 10)),
        plot.caption       = element_text(size = 7.5, color = "grey50",
                                          hjust = 0, margin = margin(t = 10),
                                          lineheight = 1.3),
        plot.margin        = margin(12, 14, 10, 12)
      )
    
    # ── TFP Level Trajectory ───────────────────────────
    p3 <- solow_data %>%
      ggplot(aes(x = year, y = rtfpna)) +
      
      # ── Shaded area under curve ──────────────────────────────
      geom_ribbon(aes(ymin = 1, ymax = rtfpna), 
                  fill = "#3B82F6", alpha = 0.15) +
      
      # ── Line with rounded joints ─────────────────────────────
      geom_line(color = "#1E40AF", 
                linewidth = 1.3, 
                lineend = "round",
                linejoin = "round") +
      
      # ── Points ────────────────────────────────────────────────
      geom_point(size = 1.8, color = "#1E40AF", alpha = 0.7) +
      
      # ── Reference line ────────────────────────────────────────
      geom_hline(yintercept = 1, 
                 linetype = "dashed", 
                 color = "grey40", 
                 linewidth = 0.5) +
      
      # ── Scales ────────────────────────────────────────────────
      scale_x_continuous(
        breaks = seq(1970, 2025, 5),
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      scale_y_continuous(n.breaks = 8) +
      
      # ── Labels ────────────────────────────────────────────────
      labs(
        title    = paste0("Total Factor Productivity Trajectory — ", country_pick),
        subtitle = "TFP index relative to 2021 baseline · Higher = more productive",
        x        = NULL,
        y        = "TFP Index (2021 = 1)",
        caption  = "Source: Penn World Tables 11.0 (rtfpna = TFP at constant national prices)"
      ) +
      
      # ── Theme ─────────────────────────────────────────────────
      theme_minimal(base_size = 11) +
      theme(
        # Panel
        panel.background   = element_rect(fill = "#FAFAFA", color = NA),
        plot.background    = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
        
        # Axis
        axis.text.x        = element_text(size = 9, color = "grey20"),
        axis.text.y        = element_text(size = 9, color = "grey20"),
        axis.title.y       = element_text(size = 9.5, margin = margin(r = 10)),
        axis.line.x        = element_line(color = "grey30", linewidth = 0.4),
        
        # Titles
        plot.title         = element_text(face = "bold", size = 13.5,
                                          color = "grey10"),
        plot.subtitle      = element_text(size = 9, color = "grey40",
                                          margin = margin(b = 10)),
        plot.caption       = element_text(size = 7.5, color = "grey50",
                                          hjust = 0, margin = margin(t = 10)),
        plot.margin        = margin(12, 14, 10, 12)
      )
    
    # ── Combine & Save ─────────────────────────────────────────
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
  "1970–1996"      = 1970:1996,
  "1997–1999"      = 1997:1999,
  "2000–2007"      = 2000:2007,
  "2008–2014"      = 2008:2014,
  "2015–2019"      = 2015:2019,
  "2020–2023"      = 2020:2023
)

countries <- list("China", "Indonesia", "Japan", "Republic of Korea", "Malaysia",
                  "Philippines", "Thailand", "Viet Nam", "Singapore")

tasks <- expand_grid(
  country_name = countries,
  period_vector = names(periods)
)

# Vietnam treatment - didnt report labsh (estimate it using the median/average ASEAN labsh)
# ASEAN labsh 
asean_labsh <- prod_asean_5 %>% 
  select(country, countrycode, year, labsh) %>% 
  filter(country %in% c("Indonesia", "Malaysia", "Thailand", "Philippines", "Singapore")) %>% 
  group_by(year) %>% 
  summarise(
    country = "Viet Nam",
    asean_labsh_mean = mean(labsh, na.rm = TRUE),
    asean_labsh_median = median(labsh, na.rm = TRUE))

solow_data_viet <- prod_asean_5 %>% 
  select(country, countrycode, year, rgdpna, emp, labsh, avh, hc, delta, rtfpna, rnna) %>% 
  filter(country == "Viet Nam") %>% 
  left_join(asean_labsh) %>% 
  ungroup() %>% 
  mutate(labsh = asean_labsh_mean,
         rtfpna = 0) %>% 
  select(country, countrycode, year, rgdpna, emp, labsh, avh, hc, delta, rtfpna, rnna) %>% 
  drop_na()


solow_data_comp <- prod_asean_5 %>% 
  select(country, countrycode, year, rgdpna, emp, labsh, avh, hc, delta, rtfpna, rnna) %>% 
  drop_na() %>% 
  add_row(solow_data_viet) %>% 
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

plot_tfp_multiple <- period_summary_multiple %>% 
  filter(country %in% c("Indonesia", "Republic of Korea", "Malaysia", "Philippines",
                        "Thailand", "China", "Viet Nam", "Japan", "Singapore")) %>% 
  pivot_longer(cols = c(Capital, Labour, TFP),
               names_to = "Var",
               values_to = "Value") %>% 
  select(-GDP_growth) %>% 
  mutate(country = factor(country, 
                          levels = c("Indonesia", "Republic of Korea", "Malaysia",
                                     "Philippines","Thailand", "China", "Viet Nam", "Japan", "Singapore")),
         period = factor(period, 
                         levels = unique(period_summary_multiple$period)),
         Var = factor(Var,
                      levels = c("TFP", "Labour", "Capital")))

# ── Color palette (consistent with earlier charts) ─────────────────────────────
ga_colors <- c(
  "Capital" = "#1E3A8A",  # Deep blue
  "Labour"  = "#3B82F6",  # Medium blue
  "TFP"     = "#60A5FA"   # Light blue
)

# ── GDP growth dot data for overlay ────────────────────────────────────────────
dot_data <- period_summary_multiple %>%
  filter(country %in% c("Indonesia", "Republic of Korea", "Malaysia",
                        "Philippines", "Thailand", "China", "Viet Nam", "Japan", "Singapore")) %>%
  mutate(
    country = factor(country,
                     levels = c("Indonesia", "Republic of Korea", "Malaysia",
                                "Philippines", "Thailand", "China", "Viet Nam", "Japan", "Singapore")),
    period  = factor(period, levels = unique(period_summary_multiple$period))
  )

# ── Highlight Indonesia label ──────────────────────────────────────────────────
country_labels <- c(
  "Indonesia"         = "INDONESIA",
  "Republic of Korea" = "South Korea",
  "Malaysia"          = "Malaysia",
  "Philippines"       = "Philippines",
  "Thailand"          = "Thailand",
  "China"             = "China",
  "Viet Nam"          = "Vietnam",
  "Japan"             = "Japan",
  "Singapore"         = "Singapore"
)

p4 <- ggplot(data   = plot_tfp_multiple,
             mapping = aes(x = period, y = Value, fill = Var)) +
  
  # ── Stacked bars ─────────────────────────────────────────────────────────────
  geom_col(position = "stack", width = 0.72, alpha = 0.92) +
  
  # ── Zero line ────────────────────────────────────────────────────────────────
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey30") +
  
  # ── GDP growth dot + line overlay ────────────────────────────────────────────
  geom_point(data     = dot_data,
             aes(x    = period, y = GDP_growth),
             inherit.aes = FALSE,
             shape    = 18, size = 2.5, color = "grey10") +
  geom_line(data      = dot_data,
            aes(x     = period, y = GDP_growth, group = 1),
            inherit.aes = FALSE,
            linetype  = "dashed", color = "grey10", linewidth = 0.6) +
  
  # ── Facet ────────────────────────────────────────────────────────────────────
  facet_wrap(~ country,
             scales   = "free_y",
             ncol     = 3,
             labeller = labeller(country = country_labels)) +
  
  # ── Scales ───────────────────────────────────────────────────────────────────
  scale_fill_manual(
    values = ga_colors,
    name   = "Source of Growth",
    guide  = guide_legend(reverse = TRUE)
  ) +
  scale_y_continuous(
    n.breaks = 5,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_x_discrete(
    labels = c(
      "1970–1996" = "Pre-Crisis\n1970–96",
      "1997–1999" = "Crisis\n1997–99",
      "2000–2007" = "Recovery\n2000–07",
      "2008–2014" = "Boom\n2008–14",
      "2015–2019" = "Post-Boom\n2015–19",
      "2020–2023" = "COVID\n2020–23"
    )
  ) +
  
  # ── Labels ───────────────────────────────────────────────────────────────────
  labs(
    title    = "Growth Decomposition Across East & Southeast Asia",
    subtitle = "Stacked bars = factor contributions (pp) · ◆ dashed line = total GDP growth",
    x        = NULL,
    y        = "Average Annual Contribution (pp)",
    caption  = "Note: Indonesia shown in bold. Vietnam labor share imputed from ASEAN-5 median.\nPeriods: Pre-Crisis (1970–96), Crisis (1997–99), Recovery (2000–07), Commodity Boom (2008–14),\nPost-Boom (2015–19), COVID & Recovery (2020–23) · Source: Penn World Table 11.0"
  ) +
  
  # ── Theme ────────────────────────────────────────────────────────────────────
  theme_minimal(base_size = 11) +
  theme(
    # Panel
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.border       = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
    
    # Facet strips - highlight Indonesia
    strip.text         = element_text(
      face = "bold", 
      size = 10,
      color = "grey20",
      margin = margin(4, 0, 4, 0)
    ),
    strip.background   = element_rect(fill = "grey90", color = NA),
    
    # Grid
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    panel.spacing.x    = unit(1.2, "lines"),
    panel.spacing.y    = unit(1.2, "lines"),
    
    # Axis
    axis.text.x        = element_text(
      angle = 0, 
      hjust = 0.5, 
      size = 7.5,
      color = "grey30",
      lineheight = 0.85
    ),
    axis.text.y        = element_text(size = 8.5, color = "grey30"),
    axis.title.y       = element_text(
      size = 9.5, 
      margin = margin(r = 10),
      color = "grey20"
    ),
    axis.line.x        = element_line(color = "grey40", linewidth = 0.3),
    
    # Legend
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.title       = element_text(size = 9.5, face = "bold"),
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.5, "cm"),
    legend.spacing.x   = unit(0.2, "cm"),
    legend.margin      = margin(t = 8, b = 0),
    legend.box.margin  = margin(0, 0, 0, 0),
    
    # Titles
    plot.title         = element_text(
      face = "bold", 
      size = 14,
      color = "grey10",
      margin = margin(b = 3)
    ),
    plot.subtitle      = element_text(
      size = 9.5, 
      color = "grey40",
      margin = margin(b = 12),
      lineheight = 1.2
    ),
    plot.caption       = element_text(
      size = 7.5, 
      color = "grey50",
      hjust = 0, 
      margin = margin(t = 12),
      lineheight = 1.35
    ),
    
    # Overall margins
    plot.margin        = margin(15, 15, 12, 15)
  )


# Manufacturing VA % GDP Indonesia vs ASEAN & East Asian countries
p5 <- apo_database %>% 
  filter(code %in% c("12700", "20300"), country %in% c("Indonesia", "Vietnam", "Korea", "China", "Malaysia", "Thailand", "Philippines")) %>% 
  pivot_longer(
    cols = `1970`:`2023`,
    names_to = "year",
    values_to = "value"
  ) %>%
  select(country, country_id, variable, year, value) %>% 
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  mutate(
    mva_pct_gdp = (Manufacturing / `GDP at market price`) * 100
  ) %>% 
  arrange(country, year) %>% 
  ggplot(mapping = aes(x = year, y = mva_pct_gdp, color = country, group = country)) +
  geom_line()

# Medium-and-high technology manufactured export, Manufactured Export, and commodity export (Highly likely that some of the commodity export is being part of manufactured export. May need to redefine the definition of Manufactured export.)

# Manufacture value added % of total value added
# Option A: Highlight Indonesia, mute others
country_colors <- c(
  "Indonesia"         = "#E63946",
  "Viet Nam"           = "#457B9D",
  "Republic of Korea" = "#1D3557",
  "China"             = "#F4A261",
  "Malaysia"          = "#2A9D8F",
  "Thailand"          = "#E9C46A",  
  "Philippines"       = "#A8DADC"
)

# ── 2. Prepare data ──────────────────────────────────────────────────────────
plot_data <- unsd_na_data %>% 
  filter(country %in% c("Indonesia", "Viet Nam", "Republic of Korea", "China", 
                        "Malaysia", "Thailand", "Philippines"), 
         approach == "Production") %>% 
  pivot_longer(
    cols = `1970`:`2024`,
    names_to = "year",
    values_to = "values") %>% 
  mutate(
    values = values/1e9,
    year = as.numeric(year)
  ) %>% 
  select(countryid, country, year, approach, indicatorname, values) %>% 
  pivot_wider(
    names_from = indicatorname,
    values_from = values
  ) %>% 
  mutate(
    manu_va = `Manufacturing (ISIC D)`/`Total Value Added`
  ) %>% 
  filter(!is.na(manu_va))

# Get latest year for each country for labeling
label_data <- plot_data %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

# ── 3. Key events ────────────────────────────────────────────────────────────
key_events <- tribble(
  ~year, ~label,
  1997,  "Asian Crisis",
  2008,  "GFC",
  2020,  "COVID-19"
)

# ── 4. Create the plot ───────────────────────────────────────────────────────
p5 <- ggplot() +
  
  # Background shading for crisis periods (optional)
  annotate("rect", xmin = 1997, xmax = 1999, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.3) +
  annotate("rect", xmin = 2008, xmax = 2010, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.3) +
  annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.3) +
  
  # Main lines with rounded joints
  geom_line(data = plot_data,
            aes(x = year, y = manu_va, colour = country, 
                group = country, linewidth = country),
            alpha = 0.9,
            lineend = "round",
            linejoin = "round") +
  
  # Highlight points at the end
  geom_point(data = label_data,
             aes(x = year, y = manu_va, colour = country),
             size = 2.5, alpha = 0.9) +
  
  # Event markers (subtle)
  geom_vline(data = key_events,
             aes(xintercept = year),
             linetype = "dotted", 
             color = "grey40", 
             linewidth = 0.4,
             alpha = 0.6) +
  
  geom_text(data = key_events,
            aes(x = year, y = 0.36, label = label),
            size = 2.5, 
            color = "grey40",
            fontface = "italic",
            hjust = 0.5,
            vjust = -0.3) +
  
  # Replace the geom_text_repel section with:
  geom_text(
    data = label_data,
    aes(x = year, y = manu_va, label = country, colour = country),
    hjust = 0,
    nudge_x = 1,
    size = 3.2,
    fontface = "bold",
    check_overlap = TRUE  # Prevents overlapping labels
  ) +
  
  # ── Scales ─────────────────────────────────────────────────────────────────
  scale_colour_manual(values = country_colors) +
  
  # Make Indonesia line thicker
  scale_linewidth_manual(
    values = c(
      "Indonesia" = 1.3,
      "Viet Nam" = 0.9,
      "Republic of Korea" = 0.9,
      "China" = 0.9,
      "Malaysia" = 0.9,
      "Thailand" = 0.9,
      "Philippines" = 0.9
    )
  ) +
  
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 0.40, 0.05),
    limits = c(0, 0.38),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  scale_x_continuous(
    breaks = seq(1970, 2024, 4),
    limits = c(1970, 2034),  
    expand = expansion(mult = c(0.01, 0))
  ) +
  
  # ── Labels ─────────────────────────────────────────────────────────────────
  labs(
    title = "The Rise and Plateau of Manufacturing in East & Southeast Asia",
    subtitle = "Manufacturing share of total value added · Seven major economies, 1970–2024",
    x = NULL,
    y = "Manufacturing share of economy",
    caption = "Note: Shaded areas indicate major economic crises.\nSource: UN National Accounts Main Aggregates Database (constant 2020 USD)"
  ) +
  
  # ── Theme ──────────────────────────────────────────────────────────────────
  theme_minimal(base_family = "sans", base_size = 11.5) +
  theme(
    # Panel & Grid
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    
    # Axis
    axis.line.x        = element_line(color = "grey30", linewidth = 0.5),
    axis.text.x        = element_text(size = 9.5, color = "grey20"),
    axis.text.y        = element_text(size = 9.5, color = "grey20"),
    axis.title.y       = element_text(size = 10, 
                                      margin = margin(r = 10),
                                      color = "grey20"),
    axis.ticks.x       = element_line(color = "grey40", linewidth = 0.3),
    axis.ticks.length  = unit(2, "pt"),
    
    # Legend - hide it since we're using direct labels
    legend.position    = "none",
    
    # Titles
    plot.title         = element_text(
      face = "bold", 
      size = 15,
      color = "grey10",
      margin = margin(b = 4),
      lineheight = 1.1
    ),
    plot.subtitle      = element_text(
      size = 10, 
      color = "grey40",
      margin = margin(b = 15),
      lineheight = 1.2
    ),
    plot.caption       = element_text(
      size = 8, 
      color = "grey50",
      hjust = 0, 
      margin = margin(t = 12),
      lineheight = 1.3
    ),
    
    # Margins
    plot.margin        = margin(15, 15, 15, 15)
  )

# Primary vs Secondary Export
# ── Palette consistent with growth accounting charts ──────────────────────────
col_2000 <- "grey70"
col_2023 <- "#2166ac"

# ── Legend key data ───────────────────────────────────────────────────────────
legend_data <- tibble(
  x     = c(2000, 2023),
  label = c("2000", "2023"),
  color = c(col_2000, col_2023)
)

pt_offset <- 0.8  # in percentage point units

p6 <- trade_data_asea_lall_broad %>%
  filter(flow_desc == "Export",
         ref_year %in% c(2000, 2023)) %>%
  select(ref_year, reporter_desc, broad_category, broad_share) %>%
  pivot_wider(names_from   = ref_year,
              values_from  = broad_share,
              names_prefix = "year_") %>%
  mutate(
    # ── Direction-aware offset ─────────────────────────────────────────────
    direction  = if_else(year_2023 >= year_2000, 1, -1),
    seg_x      = year_2000 + direction * pt_offset,
    seg_xend   = year_2023 - direction * pt_offset
  ) %>%
  
  ggplot(aes(y = reorder(broad_category, year_2023))) +
  
  # ── Connector segment ────────────────────────────────────────────────────────
  geom_segment(
    aes(
      x    = seg_x,
      xend = seg_xend,
      yend = broad_category
    ),
    arrow     = arrow(length = unit(0.18, "cm"), type = "closed"),
    linewidth = 0.65,
    color     = "grey50"
  ) +
  
  # ── Start point (1990) ───────────────────────────────────────────────────────
  geom_point(aes(x = year_2000),
             color = col_2000, size = 3.2, shape = 19) +
  
  # ── End point (2024) ─────────────────────────────────────────────────────────
  geom_point(aes(x = year_2023),
             color = col_2024, size = 3.2, shape = 19) +
  
  scale_color_manual(
    name   = NULL,
    values = c("2000" = col_2000, "2023" = col_2023),
    guide  = guide_legend(override.aes = list(size = 3.5))
  ) +
  
  # ── Facet ────────────────────────────────────────────────────────────────────
  facet_wrap(~ reporter_desc, ncol = 2) +
  
  # ── Axis ─────────────────────────────────────────────────────────────────────
  scale_x_continuous(
    labels = scales::label_number(suffix = "%", accuracy = 1),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  
  # ── Labels ───────────────────────────────────────────────────────────────────
  labs(
    title    = "Structural Export Transformation: 2000 to 2023",
    subtitle = "Arrow direction indicates shift in export share · Grey = 2000 · Blue = 2023",
    x        = "Share of Total Exports (%)",
    y        = NULL,
    caption  = "Source: UN Comtrade via Lall (2000) broad technology classification"
  ) +
  
  # ── Theme ────────────────────────────────────────────────────────────────────
  theme_minimal(base_size = 12) +
  theme(
    # Titles
    plot.title         = element_text(face = "bold", size = 13,
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9, color = "grey40",
                                      margin = margin(b = 8)),
    plot.caption       = element_text(size = 7.5, color = "grey50",
                                      hjust = 0, margin = margin(t = 8)),
    plot.margin        = margin(12, 14, 10, 12),
    
    # Facet strips
    strip.text         = element_text(face = "bold", size = 10.5),
    strip.background   = element_rect(fill = "grey93", color = NA),
    
    # Axes
    axis.text.x        = element_text(size = 8.5, color = "grey30"),
    axis.text.y        = element_text(size = 8.5, color = "grey20"),
    axis.title.x       = element_text(size = 9, margin = margin(t = 8)),
    
    # Grid — keep horizontal only (guides the eye along dumbbells)
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey88",
                                      linetype  = "dotted"),
    panel.grid.major.x = element_line(linewidth = 0.25, color = "grey92"),
    panel.grid.minor   = element_blank(),
    panel.spacing      = unit(1.2, "lines"),
    
    # Legend
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.45, "cm"),
    legend.margin      = margin(t = 2)
  )

# Employment by Industry
emp_inds <- apo_database %>% 
  filter(code %in% c("50100", "50200", "50300", "50400", "50500", "50600", "50700", "50800", "50900"),
         country %in% c("Indonesia", "Malaysia", "China", "Thailand", "Vietnam", "Korea"),
         group == "Employment by industry") %>% 
  pivot_longer(
    cols = matches("[0-9]+"),
    names_to = "year",
    values_to = "values"
  )

# Services (Wholesale + Transport + Financial + social)    
emp_inds_serv <- emp_inds %>% 
  filter(code %in% c("50600", "50700", "50800", "50900")) %>% 
  group_by(country, country_id, year, unit) %>% 
  summarise(group = "Services",
            code = "5555",
            variable = "Services",
            values = sum(values, na.rm = TRUE))

# total employment
emp_inds_tot <- emp_inds %>%
  group_by(country, country_id, year, unit) %>% 
  summarise(total_emp  = sum(values, na.rm = TRUE))


# pull gdp per cap
gdp_cap_asean <- WDI(
  country = c("ID", "MY", "TH", "PH", "CN", "JP", "VN", "KR", "SG"),
  indicator = "NY.GDP.PCAP.KD",
  start = 1970,
  end = 2025,
  extra = TRUE) %>% 
  mutate(iso3c = case_when(iso3c == "MYS" ~ "MAL",
                           iso3c == "VNM" ~ "VIE",
                           .default = iso3c))


# combined employment
emp_inds_final <- emp_inds %>% 
  filter(!code %in% c("50600","50700", "50800", "50900")) %>% 
  bind_rows(emp_inds_serv) %>% 
  arrange(country, variable) %>% 
  ungroup() %>% 
  group_by(country, country_id, year) %>% 
  mutate(check_total = sum(values, na.rm = TRUE)) %>% 
  left_join(emp_inds_tot, by = join_by(country, country_id, year)) %>% 
  select(-unit.y) %>% 
  mutate(share_emp = round((values/check_total * 100), 2)) %>% 
  left_join(gdp_cap_asean %>% 
              select(iso3c, year, NY.GDP.PCAP.KD) %>% 
              rename("gdp_per_cap" = NY.GDP.PCAP.KD) %>% 
              mutate(year = as.character(year)),
            by = join_by(country_id == iso3c, year))


# chart EMP manufacturing, agriculture and service share vs GDP per capita
# Compute per-country scale factors
# Country-specific scale factors (same approach as before)
library(ggh4x)


p7 <- emp_inds_final %>%
  mutate(year = as.numeric(year)) %>% 
  ggplot(aes(x = year)) +
  
  # Crisis shading
  annotate("rect", xmin = 1997, xmax = 1999, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.3) +
  annotate("rect", xmin = 2008, xmax = 2010, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.3) +
  annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.3) +
  
  # Event markers
  geom_vline(data = key_events,
             aes(xintercept = year),
             linetype = "dotted", color = "grey40",
             linewidth = 0.4, alpha = 0.6) +
  geom_text(data = key_events,
            aes(x = year, y = Inf, label = label),
            size = 2.5, color = "grey40", fontface = "italic",
            hjust = 0.5, vjust = 1.5) +
  
  # Employment share lines (left axis)
  geom_line(aes(y = share_emp, colour = variable, group = variable),
            linewidth = 0.8, lineend = "round", linejoin = "round", alpha = 0.9) +
  
  facet_wrap(~ country, scales = "free_y", ncol = 3) +
  
  scale_y_continuous(
    name   = "Employment Share (% of Total)",
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_x_continuous(
    breaks = seq(1970, 2023, by = 10),
    expand = expansion(mult = c(0.01, 0))
  ) +
  
  scale_colour_manual(values = c(
    "#E63946", "#457B9D", "#1D3557",
    "#F4A261", "#2A9D8F", "#E9C46A"
  )) +
  
  labs(
    title    = "Sectoral Employment Share",
    subtitle = "Employment share by sector",
    x        = NULL,
    colour   = "Sector",
    caption  = "Note: Shaded areas indicate major economic crises.\nSource: APO Database 2025"
  ) +
  
  theme_minimal(base_family = "sans", base_size = 11.5) +
  theme(
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white",   color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.5),
    axis.text.x        = element_text(size = 8, color = "grey20",
                                      angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 8, color = "grey20"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 10),
                                      color = "grey20"),
    axis.ticks.x       = element_line(color = "grey40", linewidth = 0.3),
    axis.ticks.length  = unit(2, "pt"),
    strip.text         = element_text(face = "bold", size = 10, color = "grey10"),
    strip.background   = element_rect(fill = "#FAFAFA", color = NA),
    legend.position    = "bottom",
    legend.title       = element_text(size = 9,  color = "grey20"),
    legend.text        = element_text(size = 8.5, color = "grey20"),
    plot.title         = element_text(face = "bold", size = 15, color = "grey10",
                                      margin = margin(b = 4), lineheight = 1.1),
    plot.subtitle      = element_text(size = 10, color = "grey40",
                                      margin = margin(b = 15), lineheight = 1.2),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.3),
    plot.margin        = margin(15, 15, 15, 15)
  )

# Manufacturing export 1990-2005
trade_data_asea_lall_broad %>% 
  filter(flow_desc == "Export") %>% 
  ggplot(aes(x = ref_year, y = total_broad, colour = broad_category, group = broad_category)) +
  geom_line()


p8 <- trade_data_asean %>% 
  mutate(sitc1 = substr(sitc3,start = 1, stop = 1),
         sitc2 = substr(sitc3,start = 1, stop = 2)) %>% 
  filter(sitc1 %in% c("5", "6", "7", "8")) %>% 
  filter(sitc2 != "68") %>%
  group_by(ref_year, flow_desc) %>% 
  summarise(manufacturing_value = sum(primary_value, na.rm = TRUE)/1e9) %>% 
  ungroup() %>% 
  arrange(desc(flow_desc)) %>% 
  mutate(yoy_growth = ((manufacturing_value - lag(manufacturing_value))/lag(manufacturing_value)) * 100) %>% 
  filter(ref_year <= 2005 & ref_year >= 1990) %>% 
  ggplot(aes(x = ref_year, y = yoy_growth, colour = flow_desc)) +
  geom_line(linewidth = 0.8, lineend = "round", linejoin = "round", alpha = 0.9) +
  # Crisis shading
  annotate("rect", xmin = 1997, xmax = 1999, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.3) +
  annotate("text", x = 1997, y = Inf, label = "Asian Financial Crisis",
           size = 2.5, color = "grey40", fontface = "italic",
           hjust = -0.05, vjust = 1.5) +
  # Event markers
  geom_vline(aes(xintercept = 1997),
             linetype = "dotted", color = "grey40",
             linewidth = 0.4, alpha = 0.6) +
  
  scale_y_continuous(
    name   = "YoY Growth Rate %",
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_x_continuous(
    breaks = seq(1990, 2005, by = 1),
    expand = expansion(mult = c(0.01, 0))
  ) + 
  
  scale_colour_manual(values = c(
    "#1D3557", "#E63946"
  )) +
  
  labs(
    title    = "Manufactures Exports & Imports",
    subtitle = "Year-on-Year Growth Rate: 1990-2005",
    x        = NULL,
    colour   = "Trade Direction",
    caption  = "Note: Shaded areas indicate Asian Financial Crisis.\nSource: Author Calculation, UN Comtrade"
  ) + 

  theme_minimal(base_family = "sans", base_size = 11.5) +
  theme(
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white",   color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.5),
    axis.text.x        = element_text(size = 8, color = "grey20",
                                      angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 8, color = "grey20"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 10),
                                      color = "grey20"),
    axis.ticks.x       = element_line(color = "grey40", linewidth = 0.3),
    axis.ticks.length  = unit(2, "pt"),
    strip.text         = element_text(face = "bold", size = 10, color = "grey10"),
    strip.background   = element_rect(fill = "#FAFAFA", color = NA),
    legend.position    = "bottom",
    legend.title       = element_text(size = 9,  color = "grey20"),
    legend.text        = element_text(size = 8.5, color = "grey20"),
    plot.title         = element_text(face = "bold", size = 15, color = "grey10",
                                      margin = margin(b = 4), lineheight = 1.1),
    plot.subtitle      = element_text(size = 10, color = "grey40",
                                      margin = margin(b = 15), lineheight = 1.2),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.3),
    plot.margin        = margin(15, 15, 15, 15)
  )


# Exchange rate
p10 <- imf_data_exchange %>% 
  mutate(year = str_extract(time_period, "[0-9]{4}"),
         month = str_extract(time_period, "M[0-9]{2}"),
         period = lubridate::ym(paste0(year,"-",month)),
         obs_value = as.numeric(obs_value)) %>% 
  filter(indicator == "XDC_USD" & country == "IDN" & period <= "2005-01-01") %>% 
  ungroup() %>% 
  ggplot(aes(x = period, y = obs_value)) +
  geom_line(linewidth = 0.8, lineend = "round", linejoin = "round",
            colour = "#1D3557", alpha = 0.9) +
  # Crisis shading
  annotate("rect", xmin = as.Date("1997-01-01"), xmax = as.Date("1999-01-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.3) +
  annotate("text", x = as.Date("1997-01-01"), y = Inf, label = "AFC 97-99",
           size = 2.2, color = "grey40", fontface = "italic",
           hjust = -0.1, vjust = 1.5) +
  # Event markers
  geom_vline(xintercept = as.Date("1997-01-01"),
             linetype = "dotted", color = "grey40",
             linewidth = 0.4, alpha = 0.6) +
  
  scale_y_continuous(
    name   = "Rupiah per US Dollar",
    breaks = seq(0, 18000, by = 2000),
    labels = scales::comma
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0))
  ) + 
  
  labs(
    title    = "Indonesian Rupiah Exchange Rate",
    subtitle = "Rupiah per US Dollar, Monthly: pre-2006",
    x        = NULL,
    caption  = "\nSource: IMF Exchange Rate Statistics"
  ) + 
  theme_minimal(base_family = "sans", base_size = 11.5) +
  theme(
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white",   color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.5),
    axis.text.x        = element_text(size = 8, color = "grey20",
                                      angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 8, color = "grey20"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 10),
                                      color = "grey20"),
    axis.ticks.x       = element_line(color = "grey40", linewidth = 0.3),
    axis.ticks.length  = unit(2, "pt"),
    strip.text         = element_text(face = "bold", size = 10, color = "grey10"),
    strip.background   = element_rect(fill = "#FAFAFA", color = NA),
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 12, color = "grey10",
                                      margin = margin(b = 4), lineheight = 1.1),
    plot.subtitle      = element_text(size = 10, color = "grey40",
                                      margin = margin(b = 15), lineheight = 1.2),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.3),
    plot.margin        = margin(15, 15, 15, 15)
  )

# Unemployment & GDP (two y axis)
df_plot_imf_unemp <- imf_data_unemp %>%
  select(-indicator) %>% 
  mutate(obs_value = as.numeric(obs_value),
         time_period = ym(paste0(time_period, "-01"))) %>% 
  pivot_wider(names_from = "indicator_name", values_from = "obs_value")

p11 <- df_plot_imf_unemp %>% 
  filter(country == "IDN" & time_period <= "2005-01-01") %>% 
  pivot_longer(cols = c(`Unemployment rate`, `Real GDP YoY`),
               names_to = "series", values_to = "value") %>% 
  ggplot(aes(x = time_period, y = value, colour = series)) +
  geom_line(linewidth = 0.8, lineend = "round", linejoin = "round", alpha = 0.9) +
  
  annotate("rect", xmin = as.Date("1997-01-01"), xmax = as.Date("1999-01-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.3) +
  annotate("text", x = as.Date("1997-01-01"), y = Inf, label = "AFC 97-99",
           size = 2.2, color = "grey40", fontface = "italic",
           hjust = -0.08, vjust = 1.5) +
  
  geom_vline(xintercept = as.Date("1997-01-01"),
             linetype = "dotted", color = "grey40",
             linewidth = 0.4, alpha = 0.6) +
  
  scale_y_continuous(
    name   = "Percent (%)",
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0))
  ) +
  
  scale_colour_manual(values = c(
    "Unemployment rate" = "#1D3557",
    "Real GDP YoY"      = "#E63946"
  )) +
  
  labs(
    title    = "Unemployment Rate & Real GDP Growth",
    subtitle = "Year-on-Year, 1993-2005",
    x        = NULL,
    colour   = NULL,
    caption  = "\nSource: IMF World Economic Outlook"
  ) +
  theme_minimal(base_family = "sans", base_size = 11.5) +
  theme(
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white",   color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.5),
    axis.text.x        = element_text(size = 8, color = "grey20",
                                      angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 8, color = "grey20"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 10),
                                      color = "grey20"),
    axis.ticks.x       = element_line(color = "grey40", linewidth = 0.3),
    axis.ticks.length  = unit(2, "pt"),
    legend.position    = "bottom",
    legend.title       = element_text(size = 9,  color = "grey20"),
    legend.text        = element_text(size = 8.5, color = "grey20"),
    plot.title         = element_text(face = "bold", size = 12, color = "grey10",
                                      margin = margin(b = 4), lineheight = 1.1),
    plot.subtitle      = element_text(size = 10, color = "grey40",
                                      margin = margin(b = 15), lineheight = 1.2),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.3),
    plot.margin        = margin(15, 15, 15, 15)
  )

# inflation
p12 <- imf_data_inflation %>%
  mutate(obs_value = as.numeric(obs_value),
         time_period = ym(paste0(time_period, "-01"))) %>% 
  filter(country == "IDN" & time_period <= "2005-01-01") %>% 
  ggplot(aes(x = time_period, y = obs_value)) +
  geom_line(linewidth = 0.8, lineend = "round", linejoin = "round",
            colour = "#1D3557", alpha = 0.9) +
  # Crisis shading
  annotate("rect", xmin = as.Date("1997-01-01"), xmax = as.Date("1999-01-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.3) +
  annotate("text", x = as.Date("1997-01-01"), y = Inf, label = "AFC 97-99",
           size = 2.2, color = "grey40", fontface = "italic",
           hjust = -0.05, vjust = 1.5) +
  # Event markers
  geom_vline(xintercept = as.Date("1997-01-01"),
             linetype = "dotted", color = "grey40",
             linewidth = 0.4, alpha = 0.6) +
  
  scale_y_continuous(
    name   = "YoY Change %",
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0))
  ) + 
  
  labs(
    title    = "Inflation Rate",
    subtitle = "YoY Change %, 1993-2005",
    x        = NULL,
    caption  = "Note: Shaded area indicates Asian Financial Crisis.\nSource: IMF CPI Statistics"
  ) + 
  theme_minimal(base_family = "sans", base_size = 11.5) +
  theme(
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white",   color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.5),
    axis.text.x        = element_text(size = 8, color = "grey20",
                                      angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 8, color = "grey20"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 10),
                                      color = "grey20"),
    axis.ticks.x       = element_line(color = "grey40", linewidth = 0.3),
    axis.ticks.length  = unit(2, "pt"),
    strip.text         = element_text(face = "bold", size = 10, color = "grey10"),
    strip.background   = element_rect(fill = "#FAFAFA", color = NA),
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 12, color = "grey10",
                                      margin = margin(b = 4), lineheight = 1.1),
    plot.subtitle      = element_text(size = 10, color = "grey40",
                                      margin = margin(b = 15), lineheight = 1.2),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.3),
    plot.margin        = margin(15, 15, 15, 15)
  )
  
# FDI net inflow

p13 <- sector_contr_gdp %>%
  filter(iso3c == "IDN") %>% 
  select(iso3c, year, BX.KLT.DINV.CD.WD) %>% 
  rename("FDI Net Inflow" = BX.KLT.DINV.CD.WD) %>% 
  mutate(`FDI Net Inflow` = `FDI Net Inflow`/1e9,
         year = ym(paste0(year, "-01")),
         sign = ifelse(`FDI Net Inflow` >= 0, "Positive", "Negative")) %>% 
  drop_na() %>% 
  filter(year <= "2005-01-01" & year >= "1990-01-01") %>% 
  ggplot(aes(x = year, y = `FDI Net Inflow`, fill = sign)) +
  geom_col(width = 200, alpha = 0.9) +
  # Crisis shading
  annotate("rect", xmin = as.Date("1996-09-25"), xmax = as.Date("1999-05-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.3) +
  annotate("text", x = as.Date("1996-10-01"), y = Inf, label = "AFC 97-99",
           size = 2.2, color = "grey40", fontface = "italic",
           hjust = -0.05, vjust = 1.5) +
  # Event markers
  geom_vline(xintercept = as.Date("1996-09-25"),
             linetype = "dotted", color = "grey40",
             linewidth = 0.4, alpha = 0.6) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.5, linetype = "dashed") +
  
  scale_y_continuous(
    name   = "Millions US Dollar",
    breaks = seq(-5, 10, by = 3),
    labels = function(x) paste0("$", x, "M")
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0))
  ) + 
  
  scale_fill_manual(values = c("Positive" = "#1D3557", "Negative" = "#E63946")) +
  
  labs(
    title    = "FDI Net Inflow",
    subtitle = "Millions US Dollar, 1993-2005",
    x        = NULL,
    caption  = "\nSource: World Bank, IMF Balance of Payment Statistics"
  ) + 
  theme_minimal(base_family = "sans", base_size = 11.5) +
  theme(
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white",   color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.5),
    axis.text.x        = element_text(size = 8, color = "grey20",
                                      angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 8, color = "grey20"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 10),
                                      color = "grey20"),
    axis.ticks.x       = element_line(color = "grey40", linewidth = 0.3),
    axis.ticks.length  = unit(2, "pt"),
    strip.text         = element_text(face = "bold", size = 10, color = "grey10"),
    strip.background   = element_rect(fill = "#FAFAFA", color = NA),
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 12, color = "grey10",
                                      margin = margin(b = 4), lineheight = 1.1),
    plot.subtitle      = element_text(size = 10, color = "grey40",
                                      margin = margin(b = 15), lineheight = 1.2),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.3),
    plot.margin        = margin(15, 15, 15, 15)
  )

# employment 1997-999
p14 <- emp_inds_final %>% 
  filter(country == "Indonesia") %>% 
  group_by(variable) %>% 
  arrange(year, .by_group = TRUE) %>% 
  mutate(share_change_pp = share_emp - lag(share_emp),
         year = ymd(paste0(year,"-01-01"))) %>% 
  ungroup() %>% 
  filter(year >= "1993-01-01", year <= "2005-01-01") %>% 
  ggplot(aes(x = year, y = share_change_pp, fill = variable)) +
  geom_col(position = "stack", width = 290, alpha = 0.9) +
  
  # Crisis shading
  annotate("rect", xmin = as.Date("1996-08-01"), xmax = as.Date("1999-06-01"),
           ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.3) +
  annotate("text", x = as.Date("1996-08-01"), y = Inf, label = "AFC 97-99",
           size = 2.2, color = "grey40", fontface = "italic",
           hjust = -0.05, vjust = 1.5) +
  geom_vline(xintercept = as.Date("1996-08-01"),
             linetype = "dotted", color = "grey40",
             linewidth = 0.4, alpha = 0.6) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.5) +
  
  scale_fill_manual(values = c(
    "#1D3557", "#E63946", "#2A9D8F", "#E9C46A", "#6D597A", "#457B9D"
  )) +
  
  scale_y_continuous(
    name   = "Change in employment share (pp)",
    breaks = seq(-6, 6, by = 1.5),
    labels = function(x) paste0(x, "pp")
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0))
  ) + 
  
  labs(
    title    = "Change in Employment Share by Sector",
    subtitle = "Year-on-Year, Percentage Points: 1993-2005",
    x        = NULL,
    fill     = NULL,
    caption  = "Note: Shaded area indicates Asian Financial Crisis.\nSource: Author Calculation, APO Database"
  ) + 
  theme_minimal(base_family = "sans", base_size = 11.5) +
  theme(
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white",   color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.5),
    axis.text.x        = element_text(size = 8, color = "grey20",
                                      angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 8, color = "grey20"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 10),
                                      color = "grey20"),
    axis.ticks.x       = element_line(color = "grey40", linewidth = 0.3),
    axis.ticks.length  = unit(2, "pt"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 8.5, color = "grey20"),
    plot.title         = element_text(face = "bold", size = 15, color = "grey10",
                                      margin = margin(b = 4), lineheight = 1.1),
    plot.subtitle      = element_text(size = 10, color = "grey40",
                                      margin = margin(b = 15), lineheight = 1.2),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.3),
    plot.margin        = margin(15, 15, 15, 15)
  )

