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


# ── 1. Better color palette ──────────────────────────────────────────────────
# Option A: Highlight Indonesia, mute others
country_colors <- c(
  "Indonesia"         = "#E63946",  # Bold red - your focus
  "Vietnam"           = "#457B9D",  # Muted blue
  "Republic of Korea" = "#1D3557",  # Dark blue
  "China"             = "#F4A261",  # Warm orange
  "Malaysia"          = "#2A9D8F",  # Teal
  "Thailand"          = "#E9C46A",  # Gold
  "Philippines"       = "#A8DADC"   # Light blue
)

# Option B: Categorical palette (if no country is focus)
# country_colors <- setNames(
#   c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51", "#E63946", "#457B9D"),
#   c("Indonesia", "Vietnam", "Republic of Korea", "China", "Malaysia", "Thailand", "Philippines")
# )

# ── 2. Prepare data ──────────────────────────────────────────────────────────
plot_data <- unsd_na_data %>% 
  filter(country %in% c("Indonesia", "Vietnam", "Republic of Korea", "China", 
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
      "Vietnam" = 0.9,
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
    breaks = seq(1970, 2024, 5),
    limits = c(1970, 2030),  # Extra space for labels
    expand = expansion(mult = c(0.01, 0))
  ) +
  
  # ── Labels ─────────────────────────────────────────────────────────────────
  labs(
    title = "The Rise and Plateau of Manufacturing in East & Southeast Asia",
    subtitle = "Manufacturing share of total value added · Seven major economies, 1970–2023",
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


# Medium-and-high technology manufactured export, Manufactured Export, and commodity export (Highly likely that some of the commodity export is being part of manufactured export. May need to redefine the definition of Manufactured export.)

# Primary vs Secondary Export

# Manufacturing export intensity chart










