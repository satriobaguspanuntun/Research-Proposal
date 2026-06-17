# World Bank Data
library(tidyverse)
library(ggplot2)
library(WDI)
library(tidyplots)

# Agriculture % GDP
# Manufacturing % GDP
# Services % GDP

sector_contr_gdp <- WDI(
  country = c("ID", "MY", "TH", "PH", "CN", "JP", "VN", "KR", "SG"),
  indicator = c(
    # Agriculture % GDP
    "NV.AGR.TOTL.ZS",
    # Manufacturing % GDP
    "NV.IND.MANF.ZS",
    # Services % GDP
    "NV.SRV.TOTL.ZS",
    # Mineral rents % GDP
    "NY.GDP.MINR.RT.ZS",
    # Coal rents % GDP
    "NY.GDP.COAL.RT.ZS",
    # Oil rents % GDP
    "NY.GDP.PETR.RT.ZS",
    # Gas rents % GDP
    "NY.GDP.NGAS.RT.ZS",
    # Forest rents % GDP
    "NY.GDP.FRST.RT.ZS",
    # Natural resources rents
    "NY.GDP.TOTL.RT.ZS",
    # Manufacturing exports
    "TX.VAL.MANF.ZS.UN",
    # FDI Inflow 
    "BX.KLT.DINV.CD.WD"
  ),
  start = 1970,
  end = 2025,
  extra = TRUE) %>% 
  arrange(country, year)

indo_data <- sector_contr_gdp %>% 
  filter(country == "Indonesia") %>% 
  select(country, iso2c, iso3c, year,matches("NY")) %>% 
  pivot_longer(cols = starts_with("NY"),
               names_to = "var",
               values_to = "value"
               ) %>% 
  mutate(
    var = case_when(
      var == "NY.GDP.MINR.RT.ZS" ~ "mineral_rent",
      var == "NY.GDP.COAL.RT.ZS" ~ "coal_rent",
      var == "NY.GDP.PETR.RT.ZS" ~ "oil_rent",
      var == "NY.GDP.NGAS.RT.ZS" ~ "gas_rent",
      var == "NY.GDP.FRST.RT.ZS" ~ "forest_rent",
      var == "NY.GDP.TOTL.RT.ZS" ~ "resource_rent_total",
      .default = NA
  ),
  value = round(value, digits = 4)
  ) %>% 
  drop_na()

# rents chart
# total rents
indo_tot <- indo_data %>% 
  pivot_wider(names_from = var, values_from = value) %>% 
  drop_na() %>% 
  relocate(-resource_rent_total) %>% 
  rowwise() %>% 
  mutate(total_check = sum(c_across(mineral_rent:coal_rent))) %>% 
  ungroup() %>% 
  select(country, iso2c, iso3c, resource_rent_total)

p8 <- indo_data %>%
  filter(var != "resource_rent_total") %>%
  mutate( 
    var = case_when(
    var == "mineral_rent" ~ "Mineral Rent",
    var == "coal_rent" ~ "Coal Rent",
    var == "oil_rent" ~ "Oil Rent",
    var == "gas_rent" ~ "Gas Rent",
    var == "forest_rent" ~ "Forest Rent",
    .default = NA
  ),
  value = value / 100) %>% 
  ggplot(aes(x = year, y = value, fill = var)) +
  geom_col() +                          
  labs(
    title = "Indonesia Resource Rents",
    subtitle = "Resource Rents by Type, 1970-2021",
    x     = "Year",
    y     = "% of GDP",
    fill  = "Resource Rents",
    caption = "Source: World Development Indicator, World Bank"
  ) +
  scale_y_continuous(
    labels = scales::percent) +
  scale_fill_manual(values = c(
    "#E63946", "#457B9D", "#1D3557",
    "#F4A261", "#2A9D8F", "#E9C46A"
  )) +
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

# agri, manu, serv % of GDP
indo_data_gdp <- sector_contr_gdp %>% 
  filter(country == "Indonesia") %>% 
  select(country, iso2c, iso3c, year,matches("NV")) %>% 
  pivot_longer(cols = starts_with("NV"),
               names_to = "var",
               values_to = "value"
  ) %>% 
  mutate(
    value = round(value, digits = 1)/100,
    var = case_when(var == "NV.AGR.TOTL.ZS" ~ "Agriculture",
                    var == "NV.IND.MANF.ZS" ~ "Manufacturing",
                    var == "NV.SRV.TOTL.ZS" ~ "Services")
  ) %>% 
  drop_na()


key_events <- tribble(
  ~year, ~label,
  1997,  "Asian Crisis",
  2008,  "GFC",
  2020,  "COVID-19"
)

p9 <- indo_data_gdp %>% 
  ggplot(aes(x = year, y = value, color = var)) +
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
  
  geom_line(linewidth = 0.8, lineend = "round", linejoin = "round", alpha = 0.9) +
  scale_x_continuous(
    breaks = seq(1983, 2024, by = 4),
    expand = expansion(mult = c(0.01, 0))
  ) +
  scale_y_continuous(
    name   = "% of GDP",
    labels = scales::percent,
    limits = c(0, 0.5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Indonesia Sector Shares % GDP",
    subtitle = "Agriculture, Manufacturing, and Services: 1983-2024",
    x     = NULL,
    y     = "% of GDP",
    fill = NULL,
    caption = "Source: World Development Indicator, World Bank"
  ) +
  scale_colour_manual(values = c(
    "#E63946", "#457B9D", "#E9C46A"
  )) +
  theme_minimal(base_family = "sans", base_size = 11.5) +
  theme(
    panel.background   = element_rect(fill = "#FAFAFA", color = NA),
    plot.background    = element_rect(fill = "white",   color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "white"),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.5),
    axis.text.x        = element_text(size = 8, color = "grey20"),
    axis.text.y        = element_text(size = 8, color = "grey20"),
    axis.title.y       = element_text(size = 10, margin = margin(r = 10),
                                      color = "grey20"),
    axis.ticks.x       = element_line(color = "grey40", linewidth = 0.3),
    axis.ticks.length  = unit(2, "pt"),
    strip.text         = element_text(face = "bold", size = 10, color = "grey10"),
    strip.background   = element_rect(fill = "#FAFAFA", color = NA),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 8.5, color = "grey20"),
    plot.title         = element_text(face = "bold", size = 15, color = "grey10",
                                      margin = margin(b = 4), lineheight = 1.1),
    plot.subtitle      = element_text(size = 10, color = "grey40",
                                      margin = margin(b = 15), lineheight = 1.2),
    plot.caption       = element_text(size = 8, color = "grey50", hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.3),
    plot.margin        = margin(15, 15, 15, 15)
  )

p9
