# World Bank Data
library(tidyverse)
library(ggplot2)
library(WDI)

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
    "NY.GDP.TOTL.RT.ZS"
  ),
  start = 1970,
  end = 2025,
  extra = TRUE) %>% 
  arrange(country, year)


