# Indonesia Premature Deindustrialisation
# Blog 1
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

# WDI data
wdi_data <- WDI(country = c("ID"),
                indicator = c("NV.IND.MANF.ZS",
                              "NV.IND.MANF.KD.ZG",
                              "TX.VAL.MANF.ZS.UN",
                              "TX.MNF.TECH.ZS.UN",
                              ))
  