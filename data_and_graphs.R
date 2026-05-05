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
  start = 1980, end = 2025,
  extra = TRUE
) %>% 
  rename("mva_gdp" = NV.IND.MANF.ZS,
         "mva_growth" = NV.IND.MANF.KD.ZG,
         "manuf_exp" = TX.VAL.MANF.ZS.UN,
         "med_high_manuf_exp" = TX.MNF.TECH.ZS.UN)

# productivity data (Penn world table)
productivity <- haven::read_dta(file = "~/Research-Proposal/data/pwt110.dta")

# productivity ASEAN 5 + China + Japan + Korea
asean_5 <- c("Indonesia", "Thailand", "Malaysia", "Philippines", "Vietnam", "Japan", "China", "Republic of Korea")

prod_asean_5 <- productivity %>% filter(country %in% asean_5)

prod_aseas_5_desc <- data.frame(column_names = names(prod_asean_5),
                                column_description = sapply(prod_asean_5,
                                                            function(x) ifelse(is.null(attr(x, "label")), 
                                                                               "No description", attr(x, "label"))))

# Real GDP productivity composition by Capital, Labour and TFP 
# Solow Growth Decomposition: ΔlnY = α·ΔlnK + (1−α)·ΔlnL + ΔTFP
# where  L = emp × avh × hc 

# Indonesia



