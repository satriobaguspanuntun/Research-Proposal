library(tidyverse)
library(openxlsx)
library(readxl)

unsd_na_path <- "~/Research-Proposal/data/Download-GDPconstant-USD-countries.xlsx"

unsd_na_data <-  openxlsx::read.xlsx(xlsxFile = unsd_na_path, sheet = 1)

cols_unsd <- tolower(unname(unlist(unsd_na_data[1, ])))

colnames(unsd_na_data) <- cols_unsd

unsd_na_data <- unsd_na_data[2:nrow(unsd_na_data), ]
