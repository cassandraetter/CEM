library(readxl)
library(RColorBrewer)
library(tmap)
library(dplyr)
library(tidyr)
library(magrittr)
library(colorspace)
library(countrycode)
library(reactable)
library(htmltools)

###create long dataframe###

cem_initiatives <- read_xlsx("~/CEM/CEM Matrix/2021 June Matrix.xlsx", "Initiatives") %>%
  pivot_longer(!(iso2:iso3), names_to = "workstream", values_to = "participation")

cem_campaigns <- read_xlsx("~/CEM/CEM Matrix/2021 June Matrix.xlsx", "Campaigns") %>%
  pivot_longer(!(iso2:iso3), names_to = "workstream", values_to = "participation")

sector <- read_xlsx("~/CEM/CEM Matrix/2021 June Matrix.xlsx", "Sector") 

cem <- c("BR", "CA", "CL", "CN", "DK", "FI", "FR", "DE", "EU","ID", "IN", "IT", "JP", "KR", "MX", "NL",
         "NZ", "NO", "PL", "PT", "RU", "SA", "ES", "ZA", "SE", "AE", "GB", "US")
workstream <- rbind(cem_initiatives, cem_campaigns) %>%
  filter(iso2 %in% cem  )

workstream$sector <- sector$sector[match(workstream$workstream, sector$workstream)]

