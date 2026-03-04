#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(reticulate)


#### FUNCTIONS ####


#### Import census tract population center data from US Census 2020 ####
#### LOAD DATA ####
co_CT_info <- read_excel("data/references/Census Tracts 2020 Population Centers.xlsx")

#convert CTCode to numeric
co_CT_info$CTCode <- as.numeric(co_CT_info$CTCode)

co_CT_info <- co_CT_info %>%
  mutate(
    # Convert to CHARACTER first to handle existing padding issues, then pad with zeros
    state_p = str_pad(as.character(STATEFP), 2, pad = "0"),
    county_p = str_pad(as.character(COUNTYFP), 3, pad = "0"),
    tract_p = str_pad(as.character(TRACTCE), 6, pad = "0"),
    # Concatenate
    geoid_11 = str_c(state_p, county_p, tract_p)
  )
