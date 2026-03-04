#### LOAD LIBRARIES ####
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(auk)
library(usethis)

#### SET EBIRD DATA PATH
#determine current path for awk
auk_get_awk_path()

#if path is not: C:\cygwin64\bin\gawk.exe then set it here
auk_set_awk_path("C:/cygwin64/bin/gawk.exe", overwrite=FALSE)

# set ebd path
auk_set_ebd_path("data/ebird/")

usethis::edit_r_environ()

#### DATA EXTRACTION WITH AUK ####
# resolve namespace conflicts
select <- dplyr::select

# setup data directory
dir.create("data", showWarnings = FALSE)

# tell R where the files for the sampling and EBD are held in the /data/ebird folder
ebd <- auk_ebd("ebd_US-CO_200901_202412_smp_relFeb-2025.txt", 
               file_sampling = "ebd_US-CO_200901_202412_smp_relFeb-2025_sampling.txt")

# filtering data to only complete checklists
years <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
species <- c("Melospiza melodia", "Passerculus sandwichensis", "Agelaius phoeniceus", "Corvus corax", "Molothrus ater", "Ardea ibis")

ebd_filters <- ebd %>%
  auk_complete() %>%
  auk_year(year=years) %>%
  auk_species(species) 

# quality control step to look at filters we selected
ebd_filters

# apply the filter to the dataset - this can take a long time (hours)
# output files
data_dir <- "data/ebird"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
f_ebd <- file.path(data_dir, "ebd_co_2009-2024.txt")
f_sampling <- file.path(data_dir, "ebd_co_2009-2024_sampling.txt")

# only run if the files don't already exist
if (!file.exists(f_ebd)) {
  auk_filter(ebd_filters, file = f_ebd, file_sampling = f_sampling)
}

###https://github.com/CornellLabofOrnithology/auk/issues/37
## https://cornelllabofornithology.github.io/ebird-best-practices/ebird.html
## https://cornelllabofornithology.github.io/auk/reference/auk_set_ebd_path.html 


system.file("ebd_US-CO_200901_202412_smp_relFeb-2025.txt", package = "auk") %>% 
  read_ebd() %>% 
  glimpse()

ebirddf <- read.table("data/ebird/ebd_US-CO_200901_202412_smp_relFeb-2025_sampling.txt", sep="\t", header=T)
