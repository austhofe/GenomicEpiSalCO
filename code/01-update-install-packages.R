#### UPDATE R IF NEEDED #### 
install.packages("installr")

library(installr)

updateR()

#### Add in packages #### 
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("lubridate")
install.packages("udunits2")
install.packages("readxl")
install.packages("purrr")
install.packages("readr")
install.packages("reticulate")
install.packages("usethis")
install.packages("ggridges")
install.packages("tmap")
install.packages("tidycensus")

#for using eBird data
install.packages("auk")
install.packages("remotes")
remotes::install_github("mstrimas/ebppackages")

#### Command line installations ####
# From the terminal in R studio run: pip install pandas
# Then you may need to update, so run: python.exe -m pip install --upgrade pip
