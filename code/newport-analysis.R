#### LOAD LIBRARIES ####
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)
library(reticulate)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(tidyr)
library(ggridges)
library(tmap)
library(tidycensus)

# Get only the newport cases in a df
newport <- health_clean %>%
  filter(sistr_serotype == "Newport")

newport_CO <- newport %>%
  left_join(co_CT_info, by = "geoid_11") 


# count the number of cases by censustract
newport_counts_CT <- newport_CO %>%
  group_by(geoid_11) %>%
  summarise(n = n(), .groups = "drop")

#getting census information and map for CO by censustracts 
CO_race <- get_decennial(
  geography = "tract",
  state = "CO",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))

#histogram shows the majority of CTs have Black populations below 20%
hist(CO_race$percent)

#chloropleth map of %black by CT in CO 2020
tm_shape(CO_race) + 
  tm_polygons(col = "percent",
              style = "quantile",
              n = 5,
              palette = "Purples",
              title = "Percent Black\nby Census tract\n2020 US Census") + 
  tm_layout(frame = FALSE,
            legend.outside = TRUE)



#### MERGE IN NEWPORT DATA AND MAP
newport_CO_map <- CO_race %>%
  left_join(newport_counts_CT, join_by(GEOID == geoid_11)) %>%
  mutate(ir = 100000 * (n / summary_value))

#chloropleth map 
tm_shape(newport_CO_map) + 
  tm_polygons(col = "n",
              style = "pretty",
              palette = "Greens",
              title = "Number Newport Cases\nby Census tract") + 
  tm_layout(frame = FALSE,
            legend.outside = TRUE)

#chloropleth map of ir
tm_shape(newport_CO_map) + 
  tm_polygons(col = "ir",
              style = "pretty",
              palette = "Greens",
              title = "Incidence of Newport\nby Census tract") + 
  tm_layout(frame = FALSE,
            legend.outside = TRUE)

### Not a lot of cases, so let's do it by county
newport_counts_county <- newport_CO %>%
  group_by(fips) %>%
  summarise(n = n(), .groups = "drop") 

CO_county <- get_decennial(
  geography = "county",
  state = "CO",
  variables = c(
    all = "P2_001N"),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE)

newport_CO_map_county <- CO_county %>%
  left_join(newport_counts_county, join_by(GEOID == fips)) %>%
  mutate(ir = 100000 * (n / summary_value))

#chloropleth map 
tm_shape(newport_CO_map_county) + 
  tm_polygons(col = "ir",
              style = "pretty",
              palette = "Greens",
              title = "Incidence of Newport\nby County") + 
  tm_layout(frame = FALSE,
            legend.outside = TRUE)


### Look at IR by year
newport_counts_county_year <- newport_CO %>%
  mutate(
    report_date = mdy(coalesce(first_reported_ph_date, event_onset_date)),
    year = year(report_date)) %>%
  group_by(fips, year) %>%
  summarise(n = n(), .groups = "drop") 

newport_CO_map_county_year <- CO_county %>%
  inner_join(newport_counts_county_year, join_by(GEOID == fips)) %>%
  mutate(ir = 100000 * (n / summary_value))

#chloropleth map 
tm_shape(newport_CO_map_county_year) + 
  tm_polygons(col = "ir",
              style = "pretty",
              palette = "Greens",
              title = "Incidence of Newport\nby County 2018-2024") + 
  tm_layout(frame = FALSE,
            legend.outside = TRUE) +
  tm_facets(by = "year", ncol =3)
