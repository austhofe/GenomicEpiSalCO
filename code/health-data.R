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

#### LOAD DATA ####
dua <- read_csv("data/health/DUA_Erika_Austhof_Genomics_Salmonella_CO_20250703.csv")
ncbi <- read_csv("data/health/PathDetect_Info_all.csv")
seroseq <- read_excel("data/health/serotypes_seroseq.xlsx")
sistr <- read_excel("data/health/serotypes_sistr.xlsx")
accessions <- read_excel("data/health/accession_numbers.xlsx")


#### MERGE SAMPLE INFORMATION ####
# reshape dua to long format for matching
# Note 2/6/2026: In the original data request there are 11,697 observations. CO started sequencing and inputting into NCBI consistently in 2018. So, only 4,148 Pulse Net IDs are available for pulling from NCBI (35% of data) 
dua_long <- dua %>%
  mutate(dua_row_id = row_number()) %>%
  pivot_longer(
    cols = c(PNUSA_ID_1, PNUSA_ID_2, PNUSA_ID_3, PNUSA_ID_4),
    names_to = "PNUSA_slot",
    values_to = "Strain"
  ) %>%
  filter(!is.na(Strain))

# merge with ncbi on PNUSA ID
merged_long <- dua_long %>%
  left_join(ncbi, by = "Strain")

# collapse back to one row per original dua record
merged_dua_ncbi <- merged_long %>%
  group_by(dua_row_id) %>%
  summarise(
    across(
      .cols = -c(PNUSA_slot, Strain),
      .fns = ~ first(.x)
    ),
    .groups = "drop"
  ) 


# identify missing PulseNet IDs not identified by NCBI
# Note 2/6/2026:  Verified these IDs are not in NCBI, n=86. There were also 4 samples with the same 2 PNUSA IDs (2 duplicate rows). The remaining 27 observations collapse to one single row (have >=1 PNUSA ID) but the same information in NCBI.
missingPNUSAIDs <- merged_long %>%
  group_by(dua_row_id) %>%
  summarise(
    n_ids = n(),
    n_matches = sum(!is.na(Isolate)),
    Strain
  ) %>%
  filter(n_matches == 0)
# EXPORTING DATA FRAME
write.csv(missingPNUSAIDs, "data\\missingPNUSAIDs.csv", row.names = T)

duplicate_id_counts <- dua_long %>%
  count(Strain, name = "n") %>%
  filter(n > 1)



#### SEROTYPE ANALYSIS ####
# Note 2/6/2026: With the information from NCBI (n=4,031), 3,914 had accession numbers. with those accessions I ran the code in Genome_Data.Rmd to pull data off ncbi using command line code. I then ran SeroSeq2 and SISTR to get the serotypes. 117 in both datasets did not have a matching serotype.

# count observations per serotype
seroseq_counts <- seroseq %>%
  count(`Predicted serotype`, name = "n")

sistr_counts <- sistr %>%
  count(`serovar`, name = "n")

sistr_ir <- sistr_counts %>%
  mutate(
    total_samples = sum(n),
    proportion = n / total_samples,
    ir_co = (n/5773714)*100000
  )


# select top 20 serotypes by count
top20_seroseq <- seroseq_counts %>%
  arrange(desc(n)) %>%
  filter(`Predicted serotype`!='- -:-:-') %>%
  slice_head(n = 20)

top20_sistr <- sistr_counts %>%
  arrange(desc(n)) %>%
  filter(serovar!='-:-:-') %>%
  slice_head(n = 20)


# clean and harmonize IDs
seroseq_clean <- seroseq %>%
  mutate(
    genome = str_remove(`Sample name`, "\\.fna$")
  ) %>%
  select(
    genome,
    seroseq_serotype = `Predicted serotype`
  )

sistr_clean <- sistr %>%
  select(
    genome,
    sistr_serotype = serovar
  )

# merge serotype results
# Note 2/6/2026: additional mutate steps harmonize how each program reports out on results
serotypes <- seroseq_clean %>%
  inner_join(sistr_clean, by = "genome") %>%
  mutate(serotype_match = if_else(seroseq_serotype == sistr_serotype, 1, 0)) %>%
  mutate(serotype_match = if_else(
    seroseq_serotype == 'I 4,[5],12:i:-' & sistr_serotype == 'I 1,4,[5],12:i:-', 1, serotype_match)) %>%
  mutate(serotype_match = if_else(
    seroseq_serotype == 'Typhimurium' & sistr_serotype == 'Typhimurium|Lagos', 1, serotype_match)) %>%
  mutate(serotype_match = if_else(
    seroseq_serotype == 'Paratyphi B var. L(+) tartrate+' & sistr_serotype == 'Paratyphi B var. Java', 1, serotype_match)) %>%
  mutate(serotype_match = if_else(
    seroseq_serotype == '- -:-:-' & sistr_serotype == '-:-:-', 1, serotype_match)
  )

# calculate percentage of matches
match_summary <- serotypes %>%
  summarise(
    total_samples = n(),
    matched_samples = sum(serotype_match),
    percent_match = (matched_samples / total_samples) * 100
  )

# Note 2/6/2026: There is a 96.1% concordance between SeroSeq2 and SISTR. There are a handful of cases in which one program identifies multiple strains where the other program identified one of those strains, the remaining are true mismatches. 
match_summary


#### MERGE SEROTYPE AND SAMPLE INFORMATION ####
seroseq_clean2 <- seroseq_clean %>%
  mutate(Assembly = str_split_fixed(`genome`, "_", 3)[, 1:2] %>%
           apply(1, paste, collapse = "_"))
sistr_clean2 <- sistr_clean %>%
  mutate(Assembly = str_split_fixed(`genome`, "_", 3)[, 1:2] %>%
           apply(1, paste, collapse = "_"))

health <- merged_dua_ncbi %>%
  left_join(seroseq_clean2, by = "Assembly") %>%
  left_join(sistr_clean2, by = "Assembly") 

# remove extra columns from merge
myvars <- names(health) %in% c("genome.x", "genome.y")

health_clean = health[!myvars]

### EXTRACT INFORMATION FOR LOCATION ####
# Fill in missing CT with diagnostic_ct if address_CT is missing
health_clean <- health_clean %>%
  mutate(censustract = coalesce(address_at_diagnosis_censustract, diagnostic_census_tract))

# This code pads the census tract info with a leading 0, to account for changes in reporting by year, and for future extracts, then extracts the location components in the standard 11 digit GeoID.
health_clean <- health_clean %>%
  mutate(
    # Reconstruct full 11-digit GEOID
    geoid_11 = str_pad(
      as.character(censustract),
      width = 11,
      side = "left",
      pad = "0"
    ),
    
    # Extract components
    state = str_sub(geoid_11, 1, 2),
    county = str_sub(geoid_11, 3, 5),
    fips = str_sub(geoid_11, 1, 5),
    census_tract = str_sub(geoid_11, 6, 11)
  )


## determine proportion of serotypes by year
# Step 1: Parse date and extract year
serotype_year <- health_clean %>%
  mutate(
    first_reported_ph_date = mdy(first_reported_ph_date),
    year = year(first_reported_ph_date)
  ) %>%
  filter(!is.na(year))

# Step 2: Count serovars by year
serovar_year_counts <- serotype_year %>%
  group_by(sistr_serotype, year) %>%
  summarise(n = n(), .groups = "drop")

# Step 3: Identify top 20 serovars overall
top20_serovars <- serovar_year_counts %>%
  group_by(sistr_serotype) %>%
  filter(!is.na(sistr_serotype)) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  arrange(desc(total_n)) %>%
  slice_head(n = 20) %>%
  pull(sistr_serotype)

# Step 4: Filter to top 20 serovars
top20_sistryear <- serovar_year_counts %>%
  filter(sistr_serotype %in% top20_serovars)



## determine proportion of serotypes by year NON OUTBREAK
# Step 1: Parse date and extract year
serotype_year_nooutbreak <- health_clean %>%
  mutate(
    first_reported_ph_date = mdy(first_reported_ph_date),
    year = year(first_reported_ph_date)
  ) %>%
  filter(!is.na(year)) %>%
  filter(outbreak != "Yes")

# Step 2: Count serovars by year
serovar_year_counts_nooutbreak <- serotype_year_nooutbreak %>%
  group_by(sistr_serotype, year) %>%
  summarise(n = n(), .groups = "drop")

# Step 3: Identify top 20 serovars overall
top20_serovars_nooutbreak <- serovar_year_counts_nooutbreak %>%
  group_by(sistr_serotype) %>%
  filter(!is.na(sistr_serotype)) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  arrange(desc(total_n)) %>%
  slice_head(n = 20) %>%
  pull(sistr_serotype)

# Step 4: Filter to top 20 serovars
top20_sistryear_nooutbreak <- serovar_year_counts_nooutbreak %>%
  filter(sistr_serotype %in% top20_serovars_nooutbreak)





## determine proportion of serotypes by month NON OUTBREAK
# Step 1: Parse date and extract year
serotype_month_nooutbreak <- health_clean %>%
  mutate(
    first_reported_ph_date = mdy(first_reported_ph_date),
    month = month(first_reported_ph_date)
  ) %>%
  filter(!is.na(month)) %>%
  filter(outbreak != "Yes")

# Step 2: Count serovars by year
serovar_month_counts_nooutbreak <- serotype_month_nooutbreak %>%
  group_by(sistr_serotype, month) %>%
  summarise(n = n(), .groups = "drop")

# Step 3: Identify top 20 serovars overall
top10_serovars_mo_nooutbreak <- serovar_month_counts_nooutbreak %>%
  group_by(sistr_serotype) %>%
  filter(!is.na(sistr_serotype)) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  arrange(desc(total_n)) %>%
  slice_head(n = 10) %>%
  pull(sistr_serotype)

# Step 4: Filter to top 20 serovars
top10_sistrm_nooutbreak <- serovar_month_counts_nooutbreak %>%
  filter(sistr_serotype %in% top10_serovars_mo_nooutbreak)

