# Combining the dataset used by Alesina et al. to identify periods of fiscal austerity
# with the data from SWIID

library(here)
library(dplyr)
library(tibble)
library(data.table)
library(tidyr)
library(countrycode)

# Loading data
# import and edit SWIID-data

data_path_swiid <- here("data/raw/swiid9_4.rda")

load(data_path_swiid)

# Readjusting the SWIID-data to match the Alesina et al. dataset. 
# Filtering for countries first.

swiid_tidy <- filter(swiid_summary, country %in% c(
  "Australia", "Austria", "Belgium", "Canada", "Germany", "Denmark", "Spain",
  "Finland", "France", "United Kingdom", "Ireland", "Italy", "Japan", "Portugal",
  "Sweden", "United States"))

# Loading the Alesina et al. dataset

data_path_alesina <- here("data/raw/alesina et al/eviews hierarchical - csv.csv")

narrative_fiscal <- fread(data_path_alesina,
                          sep = ";", dec = ",",
                          na.strings = c("NA", "#WERT!"))

# Transforming the iso3c countrycodes into iso.name.en format used by the SWIID

narrative_fiscal <- narrative_fiscal %>%
  mutate(Country = countrycode(Country, "iso3c", "iso.name.en"))

# Capitalizing Country and Year in SWIID to enable left_join

swiid_tidy <- swiid_tidy %>% 
  rename("Country" = "country") %>% 
  rename("Year" = "year")

data_narrative1 <- left_join(swiid_tidy, narrative_fiscal)
