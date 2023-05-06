# R-code quantitative paper on the effects of austerity on inequality
# authors: Barden, Nicolas & Heibel, Jakob
# date: 01.06.2023


library(here)
library(dplyr)
library(tibble)
library(data.table)
library(tidyr)
library(countrycode)

# SWIID-Daten // import and edit SWIID-data

data_path_swiid <- here("data/raw/swiid9_4.rda")

load(data_path_swiid)

# SWIID-Daten auf die vorhandenen Ameco-Daten reduzieren. /
# Readjust the SWIID-data to match the AMECO-data. Filtering for countries first.
# AMECO Daten (cyc. adj. nach dem Trend GDP) liegen nur für diese Länder vor

swiid_tidy <- filter(swiid_summary, country %in% c(
  "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
  "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", 
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"))

#...und für die allermeisten Länder nur ab 1995
# Filtering for year second. For most countries AMECO-data is only avaiable 
# from 1995.

swiid_tidy <- filter(swiid_tidy, year > 1994)

swiid_tidy <- select(swiid_tidy,
                     country, year, gini_disp, gini_disp_se, gini_mkt,
                     gini_mkt_se)

# AMECO Daten für die cyc. adj. Nettoent- (+) bzw. verschuldung (-)
# AMECO-data for the cyclically adjusted lending trend by GDP.

data_path_AMECO_lending <- here(
  "data/raw/cyc adj lending trend gdp 290423.csv")

#Wichtig:

# Transforming and editing the data:
AMECO_lending_trend_adj <- fread(data_path_AMECO_lending,
                                 sep = ";", dec = ",",
                                 na.strings = "-",
                                 colClasses = list(double=6:65))


AMECO_lending_tidy <- select(AMECO_lending_trend_adj, -Year)

AMECO_lending_tidy <- pivot_longer(AMECO_lending_tidy,
                           cols = c(5:64),
                           names_to = "year",
                           values_to = 
                      "Net lending adjusted for the cyclical component, % GDP"
                           )

AMECO_lending_tidy <- select(AMECO_lending_tidy,
                             -c("Variable", "Unit/Description", "Unit"))

AMECO_lending_tidy <- filter(AMECO_lending_tidy, Country %in% c(
  "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
  "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", 
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"))

AMECO_lending_tidy <- rename(AMECO_lending_tidy,
                             country = Country)

AMECO_lending_tidy <- tibble(AMECO_lending_tidy)

AMECO_lending_tidy <- AMECO_lending_tidy %>% 
  mutate(year = as.double(year))

#AMECO Daten für die cyc. adj. Ausgaben (excl. interest) bearbeiten.
# Editing AMECO-data for the cyclically adjusted expenditures by GDP.

data_path_AMECO_exp <- here(
  "data/raw/cyc adj exp trend gdp 290423.csv")

AMECO_exp_trend_adj <- fread(data_path_AMECO_exp,
                                 sep = ";", dec = ",",
                                 na.strings = "-",
                                 colClasses = list(double=6:65))


AMECO_exp_tidy <- select(AMECO_exp_trend_adj, -Year)

AMECO_exp_tidy <- pivot_longer(AMECO_exp_tidy,
                                   cols = c(5:64),
                                   names_to = "year",
                                   values_to = 
"Total expenditure, excluding interest, adjusted for the cyclical component, % GDP"
)

AMECO_exp_tidy <- select(AMECO_exp_tidy,
                             -c("Variable", "Unit/Description", "Unit"))

AMECO_exp_tidy <- filter(AMECO_exp_tidy, Country %in% c(
  "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
  "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", 
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"))

AMECO_exp_tidy <- rename(AMECO_exp_tidy,
                             country = Country)

AMECO_exp_tidy <- tibble(AMECO_exp_tidy)

AMECO_exp_tidy <- AMECO_exp_tidy %>% 
  mutate(year = as.double(year))

# AMECO Daten für die cyc. adj. Einnahmen bearbeiten.
# Editing AMECO-data for the cyclically adjusted revenue by GDP.

data_path_AMECO_rev <- here(
  "data/raw/cyc adj rev trend gdp 290423.csv")

AMECO_rev_trend_adj <- fread(data_path_AMECO_rev,
                             sep = ";", dec = ",",
                             na.strings = "-",
                             colClasses = list(double=6:65))


AMECO_rev_tidy <- select(AMECO_rev_trend_adj, -Year)

AMECO_rev_tidy <- pivot_longer(AMECO_rev_tidy,
                               cols = c(5:64),
                               names_to = "year",
                               values_to = 
                    "Total revenue adjusted for the cyclical component, % GDP"
)

AMECO_rev_tidy <- select(AMECO_rev_tidy,
                         -c("Variable", "Unit/Description", "Unit"))

AMECO_rev_tidy <- filter(AMECO_rev_tidy, Country %in% c(
  "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
  "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", 
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"))

AMECO_rev_tidy <- rename(AMECO_rev_tidy,
                         country = Country)

AMECO_rev_tidy <- tibble(AMECO_rev_tidy)

AMECO_rev_tidy <- AMECO_rev_tidy %>% 
  mutate(year = as.double(year))

#SWIID zum Tibble machen
# Transforming the SWIID-data to tibble.

swiid_tidy <- tibble(swiid_tidy)

#Datensätze zusammenführen
# Joining the data sets.

data_v1 <- left_join(swiid_tidy, AMECO_lending_tidy)
data_v2 <- left_join(data_v1, AMECO_exp_tidy)
data_v3 <- left_join(data_v2, AMECO_rev_tidy)

# Renaming for easier use.

data_v3 <- data_v3 %>%
rename("lending" =  
"Net lending adjusted for the cyclical component, % GDP") %>%
rename("expenditures" = 
"Total expenditure, excluding interest, adjusted for the cyclical component, % GDP") %>%
rename("revenue" =
"Total revenue adjusted for the cyclical component, % GDP")

# Using the AMECO exports data to control for exports (lagged).

data_path_AMECO_exports <- here(
  "data/raw/exports ameco 060523.csv")

exports_raw <- fread(data_path_OECD_gdp,
                    sep = ";", dec = ",",
                    na.strings = "-")
# tidying the data.

exports_tidy <- exports_raw %>% 
  rename("country" = "LOCATION", "exports" = "Value", "year" = "TIME") %>%
  select(-c("SUBJECT", "INDICATOR", "MEASURE", "FREQUENCY", "Flag Codes"))
# hier noch country code umwandeln


# Using the OECD growth data to control for growth (lagged). 

data_path_OECD_gdp <- here(
  "data/raw/gdp growth 060523.csv")

gdp_growth <- fread(data_path_OECD_gdp,
                                 sep = ";", dec = ",",
                                 na.strings = "-")


gdp_growth <- gdp_growth %>% 
  rename("country" = "LOCATION", "GDP" = "Value", "year" = "TIME") %>%
  select(-c("SUBJECT", "INDICATOR", "MEASURE", "FREQUENCY", "Flag Codes"))%>%
  filter(country 
 %in% c("AUT", "BEL","BGR", "CYP","DEU", "DNK", "ESP", "EST", "FIN", 
        "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA",
        "MLT", "NLD", "POL", "PRT", "ROU","SVK", "SVN", "SWE")) %>%
  mutate(gdp_growth_perc=NA)
#hier loop zur errechnung der grwoth rate, dann mergen mit datav3






#Dummys für alle 27 Länder // Können wir mMn direkt in der Regression erstellen
# siehe unten

#countries <- pull(count(data_v3, country),country)

#data_country_dummys <- data_v3

#for(i in 1:length(countries)){
#  data_country_dummys[[countries[i]]] <- ifelse(
 #   data_v3$country == countries[i], 1, 0)
#}

#Dummys für alle Jahre

# Lineare Regression
# Running the linear regression for inequality as the dependant variable
# while adjusting for fixed effects of time and country with dummy variables.

reg_dummy = lm(gini_disp ~ lending + expenditures + revenue 
               + factor(year) + factor(country), data = data_v3)
summary(reg_dummy)
