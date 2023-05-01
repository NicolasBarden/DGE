library(here)
library(dplyr)
library(tibble)
library(data.table)
library(tidyr)

#SWIID Daten einlesen und bearbeiten

data_path_swiid <- here("data/raw/swiid9_4.rda")

load(data_path_swiid)

#AMECO Daten (cyc. adj. nach dem Trend GDP) liegen nur für diese Länder vor

swiid_tidy <- filter(swiid_summary, country %in% c(
  "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
  "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", 
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"))

#...und für die allermeisten Länder nur ab 1995

swiid_tidy <- filter(swiid_tidy, year > 1994)

swiid_tidy <- select(swiid_tidy,
                     country, year, gini_disp, gini_disp_se, gini_mkt,
                     gini_mkt_se)

#AMECO Daten für die cyc. adj. Nettoent- (+) bzw. verschuldung (-)

data_path_AMECO_lending <- here(
  "data/raw/cyc adj lending trend gdp 290423.csv")

#Wichtig:

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

#AMECO Daten für die cyc. adj. Ausgaben (excl. interest) bearbeiten

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

#AMECO Daten für die cyc. adj. Einnahmen bearbeiten

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

swiid_tidy <- tibble(swiid_tidy)

#Datensätze zusammenführen

data_v1 <- left_join(swiid_tidy, AMECO_lending_tidy)
data_v2 <- left_join(data_v1, AMECO_exp_tidy)
data_v3 <- left_join(data_v2, AMECO_rev_tidy)

#Dummys für alle 27 Länder

countries <- pull(count(data_v3, country),country)

data_country_dummys <- tibble()

for(i in 1:length(countries)){
  assign(countries[i], countries[i])
}

for(i in 1:length(countries)){
  data_country_dummys <- data_v3 %>% 
    mutate(assign(countries[i], countries[i]) = ifelse(country == countries[i], 1, 0))
}

data_country_dummys <- data_v3 %>% 
  mutate(
    Cyprus = ifelse(country == countries[5], 1, 0))

assign(countries[5], countries[5])

data_country_dummys <- data_v3 %>% 
  mutate(Austria = ifelse( country == "Austria", 1, 0))