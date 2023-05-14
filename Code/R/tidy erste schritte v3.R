# R-code quantitative paper on the effects of fiscal austerity on inequality
# authors: Barden, Nicolas & Heibel, Jakob
# date: 01.06.2023


library(here)
library(dplyr)
library(tibble)
library(data.table)
library(tidyr)
library(countrycode)
library(readxl)
library(OECD)


# Loading data
# import and edit SWIID-data

data_path_swiid <- here("data/raw/swiid9_4.rda")

load(data_path_swiid)

# Readjust the SWIID-data to match the AMECO-data. Filtering for countries first.
# AMECO Daten (cyc. adj. nach dem Trend GDP) liegen nur für diese Länder vor

swiid_tidy <- filter(swiid_summary, country %in% c(
  "Belgium", "Bulgaria", "Czech Republic", "Denmark", "Germany", "Estonia", "Ireland",
  "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", 
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"))


# Filtering for year second. For most countries AMECO-data is only avaiable 
# from 1995.

swiid_tidy <- filter(swiid_tidy, year > 1994)

swiid_tidy <- select(swiid_tidy,
                     country, year, gini_disp, gini_disp_se, gini_mkt,
                     gini_mkt_se)
swiid_tidy$country[131:156] <- "Czechia"


# AMECO-data for the cyclically adjusted lending trend by GDP.

data_path_AMECO_lending <- here(
  "data/raw/cyc adj lending trend gdp 290423.csv")


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
  "Greece", "Spain", "France", "Croatia (1)", "Italy", "Cyprus", "Latvia", 
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"))

AMECO_lending_tidy$Country[AMECO_lending_tidy$Country == "Croatia (1)"] <- "Croatia"

AMECO_lending_tidy <- rename(AMECO_lending_tidy,
                             country = Country)

AMECO_lending_tidy <- tibble(AMECO_lending_tidy)

AMECO_lending_tidy <- AMECO_lending_tidy %>% 
  mutate(year = as.double(year))

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
  "Greece", "Spain", "France", "Croatia (1)", "Italy", "Cyprus", "Latvia", 
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"))

AMECO_exp_tidy$Country[AMECO_exp_tidy$Country == "Croatia (1)"] <- "Croatia"

AMECO_exp_tidy <- rename(AMECO_exp_tidy,
                             country = Country)

AMECO_exp_tidy <- tibble(AMECO_exp_tidy)

AMECO_exp_tidy <- AMECO_exp_tidy %>% 
  mutate(year = as.double(year))

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
  "Greece", "Spain", "France", "Croatia (1)", "Italy", "Cyprus", "Latvia", 
  "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
  "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
  "United Kingdom"))

AMECO_rev_tidy$Country[AMECO_rev_tidy$Country == "Croatia (1)"] <- "Croatia"

AMECO_rev_tidy <- rename(AMECO_rev_tidy,
                         country = Country)

AMECO_rev_tidy <- tibble(AMECO_rev_tidy)

AMECO_rev_tidy <- AMECO_rev_tidy %>% 
  mutate(year = as.double(year))

# Transforming the SWIID-data to tibble.

swiid_tidy <- tibble(swiid_tidy)

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


# Using the WDI growth rate data to control for annual growth rate

data_path_growth <- here("data/raw/growth data world development indicators/wdi growth raw.csv")

wdi_growth <- fread(data_path_growth,
                             sep = ",", dec = ".",na.strings = "..")
wdi_growth <- slice(wdi_growth,1:28)

# tidying the data

wdi_growth_tidy <- wdi_growth%>%
  select(-c("Series Code","Series Name", "Country Code", "2022 [YR2022]"))
colnames(wdi_growth_tidy) <- c("country","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021")
wdi_growth_tidy <- pivot_longer(wdi_growth_tidy,
                               cols = c(2:28),
                               names_to = "year",
                               values_to = 
                                 "gdp_growth_rate") %>%
                   mutate(year = as.double(year))

# reducing decimals

wdi_growth_tidy <- mutate_if(wdi_growth_tidy,is.numeric, round, digits = 2)

  data_v4 <- left_join(data_v3, wdi_growth_tidy)

  
# controlling for foreign (external) balances
  # The current external balance is the balancing item in the external
  # account of primary incomes and current transfers.
  # It represents the surplus or the deficit of the total economy on its
  # current transactions with the rest of the world.
  # The current transactions comprise trade in goods and services, primary
  # incomes and current transfers.
  
  # in percentage of GDP at market prices
  
data_path_foreign_balances <- here("data/raw/foreign balances/fb by perc gdp.csv")
  
fb_raw <- fread(data_path_foreign_balances,
                      sep = ";", dec = ",",na.strings = "..")
fb_tidy <- fb_raw%>%
  select(-c("Variable","Unit/Description", "Unit","Year")) %>%
  rename(country = Country)
fb_tidy <- pivot_longer(fb_tidy,
                                cols = c(2:28),
                                names_to = "year",
                                values_to = 
                                  "fbinpercGDP") %>%
           mutate(year = as.double(year)) %>%
           mutate(fbinpercGDP = as.double(fbinpercGDP)) 


data_v5 <- left_join(data_v4, fb_tidy)

# controlling for unemployment rate as percentage of civilian labour force


data_path_unemp <- here("data/raw/ameco unemp rate 140523.csv")

unemp_raw <- fread(data_path_unemp,
                sep = ";", dec = ",",na.strings = "-")
unemp_tidy <- unemp_raw%>%
  select(-c("Variable","Unit/Description", "Unit","Year")) %>%
  rename(country = Country)

unemp_tidy <- pivot_longer(unemp_tidy,
                        cols = c(2:28),
                        names_to = "year",
                        values_to = 
                          "unemp_rate") %>%
  mutate(year = as.double(year)) %>%
  mutate(unemp_rate = as.double(unemp_rate)) 

data_v6 <- left_join(data_v5, unemp_tidy)

#Dummys für alle Jahre


# Running the linear regression for inequality as the dependant variable
# while adjusting for fixed effects of time and country with dummy variables.

reg_dummy = lm(gini_disp ~ lending + expenditures + revenue + gdp_growth_rate + 
                 fbinpercGDP + unemp_rate
               + factor(year) + factor(country), data = data_v6)
summary(reg_dummy)

# das R squared sieht schon ganz okay aus, nur sind die 
# unabh. Variablen noch nicht sehr signifikant. Funktionale Form + lags next
# + Dummy für Euro, UK fliegt raus?
# mit dem dynlm package wäre ein lag leicht realisierbar
