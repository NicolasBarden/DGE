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

data_path_alesina2 <- here("data/raw/alesina et al/GDP_csv.csv")

# narrative_fiscal contains the 'eviews_hierarchical' worksheet of the
# 'NewComponents1978-2014_final.xlsx' file
# narrative_fiscal2 contains the 'GDP' worksheet of said excel file.

narrative_fiscal <- fread(data_path_alesina,
                          sep = ";", dec = ",",
                          na.strings = c("NA", "#WERT!"))
narrative_fiscal2 <- fread(data_path_alesina2,
                           sep = ";", dec = ",")

# We are interested in the 'Tax-impact Tot' nad the 'Spending Impact Tot' from the
# worksheet 'GDP'. These columns contain data for tax and spending based
# consolidation measures implemented in year t, measured as a percent of GDP at
# t-1.

narrative_tax_spending <- narrative_fiscal2 %>% 
  select("(Values in % GDP_t-1)", V2, "Tax -Impact", "Spending Impact")

narrative_tax_spending <- narrative_tax_spending %>% 
  rename("Country" = "(Values in % GDP_t-1)") %>% 
  rename("Year" = "V2") %>% 
  rename("Tax" = "Tax -Impact") %>% 
  rename("Spending" = "Spending Impact")

narrative_tax_spending <- narrative_tax_spending[-1,]

narrative_tax_spending$Tax <- gsub(",",".", narrative_tax_spending$Tax)
narrative_tax_spending$Spending <- gsub(",",".", narrative_tax_spending$Spending)

narrative_tax_spending$Year <- as.numeric(narrative_tax_spending$Year)
narrative_tax_spending$Tax <- as.numeric(narrative_tax_spending$Tax)
narrative_tax_spending$Spending <- as.numeric(narrative_tax_spending$Spending)

# Transforming the iso3c countrycodes into iso.name.en format used by the SWIID

narrative_fiscal <- narrative_fiscal %>% 
  mutate(Country = countrycode(Country, "iso3c", "country.name"))

narrative_tax_spending <- narrative_tax_spending %>%
  mutate(Country = countrycode(Country, "iso3c", "country.name"))

# Capitalizing Country and Year in SWIID to enable left_join

swiid_tidy <- swiid_tidy %>% 
  rename("Country" = "country") %>% 
  rename("Year" = "year")

narrative <- left_join(swiid_tidy, narrative_tax_spending)

# We also want to include some macro variables. We can extract them from the
# narrative_fiscal dataframe. 
# The macro variables are:
# utr (unemployment rate)
# CC (dummy for currency crisis)
# IC (dummy for inflation crisis)
# SMC (dummy for stock market crash)
# BC (dummy for banking crisis)
# rec_index (dummy based on the NBER recession index)
# debt_new (General Government Debt to GDP ratio, IMF)

narrative_macro <- narrative_fiscal %>% 
  select(Country, Year, utr, CC, IC, SMC, BC, rec_index, debt_new)

narrative <- left_join(narrative, narrative_macro)

# Next, we import growth data from the WDI database

growth <- WDI(
  indicator = "NY.GDP.MKTP.KD.ZG",
  start = 1978,
  end = 2014)

growth <- growth %>% 
  rename("Country" = "country") %>% 
  rename("Year" = "year") %>% 
  rename("growth" = "NY.GDP.MKTP.KD.ZG")

narrative <- left_join(narrative, growth)

write.csv(narrative, file = here("data/tidy/dat_narrative1.csv"))

library(plm)

Fe1 <- plm(
  gini_disp ~ Tax*debt_new + Spending*debt_new + utr + CC + IC + SMC + growth +
    I(growth*growth),
           data = narrative, 
           index = c("Country", "Year"),
           model = "within",
           effect = "twoways")

OLS <- lm(gini_disp ~ Tax + Spending, data = narrative2)
OLS2 <- lm(gini_disp ~ gini_mkt, data = narrative2)





# Computing GDP per capita

data_narrative1 <- data_narrative1 %>% 
  mutate(gdp_pc = (gdp/popt))

# Some first graphs
attach(narrative)

library(ggplot2)

gdpreg <- lm(log1p(gini_disp) ~ log1p(g) + I(log1p(gdp_pc)*log1p(gdp_pc)))

ggplot(mapping = aes(gdp_pc, gini_disp)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2))

# We can observe a kind of flipped Kuznets curve. This is probably because 
# the dataset incldes only rich countries.

