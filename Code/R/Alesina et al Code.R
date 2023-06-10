# Combining the dataset used by Alesina et al. to identify periods of fiscal austerity
# with the data from SWIID

library(here)
library(dplyr)
library(tibble)
library(data.table)
library(tidyr)
library(countrycode)
library(WDI)
library(plm)

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

# Filtering for year second. Country data from Alesina et al is only avaiable 
# from 1978 to 2014.

swiid_tidy <- swiid_tidy %>% 
  filter(year > 1977) %>% 
  filter(year < 2015)

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

# We are interested in the following varaibles from the worksheet 'GDP':
# 'Tax-impact Tot', 'Spending Impact Tot', 'New Plan'. 
# These columns contain data for tax and spending based consolidation measures 
# implemented in year t, measured as a percent of GDP at t-1, as well as a dummy
# variable identifying austerity periods.
# Furthermore, we are interested in the more granular data, that the dataset
# offers, especially on the distinction between the different consolidation
# measures, namely direct tax, indirect tax, transfer or consumption &
# investment spending based measures. As Alesina et al. also include announced/
# anticipated fiscal measures in which we are not interested, we have to 
# calculate the categories ourselves. For this purpose we add up unexpected 
# measures (u_t) and announced measures that were implemented in year t. 

narrative_tax_spending <- narrative_fiscal2 %>% 
  select(
    "(Values in % GDP_t-1)", V2, "Tax -Impact", "Spending Impact", "New Plan",
    "Taxes - Income", V4, "Taxes - Propetries", V11, 
    "Taxes - Goods and Services", V32, "Spending - Transfers", V81, 
    "Spending - Consumption", V60, "Spending - Salaries", V67, 
    "Spending - Investments", V74)

narrative_tax_spending <- narrative_tax_spending %>% 
  rename("Country" = "(Values in % GDP_t-1)") %>% 
  rename("Year" = "V2") %>% 
  rename("Tax" = "Tax -Impact") %>% 
  rename("Spending" = "Spending Impact") %>% 
  rename("Austerity" = "New Plan")

narrative_tax_spending <- narrative_tax_spending[-1,]

narrative_tax_spending$Tax <- gsub(",",".", narrative_tax_spending$Tax)
narrative_tax_spending$`Taxes - Income` <- gsub(
  ",",".", narrative_tax_spending$`Taxes - Income`)
narrative_tax_spending$V4 <- gsub(",",".", narrative_tax_spending$V4)
narrative_tax_spending$`Taxes - Propetries` <- gsub(
  ",",".", narrative_tax_spending$`Taxes - Propetries`)
narrative_tax_spending$V11 <- gsub(",",".", narrative_tax_spending$V11)
narrative_tax_spending$`Taxes - Goods and Services` <- gsub(
  ",",".", narrative_tax_spending$`Taxes - Goods and Services`)
narrative_tax_spending$V32 <- gsub(",",".", narrative_tax_spending$V32)
narrative_tax_spending$`Spending - Transfers` <- gsub(
  ",",".", narrative_tax_spending$`Spending - Transfers`)
narrative_tax_spending$V81 <- gsub(",",".", narrative_tax_spending$V81)
narrative_tax_spending$`Spending - Consumption` <- gsub(
  ",",".", narrative_tax_spending$`Spending - Consumption`)
narrative_tax_spending$V60 <- gsub(",",".", narrative_tax_spending$V60)
narrative_tax_spending$`Spending - Salaries` <- gsub(
  ",",".", narrative_tax_spending$`Spending - Salaries`)
narrative_tax_spending$V67 <- gsub(",",".", narrative_tax_spending$V67)
narrative_tax_spending$`Spending - Investments` <- gsub(
  ",",".", narrative_tax_spending$`Spending - Investments`)
narrative_tax_spending$V74 <- gsub(",",".", narrative_tax_spending$V74)
narrative_tax_spending$Spending <- gsub(
  ",",".", narrative_tax_spending$Spending)

narrative_tax_spending$Year <- as.numeric(narrative_tax_spending$Year)
narrative_tax_spending$Tax <- as.numeric(narrative_tax_spending$Tax)
narrative_tax_spending$Spending <- as.numeric(
  narrative_tax_spending$Spending)
narrative_tax_spending$`Taxes - Income` <- as.numeric(
  narrative_tax_spending$`Taxes - Income`)
narrative_tax_spending$V4 <- as.numeric(narrative_tax_spending$V4)
narrative_tax_spending$`Taxes - Propetries` <- as.numeric(
  narrative_tax_spending$`Taxes - Propetries`)
narrative_tax_spending$V11 <- as.numeric(narrative_tax_spending$V11)
narrative_tax_spending$`Taxes - Goods and Services` <- as.numeric(
  narrative_tax_spending$`Taxes - Goods and Services`)
narrative_tax_spending$V32 <- as.numeric(narrative_tax_spending$V32)
narrative_tax_spending$`Spending - Transfers` <- as.numeric(
  narrative_tax_spending$`Spending - Transfers`)
narrative_tax_spending$V81 <- as.numeric(narrative_tax_spending$V81)
narrative_tax_spending$`Spending - Consumption` <- as.numeric(
  narrative_tax_spending$`Spending - Consumption`)
narrative_tax_spending$V60 <- as.numeric(narrative_tax_spending$V60)
narrative_tax_spending$`Spending - Salaries` <- as.numeric(
  narrative_tax_spending$`Spending - Salaries`)
narrative_tax_spending$V67 <- as.numeric(narrative_tax_spending$V67)
narrative_tax_spending$`Spending - Investments` <- as.numeric(
  narrative_tax_spending$`Spending - Investments`)
narrative_tax_spending$V74 <- as.numeric(narrative_tax_spending$V74)

narrative_tax_spending <- narrative_tax_spending %>% 
  mutate(DirectTax = `Taxes - Income` + V4 + `Taxes - Propetries` + V11) %>% 
  mutate(IndirectTax = `Taxes - Goods and Services` + V32) %>% 
  mutate(Transfers = `Spending - Transfers` + V81) %>% 
  mutate(ConsInvSpending = `Spending - Consumption` + V60 + 
           `Spending - Salaries` + V67 + `Spending - Investments` + V74)

# We use the granular data about the austerity measures to construct hierarchical
# dummy variables that follow the subsequent reasoning: 
# If DirectTax > IndirectTax and Transfers and ConsInvSpending, then DB=1
# If IndirectTax > DirectTax and Transfers and ConsInvSpending, then IB=1
# If Transfers > IndirectTax and DirectTax and ConsInvSpending, then TB=1
# If ConsInvSpending > IndirectTax and DirectTax and Transfers, then CB=1
attach(narrative_tax_spending)

DB <- IB <- TB <- CB <- rep(0, length(DirectTax))

for (i in 1:length(DirectTax)) {
  if (DirectTax[i] > IndirectTax[i] & DirectTax[i] > Transfers[i] & DirectTax[i] > ConsInvSpending[i]) {
    DB[i] <- 1
  } else if (IndirectTax[i] > DirectTax[i] & IndirectTax[i] > Transfers[i] & IndirectTax[i] > ConsInvSpending[i]) {
    IB[i] <- 1
  } else if (Transfers[i] > DirectTax[i] & Transfers[i] > IndirectTax[i] & Transfers[i] > ConsInvSpending[i]) {
    TB[i] <- 1
  } else if (ConsInvSpending[i] > DirectTax[i] & ConsInvSpending[i] > IndirectTax[i] & ConsInvSpending[i] > Transfers[i]) {
    CB[i] <- 1
  }
}

narrative_tax_spending <- narrative_tax_spending %>% 
  mutate(DB = DB, IB = IB, TB = TB, CB = CB)

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

# generating a dummy variable for Euro-currency

narrative <- mutate(narrative, "EURO"= if_else
    (Country == "Austria" & Year >= 1999 
      | Country == "Belgium" & Year >= 1999
      | Country == "Finland" & Year >= 1999
      | Country == "France" & Year >= 1999
      | Country == "Germany" & Year >= 1999
      | Country == "Ireland" & Year >= 1999
      | Country == "Italy" & Year >= 1999
      | Country == "Portugal" & Year >= 1999
      | Country == "Spain" & Year >= 1999,
      1,0))


write.csv(narrative, file = here("data/tidy/dat_narrative1.csv"))

# Estimating FE models:

narr <- read.csv(here("data/tidy/dat_narrative1.csv"))

# Organizing our panel dataset by using the pdata.frame()-function of the plm
# (panel linear models) package, defining Country and Year as the dimensions
# of the panel dataset:

narrpd <- pdata.frame(narr, index= c("Country", "Year"))
attach(narrpd)

model1 <- plm(gini_mkt ~ Austerity*lag(utr)+ lag(growth) + factor(Year),
              index ="Country",
              model ="within",
              data= narrpd)
summary(model1)

model2 <- plm(gini_mkt ~ Austerity*lag(utr)+ lag(growth),
              index = c("Country", "Year"),
              model ="within",
              effect = "twoways",
              data= narrpd)
summary(model2)

model2 <- plm(gini_mkt ~ Tax*Spending + lag(utr),
              index = c("Country", "Year"),
              model = "within",
              data = narrpd)
summary(model2)

model3 <- lm(gini_disp ~ Transfers + DirectTax + IndirectTax + ConsInvSpending,
              data = narr)
summary(model3)

model4 <- plm(
  gini_mkt ~ Austerity*lag(utr) + lag(growth),
           data = narrpd, 
           index = c("Country", "Year"),
           model = "within",
           effect = "twoways")

model5 <- plm(
  gini_disp ~ Transfers +  lag(growth),
  data = narrpd,
  index = c("Country", "Year"),
  model = "within",
  effect = "twoways")





# Computing GDP per capita

data_narrative1 <- data_narrative1 %>% 
  mutate(gdp_pc = (gdp/popt))

# Some first graphs
attach(narrative)

library(ggplot2)

gdpreg <- lm(log1p(gini_disp) ~ log1p(growth) + I(log1p(growth)*log1p(growth)))

ggplot(mapping = aes(growth, gini_disp)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2))

# We can observe a kind of flipped Kuznets curve. This is probably because 
# the dataset incldes only rich countries.
