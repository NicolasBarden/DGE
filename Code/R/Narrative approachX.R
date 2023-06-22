# R-code quantitative paper on the effects of fiscal austerity on inequality
# authors: Barden, Nicolas & Heibel, Jakob
# date: 23.06.2023

library(here)
library(plm)
library(stargazer)
library(dplyr)
library(tidyr)
library(sandwich)
library(lmtest)
library(tidyverse)
library(WDI)
library(data.table)
library(countrycode)
library(ggplot2)

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

narr <- left_join(swiid_tidy, narrative_tax_spending)

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

narr <- left_join(narr, narrative_macro)

# Next, we import growth data from the WDI database

growth <- WDI(
  indicator = "NY.GDP.MKTP.KD.ZG",
  start = 1978,
  end = 2014)

growth <- growth %>% 
  rename("Country" = "country") %>% 
  rename("Year" = "year") %>% 
  rename("growth" = "NY.GDP.MKTP.KD.ZG")

narr <- left_join(narr, growth)

# generating a dummy variable for Euro-currency

narr <- mutate(narr, "EURO"= if_else
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

# Importing trade openness (measured as the share of imports and exports as a % 
# of GDP). Data downloaded "Our World in Data", data source: Penn World Table.

data_path_open <- here("data/raw/trade-openness.csv")

trade_openness <- fread(data_path_open)

trade_openness <- trade_openness %>% 
  select(-"Code") %>% 
  rename("Country" = "Entity") %>% 
  rename(
    "trade_openness" = "Trade openness (share of exports and imports in GDP)")

narr <- left_join(narr, trade_openness)

# Furthermore, we import data for gdp per capita (PPP, current international $)
# from the WDI database.

gdp_pc_PPP <- WDI(
  indicator = "NY.GDP.PCAP.PP.CD",
  start = 1978,
  end = 2014)

gdp_pc_PPP <- gdp_pc_PPP %>% 
  rename("Country" = "country") %>% 
  rename("Year" = "year") %>% 
  rename("gdp_pc" = "NY.GDP.PCAP.PP.CD")

narr <- left_join(narr, gdp_pc_PPP)


# Descriptive summary statistic tables

summary_stat <- narr %>% 
  select(
    "Gini on market income" = gini_mkt, 
    "Gini on disposable income" = gini_disp, 
    "Growth rate" = growth,
    "Unemployment rate" = utr, 
    "Openness" = trade_openness
  ) %>% 
  summarise_each(
    funs(mean(., na.rm = TRUE),
         sd(., na.rm = TRUE),
         min(., na.rm = TRUE), 
         max(., na.rm = TRUE))) %>% 
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  select(variable, mean, sd, min, max) %>%
  mutate_each(funs(round(., 1)), -variable)

write.table(summary_stat, file = here("output/nar_summary_stat.txt"),
            sep = ",", quote = F, row.names = F)

get_year_range <- function(variable) {
  narr %>%
    filter(!is.na({{ variable }})) %>%
    group_by(Country) %>%
    summarize(YearRange = paste(min(Year), max(Year), sep = "-")) %>%
    ungroup() %>%
    mutate(YearRange = ifelse(is.na(YearRange), "NA", YearRange)) %>%
    pull(YearRange)
}

summary_years <- data.frame(
  Country = unique(narr$Country),
  "Unemployment rate" = get_year_range(utr),
  "Growth rate" = get_year_range(growth),
  "Openness" = get_year_range(trade_openness)
)

write.table(summary_years, file = here("output/nar_summary_years.txt"),
            sep = ",", quote = F, row.names = F)

# first simple regression

reg_1<- lm(gini_disp ~ gdp_pc + Austerity + trade_openness + factor(Country), data = narr)
summary(reg_1)

# Breusch-Pagan-Test for heteroskedasticity

bptest(reg_1)

# p-value is less than 0.05. Therefore we reject h0. 
# Since our model tested positive for heteroskedaasticity, we use
# robust standard errors by using the plm package

# refining the regression for panel data
# Hausmann Test to identify, if fixed effects regression for the panel data is viable

narrpd <- pdata.frame(narr, index= c("Country", "Year"))
attach(narrpd)

reg_2 <- plm(
  gini_disp ~ log(gdp_pc) + I(log(gdp_pc)^2) + Austerity + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

reg_3 <- plm(
  gini_mkt ~ log(gdp_pc) + I(log(gdp_pc)^2) + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)


phtest(reg_2, reg_3)

# 0-hypothesis is rejected, therefor we use fixed effects


# replicating the main results of Agnello and Sousa (2014: 712)

# Organizing our panel dataset by using the pdata.frame()-function of the plm
# (panel linear models) package, defining Country and Year as the dimensions
# of the panel dataset:

narrpd <- pdata.frame(narr, index= c("Country", "Year"))
attach(narrpd)

Agnello1 <- plm(
  gini_disp ~ log(gdp_pc) + I(log(gdp_pc)^2) + Austerity + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

Agnello2 <- plm(
  gini_mkt ~ log(gdp_pc) + I(log(gdp_pc)^2) + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

stargazer(
  Agnello1, Agnello2,
  type = "html",
  covariate.labels = c("log(GDP per capita)", "log(GDP per capita²)", NA,
                       "Openness"),
  out = here("output/nar_Agnelloreplic.html"),
  title = "'Replication' of table 1, column 1 and 2 of Agnello and Sousa (2014)"
)

# regression models: unemployment austerity interaction

FEausteritydisp <- plm(
  gini_disp ~ growth + Austerity + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

FEausterityunempdisp <- plm(
  gini_disp ~ growth + Austerity*lag(utr) + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

FEausteritymkt <- plm(
  gini_mkt ~ growth + Austerity + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

FEausterityunempmkt <- plm(
  gini_mkt ~ growth + Austerity*lag(utr) + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

stargazer(
  FEausteritymkt, FEausterityunempmkt, FEausteritydisp, FEausterityunempdisp,
  type = "html",
  covariate.labels = c("Growth", NA, "Unemployment (t-1)", "Openness", 
                       "Austerity x Unemployment (t-1)"),
  out = here("output/nar_regtableAusterityUnemp.html"),
  title = "Effects of high unemployment rates before consolidation episodes"
)

# using constructed dummy variables for indirect tax, direct tax, transfer 
# spending, or consumption and investment spending driven consolidation episodes

FEgranularmkt <- plm(
  gini_mkt ~ growth + IB + DB + TB + CB + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

FEgranularmktunemp <- plm(
  gini_mkt ~ growth + IB*lag(utr) + DB*lag(utr) + TB*lag(utr) + CB*lag(utr) +
    trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)
  
FEgranulardisp <- plm(
  gini_disp ~ growth + IB + DB + TB + CB + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

FEgranulardispunemp <- plm(
  gini_disp ~ growth + IB*lag(utr) + DB*lag(utr) + TB*lag(utr) + CB*lag(utr) +
    trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

stargazer(
  FEgranularmkt, FEgranularmktunemp, FEgranulardisp, FEgranulardispunemp,
  type = "html",
  covariate.labels = c("Growth", "ITB", "Unemployment (t-1)", "DTB", "TRB", 
                       "CISB", "Openness", "ITB x Unemployment (t-1)", 
                       "DTB x Unemployment (t-1)", "TRB x Unemployment (t-1)",
                       "CISB x Unemployment (t-1)"),
  out = here("output/nar_regtablegranularUnemp.html"),
  title = "Consolidation episodes driven by different fiscal measures"
)

# using the Euro dummy variable

FEausterityEUROmkt <- plm(
  gini_mkt ~ growth + Austerity*EURO + trade_openness + lag(utr),
  model = "within",
  effect = "individual",
  data = narrpd
)

FEausterityEUROdisp <- plm(
  gini_disp ~ growth + Austerity*EURO + trade_openness + lag(utr),
  model = "within",
  effect = "individual",
  data = narrpd
)

stargazer(
  FEausterityEUROdisp, FEausterityEUROmkt,
  type = "html",
  covariate.labels = c(
    "Growth", NA, NA, "Openness", "Unemployment (t-1)",
    "Austerity x EURO"),
  title = "Effects of Austerity in eurozone countries",
  out = here("output/nar_regtableAusterityEURO.html")
)

# TA plot for regression FEausterityunempmkt

TA_plot2 <- ggplot(
  data.frame(
    gefitteteWerte = fitted(FEausterityunempmkt),
    Residuen = residuals(FEausterityunempmkt)),
  aes(x=gefitteteWerte, y=Residuen)) + 
  ggtitle("Tukey-Anscombe-Plot Table 2, column 2") +
  geom_hline(yintercept = 0) +
  geom_point()

ggsave(
  "nar_TA-regressionTable2column2.jpeg", plot = TA_plot2, path = here("output"))

# checking if the residuals of our base regression are normally distributed
# with normal q q plot

res_nar <- residuals(FEausterityunempmkt)
qqnorm(res_nar)
qqline(res_nar)

# normal distribution of the residuals is questionable


# Testing for autocorrelation

Box.test(FEausterityunempmkt$residuals, lag = 1, type = "Box-Pierce")
Box.test(FEausterityunempmkt$residuals, lag = 1, type = "Ljung-Box")

# The H_0 is rejected. As expected, autocorrelation plays a role!

coeftest(FEausterityunempmkt,
         vcoc = vcovHAC(
           FEausterityunempmkt)
)

# Some first graphs


# A graph showing the development of the Gini over the years in our countries,
# as well as year where governments enacted austerity measures (marked as red
# vertical lines).

gini_and_austerity_periods <- ggplot(
  narr, 
  aes(x = Year, y = gini_mkt, group = Country)) +
  geom_line() +
  geom_vline(data = filter(narr, Austerity == 1),
             aes(xintercept = Year),
             color = 'red') +
  facet_wrap(~ Country, scales = 'free_y', ncol = 4) +
  labs(x = 'Year', y = 'Gini on market income') +
  scale_x_continuous(
    breaks = seq(min(narr$Year), max(narr$Year), by =10)) +
  scale_y_continuous(limits = c(min(narr$gini_mkt), max(narr$gini_mkt)))

print(gini_and_austerity_periods)

ggsave("nar_gini_mkt_austerity_periods.png",
       gini_and_austerity_periods,
       "png",
       here("output")
)

gini_and_granular_periods <- ggplot(
  narr, 
  aes(x = Year, y = gini_mkt, group = Country)) +
  geom_line() +
  geom_vline(data = filter(narr, DB == 1),
             aes(xintercept = Year),
             color = 'red') +
  geom_vline(data = filter(narr, IB == 1),
             aes(xintercept = Year),
             color = 'blue') +
  geom_vline(data = filter(narr, TB == 1),
             aes(xintercept = Year),
             color = 'green') +
  geom_vline(data = filter(narr, CB == 1),
             aes(xintercept = Year),
             color = 'black') +
  facet_wrap(~ Country, scales = 'free_y', ncol = 4) +
  labs(x = 'Year', y = 'Gini on market income') +
  scale_x_continuous(breaks = seq(min(Year), max(Year), by =10)) +
  scale_y_continuous(limits = c(min(gini_mkt), max(gini_mkt)))

print(gini_and_granular_periods)

ggsave("nar_gini_mkt_granulr_periods.png",
       gini_and_granular_periods,
       "png",
       here("output")
)



ggplot(narr,
       aes(Year, gini_disp, color = Country)) +
  geom_point()

# Kuznets curve (replicating Fig. 3 in Agnello and Sousa (2014))

gdpreg <- lm(log(gini_mkt) ~ log(gdp_pc) + I(log(gdp_pc)^2))

ggplot(mapping = aes(gdp_pc, gini_mkt) +
         geom_point() +
         scale_y_log10() +
         scale_x_log10() +
         geom_smooth(method = "lm", formula = y ~ poly(x,2)))
       
       # We can observe a kind of flipped Kuznets curve. This is probably because 
       # the dataset includes only rich countries.
       