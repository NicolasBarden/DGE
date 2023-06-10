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
library(plm)
library(psych)
library(ggplot2)
library(stargazer)
library(ggfortify)
library(lmtest)


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
                     gini_mkt_se, rel_red)
swiid_tidy$country[131:156] <- "Czechia"


#examples of rising inequality

swiid_gini_ex <- filter(swiid_tidy, country %in% c(
  "Germany", "France", "Italy", "Sweden", "Finland"))



plot_1 <- ggplot(data = swiid_gini_ex) +
  geom_line(mapping = aes(x = year, y = gini_mkt,
                           color = country))
ggsave("gini_examples.pdf", plot = plot_1, path = here::here())


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

# generating a dummy variable for Euro-currency

data_v6 <- dplyr::mutate(data_v6, "EURO"=dplyr::if_else
 (country == "Austria" & year >= 1999 
   | country == "Belgium" & year >= 1999
   | country == "Croatia" & year >= 2023
   | country == "Cyprus" & year >= 2008
   | country == "Estonia" & year >= 2011
   | country == "Finland" & year >= 1999
   | country == "France" & year >= 1999
   | country == "Germany" & year >= 1999
   | country == "Greece" & year >= 2001
   | country == "Ireland" & year >= 1999
   | country == "Italy" & year >= 1999
   | country == "Latvia" & year >= 2014
   | country == "Lithuania" & year >= 2015
   | country == "Luxembourg" & year >= 1999
   | country == "Malta" & year >= 2008
   | country == "Netherlands" & year >= 1999
   | country == "Portugal" & year >= 1999
   | country == "Slovakia" & year >= 2009
   | country == "Slovenia" & year >= 2007
   | country == "Spain" & year >= 1999,
   1,0))





# plotting the data, testing for correlation and getting an overview 
# goodness of fit

data_test <- subset(data_v6, select=c(gini_disp, gini_mkt, lending, expenditures,
                                      revenue, gdp_growth_rate, fbinpercGDP, unemp_rate))

pairs.panels(data_test, col="red")


# Running the first OLS regression

regression_1 = lm(gini_mkt ~ expenditures + revenue + gdp_growth_rate + 
                 fbinpercGDP + unemp_rate + EURO
               + factor(year) + factor(country), data = data_v6)
summary(regression_1)

# Breusch-Pagan-Test for heteroskedasticity

bptest(regression_1)

# p-value is less than 0.05. Therefore we reject h0. 
# Since our model tested positive for heteroskedaasticity, we use
# robust standard errors by using the plm package

# refining the regression for panel data
# Hausmann Test to identify, if fixed effects regression for the panel data is viable

reg_2 <- plm(formula = gini_mkt ~ expenditures + revenue + gdp_growth_rate + 
                      fbinpercGDP + unemp_rate + EURO, data = data_v6, effect = "twoways",
                    model = "within", index = c("country","year"))
summary(reg_2)

reg_3 <- plm(formula = gini_mkt ~ expenditures + revenue + gdp_growth_rate + 
                      fbinpercGDP + unemp_rate + EURO, data = data_v6, effect = "twoways",
                    model = "random", index = c("country","year"))
summary(reg_3)


phtest(reg_2, reg_3)

# 0-hypothesis is rejected, therefor we use fixed effects

# reevaluating the models functional form

# interaction between expenditures and revenue
reg_4 <- plm(formula = gini_mkt ~ expenditures * revenue + gdp_growth_rate + 
               fbinpercGDP + unemp_rate + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_4)

# logarithm of gini_mkt
reg_5 <- plm(formula = log(gini_mkt) ~ expenditures + revenue + gdp_growth_rate + 
               fbinpercGDP + unemp_rate + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_5)

# lagging unemp_rate and gdp_growth_rate
reg_5 <- plm(formula = log(gini_mkt) ~ expenditures + revenue + lag(gdp_growth_rate) + 
               fbinpercGDP + lag(unemp_rate) + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_5)

# combining the results for best results regarding significant variables and p-values
reg_6 <- plm(formula = gini_mkt ~ expenditures * revenue + lag(gdp_growth_rate) + 
                      fbinpercGDP + lag(unemp_rate) + EURO, data = data_v6, effect = "twoways",
                    model = "within", index = c("country","year"))
summary(reg_6)

# omitting variables as a test for multicollinearity

# basis
reg_7 <- plm(formula = gini_mkt ~ expenditures * revenue + lag(gdp_growth_rate) + 
               fbinpercGDP + lag(unemp_rate) + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_7)

# without expenditures and revenue

reg_8 <- plm(formula = gini_mkt ~ lag(gdp_growth_rate) + 
               fbinpercGDP + lag(unemp_rate) + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_8)

# without lag(gdp_growth_rate)
reg_9 <- plm(formula = gini_mkt ~ expenditures * revenue +  
               fbinpercGDP + lag(unemp_rate) + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_9) # p-value gets higher, but the level of significance declines

# without fbinpercGDP
reg_10 <- plm(formula = gini_mkt ~ expenditures * revenue + lag(gdp_growth_rate) 
              + lag(unemp_rate) + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_10)

# without lag(unemp_rate)
reg_11 <- plm(formula = gini_mkt ~ expenditures * revenue + lag(gdp_growth_rate) + 
               fbinpercGDP  + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_11)

# without EURO
reg_12 <- plm(formula = gini_mkt ~ expenditures * revenue + lag(gdp_growth_rate) + 
               fbinpercGDP + lag(unemp_rate), data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_12)

# lags in finaler form um endogenitätsprobleme zu vermeiden
# Results of these tests:

# aesthetic output
reg_output1 <- stargazer::stargazer(reg_7, reg_8, reg_9, reg_10, reg_11, reg_12, type = "html",
                                   title = "Results",
                                   align = TRUE,
                                   style = "aer",
                                   out = "Results_1.htm")


# testing the linear relationship assumption with Tukey-Anscombe-plot

ggplot(
  data.frame(
    gefitteteWerte = fitted(reg_7),
    Residuen = residuals(reg_7)),
  aes(x=gefitteteWerte, y=Residuen)) + 
  ggtitle("Tukey-Anscombe-Plot Regression 7") +
  geom_hline(yintercept = 0) +
  geom_point()


#assumption tests without robust standard errors

plot(lm(formula = gini_mkt ~ expenditures * revenue + lag(gdp_growth_rate) + 
          fbinpercGDP + lag(unemp_rate) + EURO, data = data_v6))
# testing the normal distribution of the residuals wuth normal Q-Q-plot


#Normal Q-Q: Used to examine whether the residuals are normally 
#distributed. It’s good if residuals points follow the straight dashed line.

#Scale-Location (or Spread-Location): Used to check the homogeneity of 
#variance of the residuals (homoscedasticity). Horizontal line with equally
#spread points is a good indication of homoscedasticity. 
#This is not the case in our model, where we have a heteroscedasticity problem.

#Residuals vs Leverage. Used to identify influential cases, 
#that is extreme values that might influence the regression results 
#when included or excluded from the analysis.
#An influential value is a value, which inclusion or exclusion can alter the 
#results of the regression analysis. Such a value is associated with a large residual.
#Not all outliers (or extreme data points) are influential in linear regression analysis.
#Statisticians have developed a metric called Cook’s distance to determine 
#the influence of a value. This metric defines influence as a combination of 
#leverage and residual size.
#A rule of thumb is that an observation has high influence if 
#Cook’s distance exceeds 4/(n - p - 1)(P. Bruce and Bruce 2017), 
#where n is the number of observations and p the number of predictor variables.
#The Residuals vs Leverage plot can help us to find influential observations if any. 
#On this plot, outlying values are generally located at the upper right corner 
#or at the lower right corner. Those spots are the places where data points 
#can be influential against a regression line.



# fixed effects grafik mit gini auf der y-achse und austerity oder unemp auf x


