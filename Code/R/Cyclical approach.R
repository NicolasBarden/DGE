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
library(GGally)



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


#OECD-data for the CAPB, CACD, CACR as a measure of austerity

#importing and transforming CAPB data

data_path_CAPB <- here("data/raw/OECD 06.23 CAPB.csv")

oecd_CAPB <- fread(data_path_CAPB,
                   sep = ",", dec = ".",
                   na.strings = "-")

oecd_CAPB_tidy <- oecd_CAPB %>%
  select(c("Country", "Time", "Value")) %>%
  rename(country = Country, year = Time, CAPB = Value) %>%
  filter(country %in% c(
    "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
    "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", 
    "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
    "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
    "United Kingdom"))
oecd_CAPB_tidy

oecd_CAPB_tidy <- dplyr::mutate(oecd_CAPB_tidy, "changeCAPB")
oecd_CAPB_tidy <- oecd_CAPB_tidy %>%
  rename("changeCAPB" = '"changeCAPB"')
oecd_CAPB_tidy$changeCAPB <- c(NA, diff(oecd_CAPB_tidy$CAPB) / oecd_CAPB_tidy$CAPB[-length(oecd_CAPB_tidy$CAPB)]) * 100
oecd_CAPB_tidy


#importing and transforming CACD data

data_path_CACD <- here("data/raw/OECD 06.23 CACD.csv")

oecd_CACD <- fread(data_path_CACD,
                   sep = ",", dec = ".",
                   na.strings = "-")

oecd_CACD_tidy <- oecd_CACD %>%
  select(c("Country", "Time", "Value")) %>%
  rename(country = Country, year = Time, CACD = Value) %>%
  filter(country %in% c(
    "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
    "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", 
    "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
    "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
    "United Kingdom"))
oecd_CACD_tidy


oecd_CACD_tidy <- dplyr::mutate(oecd_CACD_tidy, "changeCACD")
oecd_CACD_tidy <- oecd_CACD_tidy %>%
  rename("changeCACD" = '"changeCACD"')
oecd_CACD_tidy$changeCACD <- c(NA, diff(oecd_CACD_tidy$CACD) / oecd_CACD_tidy$CACD[-length(oecd_CACD_tidy$CACD)]) * 100
oecd_CACD_tidy

#importing and transforming CACR data

data_path_CACR <- here("data/raw/OECD 06.23 CACR.csv")

oecd_CACR <- fread(data_path_CACR,
                   sep = ",", dec = ".",
                   na.strings = "-")

oecd_CACR_tidy <- oecd_CACR %>%
  select(c("Country", "Time", "Value")) %>%
  rename(country = Country, year = Time, CACR = Value) %>%
  filter(country %in% c(
    "Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia", "Ireland",
    "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", 
    "Lithuania", "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria",
    "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden",
    "United Kingdom"))
oecd_CACR_tidy


oecd_CACR_tidy <- dplyr::mutate(oecd_CACR_tidy, "changeCACR")
oecd_CACR_tidy <- oecd_CACR_tidy %>%
  rename("changeCACR" = '"changeCACR"')
oecd_CACR_tidy$changeCACR <- c(NA, diff(oecd_CACR_tidy$CACR) / oecd_CACR_tidy$CACR[-length(oecd_CACR_tidy$CACR)]) * 100
oecd_CACR_tidy


# Transforming the SWIID-data to tibble.

swiid_tidy <- tibble(swiid_tidy)

# Joining the data sets.

data_v1 <- left_join(swiid_tidy, oecd_CAPB_tidy)
data_v2 <- left_join(data_v1, oecd_CACD_tidy)
data_v3 <- left_join(data_v2, oecd_CACR_tidy)


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

data_v5 <- left_join(data_v4, unemp_tidy)

# generating a dummy variable for Euro-currency

data_v5 <- dplyr::mutate(data_v5, "EURO"=dplyr::if_else
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

# Importing trade openess (measured as the share of imports and exports as a % 
# of GDP). Data downloaded "Our World in Data", data source: Penn World Table.

data_path_open <- here("data/raw/trade-openness.csv")

trade_openness <- fread(data_path_open)

trade_openness <- trade_openness %>% 
  select(-"Code") %>% 
  rename("country" = "Entity") %>% 
  rename("year" = "Year") %>% 
  rename(
    "trade_openness" = "Trade openness (share of exports and imports in GDP)")

data_v6 <- left_join(data_v5, trade_openness)

write.csv(data_v6, file = here("data/tidy/data_v6.csv"))

# Descriptive summary statistic tables

summary_stat <- data_v6 %>% 
  select(
    "Gini on market income" = gini_mkt, 
    "Gini on disposable income" = gini_disp, 
    "Growth rate" = gdp_growth_rate,
    "Unemployment rate" = unemp_rate, 
    "Openness" = trade_openness,
    CAPB, CACD, CACR
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

write.table(summary_stat, file = here("output/summary_stat_cyclical.txt"),
            sep = ",", quote = F, row.names = F)

get_year_range <- function(variable) {
  data_v6 %>%
    filter(!is.na({{ variable }})) %>%
    group_by(country) %>%
    summarize(YearRange = paste(min(year), max(year), sep = "-")) %>%
    ungroup() %>%
    mutate(YearRange = ifelse(is.na(YearRange), "NA", YearRange)) %>%
    select(country, YearRange)
}

summary_years_list <- list(
  get_year_range(unemp_rate),
  get_year_range(gdp_growth_rate),
  get_year_range(trade_openness),
  get_year_range(CAPB),
  get_year_range(CACD),
  get_year_range(CACR))

summary_years <- summary_years_list %>% 
  reduce(full_join, by = "country") %>% 
  rename("Unemployment rate" = YearRange.x) %>% 
  rename("Growth rate" = YearRange.y) %>% 
  rename("Openness" = YearRange.x.x) %>% 
  rename("CAPB" = YearRange.y.y) %>% 
  rename("CACD" = YearRange.x.x.x) %>% 
  rename("CACR" = YearRange.y.y.y)

write.table(summary_years, file = here("output/summary_years_cyclical.txt"),
            sep = ",", quote = F, row.names = F)

# plotting the data, testing for correlation and getting an overview 
# goodness of fit

data_test <- subset(data_v6, select=c(gini_disp, gini_mkt, CAPB, CACR,
                                      CACD, gdp_growth_rate, trade_openness, unemp_rate))


# alternative

plot_2 <- ggpairs(data_test, proportions = "auto")
ggsave("scatterplot.jpeg", plot = plot_2, path = here::here())


# Running the first OLS regression

regression_1 = lm(gini_mkt ~ CAPB + gdp_growth_rate + 
                    trade_openness + unemp_rate + EURO
               + factor(year) + factor(country), data = data_v6)
summary(regression_1)

# Breusch-Pagan-Test for heteroskedasticity

bptest(regression_1)

# p-value is less than 0.05. Therefore we reject h0. 
# Since our model tested positive for heteroskedaasticity, we use
# robust standard errors by using the plm package

# refining the regression for panel data
# Hausmann Test to identify, if fixed effects regression for the panel data is viable

reg_2 <- plm(formula = gini_mkt ~ CAPB + gdp_growth_rate + 
               trade_openness + unemp_rate + EURO, data = data_v6, effect = "twoways",
                    model = "within", index = c("country","year"))
summary(reg_2)

reg_3 <- plm(formula = gini_mkt ~ CAPB + gdp_growth_rate + 
               trade_openness + unemp_rate + EURO, data = data_v6, effect = "twoways",
                    model = "random", index = c("country","year"))
summary(reg_3)


phtest(reg_2, reg_3)

# 0-hypothesis is rejected, therefor we use fixed effects

# trying to reproduce Schneider et al.

reg_Sch_et_al1 <- plm(formula = gini_mkt ~  CAPB * EURO + lag(CAPB) *EURO + lag(gdp_growth_rate) + 
                                lag(trade_openness) + EURO, data = data_v6, effect = "twoways",
                              model = "within", index = c("country","year"))
summary(reg_Sch_et_al1)

reg_Sch_et_al2 <- plm(formula = gini_disp ~  CAPB * EURO + lag(CAPB) *EURO + lag(gdp_growth_rate) + 
                       lag(trade_openness) + EURO, data = data_v6, effect = "twoways",
                     model = "within", index = c("country","year"))
summary(reg_Sch_et_al2)


reg_Sch_et_al3 <- plm(formula = gini_mkt ~ CACD * EURO + lag(CACD) *EURO + 
                       CACR * EURO + lag(CACR) *EURO +
                       lag(gdp_growth_rate) + 
                       lag(trade_openness), data = data_v6, effect = "twoways",
                     model = "within", index = c("country","year"))
summary(reg_Sch_et_al3)

reg_Sch_et_al4 <- plm(formula = gini_mkt ~ CACD * EURO + lag(CACD) *EURO + 
                        CACR * EURO + lag(CACR) *EURO +
                        lag(gdp_growth_rate) + 
                        lag(trade_openness), data = data_v6, effect = "twoways",
                      model = "within", index = c("country","year"))
summary(reg_Sch_et_al4)

reg_output_Schn <- stargazer::stargazer(
  reg_Sch_et_al1, reg_Sch_et_al2, reg_Sch_et_al3, reg_Sch_et_al4, type = "html",
       title = "'Replication' of table 6, column 1 and 2 of Schneider et al. (2016)",
       out = here("output/Schneiderreplic.htm"))

# reevaluating the models functional form

# interaction between EURO and austerity proxy as in Schneider et al.
reg_4 <- plm(formula = gini_mkt ~ CAPB * EURO + lag(CAPB) + gdp_growth_rate + 
               trade_openness + unemp_rate + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_4)

# interaction between unemployment rate and the measures for austerity

reg_4a <- plm(formula = gini_mkt ~ CAPB * EURO + lag(CAPB) + gdp_growth_rate + 
               trade_openness + unemp_rate*CAPB , data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_4a)

# logarithm of gini_mkt
reg_5 <- plm(formula = log(gini_mkt) ~ CAPB * EURO+ lag(CAPB) +gdp_growth_rate + 
               trade_openness + unemp_rate + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_5)

# lagging unemp_rate and gdp_growth_rate
reg_5 <- plm(formula = log(gini_mkt) ~ CAPB *EURO + lag(CAPB) +lag(gdp_growth_rate) + 
               trade_openness + lag(unemp_rate) + EURO, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_5)

# combining the results for best results regarding significant variables and p-values
reg_6 <- plm(formula = gini_mkt ~ CAPB * EURO + lag(CAPB) + gdp_growth_rate + 
               trade_openness + lag(unemp_rate), data = data_v6, effect = "twoways",
                    model = "within", index = c("country","year"))
summary(reg_6)

# CACR*CACD more significant than CAPB
reg_7 <- plm(formula = gini_mkt ~ CACD * EURO + lag(CACD) + CACR * EURO + lag(CACR) +
               lag(gdp_growth_rate) + 
               trade_openness + lag(unemp_rate), data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_7)



# omitting variables as a test for multicollinearity

# base model
reg_8 <- plm(formula = gini_mkt ~ CACD * EURO + lag(CACD) + CACR * EURO + lag(CACR) +
               lag(gdp_growth_rate) + trade_openness
               + lag(unemp_rate) * CACD +  lag(unemp_rate) * CACR, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_8)

# without austerity proxy

reg_9 <- plm(formula = gini_mkt ~ 
               lag(gdp_growth_rate) + 
               trade_openness + lag(unemp_rate), data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_9)

# without lag(gdp_growth_rate)
reg_10 <- plm(formula = gini_mkt ~ CACD * EURO + lag(CACD) + CACR * EURO + lag(CACR) +
                trade_openness + lag(unemp_rate) +
                + lag(unemp_rate) * CACD +  lag(unemp_rate) * CACR, data = data_v6, effect = "twoways",
              model = "within", index = c("country","year"))
summary(reg_10) 

# without trade_openness
reg_11 <- plm(formula = gini_mkt ~ CACD * EURO + lag(CACD) + CACR * EURO + lag(CACR) +
                gdp_growth_rate + lag(unemp_rate) * CACD +  lag(unemp_rate) * CACR,
                data = data_v6, effect = "twoways",
                model = "within", index = c("country","year"))
summary(reg_11)

# without lag(unemp_rate)
reg_12 <- plm(formula = gini_mkt ~ CACD * EURO + lag(CACD) + CACR * EURO + lag(CACR) +
                gdp_growth_rate + 
                trade_openness, data = data_v6, effect = "twoways",
              model = "within", index = c("country","year"))
summary(reg_12)

# without EURO
reg_13 <- plm(formula = gini_mkt ~ CACD + lag(CACD) + CACR + lag(CACR) +
                gdp_growth_rate + trade_openness  + lag(unemp_rate) * CACD +
                lag(unemp_rate) * CACR, data = data_v6, effect = "twoways",
                model = "within", index = c("country","year"))
summary(reg_13)

# Results of testing omitting variables:

reg_output1 <- stargazer::stargazer(reg_8, reg_9, reg_10, reg_11, reg_12, reg_13,
                                   type = "html",
                                   title = "Results omitted",
                                   align = TRUE,
                                   style = "aer",
                                   out = "Results_omitted.htm")

# most significant regression

reg_8 <- plm(formula = gini_mkt ~ CACD * EURO + lag(CACD) + CACR * EURO + lag(CACR) +
               lag(gdp_growth_rate) + trade_openness
               + lag(unemp_rate) * CACD +  lag(unemp_rate) * CACR,
               data = data_v6, effect = "twoways",
               model = "within", index = c("country","year"))
summary(reg_8)

# regression assumption testing with our most significant base model 

# testing the linear relationship assumption with Tukey-Anscombe-plot

TA_plot <- ggplot(
  data.frame(
    gefitteteWerte = fitted(reg_8),
    Residuen = residuals(reg_8)),
  aes(x=gefitteteWerte, y=Residuen)) + 
  ggtitle("Tukey-Anscombe-Plot Regression 8") +
  geom_hline(yintercept = 0) +
  geom_point()
TA_plot
ggsave("cyc_TA-plot.jpeg", plot = TA_plot, path = here::here("output"))

# checking if the residuals of our base regression are normally distributed
# with normal q q plot

res_8 <- residuals(reg_8)
qqnorm(res_8)
qqline(res_8)

# Testing for autocorrelation

Box.test(res_8, lag = 1, type = "Box-Pierce")
Box.test(res_8, lag = 1, type = "Ljung-Box")

# The H_0 is rejected. As expected, autocorrelation plays a role!

coeftest(reg_8,
         vcoc = vcovHAC(
           reg_8)
)



# regression results, with different gini-coefficients and varying the austerity proxy

reg_8 <- plm(formula = gini_mkt ~ CACD * EURO + lag(CACD) + CACR * EURO + lag(CACR) +
               lag(gdp_growth_rate) + 
               trade_openness + lag(unemp_rate) * CACD +  lag(unemp_rate) * CACR,
               data = data_v6, effect = "twoways",
               model = "within", index = c("country","year"))
summary(reg_8)

reg_14 <- plm(formula = gini_disp ~ CACD * EURO + lag(CACD) + CACR * EURO + lag(CACR) +
               lag(gdp_growth_rate) + trade_openness + 
                + lag(unemp_rate) * CACD +  lag(unemp_rate) * CACR,
               data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_14)

reg_15 <- plm(formula = gini_mkt ~ CAPB * EURO + lag(CAPB) +
                lag(gdp_growth_rate) + 
                trade_openness + lag(unemp_rate)*CAPB, data = data_v6, effect = "twoways",
              model = "within", index = c("country","year"))
summary(reg_15)

reg_16 <-plm(formula = gini_disp ~ CAPB * EURO + lag(CAPB) +
               lag(gdp_growth_rate) + 
               trade_openness + lag(unemp_rate)*CAPB, data = data_v6, effect = "twoways",
             model = "within", index = c("country","year"))
summary(reg_16)

reg_output2 <- stargazer(
  reg_8, reg_14, reg_15, reg_16, 
  type = "html",
  title = "Effects of high unemployment rates before consolidation episodes",
  covariate.labels = c(NA, NA, NA, "CACD (t-1)", NA, "CACR (t-1)", "CAPB (t-1)",
                       "Growth (t-1)", "Openness", "Unemployment (t-1)",
                       "CACD x EURO", "CACR x EURO", "CACD x Unemployment (t-1)",
                       "CACR x Unemployment (t-1)", "CAPB x EURO",
                       "CAPB x Unemployment (t-1)"),
  out = here("output/Schneiderresunemp.htm"))

