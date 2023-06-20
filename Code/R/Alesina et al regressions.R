library(here)
library(ggplot2)
library(plm)
library(stargazer)
library(dplyr)

# Estimating FE models:

narr <- read.csv(here("data/tidy/dat_narrative1.csv"))

# Organizing our panel dataset by using the pdata.frame()-function of the plm
# (panel linear models) package, defining Country and Year as the dimensions
# of the panel dataset:

narrpd <- pdata.frame(narr, index= c("Country", "Year"))
attach(narrpd)

FEausteritydisp <- plm(
  log(gini_disp) ~ growth + Austerity + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

FEausterityunempdisp <- plm(
  log(gini_disp) ~ growth + Austerity*lag(utr) + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

FEausteritymkt <- plm(
  log(gini_mkt) ~ growth + Austerity + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

FEausterityunempmkt <- plm(
  log(gini_mkt) ~ growth + Austerity*lag(utr) + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

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

FEausterityEUROmkt <- plm(
  log(gini_mkt) ~ growth + Austerity*EURO + trade_openness + lag(utr),
  model = "within",
  effect = "individual",
  data = narrpd
)

FEausterityEUROdisp <- plm(
  log(gini_disp) ~ growth + Austerity*EURO + trade_openness + lag(utr),
  model = "within",
  effect = "individual",
  data = narrpd
)

FEausterityEUROdisp <- plm(
  log(gini_disp) ~ growth + Austerity*EURO + trade_openness + lag(utr),
  model = "within",
  effect = "individual",
  data = narrpd
)


FEred <- plm(
  rel_red ~ growth + Austerity + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)

FEredausterityunemp <- plm(
  rel_red ~ growth + Austerity*lag(utr) + trade_openness,
  model = "within",
  effect = "individual",
  data = narrpd
)


OLS_ginimkt_growth_austerity_openness <- lm(
  gini_mkt ~ growth + Austerity + trade_openness
)

FE_ginimkt_growth_austerity_openness <- plm(
  gini_mkt ~ lag(growth) + Austerity + lag(trade_openness),
  model = "within",
  effect = "twoways",
  data = narrpd
)

FE_ginidisp_growth_austerity_openness <- plm(
  gini_disp ~ growth + Austerity + trade_openness,
  index = c("Country", "Year"),
  model = "within",
  effect = "twoways",
  data = narrpd
)

OLS_ginimkt_growth_DB_IB_TB_CB_openness <- lm(
  gini_mkt ~ growth + DB + IB + TB + CB + trade_openness
)

OLS_ginidisp_growth_DB_IB_TB_CB_openness <- lm(
  gini_disp ~ growth + DB + IB + TB + CB + trade_openness
)

FE_ginidisp_growth_DB_IB_TB_CB_openness <- plm(
  gini_disp ~ growth + DB + IB + TB + CB + trade_openness,
  index = c("Country", "Year"),
  model = "within",
  effect = "twoways",
  data = narrpd
)

FE_ginimkt_Austerityu_growth_openness <- plm(
  gini_disp ~ Austerity*utr+ growth + trade_openness,
  index =c("Country", "Year"),
  model ="within",
  effect = "twoways",
  data= narrpd
)

FE_ginidisp_Transfers_DirectTax_IndirectTax_ConsInvSpending_growth <- 
  plm(gini_disp ~ Transfers*EURO + DirectTax*EURO + IndirectTax*EURO + 
        ConsInvSpending*EURO + growth + trade_openness,
      index = c("Country", "Year"),
      model = "within",
      effect = "twoways",
      data = narrpd
  )

FE_dummys_austerity_EURO <- plm(
  gini_disp ~ DB*EURO + IB*EURO + TB*EURO + CB*EURO + trade_openness + growth,
  index = c("Country", "Year"),
  model ="within",
  effect = "twoways",
  data= narrpd)

FE_dummys_austeritytax_EURO <- plm(
  gini_disp ~ DB*EURO + IB*EURO + trade_openness + growth,
  index = c("Country", "Year"),
  model ="within",
  effect = "twoways",
  data= narrpd)

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

stargazer(
  FEausteritymkt, FEausterityunempmkt, FEausteritydisp, FEausterityunempdisp,
  type = "html",
  covariate.labels = c("Growth", NA, "Unemployment (t-1)", "Openness", 
    "Austerity x Unemployment (t-1)"),
  out = here("output/AlesinaregtableAusterityUnemp.html")
  )

stargazer(
  FEgranularmkt, FEgranularmktunemp, FEgranulardisp, FEgranulardispunemp,
  type = "html",
  covariate.labels = c("Growth", "ITB", "Unemployment (t-1)", "DTB", "TRB", 
                       "CISB", "Openness", "ITB x Unemployment (t-1)", 
                       "DTB x Unemployment (t-1)", "TRB x Unemployment (t-1)",
                       "CISB x Unemployment (t-1)"),
  out = here("output/AlesinaregtablegranularUnemp.html")
  )

stargazer(
  FEausterityEUROdisp, FEausterityEUROmkt,
  type = "html",
  out = here("output/AlesinaregtableAusterityEURO.html")
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

ggsave("gini_mkt_austerity_periods.png",
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

ggsave("gini_mkt_granulr_periods.png",
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
         geom_smooth(method = "lm", formula = y ~ poly(x,2))
       
       # We can observe a kind of flipped Kuznets curve. This is probably because 
       # the dataset includes only rich countries.
       