# Summary statistics 
# Variables of interest
# Dependent variable: change in average case rate per 100 students between
# fall and spring of 2021-22. Fall calculated as average of October, November, 
# and December and Spring as average of January, February, and March.
# 
# Independent variables:
# School level:
# percent free and reduced lunch eligible from 2020 NCES
# percent black, white, hispanic, asian, hawaiian, other
# 
# County
# SVI
# case count (NEED)
# 
# locale, region
# 
# POLICY VARIABLES
# Vaccination max: 21
# Etiquette max: 8
# Masks max: 15
# Physical distancing max: 6
# Cohorting or staggering max: 3
# Testing and Screening max: 8
# Stay Home max: 10
# Trace and Quarantine max: 10
# Cleaning max: 6
# Ventilation max: 16
# 
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
df <- readRDS("analytic_data.rds")

predictors <- df %>%
  select(vaccination:ventilation,starts_with(c("percent","rpl")),ends_with("75")) %>%
  names()

list_sum_stats <- list(n=~sum(!is.na(.)),
                       min=~round(min(.,na.rm=T),2),
                  max=~round(max(.,na.rm=T),2),
                  mean=~round(mean(.,na.rm=T),2),
                  median=~round(median(.,na.rm=T),2),
                  stdev=~round(sd(.,na.rm=T),2),
                  iqr=~round(IQR(.,na.rm=T),2),
                  tiles = ~quantile(.,na.rm=T,c(0.25,.75)),
                  prob = ~c(0.25,.75))

summary_statistics <- df %>% 
  summarise(across(all_of(c("changeinrate",predictors)),list_sum_stats))%>%
  pivot_longer(cols=everything(),names_to=c("construct",".value"),names_sep="_")%>%
  pivot_wider(names_from = prob,values_from=tiles)

dichotomous_vars <- summary_statistics %>%
  select(construct)%>%
  filter(str_detect(construct,"75$"))%>%
  pull()

# t-test looking at differences between change in case rates by dichotomous vars
ttest_results<-map_dfr(dichotomous_vars[-5], ~
              glue::glue("changeinrate ~ {.x}")%>%
                as.formula()%>%
                t.test(data=df,na.action = na.omit) %>%
                tidy()%>%
                mutate(predictor = .x,
                       across(where(is.numeric),~round(.,3)))%>%
                relocate(predictor, .before=1)
              # %>%
              #   rename(group0 = estimate1, group1=estimate2)
)
                         