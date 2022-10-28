# Analytic dataset construction
# Use analytic_data from analytic_data.rds
library(dplyr)
library(tidyr)
library(janitor)

readRDS("processed_data.rds")%>%
  list2env(envir=.GlobalEnv)
strategies <- c("Vaccination","Etiquette","Masks","Physical Distancing",
                "Cohorting or Staggering","Testing and Screening","Stay Home",
                "Trace and Quarantine","Cleaning","Ventilation")
strategies <- c("vaccination","etiquette","masks","physicaldistancing",
                "cohortingorstaggering","testingandscreening","stayhome",
                "traceandquarantine","cleaning","ventilation")
# To finalize the analytic dataset there are a few adjustments that need to be made
# First, divide rolling_mean by enrollment; second, create dichotomous indicator
# for each strategy indicating if strategy scores in 75th percentile or greater
final <- analytic %>%
  filter(month %in% c("October","January"))%>%
  select(-c(hd,student,date,policymonth),-c(americanindianoralaskanative:white),
         -starts_with("spl"))%>%
  group_by(qid,schoollevel)%>%
  mutate(caserate = rollingmean/enrollment*100)%>%
  select(qid,schoollevel,district,month,quarterenacted,enrollment,caserate,everything(),-rollingmean)%>%
  group_by(qid,schoollevel)%>%
  pivot_wider(names_from=month,values_from=caserate)%>%
  ungroup()%>%
  mutate(across(starts_with("rpl"),~100*.),
         changeinrate = January - October,
         across(all_of(strategies),~ifelse(. >= quantile(.,c(.5)),1,0),.names = "{.col}75")
         )%>%
  relocate(changeinrate,.before=vaccination)

saveRDS(final,"analytic_data.rds")
writexl::write_xlsx(final,path="analytic_data.xlsx")
