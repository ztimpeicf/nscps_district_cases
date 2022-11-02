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
  select(-c(hd,student,date,policymonth),-c(americanindianoralaskanative:white))%>%
  group_by(qid,schoollevel)%>%
  mutate(caserate = rollingmean/enrollment*100)%>%
  select(qid,schoollevel,district,month,quarterenacted,enrollment,caserate,everything(),-rollingmean)%>%
  group_by(qid,schoollevel)%>%
  pivot_wider(names_from=month,values_from=caserate)%>%
  ungroup()%>%
  mutate(rplthemes = rplthemes*100,
         changeinrate = January - October,
         across(all_of(strategies),~case_when(quarterenacted %in% c("First","Second")~.,
                                              TRUE ~ 0),.names = "{.col}quarter"), # If policy enacted during 1st or 2nd quarter of school year, score 1. otherwise 0.
         across(all_of(strategies),~ifelse(. >= quantile(.,c(.5)),1,0),.names = "{.col}.50"), # If school at 50th percentile 1, otherwise 0.
         across(c(all_of(strategies),ends_with("quarter")),~ifelse(. >= quantile(.,c(.75)),1,0),.names = "{.col}.75"),
         across(ends_with("quarter"),~ifelse(. > 0,1,0),.names = "{.col}.nonzero")
         )%>%
  relocate(changeinrate,.before=vaccination)

saveRDS(final,"analytic_data.rds")
writexl::write_xlsx(final,path="analytic_data.xlsx")
