# Analytic dataset construction
# Use analytic_data from analytic_data.rds
library(dplyr)
library(tidyr)
library(janitor)

readRDS("processed_data.rds")%>%
  list2env(envir=.GlobalEnv)

strategies <- c("vaccination","masks","physicaldistancing",
                "screeningtestingforstudents","stayhome","contacttracing",
                "quarantine","cleaning","hepafilters","hvacsystems")
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
                                              TRUE ~ 0))
  )%>%
  relocate(changeinrate,.before=vaccination)

saveRDS(final,"analytic_data.rds")
writexl::write_xlsx(final,path="analytic_data.xlsx")
