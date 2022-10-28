# This project corresponds to Analysis#2:
# What is the association between districts’ Covid-19 prevention policies 
# and subsequent Covid-19 schools’ caseloads?

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(janitor)
library(zoo)
library(readxl)
library(stringr)
# Will need data from three main sources: state-reported case count (cases.rds),
# district policy data (district_policy_dataset.xlsx),
# and community factors (community prevalence, SVI, etc.)

# State data
readRDS("raw_data/cases.rds")%>%
  list2env(envir=.GlobalEnv)

# use cleaned
# calculate the 3-month rolling average. For initial analyses, likely to use
# the average of October, November, and December, for base, and Jan, Feb, and March 
# for post. To get this value for each school, filter for the first month of the 
# rolling average you want (e.g., October for Oct, Nov, and Dec).
full_cases <- cleaned %>%
  select(qid=id,school_level,hd=HD,date,student)%>%
  group_by(qid,school_level)%>%
  mutate(month = month.name[clock::get_month(date)],
    rolling_mean = zoo::rollapply(student,width=3,
                                   FUN=function(x)mean(x,na.rm=T),
                                   partial = TRUE,align="left")
  )%>%
  ungroup()


# District policy dataset
# Use Quarter as period of strategy onset
# Use item scores to calculate sum score rather than 
# using pre-calculated sum scores (columns ending in SS)
policy_items <- readxl::read_xlsx("district_policy_items.xlsx")%>%
  mutate(Item = str_replace_all(Item,"[:punct:]",""))%>%
  tibble::deframe()

policy <- readxl::read_xlsx("raw_data/district_policy_assessments.xlsx")%>%
  select(qid=MDR,school_level=`School Level`,district=District,Month,Quarter,26:last_col(),-ends_with("SS"))%>%
  pivot_longer(cols=6:46)%>%
  mutate(name = str_replace_all(name,"[:punct:]",""),
         construct = stringr::str_replace_all(name,policy_items))%>%
  group_by(across(qid:Quarter),construct)%>%
  mutate(total = sum(value,na.rm=T))%>%
  ungroup()%>%
  select(-name,-value)%>%
  unique()%>%
  pivot_wider(id_cols=qid:Quarter,names_from=construct,values_from=total)%>%
  mutate(Quarter = str_extract(Quarter,"^[:alpha:]+"))%>%
  rename(policy_month=Month,quarter_enacted=Quarter)

policy %>%
summarise(across(where(is.numeric),~max(.,na.rm=TRUE)))%>%
  View()

# School characteristics
nces <- readRDS("raw_data/nces_stats.rds")%>%
  select(ncessch,derived_total_enrolled:last_col())%>%
  unique()

# School information from the masterlist
ml <- readRDS("raw_data/sampling_masterlist.rds")%>%
  select(ncessch,qid,school_level,enrollment,state,county,region,locale)%>%
  inner_join(nces,by="ncessch",na_matches="never")

# SVI information
svi <- readRDS("raw_data/svi.rds")%>%
  clean_names()%>%
  select(qid,school_level,rpl_theme1:last_col())




final <- list(cases=full_cases,district_policies=policy,ml=ml,svi=svi)
analytic <- purrr::reduce(final,inner_join,by=c("qid","school_level")) %>%
  clean_names()%>%
  rename_with(.fn=~gsub("_","",.))
final <- purrr::prepend(final,list(analytic=analytic))
saveRDS(final,"processed_data.rds")
writexl::write_xlsx(final,path="processed_data.xlsx")


