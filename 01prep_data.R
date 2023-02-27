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
library(clock)
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
policy_info <- readxl::read_xlsx("district_policy_items.xlsx")
policy_selections <- policy_info %>%
  filter(cdc==1)%>%
  mutate(Item = str_replace_all(Item,"[:punct:]",""))
policy_items <- policy_info %>%
  mutate(Item = str_replace_all(Item,"[:punct:]","")) %>%
  filter(cdc==1)%>%
  select(c(1:2))%>%
  tibble::deframe()

policy <- readxl::read_xlsx("raw_data/2021-2022 School District COVID-19 Policy and Guidance Catalog.xlsx",
                            sheet = 'Document Catalog',col_names=TRUE,skip = 1)%>%
  select(qid='MDR Number',school_level=`School Level`,district='District Name','Time Period Published',
         any_of(policy_info$Item))%>%
  mutate(qid = ifelse(str_count(qid)==11,paste0('0',qid),qid),
    quarter_enacted = case_when(tolower(`Time Period Published`) %in% tolower(c('July','August','September','October','November','December','First Quarter','Second Quarter'))~1,
                                `Time Period Published` %in% c('Missing','-1') ~ NA_real_,
                                TRUE ~ 0),
    .before = 1
  )%>%
  select(-`Time Period Published`)%>%
  pivot_longer(5:last_col())%>%
  mutate(name = str_remove_all(name,"[:punct:]"),
         construct = stringr::str_replace_all(name,policy_items))%>%
  filter(name %in% names(policy_items))%>%
  inner_join(policy_selections, by=c("name"="Item","construct"="Construct"))%>%
  mutate(achieved = ifelse(value==score_needed,1,0))%>%
  select(-c(name,value,cdc,score_needed)) %>%
  group_by(across(quarter_enacted:construct))%>%
  mutate(total = case_when((operation == "either" & sum(achieved)>0)|
                             (operation=="all" & sum(achieved)==n())~1,
                           TRUE ~ 0)
  )%>%
  select(-operation,-achieved)%>%
  ungroup()%>%
  unique()%>%
  pivot_wider(id_cols=quarter_enacted:district,names_from=construct,values_from=total)


#policy %>%
#summarise(across(where(is.numeric),~max(.,na.rm=TRUE)))

# School characteristics
# There is some missingness from the 2021 NCES, so replace it with previous 
# value
nces21 <- readRDS("raw_data/nces_2021.rds")%>%
  select(ncessch,derived_total_enrolled=total_enrolled,everything())%>%
  unique()%>%
  rename_with(.fn=~gsub("_","",.x),starts_with("percent"))%>%
  rename_with(.fn=~gsub("enr","",.x),starts_with("percent"))

nces19 <- readRDS("raw_data/nces_stats.rds")%>%
  rename_with(.fn=~gsub("_","",.x),starts_with("percent"))%>%
  rename(percentfreereducedlunch=percentstudentsfreereducedlunch)%>%
  select(names(nces21))
nces <- nces21 %>%
  rows_update(nces19,by="ncessch",unmatched="ignore")


# School information from the masterlist
locale <- readRDS("raw_data/locale.rds")
ml <- readRDS("raw_data/sampling_masterlist.rds")%>%
  select(ncessch,qid,school_level,enrollment,district_name,state,fips,region,locale)%>%
  left_join(nces,by="ncessch",na_matches="never")%>%
  mutate(derived_total_enrolled = ifelse(is.na(derived_total_enrolled),enrollment,derived_total_enrolled),
         locale = na_if(locale, "Missing")) %>% # locale variable
           rows_patch(locale, by = "ncessch", unmatched = "ignore")
table(ml$locale,useNA="always")
# SVI information
svi <- readRDS("raw_data/svi.rds")%>%
  clean_names()%>%
  select(qid,school_level,rpl_themes)

# HHS Protect county cases and deaths
covid <- readRDS("raw_data/cases_deaths.rds")%>%
  purrr::pluck("df")%>%
  select(cnty_fips,x2021_10_15_new_cases_7_day_rolling_average:x2022_03_15_new_cases_7_day_rolling_average)%>%
  select(cnty_fips,contains("cases"))%>%
  rename_with(.fn = ~gsub("_new_cases_7_day_rolling_average","",.),starts_with("x"))%>%
  pivot_longer(starts_with("x"))%>%
  group_by(cnty_fips)%>%
  mutate(name = str_extract_all(name,"[:digit:].*")%>%unlist(),
         name = date_parse(name,format="%Y_%m_%d")%>%as_year_month_day(),
         cnty_cases = zoo::rollapply(value,width=3,
                                       FUN=function(x)mean(x,na.rm=T),
                                       partial = TRUE,align="left")
         )%>%
  select(-value)%>%
  filter(get_month(name) %in% c(1,10))%>%
  inner_join(ml[,c("fips","qid","school_level")],by=c("cnty_fips"="fips"))%>%
  pivot_wider(id_cols=c(cnty_fips,qid,school_level),values_from=cnty_cases,names_from=name)%>%
  rename(october=c(4),january=c(5))%>%
  mutate(cntycaseschange = january - october)%>%
  ungroup()%>%
  select(qid,school_level,cntycaseschange)%>%
  unique()
  

  


final <- list(cases=full_cases,district_policies=policy,ml=ml,svi=svi,covid=covid)
analytic <- purrr::reduce(final,inner_join,by=c("qid","school_level")) %>%
  clean_names()%>%
  rename_with(.fn=~gsub("_","",.))
final <- purrr::prepend(final,list(analytic=analytic))
saveRDS(final,"processed_data.rds")
writexl::write_xlsx(final,path="processed_data.xlsx")


