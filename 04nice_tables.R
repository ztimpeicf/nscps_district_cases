# Prepare statistics tables for presentation in .qmd
library(dplyr)
library(tidyr)
library(stringr)
readRDS("raw_summary_statistics.rds")%>%
  list2env(envir=.GlobalEnv)

replace <- readxl::read_xlsx("construct_texts.xlsx")

# table 1: summary statistics of outcome and covariates
tbl1 <- summary_statistics %>%
  filter(str_detect(construct,"changeinrate|percent"))%>%
  left_join(replace,by=c("construct"="construct"))%>%
  transmute(
    Construct = replace,
    #construct = gsub("quarter","",construct),
    min_max = glue::glue("{n} ({min}, {max})"),
    mean_median = glue::glue("{mean} ({stdev})")
  )
  

# Section of measures before dichotomization
# table 2: summary statistics of 10 policy categories implemented in fall 2021
# and covariates. 
tbl2 <- summary_statistics %>%
  filter(str_detect(construct,"quarter$|percent"))%>%
  left_join(continuous_correlations,by=c("construct"="predictor"))%>%
  left_join(replace,by="construct")%>%
  transmute(
    Construct = replace,
    min_max = glue::glue("{n} ({min}, {max})"),
    mean_median = glue::glue("{mean} ({stdev})"),
    est_pval = glue::glue("{estimate} ({p.value})")
  )

# table 3: summary statistics of 10 policy categories implemented in fall 2021
# and dichotomized to 0/1 75th percentile
tbl3 <- summary_statistics %>%
  filter(str_detect(construct,"quarter.75$"))%>%
  left_join(replace,by="construct")%>%
  left_join(dichotomous_stats,by="construct")%>%
  mutate(
    min_max = glue::glue("{n} ({min}, {max})"),
    mean_median = glue::glue("{mean} ({stdev})"),
    t_estimate = glue::glue("{estimate} ({p.value})")
  )  %>%
    select(Construct=replace,min_max,mean_median,`No policy_mean`,`Has policy_mean`,t_estimate)

tables <- list(tbl1=tbl1,tbl2=tbl2,tbl3=tbl3)
saveRDS(tables,"output_tables.rds")
writexl::write_xlsx(tables,path="output_tables.xlsx")
