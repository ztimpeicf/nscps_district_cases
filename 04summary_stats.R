# Summary statistics 
# Variables of interest
# Dependent variable: change in average case rate per 100 students between
# fall and spring of 2021-22. Fall calculated as average of October, November, 
# and December and Spring as average of January, February, and March.
# 
# Independent variables:
# School level:
# percent free and reduced lunch eligible from 2020 NCES*
# percent black, white, hispanic, asian, hawaiian, other*
# 
# County
# SVI*
# case count (NEED)
# 
# locale, region*
# 
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(stringr)
# remove outlier
#rmve <- readRDS("outlier_schools.rds")

full_df <- readRDS("analytic_data.rds")
#%>%
 
# anti_join(rmve)

df <- full_df %>%
  filter(!is.na(changeinrate))
#,!qid %in% rmve
predictors <- df %>%
  select(vaccination:hvacsystems,starts_with(c("cnty","percent","rpl"))) %>%
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
  filter(!is.na(changeinrate))%>%
  summarise(across(all_of(c("changeinrate",predictors)),list_sum_stats))%>%
  pivot_longer(cols=everything(),names_to=c("construct",".value"),names_sep="_")%>%
  pivot_wider(names_from = prob,values_from=tiles)

dichotomous_vars <- summary_statistics %>%
  filter(max==1,!str_detect(construct,"^percent"))%>%
  select(construct)%>%
  pull()%>%
  rlang::set_names()

# t-test looking at differences between change in case rates by dichotomous vars
ttest_results<- map_dfr(dichotomous_vars, ~
                glue::glue("changeinrate ~ {.x}")%>%
                as.formula()%>%
                t.test(data=df,na.action = na.omit) %>%
                tidy()%>%
                mutate(construct = .x,
                       across(where(is.numeric),~round(.,3)))%>%
                relocate(construct, .before=1)              %>%
                rename(`No policy_mean` = estimate1, `Has policy_mean`=estimate2)
)
n_fn <- function(x){
  x_sym <- sym(x)

  df %>%
    group_by({{x_sym}})%>%
    select({{x_sym}})%>%
    count()%>%
    rename(category=c(1)) %>%
    mutate(category = ifelse(category==0,"No policy_n","Has policy_n"))
}
dichotomous_ns <- map_dfr(dichotomous_vars,n_fn,.id="construct")%>%
  pivot_wider(id_cols=construct,names_from=category,values_from=n)

dichotomous_stats <- inner_join(dichotomous_ns,ttest_results)

# ANOVA looking at region and locale
categorical_vars <- c("region","locale","state")

anovas <- map_dfr(categorical_vars, ~
  glue::glue("changeinrate ~ {.x}")%>%
    as.formula()%>%
    aov(data=df)%>%
    tidy()%>%
    filter(term == .x)
)

# Pearson correlations for the continuous variables
continuous_vars <-summary_statistics %>%
  select(construct)%>%
  filter(str_detect(construct,"^cnty|^percent|^rpl"))%>%
  pull()

scaled_df <- df %>%
  select(changeinrate,all_of(continuous_vars))%>%
  mutate(across(!changeinrate,~ (. - mean(.,na.rm=T)) / sd(.,na.rm=T)))

continuous_correlations <- map_dfr(continuous_vars, ~
            glue::glue("~ changeinrate + {.x}")%>%
            as.formula()%>%
            cor.test(data=scaled_df,method="pearson") %>%
            tidy() %>%
              mutate(predictor = .x,
                     across(where(is.numeric),~round(.,3)))%>%
              relocate(predictor, .before=1)
  )

# write results to excel file
summary_list <- list(summary_statistics=summary_statistics,dichotomous_stats=dichotomous_stats,
                     anovas=anovas,continuous_correlations=continuous_correlations)

saveRDS(summary_list,"raw_summary_statistics.rds")
writexl::write_xlsx(summary_list,path="raw_summary_statistics.xlsx")
