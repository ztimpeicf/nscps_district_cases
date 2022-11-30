# Prepare statistics tables for presentation in .qmd
library(dplyr)
library(tidyr)
library(stringr)
readRDS("raw_summary_statistics.rds") %>%
  list2env(envir = .GlobalEnv)

replace <- readxl::read_xlsx("construct_texts.xlsx")
# Rows insert to signify the region and locale in tbl1
insert <- tibble(Construct= c("Region","Locale"),min_max = c("",""),mean_stdev = c("",""),est_pval = c("",""))
# table 1: summary statistics of outcome and covariates
tbl1 <- summary_statistics %>%
  filter(str_detect(construct, "changeinrate|percent|rplthemes|cntycases")) %>%
  left_join(replace, by = c("construct" = "construct")) %>%
  left_join(continuous_correlations, by = c("construct" = "predictor")) %>%
  mutate(Construct = replace)%>%
  bind_rows(categorical_counts)%>%
  transmute(
    Construct = Construct,
    min_max = glue::glue("{n} ({min}, {max})"),
    mean_stdev = glue::glue("{mean} ({stdev})"),
    est_pval = ifelse(!is.na(estimate),glue::glue("{estimate} ({p.value})"),"")
  )%>%
  rows_insert(insert)

# table 2: summary statistics of 10 policy categories implemented in fall 2021
# and dichotomized to 0/1 any nonzero score
tbl2 <- summary_statistics %>%
  left_join(dichotomous_stats, by = "construct") %>%
  left_join(replace)%>%
  na.omit() %>%
  
  mutate(
    min_max = glue::glue("{n} ({min}, {max})"),
    mean_stdev = glue::glue("{mean} ({stdev})"),
    t_estimate = glue::glue("{estimate} ({p.value})")
  )  %>%
  select(Construct = replace,
         min_max,
         mean_stdev,
         `No policy_mean`,
         `Has policy_mean`,
         t_estimate)


# Import the machine learning results
readRDS("forest_list_statistics.rds") %>%
  list2env(envir = .GlobalEnv)
# Table 3 is results from using party::cforest to identify the most important
# predictors
tbl3 <- covariate_importance %>%
  left_join(replace, by = c("name" = "construct")) %>%
  mutate(replace = case_when(is.na(replace) ~ str_to_title(name), TRUE ~ replace)) %>%
  select(Construct = replace, 'Percent' = pos_ranked) %>%
  arrange(desc(c(2)))


# Table 4 is results from using party::cforest to identify the most important
# predictors from the strategies
tbl4 <- strategy_importance %>%
  left_join(replace, by = c("name" = "construct")) %>%
  mutate(replace = case_when(is.na(replace) ~ str_to_title(name), TRUE ~ replace)) %>%
  select(Construct = replace, 'Percent' = pos_ranked) %>%
  arrange(desc(c(2)))

# Import model results

readRDS("model_results.rds")%>%
  map(~.x %>% mutate(across(where(is.numeric),~round(.,2)),
                     column = glue::glue("{Estimate} ({lower}, {upper})"))%>%
        left_join(replace,by=c("name"="construct"))%>%
        janitor::clean_names()%>%
        select(replace,`Coefficient (95% interval)`=column,'p-value'=pr_t)%>%
        relocate(Strategy = replace,.before=c(1)))%>%
  list2env(envir=.GlobalEnv)

# Import cumulative model results
model_comps <- readRDS("cumulative_results.rds") %>%
  janitor::clean_names()%>%
  mutate(across(where(is.numeric),~as.character(round(.,3))))%>%
  select(model,aic,bic,pr_chisq)%>%
  unique()%>%
  filter(model %in% c("0.1","0.2","0.5"))%>%
  pivot_longer(cols=-model)%>%
  pivot_wider(id_cols=name,names_from=model,values_from=value)

cumulative_table <- readRDS("cumulative_results.rds") %>%
  janitor::clean_names()%>%
  filter(model %in% c("0.1","0.2","0.5"), stringr::str_detect(name,"^ss"))%>%
  mutate(across(where(is.numeric),~as.character(round(.,2))),
         column = as.character(glue::glue("{estimate} ({lower}, {upper})")))%>%
  select(model,name,column,pr_t)%>%
  pivot_wider(id_cols = name,names_from = model,values_from = column:pr_t,names_glue="{model}_{.value}")%>%
  select(name,starts_with(c("0.1","0.2","0.5")))%>%
  left_join(replace,by=c("name"="construct"))%>%
  rename_with(.fn = ~ gsub("column",'Coefficient (95% CI)',.x),.cols=contains("column"))%>%
  rename_with(.fn = ~ gsub("pr_t","p-value",.x),.cols=contains("pr_"))%>%
  relocate('Cumulative number strategies' = replace,.before=c(1))
comparison_names <- cumulative_table %>% select(c(1),contains("Coefficient"))%>%names()
names(model_comps)<-comparison_names

cumulative_table <- bind_rows(cumulative_table,model_comps)

tables <-
  list(
    tbl1 = tbl1,
    tbl2 = tbl2,
    tbl3 = tbl3,
    tbl4 = tbl4,
    tbl5 = lonely_inds,
    tbl6 = full_model,
    tbl7 = reduced_model,
    tbl8 = cumulative_table
  )
saveRDS(tables, "output_tables.rds")
writexl::write_xlsx(tables, path = "output_tables.xlsx")
