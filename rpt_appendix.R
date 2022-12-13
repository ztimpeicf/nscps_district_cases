library(dplyr)
library(tidyr)
library(stringr)


replace <- readxl::read_xlsx("construct_texts.xlsx")
# Rows insert to signify the region and locale in tbl1
insert <- tibble(Construct= c("Region","Locale"),min_max = c("",""),mean_stdev = c("",""),est_pval = c("",""))
cumulative_table <- readRDS("cumulative_results.rds") %>%
  janitor::clean_names()%>%
  filter(!model %in% c("0.1","0.2","final"), stringr::str_detect(name,"^ss"))%>%
  mutate(across(where(is.numeric),~as.character(round(.,2))),
         column = as.character(glue::glue("{estimate} ({lower}, {upper})")))%>%
  select(model,name,column,pr_t)%>%
  pivot_wider(id_cols = name,names_from = model,values_from = column:pr_t,names_glue="{model}_{.value}")%>%
  select(name,starts_with(c("0.5","0.6","0.7","0.8","1")))%>%
  left_join(replace,by=c("name"="construct"))%>%
  rename_with(.fn = ~ gsub("column",'Coefficient (95% CI)',.x),.cols=contains("column"))%>%
  rename_with(.fn = ~ gsub("pr_t","p-value",.x),.cols=contains("pr_"))%>%
  relocate('Cumulative number strategies' = replace,.before=c(1))

saveRDS(cumulative_table, "output_appendix.rds")
writexl::write_xlsx(cumulative_table, path = "output_appendix.xlsx")
