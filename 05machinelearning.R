# Use the party package and party::cforest() to identify the most predictive
# covariates and strategies as a means of condensing variables for the 
# final analysis.
library(party)
library(dplyr)
library(tidyr)
rmve <- readRDS("outlier_schools.rds")

full_df <- readRDS("analytic_data.rds")%>%
  anti_join(rmve)

df <- full_df %>%
  select(changeinrate,region,locale,state,schoollevel,
         vaccination:hvacsystems,derivedtotalenrolled,
         percentamericanindianoralaskanative:cntycaseschange
         )%>%
  mutate(across(derivedtotalenrolled:cntycaseschange,~ (. - mean(.,na.rm=T)) / sd(.,na.rm=T))
         )

# set multiple seeds as party package suggests
numbers <- seq.int(from=1, to=1000, by = 10)
# Covariates df
forest_df <- df %>% select(-c(vaccination:hvacsystems))%>%
  mutate(across(where(is.character),~ factor(.)))%>%
  na.omit()

# Run varimp 100 times to get number of times each variable ranks in top five of 
# importance
covariate_importance <- purrr::map_dfr(numbers, function(x){
  set.seed(x)
  randomforest <- cforest(changeinrate ~ .,
                          data = forest_df, control =
                            cforest_unbiased(ntree=100)
  )
  
  tmp <- varimp(randomforest,conditional = T)%>%
    tibble::enframe()  %>%
    arrange(desc(value))%>%
    mutate(rank = row_number())
}) %>%
  group_by(name)%>%
  summarise(ranks = sum(rank <= 5)/length(numbers)*100,
            positives = sum(value > 0)/length(numbers)*100,
            pos_ranked = sum(value > 0 & rank <= 5)/length(numbers)*100
            )%>%
  ungroup()%>%
  select(name,positives,ranks,pos_ranked)%>%
  arrange(desc(pos_ranked))

# Same thing for the policy variables
# 
# strategy df
strategy_df <- df %>% select(changeinrate,vaccination:hvacsystems)%>%
  mutate(across(where(is.character),~ factor(.)))%>%
  na.omit()

# Run varimp 100 times to get number of times each variable ranks in top five of 
# importance
strategy_importance <- purrr::map_dfr(numbers, function(x){
  set.seed(x)
  randomforest <- cforest(changeinrate ~ .,
                          data = strategy_df, control =
                            cforest_unbiased(ntree=100)
  )
  
  tmp <- varimp(randomforest,conditional = T)%>%
    tibble::enframe()  %>%
    arrange(desc(value))%>%
    mutate(rank = row_number())
}) %>%
  group_by(name)%>%
  summarise(ranks = sum(rank < 5)/length(numbers)*100,
            positives = sum(value > 0 )/length(numbers)*100,
            pos_ranked = sum(value >0 & rank <= 5)/length(numbers)*100
            )%>%
  ungroup()%>%
  select(name,positives,ranks,pos_ranked)%>%
  arrange(desc(pos_ranked))

# write results to excel file
forest_list <- list(covariate_importance=covariate_importance,strategy_importance=strategy_importance)

saveRDS(forest_list,"forest_list_statistics.rds")
writexl::write_xlsx(forest_list,path="forest_list_statistics.xlsx")
