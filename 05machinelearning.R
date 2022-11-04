# Use tidymodels package to test covariates for most predictive characteristics
library(tidymodels)
library(dplyr)
library(tidyr)
library(rpart.plot)

full_df <- readRDS("analytic_data.rds")

df <- full_df %>%
  select(changeinrate,region,locale,derivedtotalenrolled,percentamericanindianoralaskanative:cntycaseschange)%>%
  na.omit()


hist(df$changeinrate)
hist(log(df$changeinrate))


set.seed(502)
df_split <- initial_split(df, prop = 0.80, strata = changeinrate)
df_train <- training(df_split)
df_test  <-  testing(df_split)

case_rec <- 
  recipe(changeinrate ~ . , data=df_train) %>%
  step_log(derivedtotalenrolled,base=10)%>%
  step_dummy(all_nominal_predictors())%>%
  step_center(all_numeric_predictors())

lm_model <- linear_reg() %>%
  set_engine("lm")

lm_wflow <- 
  workflow() %>%
  add_model(lm_model)%>%
  add_recipe(case_rec)

lm_fit <- fit(lm_wflow,df_train)

df_test_res <- predict(lm_fit,new_data = df_test %>% select(-changeinrate))

df_test_res <- bind_cols(df_test_res,df_test %>% select(changeinrate))
df_test_res



check <- lm_fit %>% 
  # This returns the parsnip object:
  extract_fit_parsnip() %>% 
  # Now tidy the linear model object:
  tidy()%>%
  filter(p.value <= .05)%>%
  View()

ggplot(df_test_res, aes(x = changeinrate, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Change in Case Rates", x = "Change in Case Rates") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()


# Try a tree-based approach
tree_model <- 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <- 
  tree_model %>% 
  fit(changeinrate ~ . , data=df_train)

df_test_res <- predict(tree_fit,new_data = df_test %>% select(-changeinrate))

df_test_res <- bind_cols(df_test_res,df_test %>% select(changeinrate))
df_test_res

rpart.plot(tree_fit[["fit"]])
