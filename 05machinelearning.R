# Use rpart package to test covariates for most predictive characteristics
library(rpart)
library(dplyr)
library(tidyr)

full_df <- readRDS("analytic_data.rds")

df <- full_df %>%
  #filter(!is.na(changeinrate))%>%
  select(changeinrate,percentamericanindianoralaskanative:cntycaseschange)%>%
  na.omit()


#Run regression Tree model
tree_mod <- rpart(data=df,
                  formula = changeinrate ~ .,
                  method  = "anova"
)

rpart::plot.rpart(tree_mod) #visualize the tree
