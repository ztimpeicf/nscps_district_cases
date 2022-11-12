# Use tidymodels package to test covariates for most predictive characteristics
library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
library(purrr)

# Read in analytic data and standardize
full_df <- readRDS("analytic_data.rds")

df <- full_df %>%
  select(changeinrate,region,locale,schoollevel,state, vaccination:hvacsystems,
         derivedtotalenrolled,percentamericanindianoralaskanative:cntycaseschange)%>%
  mutate(
   across(derivedtotalenrolled:cntycaseschange,~ (. - mean(.,na.rm=T)) / sd(.,na.rm=T))
        )

# Read in machine learning results to select covariates for models
readRDS("forest_list_statistics.rds")%>%
  list2env(envir=.GlobalEnv)
# Check state, region, locale, and district for evidence of significant variation between groups

## MLM sequence
# 1 test empty models with higher level variables to determine if MLM is appropriate
#   Use the ranova as an official means
#   Calculate ICC as well
randos <- c("region","locale","state")
names(randos) <- randos

empty_models <- function(x,y="changeinrate",df){
  
  tmp <- glue::glue("{ y } ~ (1|{ x })")%>%
    as.formula()%>%
    lmer(.,data=df, REML = FALSE) 
  
  out <- tmp %>%
    lmerTest::ranova()    %>%
    select(pval = last_col())%>%
    na.omit()%>%
    tibble::deframe()
  
  var_dev <- summary(tmp)%>%
    .[["varcor"]]%>%
    .[[x]]%>%
    attr(.,"stddev")
  
  resid_dev <- summary(tmp)%>%
    .[["varcor"]]%>%
    attr(.,"sc")
  
  icc <-(var_dev^2)/(var_dev^2 + resid_dev^2)
  
  tibble(pval= out,icc=icc)
}

empty_results <- purrr::map_df(randos,empty_models,.id="variable",df=full_df)

#The only nesting variable to have a significant association was state, with an 
# ICC of around .1. These results align with the machine learning results 
# pointing to state as the strongest predictor.

#########################################3

# 2 fit the models with one for each individual strategy
strategies <- df %>%
  select(vaccination:hvacsystems)%>%
  names()

# For covariates at the school level, take the predictors that were among the top 
# five at least 20% of the iterations at the machine learning stage.

school_covariates <- covariate_importance %>%
  filter(pos_ranked > 0, !name %in% c("region","state"))%>%
  select(name)%>%
  pull()

# Function to test each strategy without other strategies as predictors

strategy_model <- function(x,y=school_covariates){
  tmp <- paste0(y,collapse = " + ")
  glue::glue("changeinrate ~ { x } + { tmp } + (1|state)")%>%
    as.formula()%>%
    lmer(.,data=df)
}

individual_strategies_results <- map(strategies,strategy_model)

boottype <- "perc"

strategy_cis <- function(x,b=boottype){
  coefs <- summary(x)%>%
    pluck("coefficients")  %>%
    tibble::as_tibble(rownames="name")
  
  cis <- confint(x,method=c("boot"),boot.type=b)%>%
    tibble::as_tibble(rownames="name")%>%
    magrittr::set_colnames(c("name","lower","upper"))
  
  final <- left_join(coefs,cis,by="name")
}

#test <- strategy_cis(x=individual_strategies_results[[1]])
individual_strategies_coefs <- map_df(individual_strategies_results,strategy_cis)%>%
  filter(name %in% strategies)


# 3 fit the model with all strategies as predictors
# All strategies in one model
all_strategies <- paste0(strategies,collapse = " + ")
school_covs = paste0(school_covariates,collapse="+")
all_strats_results <- paste0("changeinrate ~ ",all_strategies,"+",school_covs,"+ cntycaseschange + (1|state)")%>%
  as.formula()%>%
  lmer(.,data=df)

all_strats_results <- strategy_cis(all_strats_results)%>%
  filter(name %in% strategies)

# 4 check model assumptions

# 5 Summarise results


