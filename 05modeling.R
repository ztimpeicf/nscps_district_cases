# Use tidymodels package to test covariates for most predictive characteristics
library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
library(purrr)


full_df <- readRDS("analytic_data.rds")

df <- full_df %>%
  select(changeinrate,region,locale,district,state,ends_with("quarter.75"),derivedtotalenrolled,percentamericanindianoralaskanative:cntycaseschange)%>%
  mutate(
  across(derivedtotalenrolled:cntycaseschange,~ (. - mean(.,na.rm=T)) / sd(.,na.rm=T)))%>%
  #na.omit()%>%
  rename_with(.fn=~gsub("quarter.75","",.x),ends_with("75"))


# Check region and district for evidence of significan variation between groups
randos <- c("region","locale","district","state")
names(randos) <- randos

r_test <- function(x,y="changeinrate",df){
  
  glue::glue("{ y } ~ (1|{ x })")%>%
    as.formula()%>%
    lmer(.,data=df, REML = FALSE) %>%
     lmerTest::ranova()%>%
      select(pval = last_col())%>%
       na.omit()%>%
        tibble::deframe()
}

r_test(x="region",y="changeinrate",df=full_df)
level_vars <- purrr::map_df(randos,r_test,.id="variable",y="changeinrate",df=full_df)%>%
  pivot_longer(cols=everything(),names_to="variable",values_to="pvalue")
level_vars <- purrr::map_df(randos,r_test,.id="variable",df=df)%>%
  pivot_longer(cols=everything(),names_to="variable",values_to="pvalue")
lmer(changeinrate ~ (1|region),data=full_df,REML=F)
# %>%
#   filter(pvalue < .10)%>%
#   select(variable)%>%
#   pull()
# The only higher-level variable with significant association to the case counts 
# was the region variable. Will include this in initial analyses but not locale nor
# district.
covariates <- df %>%
  select(derivedtotalenrolled:cntycaseschange) %>%
  names()
# Covariates as significant predictors?
tmp <- paste0(covariates,collapse = " + ")
check_covs <- paste0("log_rate ~ ",tmp, " + (1|region)")%>%
  as.formula()%>%
  lmer(.,data=df, REML = FALSE) 

%>%
  summary()%>%
  pluck("coefficients")%>%
  as_tibble(rownames="name")

covs_for_model <- check_covs %>%
  janitor::clean_names()%>%
  filter(abs(t_value)> 1,!name=="(Intercept)")%>%
  select(name)%>%
  pull()


strategy_model <- function(x,y=covs_for_model){
  tmp <- paste0(covs_for_model,collapse = " + ")
  glue::glue("log_rate ~ { x } + { tmp } + (1|region)")%>%
    as.formula()%>%
    lmer(.,data=df) %>%
     summary()
}

strategies <- df %>%
  select(vaccination:ventilation)%>%
  names()

models <- map(strategies,strategy_model)

strategy_coefs <- map_df(models,~pluck(.x,"coefficients")%>%
                                  tibble::as_tibble(rownames="name")
                         )%>%
  filter(name %in% strategies)

# Test CI generations
test <- confint(models[[1]],method=c("boot"),boot.type=c("perc"))
