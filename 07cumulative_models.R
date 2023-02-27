library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
library(purrr)
library(HLMdiag)

# Read in analytic data and standardize
rmve <- readRDS("outlier_schools.rds")

full_df <- readRDS("analytic_data.rds")%>%
  anti_join(rmve)
# test <- full_df %>%
#   select(changeinrate)%>%
#   na.omit()
# 
# hist(test$changeinrate,breaks=100)

df <- full_df %>%
  select(changeinrate,region,locale,schoollevel,state, vaccination:hvacsystems,
         derivedtotalenrolled,percentamericanindianoralaskanative:cntycaseschange)%>%
  mutate(
    across(derivedtotalenrolled:cntycaseschange,~ (. - mean(.,na.rm=T)) / sd(.,na.rm=T))
  )

# Read in machine learning results to select covariates for models
readRDS("forest_list_statistics.rds")%>%
  list2env(envir=.GlobalEnv)

#########################################3

# 2 fit the models with one for each individual strategy
strategies <- df %>%
  select(vaccination:hvacsystems)%>%
  names()

# For covariates at the school level, take the predictors that were among the top 
# five at least 20% of the iterations at the machine learning stage.

school_covariates <- covariate_importance %>%
  filter((positives > 50|name %in% c("rplthemes", "cntycaseschange")), !name %in% c("region","state"))%>%
  select(name)%>%
  pull()
school_covs <- paste0(school_covariates,collapse = " + ")
# Look at school_covariates to decide which covariates to include
# Function to test each strategy without other strategies as predictors

strategy_model <- function(x,y=school_covariates){
  tmp <- paste0(school_covariates,collapse=" + ")
  #tmp <- paste0(c("percentblackorafricanamerican","cntycaseschange","rplthemes",
  #               "percentamericanindianoralaskanative" ,"derivedtotalenrolled", 
  #              "schoollevel","locale"),collapse = " + ")
  glue::glue("changeinrate ~ { x } + { tmp } + (1|region/state)")%>%
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

individual_strategies_coefs <- map_df(individual_strategies_results,strategy_cis)%>%
  filter(name %in% strategies)



rounded_vals <- individual_strategies_coefs %>%
  janitor::clean_names()%>%
  mutate(pr_t = plyr::round_any(pr_t,.1,ceiling))

cutoffs <- rounded_vals %>%
  select(pr_t)%>%
  arrange(pr_t)%>%
  unique()%>%
  pull()

cutoff_strats <- map(cutoffs, ~
              individual_strategies_coefs %>%
              janitor::clean_names()%>%
              filter(pr_t <= .x)%>%
              select(name)%>%
              pull()
              )

cumulative_dfs <- map(cutoff_strats,~
                        df %>%
                        rowwise()%>%
                        mutate(ss = factor(sum(c_across(all_of(.x)),na.rm=T),
                                           levels=seq.int(0,length(.x))),
                               .before=1) )

models <- map(cumulative_dfs, ~
                paste0("changeinrate ~ ss","+",school_covs,"+ (1|region/state)")%>%
                as.formula()%>%
                lmer(.,data=.x))

fit_comps <- anova(models[[1]],models[[2]],models[[3]]) %>%
  cbind(model=c("0.1","0.2","0.5"))

cumulative_summaries <- map(models, ~
                              .x %>%
                              strategy_cis()
)%>%
  set_names(nm=cutoffs)%>%
  bind_rows(.id="model")%>%
  left_join(fit_comps)

test_dfs <- df %>%
  rowwise()%>%
  mutate(ss = sum(c_across(all_of(cutoff_strats[[1]])),na.rm=T),
         ss4 = ifelse(ss==4,1,0),
         ss5 = ifelse(sum(c_across(all_of(cutoff_strats[[2]])),na.rm=T)==5,1,0),
         ss4 = ifelse(ss5==1,0,ss4),
         .before=1) 
model1 <- paste0("changeinrate ~ ss4 +ss5","+",school_covs,"+ (1|region/state)")%>%
  as.formula()%>%
  lmer(.,data=test_dfs)
summary(model1)

final <- strategy_cis(model1)%>%
  mutate(model="final")
cumulative_summaries <- bind_rows(cumulative_summaries,final) 
saveRDS(cumulative_summaries,"cumulative_results.rds")
writexl::write_xlsx(cumulative_summaries,path="cumulative_results.xlsx")
