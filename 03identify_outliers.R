library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
library(purrr)
library(HLMdiag)

# Read in analytic data and standardize
full_df <- readRDS("analytic_data.rds")

df <- full_df %>%
  select(qid,changeinrate,region,locale,schoollevel,state, vaccination:hvacsystems,
         derivedtotalenrolled,percentamericanindianoralaskanative:cntycaseschange)%>%
  mutate(
    across(derivedtotalenrolled:cntycaseschange,~ (. - mean(.,na.rm=T)) / sd(.,na.rm=T)),
    ind = row_number()
  )%>%
  relocate(ind,.before=c(1))

# Test for outliers using the HLMdiag package. Follow book
# Use key strategy and covariate predictors
predictor_names <- df %>%
  select(vaccination:hvacsystems,derivedtotalenrolled,percentblackorafricanamerican,
         percenttwoormoreraces,percentamericanindianoralaskanative,rplthemes,cntycaseschange)%>%
  names()

# Model results to use for outlier detection
out_model <- paste0(predictor_names,collapse = " + ")%>%
  paste0("changeinrate ~ ",.,"+ (1|state)")%>%
  as.formula()%>%
  lmer(.,data=df)

# Use the HLMdiag package
state_diagnostics <- hlm_augment(out_model)

outlier_schools <- state_diagnostics %>%
  mutate(changeinrate = changeinrate,
         sd_rate = sd(changeinrate),
         flag = ifelse(abs(changeinrate) > sd_rate*3.5,1,0)
         )%>%
  filter(flag==1)%>%
  select(id)%>%
  pull()

outlier_schools <- df %>%
  filter((ind %in% outlier_schools))%>%
  select(qid,schoollevel)
saveRDS(outlier_schools,"outlier_schools.rds")
