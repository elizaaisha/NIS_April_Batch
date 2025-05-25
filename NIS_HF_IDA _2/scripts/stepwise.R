# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)
library(pscl)

# Extra: Libraries for DOCX export
library(flextable)
library(officer)

# Install 'fastsurvey' from GitHub if not already installed
# remotes::install_github("bschneidr/fastsurvey")

# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- F

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./data/NIS_NASH_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_NASH.parquet/") |> collect()
}


# Set factor references
dta <- dta |>
  mutate(
    NASH_NAFLD_Categories = fct_relevel(NASH_NAFLD_Categories, "Without NAFLD/NASH"),
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    LOS = as.numeric(LOS)
  )

# #propensity score matching
# dta <- dta %>%
#   filter(!is.na(NASH_NAFLD_Categories) & !is.na(AGE) & !is.na(FEMALE) & !is.na(RACE) & !is.na(ZIPINC_QRTL) & !is.na(Insurance)
#          & !is.na(Hosp_Census_Region) & !is.na(HOSP_BEDSIZE) & !is.na(HOSP_LOCTEACH) & !is.na(HTN) & !is.na(Hyperlip) & !is.na(CAD)
#          & !is.na(PVD) & !is.na(COPD) & !is.na(Dementia) & !is.na(OSA) & !is.na(Alcohol) & !is.na(Valv_HD) & !is.na(Afib) & !is.na(CA) &
#            !is.na(BMI) & !is.na(elixsum))

# # Recode to binary (1 = with NAFLD, 0 = without NAFLD)
# dta <- dta %>%
#   mutate(
#     NAFLD_binary = if_else(NASH_NAFLD_Categories == "With NAFLD/NASH", 1, 0)
#   )

# # Run matching
# matched_data <- matchit(NAFLD_binary ~ AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
#                           HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + Hyperlip + CAD + PVD + COPD + Dementia + OSA + Alcohol +
#                           Valv_HD + Afib + CA + BMI + IBD,
#                         data = dta, method = "nearest", distance = "glm", discard = "none",
#                         m.order = "largest", caliper = 0.2, ratio = 1, s.weights = ~TRENDWT)
# Run matching
# matched_data <- matchit(NAFLD_binary ~ AGE + FEMALE + RACE + elixsum + HTN + Hyperlip + CAD + OSA + COPD,
#                         data = dta, method = "nearest", distance = "glm", discard = "none",
#                         m.order = "largest", caliper = 0.2, ratio = 1, s.weights = ~TRENDWT)
#
# # Extract matched data
# matched_dta <- match.data(matched_data)

# # Set survey options for lonely PSUs
# options(survey.lonely.psu = "adjust")
# options(survey.adjust.domain.lonely = TRUE)
#
# # Define survey design
# dsgn <- svydesign(
#   id = ~ HOSPID,
#   strata = ~ NIS_STRATUM,
#   weights = ~ TRENDWT,
#   data = dta,
#   nest = TRUE
# )

# # Set survey options for lonely PSUs
# options(survey.lonely.psu = "adjust")
# options(survey.adjust.domain.lonely = TRUE)
#
# # Define survey design
# dsgn <- svydesign(
#   id = ~ HOSPID,
#   strata = ~ NIS_STRATUM,
#   weights = ~ TRENDWT,
#   data = dta,
#   nest = TRUE
# )
#
# # Subset data for specific condition
# dsgn <- subset(dsgn, HF_CKD_T2DM == "Yes" & !is.na(NASH_NAFLD_Categories) & !is.na(AGE) & !is.na(FEMALE) & !is.na(RACE) & !is.na(ZIPINC_QRTL) & !is.na(Insurance)
#                & !is.na(Hosp_Census_Region) & !is.na(HOSP_BEDSIZE) & !is.na(HOSP_LOCTEACH) & !is.na(HTN) & !is.na(Hyperlip) & !is.na(CAD)
#                & !is.na(PVD) & !is.na(COPD) & !is.na(Dementia) & !is.na(OSA) & !is.na(Alcohol) & !is.na(Valv_HD) & !is.na(Afib) & !is.na(CA) &
#                  !is.na(Obesity) & !is.na(elixsum) & !is.na(Smoking))

# # Clean up memory
# rm(dta)
# gc()
#
# # Create the regression model for all cause mortality
# reg_model_died <- svyglm(
#   DIED ~ NASH_NAFLD_Categories + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH +
#     HTN + Hyperlip + CAD + PVD + COPD + Dementia + OSA + Alcohol + Valv_HD + Afib + CA + BMI + Anemia,
#   design = dsgn,
#   family = "quasibinomial"
# )
#
# # Print regression table
# tbl_regression(reg_model_died, exponentiate = T)

# # Null model
# model_null <- glm(DIED ~ 1, data = dta, family = binomial)
#
# # Full model with all covariates
# model_full <- glm(
#   DIED ~ NASH_NAFLD_Categories + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance +
#     Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + Hyperlip + CAD + PVD +
#     COPD + Dementia + OSA + Alcohol + Valv_HD + Afib + CA + BMI + Anemia + Smoking + pStroke + pCardiacSurg + Pacemaker + RHD + Endocarditis +
#   Myocarditis + Cardiomyopathy + Pericarditis + Pulm_HD + Conduction_Disorder + Hypothyroidism + Hyperthyroidism + Chronic_Pancreatitis + Malnutrition +
#     Fluid_disorders + Cystic_fibrosis + cancer_history + UTI + Tuberculosis + HIV + Hepatitis + Influenza + Pneumonia + CHD + Depression + Epilepsy,
#   data = dta,
#   family = binomial
# )

# Start by defining the null and full models
model_null <- glm(DIED ~ 1, data = dta, family = binomial)

# Full model with all covariates (as per your provided model)
model_full <- glm(
  DIED ~ NASH_NAFLD_Categories + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance +
    Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + Hyperlip + CAD + PVD +
    COPD + Dementia + OSA + Alcohol + Valv_HD + Afib + CA + BMI + Anemia + Smoking + pStroke + pCardiacSurg + Pacemaker + RHD + Endocarditis +
    Myocarditis + Cardiomyopathy + Pericarditis + Pulm_HD + Conduction_Disorder + Hypothyroidism + Hyperthyroidism + Chronic_Pancreatitis + Malnutrition +
    Fluid_disorders + Cystic_fibrosis + cancer_history + UTI + Tuberculosis + HIV + Hepatitis + Influenza + Pneumonia + CHD + Depression + Epilepsy,
  data = dta,
  family = binomial
)

# Perform stepwise selection (both forward and backward)
stepwise_model <- step(
  model_full,
  direction = "both",
  trace = 1,
  scope = list(lower = model_null, upper = model_full)
)

# Display the stepwise model summary
summary(stepwise_model)

# # Candidate covariates
# covariates <- c(
#   "NASH_NAFLD_Categories", "AGE", "FEMALE", "RACE", "elixsum", "ZIPINC_QRTL", "Insurance",
#   "Hosp_Census_Region", "HOSP_BEDSIZE", "HOSP_LOCTEACH", "HTN", "Hyperlip", "CAD", "PVD",
#   "COPD", "Dementia", "OSA", "Alcohol", "Valv_HD", "Afib", "CA", "BMI", "Anemia", "Smoking",
#   "pStroke", "pCardiacSurg", "Pacemaker", "RHD", "Endocarditis", "Myocarditis", "Cardiomyopathy",
#   "Pericarditis", "Pulm_HD", "Conduction_Disorder", "Hypothyroidism", "Hyperthyroidism",
#   "Chronic_Pancreatitis", "Malnutrition", "Fluid_disorders", "Cystic_fibrosis", "cancer_history",
#   "UTI", "Tuberculosis", "HIV", "Hepatitis", "Influenza", "Pneumonia", "CHD", "Depression", "Epilepsy"
# )


# Run forward selection
model_los_r2 <- forward_selection_r2(data = dta, outcome = "LOS", candidate_vars = covariates)

# View model summary
summary(model_los_r2)


# Create the regression model
reg_model_LOS <- svyglm(
  LOS ~ NASH_NAFLD_Categories + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH +
    HTN + Hyperlip + CAD + PVD + COPD + Dementia + OSA + Alcohol + Valv_HD + Afib + CA + Obesity,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_LOS, exponentiate = F)

# Create the regression model for Total Charge (Inflation Adjusted)
reg_model_TOTCHG <- svyglm(
  adj_TOTCHG ~ NASH_NAFLD_Categories + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH +
    HTN + Hyperlip + CAD + PVD + COPD + Dementia + OSA + Alcohol + Valv_HD + Afib + CA + BMI,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_TOTCHG, exponentiate = F)




# #Tests
#
# library(car)
# vif(reg_model_died)  # Or reg_model_LOS, reg_model_TOTCHG
