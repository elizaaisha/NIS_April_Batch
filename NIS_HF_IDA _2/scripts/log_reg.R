# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)
library(easystats)
library(car)


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
  dta <- arrow::open_dataset("./data/NIS_HF_IDA_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_HF_IDA_2.parquet/") |> collect()
}


# Set factor references
dta <- dta |>
  mutate(
    HF_Categories = fct_relevel(HF_Categories, "Without heart failure"),
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    LOS = as.numeric(LOS)
  )


# Set survey options for lonely PSUs
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

# Define survey design
dsgn <- svydesign(
  id = ~ HOSPID,
  strata = ~ NIS_STRATUM,
  weights = ~ TRENDWT,
  data = dta,
  nest = TRUE
)

# Clean up memory
rm(dta)
gc()

# Create the regression model for all cause mortality
reg_model_died <- svyglm(
  DIED ~ HF_Categories + AGE + FEMALE + RACE + elixsum  + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH +
    HTN + Hyperlip + PVD + CAD + COPD + Valv_HD + Dementia + CA + Fluid_disorders + Afib + BMI + Pacemaker + pMI + Depression + pCardiacSurg,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_died, exponentiate = T)

r2(reg_model_died)
vif(reg_model_died)

performance_hosmer(reg_model_died)

model_performance(reg_model_died)


# Create the regression model
reg_model_LOS <- svyglm(
  LOS ~ HF_Categories + AGE + FEMALE + RACE + elixsum  + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH +
    HTN + Hyperlip + PVD + CAD + COPD + Valv_HD + Dementia + CA + Fluid_disorders + Afib + BMI + Pacemaker + pMI + Depression + pCardiacSurg,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_LOS, exponentiate = F, label = reg_var_labels)
r2(reg_model_LOS)


# Create the regression model for Total Charge (Inflation Adjusted)
reg_model_TOTCHG <- svyglm(
  adj_TOTCHG ~ HF_Categories + AGE + FEMALE + RACE + elixsum  + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH +
    HTN + Hyperlip + PVD + CAD + COPD + Valv_HD + Dementia + CA + Fluid_disorders + Afib + BMI + Pacemaker + pMI + Depression + pCardiacSurg,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_TOTCHG, exponentiate = F, label = reg_var_labels)
r2(reg_model_TOTCHG)


