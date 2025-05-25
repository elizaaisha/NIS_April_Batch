# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)

# Install 'fastsurvey' from GitHub if not already installed
# remotes::install_github("bschneidr/fastsurvey")

# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- F

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./data/NIS_PD_AP_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_PD_AP.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    PD_AP = fct_relevel(PD_AP, "Parkinson's disease without aspiration pneumonia"),
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    MVent = as.factor(MVent),
    Sepsis = as.factor(Sepsis),
    LOS = as.numeric(LOS),
    Favorable_Discharge = as.factor(Favorable_Discharge),
    Disp_short = as.factor(Disp_short),
    Disp_long = as.factor(Disp_long),
    Pall_Care = as.factor(Pall_Care),
    ARF = as.factor(ARF)
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

# Subset data for specific condition
dsgn <- subset(dsgn, PD == "Yes")

# Clean up memory
rm(dta)
gc()

# Create the regression model for all cause mortality
reg_model_died <- svyglm(
  DIED ~ PD_AP + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
    HTN + DM + Cancer + CHF + Hyperlip + CAD + PVD + CKD + COPD + Cardiac_arrhythmia + Dementia + Liver_Dis + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_died, exponentiate = T, label = reg_var_labels)

r2(reg_model_died)
vif(reg_model_died)

# Create the regression model for all cause mortality
reg_model_sepsis <- svyglm(
  Sepsis ~ PD_AP + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
    HTN + DM + Cancer + CHF + Hyperlip + CAD + PVD + CKD + COPD + Cardiac_arrhythmia + Dementia + Liver_Dis + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_sepsis, exponentiate = T)

r2(reg_model_sepsis)
vif(reg_model_sepsis)

# Create the regression model for Disp_home
reg_model_Disp_home <- svyglm(
  Favorable_Discharge ~ PD_AP + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
    HTN + DM + Cancer + CHF + Hyperlip + CAD + PVD + CKD + COPD + Cardiac_arrhythmia + Dementia + Liver_Dis + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Disp_home, exponentiate = T)

# Create the regression model for Disp_short
reg_model_Disp_short <- svyglm(
  Disp_short ~ PD_AP + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
    HTN + DM + Cancer + CHF + Hyperlip + CAD + PVD + CKD + COPD + Cardiac_arrhythmia + Dementia + Liver_Dis + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Disp_short, exponentiate = T)

# Create the regression model for Disp_long
reg_model_Disp_long <- svyglm(
  Disp_long ~ PD_AP + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
    HTN + DM + Cancer + CHF + Hyperlip + CAD + PVD + CKD + COPD + Cardiac_arrhythmia + Dementia + Liver_Dis + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Disp_long, exponentiate = T)

# Create the regression model for home discharge
reg_model_Disp_home <- svyglm(
  Disp_home ~ PD_AP + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
    HTN + DM + Cancer + CHF + Hyperlip + CAD + PVD + CKD + COPD + Cardiac_arrhythmia + Dementia + Liver_Dis + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Disp_home, exponentiate = T)

# Create the regression model for MVent
reg_model_MVent <- svyglm(
  MVent ~ PD_AP + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
    HTN + DM + Cancer + CHF + Hyperlip + CAD + PVD + CKD + COPD + Cardiac_arrhythmia + Dementia + Liver_Dis + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_MVent, exponentiate = T)

# Create the regression model for palliative care consultation
reg_model_Pall_Care <- svyglm(
  Pall_Care ~ PD_AP + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
    HTN + DM + Cancer + CHF + Hyperlip + CAD + PVD + CKD + COPD + Cardiac_arrhythmia + Dementia + Liver_Dis + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Pall_Care, exponentiate = T)

# Create the regression model for ARF
reg_model_ARF <- svyglm(
  ARF ~ PD_AP + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region +
    HTN + DM + Cancer + CHF + Hyperlip + CAD + PVD + CKD + COPD + Cardiac_arrhythmia + Dementia + Liver_Dis + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
reg_model_ARF <- tbl_regression(reg_model_ARF, exponentiate = T)
reg_model_ARF
