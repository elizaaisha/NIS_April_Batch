# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)

# Extra: Libraries for DOCX export
library(flextable)
library(officer)

# Install 'fastsurvey' from GitHub if not already installed
# remotes::install_github("bschneidr/fastsurvey")

# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- TRUE

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./nis_data/NIS_Cardio4_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./nis_data/NIS_Cardio4.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    Stroke_Afib = fct_relevel(Stroke_Afib, "Stroke without Atrial Fibrillation"),
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

# Subset data for specific condition
dsgn <- subset(dsgn, Stroke == "Yes")

# Clean up memory
rm(dta)
gc()

# Create the regression model
reg_model_LOS <- svyglm(
  LOS ~ ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_LOS, exponentiate = F, label = reg_var_labels)

# Create the regression model for Total Charge (Inflation Adjusted)
reg_model_TOTCHG <- svyglm(
  adj_TOTCHG ~ ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_TOTCHG, exponentiate = F, label = reg_var_labels)
