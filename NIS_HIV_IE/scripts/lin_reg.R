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
TESTING <- TRUE

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./nis_data/NIS_Afib_IE_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./nis_data/NIS_Afib_IE.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    Stroke_Afib = fct_relevel(IE_Afib, "Infective Endcarditis without Atrial Fibrillation"),
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
  LOS ~ IE_HIV + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_LOS, exponentiate = F)

# Create the regression model for Total Charge (Inflation Adjusted)
reg_model_TOTCHG <- svyglm(
  adj_TOTCHG ~ IE_HIV + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
  design = dsgn,
  family = "gaussian"
  )

# Print regression table
tbl_regression(reg_model_TOTCHG, exponentiate = F)
