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
  dta <- arrow::open_dataset("./data/NIS_HIV_IE_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_HIV_IE.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    IE_HIV = fct_relevel(IE_HIV, "Infective Endocarditis without HIV"),
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    LOS = as.numeric(LOS),
    ARF = as.factor(ARF),
    AIS = as.factor(AIS),
    AHF = as.factor(AHF)
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
dsgn <- subset(dsgn, IE == "Yes" & AGE > 18)

# Clean up memory
rm(dta)
gc()

# Create the regression model for all cause mortality
reg_model_died <- svyglm(
  DIED ~ IE_HIV + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_died, exponentiate = T)
#
# # Create the regression model for ARF
# reg_model_ARF <- svyglm(
#   ARF ~ IE_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
#     CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
#   design = dsgn,
#   family = "quasibinomial"
# )
#
# # Print regression table
# reg_model_ARF <- tbl_regression(reg_model_ARF, exponentiate = T, label = reg_var_labels)
# reg_model_ARF
#
# # Create the regression model for AIS
# reg_model_AIS <- svyglm(
#   AIS ~ IE_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
#     CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
#   design = dsgn,
#   family = "quasibinomial"
# )
#
# # Print regression table
# reg_model_AIS <- tbl_regression(reg_model_AIS, exponentiate = T, label = reg_var_labels)
# reg_model_AIS
#
# # Create the regression model for AHF
# reg_model_AHF <- svyglm(
#   AHF ~ IE_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
#     CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
#   design = dsgn,
#   family = "quasibinomial"
# )
#
# # Print regression table
# reg_model_AHF <- tbl_regression(reg_model_AHF, exponentiate = T, label = reg_var_labels)
# reg_model_AHF
