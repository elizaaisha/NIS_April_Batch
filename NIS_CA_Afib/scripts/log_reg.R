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
  dta <- arrow::open_dataset("./data/NIS_CA_Afib_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_CA_Afib.parquet") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    CA_Afib = fct_relevel(CA_Afib, "Atrial Fibrillation without Cancer"),
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    AMI = as.factor(AMI),
    AIS = as.factor(AIS),
    VTE = as.factor(VTE),
    MB = as.factor(ME),
    PE = as.factor(PE),
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
dsgn <- subset(dsgn, A_Fib == "Yes" & AGE > 18 & !is.na(CA_Afib))


# Clean up memory
rm(dta)
gc()

# # Create the regression model for all cause mortality
# reg_model_died <- svyglm(
#   DIED ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM,
#   design = dsgn,
#   family = "quasibinomial"
# )
#
# # Print regression table
# tbl_regression(reg_model_died, exponentiate = T)

# Create the regression model for AMI
reg_model_AMI <- svyglm(
  AMI ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
reg_model_AMI <- tbl_regression(reg_model_AMI, exponentiate = T, label = reg_var_labels)
reg_model_AMI

# Create the regression model for AIS
reg_model_AIS <- svyglm(
  AIS ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
reg_model_AIS <- tbl_regression(reg_model_AIS, exponentiate = T, label = reg_var_labels)
reg_model_AIS

# Create the regression model for VTE
reg_model_VTE <- svyglm(
  VTE ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
reg_model_VTE <- tbl_regression(reg_model_AHF, exponentiate = T, label = reg_var_labels)
reg_model_VTE

# Create the regression model
reg_model_LOS <- svyglm(
  LOS ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_LOS, exponentiate = F)

# Create the regression model for Total Charge (Inflation Adjusted)
reg_model_TOTCHG <- svyglm(
  adj_TOTCHG ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_TOTCHG, exponentiate = F)
