# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)
library(car)
library(easystats)

# Install 'fastsurvey' from GitHub if not already installed
# remotes::install_github("bschneidr/fastsurvey")

# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- T

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
    MB = as.factor(MB),
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
dsgn <- subset(dsgn, A_Fib == "Yes" & CatheterAblation == "No" & AGE > 18 & !is.na(CA_Afib))

# Clean up memory
rm(dta)
gc()

# Create the regression model for all cause mortality
reg_model_died <- svyglm(
  DIED ~ CA_Afib + AGE + FEMALE + RACE + elixsum + ZIPINC_QRTL + Insurance + Hosp_Census_Region + PL_NCHS + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + VHD + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Smoker,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_died, exponentiate = T)

vif(reg_model_died)

# Create the regression model for AMI
reg_model_AMI <- svyglm(
  AMI ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + PL_NCHS + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
reg_model_AMI <- tbl_regression(reg_model_AMI, exponentiate = T)
reg_model_AMI

# Create the regression model for AIS
reg_model_AIS <- svyglm(
  AIS ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + PL_NCHS + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + VHD + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Smoker,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
model_AIS <- tbl_regression(reg_model_AIS, exponentiate = T)
model_AIS

vif(reg_model_AIS)

model_performance(reg_model_AIS)

r2(reg_model_AIS)

# Create the regression model for VTE
reg_model_VTE <- svyglm(
  VTE ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Tobb,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
reg_model_VTE <- tbl_regression(reg_model_VTE, exponentiate = T)
reg_model_VTE

# Create the regression model for MB
reg_model_MB <- svyglm(
  MB ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Tobb,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
reg_model_MB <- tbl_regression(reg_model_MB, exponentiate = T)
reg_model_MB

# Create the regression model for PE
reg_model_PE <- svyglm(
  PE ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Tobb,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
reg_model_PE <- tbl_regression(reg_model_PE, exponentiate = T)
reg_model_PE

# Create the regression model
reg_model_LOS <- svyglm(
  LOS ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Tobb,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_LOS, exponentiate = F)

# Create the regression model for Total Charge (Inflation Adjusted)
reg_model_TOTCHG <- svyglm(
  adj_TOTCHG ~ CA_Afib + AGE + FEMALE + RACE + charlindex + ZIPINC_QRTL + Insurance + Hosp_Census_Region + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + OSA + Anemia + Alcohol + Obesity + pStroke + pCardiacSurg + Tobb,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_TOTCHG, exponentiate = F)
