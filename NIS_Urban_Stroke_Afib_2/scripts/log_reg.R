# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)
library(easystats)
library(car)


# Install 'fastsurvey' from GitHub if not already installed
# remotes::install_github("bschneidr/fastsurvey")

# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- F

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./nis_data/NIS_Stroke_Afib_Urban_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./nis_data/NIS_Stroke_Afib_Urban_2.parquet/") |> collect()
}
# Set factor references
dta <- dta |>
  mutate(
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    LOS = as.numeric(LOS),
    ICH = as.factor(ICH),
    Endovascular_Thrombectomy = as.factor(Endovascular_Thrombectomy),
    MT = as.factor(MT)
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
dsgn <- subset(dsgn, Stroke == "Yes" & A_Fib == "Yes" & !is.na(ZIPINC_QRTL))

# Clean up memory
rm(dta)
gc()

# Create the regression model
reg_model_died <- svyglm(
  DIED ~ HOSP_LOCTEACH + ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_died, exponentiate = T)

# Create the regression model for mechanical thrombectomy
reg_model_MT <- svyglm(
  MT ~ HOSP_LOCTEACH + ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_MT, exponentiate = T)

# Create the regression model for intracranial hemorrhage
reg_model_ICH <- svyglm(
  ICH ~ HOSP_LOCTEACH + ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_ICH, exponentiate = T)

# Create the regression model for Endovascular Thrombectomy
reg_model_Endovascular_Therapy <- svyglm(
  Endovascular_Thrombectomy ~ HOSP_LOCTEACH + ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Endovascular_Therapy, exponentiate = T)

vif(reg_model_Endovascular_Therapy)

r2(reg_model_Endovascular_Therapy)
