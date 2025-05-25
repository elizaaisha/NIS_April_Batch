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
  dta <- arrow::open_dataset("./nis_data/NIS_Stroke_Afib_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./nis_data/NIS_Stroke_Afib.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    MT = as.factor(MT),
    ICH = as.factor(ICH),
    Endovascular_Therapy = as.factor(Endovascular_Therapy),
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
dsgn <- subset(dsgn, Stroke == "Yes" & A_Fib == "Yes" & !is.na(ZIPINC_QRTL))

# Clean up memory
rm(dta)
gc()

# Create the regression model
reg_model_died <- svyglm(
  DIED ~ PL_NCHS + ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_died, exponentiate = T, label = reg_var_labels)

# Create the regression model for mechanical thrombectomy
reg_model_MT <- svyglm(
  MT ~ PL_NCHS + ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_MT, exponentiate = T, label = reg_var_labels)

# Create the regression model for intracranial hemorrhage
reg_model_ICH <- svyglm(
  ICH ~ PL_NCHS + ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_ICH, exponentiate = T, label = reg_var_labels)

# Create the regression model for Endovascular Thrombectomy
reg_model_Endovascular_Therapy <- svyglm(
  Endovascular_Therapy ~ PL_NCHS + ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Endovascular_Therapy, exponentiate = T, label = reg_var_labels)

vif(reg_model_Endovascular_Therapy)

r2(reg_model_Endovascular_Therapy)
