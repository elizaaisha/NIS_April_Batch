# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)
library(easystats)
library(car)


# # Extra: Libraries for DOCX export
# library(flextable)
# library(officer)

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
  dta <- arrow::open_dataset("./nis_data/NIS_Stroke_Afib_Socioeconomic.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    MT = as.factor(MT),
    ICH = as.factor(ICH),
    Sepsis = as.factor(Sepsis),
    Endovascular_Thrombectomy = as.factor(Endovascular_Thrombectomy),
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
dsgn <- subset(dsgn, Stroke == "Yes" & A_Fib == "Yes" & !is.na(ZIPINC_QRTL) & !is.na(Insurance))

# Clean up memory
rm(dta)
gc()

# Create the regression model
reg_model_died <- svyglm(
  DIED ~ ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Cancer + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
  )

# Print regression table
tbl_regression(reg_model_died, exponentiate = T, label = reg_var_labels)

r2(reg_model_died)

# Create the regression model for mechanical thrombectomy
reg_model_EVT <- svyglm(
  Endovascular_Thrombectomy ~ ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_EVT, exponentiate = T, label = reg_var_labels)

# Create the regression model for intracranial hemorrhage
reg_model_ICH <- svyglm(
  ICH ~ ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_ICH, exponentiate = T)

# Create the regression model for sepsis
reg_model_sepsis <- svyglm(
  Sepsis ~ ZIPINC_QRTL + Insurance + AGE + FEMALE + RACE + charlindex + Insurance + Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH + HTN + DM + Hyperlip + CAD + PVD + CHF +
    CKD + COPD + Dementia + Liver_Dis + Obesity + pStroke + pCardiacSurg + Pacemaker_defib + Fluid_disorders,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_sepsis, exponentiate = T)
