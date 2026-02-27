# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)

# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- F

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./nis_data/NIS_CRC_LiverDis_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./nis_data/NIS_CRC_LiverDis.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = fct_relevel(DIED, "No"),
    LOS = as.numeric(LOS),
    adj_TOTCHG = as.numeric(adj_TOTCHG),
    Sepsis = as.factor(Sepsis),
    WoundInfection = as.factor(WoundInfection),
    # Abcess = as.factor(Abcess),
    Pneumonia = as.factor(Pneumonia),
    VTE = as.factor(VTE),
    GIHemorrhage = as.factor(GIHemorrhage),
    # AMI = as.factor(AMI),
    AcuteRespiratoryFailure = as.factor(AcuteRespiratoryFailure),
    # MechanicalVentilation = as.factor(MechanicalVentilation),
    # Stroke = as.factor(Stroke),
    AcuteKidneyInjury = as.factor(AcuteKidneyInjury)
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
dsgn <- subset(dsgn,  ColorectalCancer == "Yes" & Colectomy == "Yes" & AGE >= 18)

# Clean up memory
rm(dta)
gc()

# Create baseline table
tbl_svybaseline <- dsgn %>%
  tbl_svysummary(
    by = Cirrhosis,
    include = baseline_var,
    label = baseline_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p()

# Print baseline table
tbl_svybaseline

# Create outcomes table
tbl_svyoutcomes <- dsgn %>%
  tbl_svysummary(
    by = Cirrhosis,
    include = outcome_var,
    label = outcome_var_labels,
    missing = "no"
  ) %>%
  add_p()

# Print outcomes table
tbl_svyoutcomes
