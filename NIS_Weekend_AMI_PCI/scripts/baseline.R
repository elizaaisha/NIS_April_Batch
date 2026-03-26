# Load libraries
library(tidyverse)
library(easystats)
library(arrow)
library(survey)
library(gtsummary)
library(cardx)
library(broom.helpers)


# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- F

#Import data
if (TESTING) {
  dta <- arrow::open_dataset(
    "./nis_data/NIS_Weekend_AMI_PCI_testing.parquet"
  ) |>
    collect()
} else {
  dta <- arrow::open_dataset(
    "./nis_data/NIS_Weekend_AMI_PCI.parquet/"
  ) |>
    collect()
}

# Set factor references
dta <- dta |>
  mutate(
    RACE_alt = fct_relevel(RACE_alt, "White"),
    DIED = fct_relevel(DIED, "No"),
    LOS = as.numeric(LOS),
    adj_TOTCHG = as.numeric(adj_TOTCHG),
    AWEEKEND = fct_relevel(AWEEKEND, "Monday-Friday"),
    Mechanical_Ventilation = fct_relevel(Mechanical_Ventilation, "No"),
    Favorable_Discharge = fct_relevel(Favorable_Discharge, "No")
    )

# Set survey options for lonely PSUs
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

# Define survey design
dsgn <- svydesign(
  id = ~HOSPID,
  strata = ~NIS_STRATUM,
  weights = ~TRENDWT,
  data = dta,
  nest = TRUE
)

# Subset data for specific condition
dsgn <- subset(
  dsgn,
  AGE >= 18 &
    AMI == "Yes" &
    PCI == "Yes" &
    !is.na(AWEEKEND)
)

# Clean up memory
rm(dta)
gc()

# # Create baseline table
# tbl_svybaseline <- dsgn %>%
#   tbl_svysummary(
#     by = AWEEKEND,
#     include = baseline_var,
#     label = baseline_var_labels,
#     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#     missing = "no"
#   ) %>%
#   add_p() |>
#   add_overall()
#
# # Print baseline table
# tbl_svybaseline

# Create outcomes table
tbl_svyoutcomes <- dsgn %>%
  tbl_svysummary(
    by = AWEEKEND,
    include = outcome_var,
    label = outcome_var_labels,
    missing = "no"
  ) %>%
  add_p() |>
  add_overall()

# Print outcomes table
tbl_svyoutcomes
