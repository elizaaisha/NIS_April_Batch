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
dsgn <- subset(dsgn, IE == "Yes" & AGE > 18)


# Clean up memory
rm(dta)
gc()


# Create baseline table
tbl_svybaseline <- dsgn %>%
  tbl_svysummary(
    by = IE_HIV,
    include = baseline_var,
    label = baseline_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p() %>%
  add_overall()

# Print baseline table
tbl_svybaseline

# Create outcomes table
tbl_svyoutcomes <- dsgn %>%
  tbl_svysummary(
    by = IE_HIV,
    include = outcome_var,
    label = outcome_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p() %>%
  add_overall()

# Print outcomes table
tbl_svyoutcomes
