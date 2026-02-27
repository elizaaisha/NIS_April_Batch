# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)

# Extra: Libraries for DOCX export
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
  dta <- arrow::open_dataset("./data/NIS_HF_IDA_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_HF_IDA_2_DX1.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    HF = fct_relevel(HF, "without Heart Failure"),
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
dsgn <- subset(dsgn, IDA == "Yes" & HF == "Yes" & !is.na(HF_Categories) & AGE > 18)

# Clean up memory
rm(dta)
gc()


# # Create baseline table
# tbl_svybaseline <- dsgn %>%
#   tbl_svysummary(
#     by = HF_Categories,
#     include = baseline_var,
#     label = baseline_var_labels,
#     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#     missing = "no"
#   ) %>%
#   add_p()
#
# # Print baseline table
# tbl_svybaseline

# Create outcomes table
tbl_svyoutcomes <- dsgn %>%
  tbl_svysummary(
    by = HF_Categories,
    include = outcome_var,
    label = outcome_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p()

# Print outcomes table
tbl_svyoutcomes
