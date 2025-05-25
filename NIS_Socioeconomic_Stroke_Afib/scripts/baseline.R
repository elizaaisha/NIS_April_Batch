# Load libraries
library(yaml)
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)

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

 table(dta$Stroke_Afib)

 # Set factor references
dta <- dta |>
  mutate(
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    LOS = as.numeric(LOS),
    ICH = as.factor(ICH),
    Sepsis = as.factor(Sepsis)
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
dsgn1 <- subset(dsgn, Stroke == "Yes" & A_Fib == "Yes" & !is.na(ZIPINC_QRTL))

dsgn2 <- subset(dsgn, Stroke == "Yes" & A_Fib == "Yes" & !is.na(Insurance))

# Clean up memory
rm(dta)
gc()


# # Create baseline table
# tbl_svybaseline <- dsgn1 %>%
#   tbl_svysummary(
#     by = ZIPINC_QRTL,
#     include = baseline_var,
#     label = baseline_var_labels,
#     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#     missing = "no"
#   ) %>%
#   add_p() %>%
#   add_overall()
#
# # Print baseline table
# tbl_svybaseline1

# Create baseline table
tbl_svybaseline2 <- dsgn2 %>%
  tbl_svysummary(
    by = Insurance,
    include = baseline_var,
    label = baseline_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p() %>%
  add_overall()

# Print baseline table
tbl_svybaseline2

# Create outcomes table
tbl_svyoutcomes1 <- dsgn1 %>%
  tbl_svysummary(
    by = ZIPINC_QRTL,
    include = outcome_var,
    label = outcome_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p()

# Print outcomes table
tbl_svyoutcomes1

# Create outcomes table
tbl_svyoutcomes2 <- dsgn2 %>%
  tbl_svysummary(
    by = Insurance,
    include = outcome_var,
    label = outcome_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p() %>%
  add_overall()

# Print outcomes table
tbl_svyoutcomes2
