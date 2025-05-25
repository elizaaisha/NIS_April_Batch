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
   dta <- arrow::open_dataset("./data/NIS_CA_Afib.parquet/") |> collect()
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


# # Create baseline table
# tbl_svybaseline <- dsgn %>%
#   tbl_svysummary(
#     by = CA_Afib,
#     include = baseline_var,
#     label = baseline_var_labels,
#     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#     missing = "no"
#   ) %>%
#   add_p() %>%
#   add_overall()
#
# # Print baseline table
# tbl_svybaseline

# Create outcomes table
tbl_svyoutcomes <- dsgn %>%
  tbl_svysummary(
    by = CA_Afib,
    include = outcome_var,
    label = outcome_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p() %>%
  add_overall()

# Print outcomes table
tbl_svyoutcomes
