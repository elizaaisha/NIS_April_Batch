# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)

# Load external dependencies
source("./scripts/labels.R")

dta <- arrow::open_dataset("./nis_data/NIS_MitraClip.parquet/") |> collect()

# Set factor references
dta <- dta |>
  mutate(
    BMI = fct_relevel(BMI, BMI_order),
    DIED = fct_relevel(DIED, "No"),
    LOS = as.numeric(LOS),
    adj_TOTCHG = as.numeric(adj_TOTCHG)
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
dsgn <- subset(dsgn, A_Fib == "Yes" & !is.na(MitraClip_OMVR))

# Clean up memory
rm(dta)
gc()

# # Create baseline table
# tbl_svybaseline <- dsgn %>%
#   tbl_svysummary(
#     by = BMI,
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
    by = MitraClip_OMVR,
    include = outcome_var,
    label = outcome_var_labels,
    missing = "no"
  ) %>%
  add_p()

# Print outcomes table
tbl_svyoutcomes
