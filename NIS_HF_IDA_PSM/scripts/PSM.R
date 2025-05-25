# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)
library(MatchIt)
library(cobalt)

# Extra: Libraries for DOCX export
library(flextable)
library(officer)

# Install 'fastsurvey' from GitHub if not already installed
# remotes::install_github("bschneidr/fastsurvey")

# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- T

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./data/NIS_NASH_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_NASH.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    LOS = as.numeric(LOS)
  )

#propensity score matching
dta <- dta %>%
  filter(!is.na(NASH_NAFLD_Categories) & !is.na(AGE) & !is.na(FEMALE) & !is.na(RACE) & !is.na(ZIPINC_QRTL) & !is.na(Insurance)
         & !is.na(Hosp_Census_Region) & !is.na(HOSP_BEDSIZE) & !is.na(HOSP_LOCTEACH) & !is.na(HTN) & !is.na(Hyperlip) & !is.na(CAD)
         & !is.na(PVD) & !is.na(COPD) & !is.na(Dementia) & !is.na(OSA) & !is.na(Alcohol) & !is.na(Valv_HD) & !is.na(Afib) & !is.na(CA) &
           !is.na(BMI) & !is.na(IBD) & !is.na(elixsum))

# Recode to binary (1 = with NAFLD, 0 = without NAFLD)
dta <- dta %>%
  mutate(
    NAFLD_binary = if_else(NASH_NAFLD_Categories == "With NAFLD/NASH", 1, 0)
  )

# Run matching
matched_data <- matchit(NAFLD_binary ~ AGE + FEMALE + RACE + elixsum + HTN + Hyperlip + CAD + OSA + COPD,
                        data = dta, method = "nearest", distance = "glm", discard = "none",
                        m.order = "largest", caliper = 0.2, ratio = 1, s.weights = ~TRENDWT)

#Dementia + OSA + Alcohol + Valv_HD + Afib + CA + BMI
# Extract matched data
matched_dta <- match.data(matched_data)

balance_table <- bal.tab(matched_data)
print(balance_table)

# Set survey options for lonely PSUs
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

# Define survey design
dsgn <- svydesign(
  id = ~ HOSPID,
  strata = ~ NIS_STRATUM,
  weights = ~ TRENDWT,
  data = matched_dta,
  nest = TRUE
)

# Subset data for specific condition
dsgn <- subset(dsgn, HF_CKD_T2DM == "Yes")


# Clean up memory
rm(dta)
gc()


# Create baseline table
tbl_svybaseline <- dsgn %>%
  tbl_svysummary(
    by = NASH_NAFLD_Categories,
    include = baseline_var,
    label = baseline_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p()

# Print baseline table
tbl_svybaseline

# Create outcomes table
tbl_outcomes <- dsgn %>%
  tbl_svysummary(
    by = NASH_NAFLD_Categories,
    include = outcome_var,
    label = outcome_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p()

# Print outcomes table
tbl_outcomes
