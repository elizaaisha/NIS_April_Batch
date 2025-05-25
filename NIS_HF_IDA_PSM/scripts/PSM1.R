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
TESTING <- F

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./data/NIS_HF_IDA_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_HF_IDA_2.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    HF = fct_relevel(HF_Categories, "Heart failure with preserved ejection fraction"),
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = as.factor(DIED),
    LOS = as.numeric(LOS)
  )


#propensity score matching
dta <- dta %>%
  filter(!is.na(HF_Categories) & !is.na(AGE) & !is.na(FEMALE) & !is.na(RACE) & !is.na(ZIPINC_QRTL) & !is.na(Insurance)
         & !is.na(Hosp_Census_Region) & !is.na(HTN) & !is.na(Hyperlip) & !is.na(CAD)
         & !is.na(PVD) & !is.na(COPD) & !is.na(Dementia) & !is.na(OSA) & !is.na(Alcohol)
         & !is.na(Valv_HD) & !is.na(Dementia) & !is.na(CA) & !is.na(Fluid_disorders) & !is.na(Obesity) & !is.na(Pacemaker)
         & !is.na(pMI) & !is.na(Malnutrition) & !is.na(pCardiacSurg))

# Recode to binary (1 = with NAFLD, 0 = without NAFLD)
dta <- dta %>%
  mutate(
    HF_binary = if_else(HF_Categories == "Heart failure with preserved ejection fraction", 1, 0)
  )

# Run matching
matched_data <- matchit(HF_binary ~ AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + Hosp_Census_Region + elixsum + HTN + Hyperlip + CAD + PVD + COPD + Dementia + OSA + Alcohol +
                          Valv_HD + Fluid_disorders + Pacemaker + Obesity + CA + Obesity + Malnutrition + pCardiacSurg,
                        data = dta, method = "nearest", distance = "glm", discard = "none",
                        m.order = "largest", caliper = 0.2, ratio = 1, s.weights = ~TRENDWT)

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
dsgn <- subset(dsgn, IDA == "Yes")


# Clean up memory
rm(dta)
gc()


# # Create baseline table
# tbl_svybaseline <- dsgn %>%
#   tbl_svysummary(
#     by = NASH_NAFLD_Categories,
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
tbl_outcomes <- dsgn %>%
  tbl_svysummary(
    by = HF_Categories,
    include = outcome_var,
    label = outcome_var_labels,
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p()

# Print outcomes table
tbl_outcomes
