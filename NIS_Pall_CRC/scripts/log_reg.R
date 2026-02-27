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
  dta <- arrow::open_dataset("./data/NIS_Palliative_CRC_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_Palliative_CRC.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    Palliative_Care = as.factor(Palliative_Care),
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
dsgn <- subset(dsgn, Colorectal_CA == "Yes" & !is.na(Palliative_Care))

# Clean up memory
rm(dta)
gc()

# Palliative_Care
#Create the regression model
reg_model <- svyglm(
  Palliative_Care ~
    AGE +
    FEMALE +
    RACE +
    Insurance +
    ZIPINC_QRTL +
    Hosp_Census_Region +
    HOSP_BEDSIZE +
    HOSP_LOCTEACH +
    APRDRG_Risk_Mortality +
    elixsum,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model, exponentiate = T, label = reg_var_labels)
