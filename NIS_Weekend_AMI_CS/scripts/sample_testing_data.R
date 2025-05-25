# Load necessary libraries
library(tidyverse)
library(arrow)

# Import data
dta <- arrow::open_dataset(
  "./nis_data/NIS_MMP_WeekendEVS_AIS_Outcomes.parquet/"
)

# Create smaller data sample for testing use
dta_sample <- dta |>
  filter(
    AGE >= 18 &
      Acute_Ischemic_Stroke == "Yes" &
      EV_Thrombectomy == "Yes" &
      !is.na(AWEEKEND)
  ) |>
  compute()

# Export sampled testing data
arrow::write_parquet(
  dta_sample,
  "./nis_data/NIS_MMP_WeekendEVS_AIS_Outcomes_testing.parquet"
)

# Load sampled testing data
testing_dta <- read_parquet(
  "./nis_data/NIS_MMP_WeekendEVS_AIS_Outcomes_testing.parquet"
)

# Check memory size of sampled data
object.size(testing_dta)
