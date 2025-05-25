# Load necessary libraries
library(tidyverse)
library(arrow)

# Import data
dta <- arrow::open_dataset("./data/NIS_PD_AP.parquet")

# Create smaller data sample for testing use
dta_sample <- dta |>
  filter(!is.na(PD_AP)) |>
  slice_sample(n = 10000) |> compute()

# Export sampled testing data
arrow::write_parquet(dta_sample, "./data/NIS_PD_AP_testing.parquet")

# Load sampled testing data
testing_dta <- read_parquet("./data/NIS_PD_AP_testing.parquet")

# Check memory size of sampled data
object.size(testing_dta)


View(testing_dta)
