# Load necessary libraries
library(tidyverse)
library(arrow)

# Import data
dta <- arrow::open_dataset("./data/NIS_CA_Afib.parquet/")

# Create smaller data sample for testing use
dta_sample <- dta |>
  filter(A_Fib == "Yes") |>
  slice_sample(n = 10000) |> compute()

# Export sampled testing data
arrow::write_parquet(dta_sample, "./data/NIS_CA_Afib_testing.parquet.parquet")

# Load sampled testing data
testing_dta <- read_parquet("./data/NIS_CA_Afib_testing.parquet")

# Check memory size of sampled data
object.size(testing_dta)


View(testing_dta)
