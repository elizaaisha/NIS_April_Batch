# Load necessary libraries
library(tidyverse)
library(arrow)

# Import data
dta <- arrow::open_dataset("./nis_data/NIS_Stroke_Afib_Socioeconomic.parquet/")

# Create smaller data sample for testing use
dta_sample <- dta |>
  filter(!is.na(Stroke) & !is.na(A_Fib)) |>
  slice_sample(n = 10000) |> compute()

# Export sampled testing data
arrow::write_parquet(dta_sample, "./nis_data/NIS_Stroke_Afib_testing.parquet")

# Load sampled testing data
testing_dta <- read_parquet("./nis_data/NIS_Stroke_Afib_testing.parquet")

# Check memory size of sampled data
object.size(testing_dta)


