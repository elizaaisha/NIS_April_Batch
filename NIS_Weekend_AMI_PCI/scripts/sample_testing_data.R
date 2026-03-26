# Load necessary libraries
library(tidyverse)
library(arrow)

# Import data
dta <- arrow::open_dataset(
  "./nis_data/NIS_Weekend_AMI_PCI.parquet/"
)

# Create smaller data sample for testing use
dta_sample <- dta |>
  filter(AMI == "Yes" & PCI == "Yes") |>
  slice_sample(n = 200) |> compute()

# Export sampled testing data
arrow::write_parquet(dta_sample, "./nis_data/NIS_Weekend_AMI_PCI_testing.parquet")

# Load sampled testing data
testing_dta <- read_parquet("./nis_data/NIS_Weekend_AMI_PCI_testing.parquet")

# Check memory size of sampled data
object.size(testing_dta)
