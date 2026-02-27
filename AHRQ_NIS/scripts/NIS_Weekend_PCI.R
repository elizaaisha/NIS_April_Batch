# Import libraries
library(arrow) # For handling Parquet files
library(dplyr) # For data manipulation
library(stringr) # For string manipulation
library(duckdb) # For DuckDB
library(dbplyr) # For DuckDB dplyr
library(tictoc) # For timing

# Import datasets
datasets <- list(
  arrow::open_dataset("./nis_data/NIS_2018.parquet/"),
  arrow::open_dataset("./nis_data/NIS_2019.parquet/"),
  arrow::open_dataset("./nis_data/NIS_2020.parquet/")
)

# Import external helpers
source("./scripts/helpers/functions/clean_NIS_base_data.R")
source("./scripts/helpers/constants/Const_Apr_Eliza.R")

# Define global variables
# Create variables with CPI values for the relevant years
cpi_2020 <- 258.811
cpi_2019 <- 255.657
cpi_2018 <- 251.107

tic()
# Transform project-specific variables onto base cleaned data
dta_base <- clean_NIS_base_data(datasets)

dta_clean <- dta_base |>
  mutate(
    # Inflation adjust to 2020 dollars
    adj_TOTCHG = case_when(
      YEAR == 2018 ~ round(TOTCHG * (cpi_2020 / cpi_2018)),
      YEAR == 2019 ~ round(TOTCHG * (cpi_2020 / cpi_2019)),
      YEAR == 2020 ~ round(TOTCHG * (cpi_2020 / cpi_2020))
    ),
    RACE_alt = case_when(
      RACE == "Native American" ~ "Other",
      .default = RACE
    ),
    PAY1_alt = case_when(
      PAY1 == "Self-pay" ~ "Other",
      PAY1 == "No charge" ~ "Other",
      .default = PAY1
    ),
    AMI = case_when(
      str_detect(I10_DX1, isAMI) ~ "Yes",
      .default = "No"
    ),
    CS = case_when(
      str_detect(DX10_Combined, isCardioShock) ~ "Yes",
      .default = "No"
    ),
    Mechanical_Ventilation = case_when(
      str_detect(PR10_Combined, isMechanicalVentilation) ~ "Yes",
      .default = "No"
    ),
    # Intracranial_Hemorrhage = case_when(
    #   str_detect(DX10_Combined, isIntracranialHemorrhage) ~ "Yes",
    #   .default = "No"
    # ),
    PCI = case_when(
      str_detect(PR10_Combined, isPCIProcedure) ~ "Yes",
      .default = "No"
    ),
    MCS = case_when(
      str_detect(PR10_Combined, isMechanicalCirculatorySupport) ~ "Yes",
      .default = "No"
    ),
    ECMO = case_when(
      str_detect(PR10_Combined, isECMO) ~ "Yes",
      .default = "No"
    ),
    AcuteKidneyInjury = case_when(
      str_detect(DX10_Combined, isAcuteKidneyInjury) ~ "Yes",
      .default = "No"
    ),
    GIHemorrhage = case_when(
      str_detect(DX10_Combined, isGIHemorrhage) ~ "Yes",
      .default = "No"
    ),
    Thrombolytic_Drugs = case_when(
      str_detect(PR10_Combined, isThrombolyticDrugs) ~ "Yes",
      .default = "No"
    ),
    Favorable_Discharge = case_when(
      DISPUNIFORM %in% c("Routine discharge to home/self-care", "Home health care") ~ "Yes",
      .default = "No"
    )
  ) |>
  select(
    # Base variables
    YEAR,
    HOSPID,
    NIS_STRATUM,
    TRENDWT,
    LOS,
    DIED,
    TOTCHG,
    adj_TOTCHG,
    # Additional variables
    AGE,
    FEMALE,
    RACE,
    PAY1,
    ZIPINC_QRTL,
    ELECTIVE,
    HOSP_REGION,
    HOSP_BEDSIZE,
    HOSP_LOCTEACH,
    elixsum,
    charlindex,
    grpci,
    # Study-specific variables
    RACE_alt,
    PAY1_alt,
    AWEEKEND,
    TRAN_IN,
    TRAN_OUT,
    APRDRG_Severity,
    AMI,
    CS,
    Mechanical_Ventilation,
    # Intracranial_Hemorrhage,
    PCI,
    MCS,
    ECMO,
    AcuteKidneyInjury,
    GIHemorrhage,
    Favorable_Discharge,
    Thrombolytic_Drugs
  )

# Convert cleaned data to arrow dataset
dta_clean <- dta_clean |> to_arrow()

# Write cleaned data to parquet dataset
arrow::write_dataset(
  dta_clean,
  "./nis_data/NIS_Weekend_AMI_PCI.parquet",
  format = "parquet",
  partitioning = "YEAR"
)

toc()
