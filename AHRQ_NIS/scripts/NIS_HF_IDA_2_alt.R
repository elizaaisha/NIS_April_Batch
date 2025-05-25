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

# Import external functions
source("./scripts/fn_NIS_base_data_clean.R")

# Define global variables
# Create variables with CPI values for the relevant years
cpi_2020 <- 258.811
cpi_2019 <- 255.657
cpi_2018 <- 251.107

tic()
# Transform project specific variables

dta_base <- fn_NIS_base_data_clean(datasets)

dta_clean <- dta_base |>
  mutate(
    HF = case_when(
      str_detect(DX10_Combined, "I502|I503|I504") ~ "Yes",
      .default = "No"
    ),
    IDA = case_when(
      str_detect(DX10_Combined, "D50\\d*") ~ "Yes",
      .default = "No"
    ),
    HFrEF = case_when(str_detect(DX10_Combined, "I502\\d*|I504\\d*") ~ "Yes", .default = "No"),
    HFpEF = case_when(str_detect(DX10_Combined, "I503\\d*") ~ "Yes", .default = "No"),
    HF_Categories = case_when(
      HFrEF == "Yes" ~ "Heart failure with reduced ejection fraction",
      HFpEF == "Yes" ~ "Heart failure with preserved ejection fraction",
      .default = NA
    ),
    Insurance = case_when(
      PAY1 == "Self-pay" ~ "Private",
      PAY1 == "No charge" ~ "Private",
      .default = PAY1
    ),
    Hosp_Census_Region = case_when(
      HOSP_DIVISION == "Pacific" | HOSP_DIVISION == "Mountain" ~ "West",
      HOSP_DIVISION == "West North Central" |
        HOSP_DIVISION == "East North Central" ~ "Midwest",
      HOSP_DIVISION == "Middle Atlantic" |
        HOSP_DIVISION == "New England" ~ "Northeast",
      HOSP_DIVISION == "West South Central" |
        HOSP_DIVISION == "East South Central" |
        HOSP_DIVISION == "South Atlantic" ~ "South"
    ),
    adj_TOTCHG = case_when(
      # Inflation adjust to 2020 dollars
      YEAR == 2018 ~ round(TOTCHG * (cpi_2020 / cpi_2018)),
      YEAR == 2019 ~ round(TOTCHG * (cpi_2020 / cpi_2019)),
      YEAR == 2020 ~ round(TOTCHG * (cpi_2020 / cpi_2020))
    ),
    HTN = case_when(ynel6 == "Yes" |  ynel31 == "Yes" ~ "Yes", .default = "No"),
    Hyperlip = case_when(str_detect(DX10_Combined, "E78\\d*") ~ "Yes", .default = "No"),
    CAD = case_when(str_detect(DX10_Combined, "I251\\d*") ~ "Yes", .default = "No"),
    PVD = case_when(ynel5 == "Yes" ~ "Yes", .default = "No"),
    COPD = case_when(ynch6 == "Yes" ~ "Yes", .default = "No"),
    Dementia = case_when(ynch5 == "Yes" ~ "Yes", .default = "No"),
    OSA = case_when(str_detect(DX10_Combined, "G4733") ~ "Yes", .default = "No"),
    pMI = case_when(pMI == 1 ~ "Yes", .default = "No"),
    Alcohol = case_when(ynel27 == "Yes" ~ "Yes", .default = "No"),
    Valv_HD = case_when(ynel3 == "Yes" ~ "Yes", .default = "No"),
    Afib = case_when(afib == 1 ~ "Yes", .default = "No"),
    CA = case_when(ynch14 == "Yes" ~ "Yes", .default = "No"),
    BMI = case_when(
      str_detect(DX10_Combined, "Z681\\d*") ~ "Underweight (BMI ≤19.9kg/m²)",
      str_detect(DX10_Combined, "Z682[0-4]\\d*") ~ "Normal weight (BMI 20.0-24.9kg/m²)",
      str_detect(DX10_Combined, "Z682[5-9]\\d*") ~ "Overweight (BMI 25.0-29.9kg/m²)",
      str_detect(DX10_Combined, "Z683[0-4]\\d*") ~ "Class I obesity (BMI 30.0-34.9kg/m²)",
      str_detect(DX10_Combined, "Z683[5-9]\\d*") ~ "Class II obesity (BMI 35.0-39.9kg/m²)",
      str_detect(DX10_Combined, "Z684\\d*") ~ "Class III obesity (BMI ≥40.0kg/m²)"
    ),
    IBD = case_when(str_detect(DX10_Combined, "K50\\d* | K51\\d*") ~ "Yes", .default = "No"),
    Smoking = case_when(str_detect(DX10_Combined, "F17210 | F17213 | F17218 | F17219 | Z720") ~ "Yes", .default = "No"),
    Obesity = case_when(ynel22 == "Yes" ~ "Yes", .default = "No"),
    pStroke = case_when(str_detect(DX10_Combined, "Z8673") | str_detect(DX10_Combined, "I693\\d*") ~ "Yes", .default = "No"),
    pCardiacSurg = case_when(str_detect(DX10_Combined, "Z95[1-4]") ~ "Yes", .default = "No"),
    Pacemaker = case_when(str_detect(DX10_Combined, "Z950") | str_detect(DX10_Combined, "Z95810") ~ "Yes", TRUE ~ "No"),
    Hypothyroidism = case_when(ynel12 == "Yes" ~ "Yes", .default = "No"),
    Hyperthyroidism = case_when(str_detect(DX10_Combined, "E0500|E0501|E0510|E0511|E0520|E0521|E0530|E0531|E0540|E0541|E0580|E0581|E0590|E0591") ~ "Yes", .default = "No"),
    Chronic_Pancreatitis = case_when(str_detect(DX10_Combined, "K860|K861") ~ "Yes", .default = "No"),
    Malnutrition = case_when(str_detect(DX10_Combined, "E40|E41|E42|E43|E440|E441|E45|E46") ~ "Yes", .default = "No"),
    Fluid_disorders = case_when(ynel24 == "Yes" ~ "Yes", .default = "No"),
    cancer_history = case_when(str_detect(DX10_Combined, "Z8500|Z8501|Z85020|Z85028|Z85030|Z85038|Z85040|Z85048|Z8505|Z85060|Z85068|Z8507|Z8509|Z85110|Z85118|Z8512|Z8520|Z8521|Z8522|Z85230|Z85238|Z8529|Z853|Z8540|Z8541|Z8542|Z8543|Z8544|Z8545|Z8546|Z8547|Z8548|Z8549|Z8550|Z8551|Z85520|Z85528|Z8553|Z8554|Z8559|Z856|Z8571|Z8572|Z8579|Z85810|Z85818|Z85819|Z85820|Z85821|Z85828|Z85830|Z85831|Z85840|Z85841|Z85848|Z85850|Z85858|Z8589|Z859|Z86000|Z86001|Z86002|Z86003|Z86004|Z86005|Z86006") ~ "Yes", .default = "No"),
    Depression = case_when(ynel30 == "Yes" ~ "Yes", .default = "No")
    ) |>
  select(
    YEAR,
    HOSPID,
    NIS_STRATUM,
    TRENDWT,
    Insurance,
    Hosp_Census_Region,
    AGE,
    FEMALE,
    RACE,
    elixsum,
    charlindex,
    ZIPINC_QRTL,
    HOSP_DIVISION,
    HOSP_BEDSIZE,
    HOSP_LOCTEACH,
    DIED,
    LOS,
    TOTCHG,
    adj_TOTCHG,
    HTN,
    Hyperlip,
    CAD,
    PVD,
    COPD,
    Dementia,
    OSA,
    Alcohol,
    Valv_HD,
    Afib,
    CA,
    BMI,
    IBD,
    Smoking,
    Obesity,
    pStroke,
    pCardiacSurg,
    Pacemaker,
    Hypothyroidism,
    Hyperthyroidism,
    Chronic_Pancreatitis,
    Malnutrition,
    Fluid_disorders,
    cancer_history,
    Depression,
    pMI,
    HFpEF,
    HFrEF,
    HF,
    IDA,
    HF_Categories
  )

# Convert cleaned data to arrow dataset
dta_clean <- dta_clean |> to_arrow()

# Write cleaned data to parquet dataset
arrow::write_dataset(
  dta_clean,
  "./nis_data/NIS_HF_IDA_2.parquet",
  format = "parquet",
  partitioning = "YEAR"
)

toc()

