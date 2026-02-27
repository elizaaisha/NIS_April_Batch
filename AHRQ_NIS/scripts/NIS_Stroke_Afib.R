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
    # Stroke = case_when(
    #   str_detect(I10_DX1, "^I63\\d*|^I436\\d*|^I978\\d*|^R297\\d*") ~ "Yes",
    #   .default = "No"
    # ),
    Stroke = case_when(
      str_detect(I10_DX1, "^I63\\d*") ~ "Yes",
      .default = "No"
    ),
    A_Fib = case_when(afib == 1 ~ "Yes", .default = "No"),
    Insurance = case_when(
      PAY1 == "Self-pay" ~ "Other",
      PAY1 == "No charge" ~ "Other",
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
    HTN = case_when(htn == 1 ~ "Yes", .default = "No"),
    DM = case_when(dm == 1 ~ "Yes", .default = "No"),
    Hyperlip = case_when(str_detect(DX10_Combined, "E78\\d*") ~ "Yes", .default = "No"),
    CAD = case_when(str_detect(DX10_Combined, "I251\\d*") ~ "Yes", .default = "No"),
    PVD = case_when(ynel5 == "Yes" ~ "Yes", .default = "No"),
    CHF = case_when(ynel1 == "Yes" ~ "Yes", .default = "No"),
    CKD = case_when(str_detect(DX10_Combined, "N18\\d*") ~ "Yes", .default = "No"),
    COPD = case_when(ynch6 == "Yes" ~ "Yes", .default = "No"),
    Dementia = case_when(ynch5 == "Yes" ~ "Yes", .default = "No"),
    Liver_Dis = case_when(ynel14 == "Yes" ~ "Yes", .default = "No"),
    OSA = case_when(str_detect(DX10_Combined, "G4733") ~ "Yes", .default = "No"),
    Anemia = case_when(ynel25 == "Yes" | ynel26 == "Yes" ~ "Yes", .default = "No"),
    Alcohol = case_when(ynel27 == "Yes" ~ "Yes", .default = "No"),
    Obesity = case_when(ynel22 == "Yes" ~ "Yes", .default = "No"),
    pStroke = case_when(str_detect(DX10_Combined, "Z8673") | str_detect(DX10_Combined, "I693\\d*") ~ "Yes", .default = "No"),
    pCardiacSurg = case_when(str_detect(DX10_Combined, "Z95[1-4]") ~ "Yes", .default = "No"),
    Pacemaker_defib = case_when(str_detect(DX10_Combined, "Z950") | str_detect(DX10_Combined, "Z95810") ~ "Yes", TRUE ~ "No"),
    Tobb = case_when(str_detect(DX10_Combined, "Z720\\d*") ~ "Yes", .default = "No"),
    TPA = case_when(str_detect(PR10_Combined, "3E03317") ~ "Yes", .default = "No"),
    ICH = case_when(str_detect(DX10_Combined, "I60\\d*|I61\\d*|I62\\d*") ~ "Yes", .default = "No"),
    Fluid_disorders = case_when(ynel24 == "Yes" ~ "Yes", .default = "No"),
    cancer_history = case_when(str_detect(DX10_Combined, "Z8500|Z8501|Z85020|Z85028|Z85030|Z85038|Z85040|Z85048|Z8505|Z85060|Z85068|Z8507|Z8509|Z85110|Z85118|Z8512|Z8520|Z8521|Z8522|Z85230|Z85238|Z8529|Z853|Z8540|Z8541|Z8542|Z8543|Z8544|Z8545|Z8546|Z8547|Z8548|Z8549|Z8550|Z8551|Z85520|Z85528|Z8553|Z8554|Z8559|Z856|Z8571|Z8572|Z8579|Z85810|Z85818|Z85819|Z85820|Z85821|Z85828|Z85830|Z85831|Z85840|Z85841|Z85848|Z85850|Z85858|Z8589|Z859|Z86000|Z86001|Z86002|Z86003|Z86004|Z86005|Z86006") ~ "Yes", .default = "No"),
    Sepsis = case_when(str_detect(DX10_Combined, "A021|A207|A227|A267|A327|A392|A393|A394|A40\\d*|A41\\d*|A427|A5486|B377|I76\\d*|R6520|R6521|T8112XA|T8144XA") ~ "Yes", .default = "No"),
    Cancer = case_when(ynch14 == "Yes" ~ "Yes", .default = "No"),
    Endovascular_Thrombectomy = case_when(str_detect(PR10_Combined, "03C[A-Z]3ZZ") ~ "Yes", .default = "No")
  ) |>
  select(
    YEAR,
    HOSPID,
    NIS_STRATUM,
    TRENDWT,
    Stroke,
    A_Fib,
    Insurance,
    Hosp_Census_Region,
    AGE,
    FEMALE,
    RACE,
    charlindex,
    ZIPINC_QRTL,
    HOSP_DIVISION,
    HOSP_BEDSIZE,
    HOSP_LOCTEACH,
    HTN,
    DM,
    Hyperlip,
    CAD,
    PVD,
    CHF,
    CKD,
    COPD,
    Dementia,
    Cancer,
    Liver_Dis,
    OSA,
    Anemia,
    Alcohol,
    Obesity,
    pStroke,
    pCardiacSurg,
    Pacemaker_defib,
    Tobb,
    TPA,
    DIED,
    LOS,
    TOTCHG,
    adj_TOTCHG,
    ICH,
    Sepsis,
    cancer_history,
    Fluid_disorders,
    Endovascular_Thrombectomy
  )

# Convert cleaned data to arrow dataset
dta_clean <- dta_clean |> to_arrow()

# Write cleaned data to parquet dataset
arrow::write_dataset(
  dta_clean,
  "./nis_data/NIS_Stroke_Afib_Socioeconomic.parquet",
  format = "parquet",
  partitioning = "YEAR"
)

toc()

