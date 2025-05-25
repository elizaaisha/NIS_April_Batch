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
    PD = case_when(
      str_detect(DX10_Combined, "G20\\d*") ~ "Yes", # Exact match for Parkinson's disease
      .default = "No"
    ),

    AP = case_when(
      str_detect(DX10_Combined, "J690") ~ "Yes", # Exact match for aspiration pneumonia
      .default = "No"
    ),
    PD_AP = case_when(
      AP == "Yes" & PD == "Yes" ~ "Parkinson's disease with aspiration pneumonia",
      AP == "No" & PD == "Yes" ~ "Parkinson's disease without aspiration pneumonia"
    ),
    PD_AP_2 = case_when(
      AP == "Yes" & PD == "Yes" ~ "Aspiration pneumonia with Parkinson's disease",
      AP == "Yes" & PD == "No" ~ "Aspiration pneumonia without Parkinson's disease"
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
    HTN = case_when(htn == 1 ~ "Yes", .default = "No"),
    DM = case_when(dm == 1 ~ "Yes", .default = "No"),
    Hyperlip = case_when(str_detect(DX10_Combined, "E78\\d*") ~ "Yes", .default = "No"),
    CAD = case_when(str_detect(DX10_Combined, "I251\\d*") ~ "Yes", .default = "No"),
    PVD = case_when(ynel5 == "Yes" ~ "Yes", .default = "No"),
    CHF = case_when(ynel1 == "Yes" ~ "Yes", .default = "No"),
    CKD = case_when(str_detect(DX10_Combined, "N18\\d*") ~ "Yes", .default = "No"),
    COPD = case_when(ynch6 == "Yes" ~ "Yes", .default = "No"),
    Cancer = case_when(ynch14 == "Yes" ~ "Yes", .default = "No"),
    Dementia = case_when(ynch5 == "Yes" ~ "Yes", .default = "No"),
    Liver_Dis = case_when(ynel14 == "Yes" ~ "Yes", .default = "No"),
    OSA = case_when(str_detect(DX10_Combined, "G4733") ~ "Yes", .default = "No"),
    Anemia = case_when(ynel25 == "Yes" | ynel26 == "Yes" ~ "Yes", .default = "No"),
    Alcohol = case_when(ynel27 == "Yes" ~ "Yes", .default = "No"),
    Obesity = case_when(ynel22 == "Yes" ~ "Yes", .default = "No"),
    Cardiac_arrhythmia = case_when(ynel2 == "Yes" ~ "Yes", .default = "No"),
    pStroke = case_when(str_detect(DX10_Combined, "Z8673") | str_detect(DX10_Combined, "I693\\d*") ~ "Yes", .default = "No"),
    pCardiacSurg = case_when(str_detect(DX10_Combined, "Z95[1-4]") ~ "Yes", .default = "No"),
    Pacemaker_defib = case_when(str_detect(DX10_Combined, "Z950") | str_detect(DX10_Combined, "Z95810") ~ "Yes", TRUE ~ "No"),
    Tobb = case_when(str_detect(DX10_Combined, "Z720.*") ~ "Yes", .default = "No"),
    ARF = case_when(str_detect(DX10_Combined, "J960.*") ~ "Yes", .default = "No"),
    Pall_Care = case_when(
      str_detect(DX10_Combined, "Z515.*") ~ "Yes",
      .default = "No"
    ),
    Sepsis = case_when(str_detect(DX10_Combined, "A021|A207|A227|A267|A327|A392|A393|A394|A40\\d*|A41\\d*|A427|A5486|B007|B377|I76|O0337|O0387|O0487|O0737|O0887|O85|O8604|P36\\d*|R6520|R6521|T8112XA|T8144XA") ~ "Yes", .default = "No"),
    MVent = case_when(str_detect(DX10_Combined, "5A0955Z|5A0945Z|5A0935Z|5A1935Z|5A1945Z|5A1955Z") ~ "Yes", .default = "No"),
    Disp_home = case_when(DISPUNIFORM == 1 ~ "Yes", .default = "No"),
    Disp_short = case_when(DISPUNIFORM == 2 ~ "Yes", .default = "No"),
    Disp_long = case_when(DISPUNIFORM == 5 ~ "Yes", .default = "No"),
    Disp_home_health = case_when(DISPUNIFORM == 6 ~ "Yes", .default = "No"),
    Favorable_Discharge = case_when(
      DISPUNIFORM %in% c(1, 6) ~ "Yes",
      .default = "No"
    ),
    Fluid_disorders = case_when(ynel24 == "Yes" ~ "Yes", .default = "No")
    # DISPUNIFORM = case_when(
    #   DISPUNIFORM == 1 ~ "Home discharge",
    #   DISPUNIFORM == 2 ~ "Transfer to short‐term hospital",
    #   DISPUNIFORM == 5 ~ "s",
    #   DISPUNIFORM == 6 ~ "Transfer to home health")
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
    Liver_Dis,
    OSA,
    Cancer,
    Anemia,
    Alcohol,
    Obesity,
    pStroke,
    pCardiacSurg,
    Pacemaker_defib,
    Tobb,
    Sepsis,
    DIED,
    LOS,
    PD_AP,
    PD,
    AP,
    PD_AP_2,
    TOTCHG,
    adj_TOTCHG,
    Sepsis,
    Pall_Care,
    ARF,
    Cancer,
    Cardiac_arrhythmia,
    MVent,
    Disp_home,
    Disp_short,
    Disp_long,
    Disp_home_health,
    Favorable_Discharge,
    elixsum,
    Fluid_disorders
  )

# Convert cleaned data to arrow dataset
dta_clean <- dta_clean |> to_arrow()

# Write cleaned data to parquet dataset
arrow::write_dataset(
  dta_clean,
  "./nis_data/NIS_PD_AP.parquet",
  format = "parquet",
  partitioning = "YEAR"
)

toc()

