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
    CA = case_when(str_detect(DX10_Combined, "C.*") ~ "Yes", .default = "No"),
    A_Fib = case_when(str_detect(DX10_Combined, "I480.*|I481.*|I482.*") ~ "Yes", .default = "No"),
    CA_Afib = case_when(
      CA == "Yes" & A_Fib == "Yes" ~ "Atrial Fibrillation with Cancer",
      CA == "No" & A_Fib == "Yes" ~ "Atrial Fibrillation without Cancer"
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
    Dementia = case_when(ynch5 == "Yes" ~ "Yes", .default = "No"),
    Liver_Dis = case_when(ynel14 == "Yes" ~ "Yes", .default = "No"),
    OSA = case_when(str_detect(DX10_Combined, "G4733") ~ "Yes", .default = "No"),
    Anemia = case_when(ynel25 == "Yes" | ynel26 == "Yes" ~ "Yes", .default = "No"),
    Alcohol = case_when(ynel27 == "Yes" ~ "Yes", .default = "No"),
    Obesity = case_when(ynel22 == "Yes" ~ "Yes", .default = "No"),
    Smoker = case_when(str_detect(DX10_Combined, "F17210 | F17213 | F17218 | F17219 | Z720") ~ "Yes", .default = "No"),
    VHD = case_when(ynel3 == "Yes" ~ "Yes", .default = "No"),
    pStroke = case_when(str_detect(DX10_Combined, "Z8673") | str_detect(DX10_Combined, "I693\\d*") ~ "Yes", .default = "No"),
    pCardiacSurg = case_when(str_detect(DX10_Combined, "Z95[1-4]") ~ "Yes", .default = "No"),
    Pacemaker_defib = case_when(str_detect(DX10_Combined, "Z950") | str_detect(DX10_Combined, "Z95810") ~ "Yes", TRUE ~ "No"),
    Tobb = case_when(str_detect(DX10_Combined, "Z720\\d*") ~ "Yes", .default = "No"),
    AMI = case_when(str_detect(DX10_Combined, "1214\\d*|I21[0-3]\\d*|I22[0-1]\\d*|I22[8-9]\\d*") ~ "Yes", .default = "No"),
    VTE = case_when(str_detect(DX10_Combined, "I82\\d*|I26\\d*") ~ "Yes", .default = "No"),
    AIS = case_when(str_detect(DX10_Combined, "163\\d*") ~ "Yes", .default = "No"),
    MB = case_when(str_detect(DX10_Combined, "K920.*|K921.*|K922.*|K250.*|K252.*|K254.*|K256.*|K260.*|K262.*|K264.*|K266.*|K270.*|K272.*|K27.*|K276.*|K280.*|K282.*|K284.*|K286.*|K2901.*|K2921.*|K2931.*|K2941.*|K2951.*|K2961.*|K2971.*|K2981.*|K2991.*|R31.*|R04.*|K661.*|I60.*|I61.*|I62.*") ~ "Yes", .default = "No"),
    # PE = case_when(str_detect(DX10_Combined, "I26\\d*") ~ "Yes", .default = "No"),
    CatheterAblation = case_when(str_detect(PR10_Combined, "02583ZZ|025S3ZZ|025T3ZZ|02563ZZ|02573ZZ|02553ZZ") ~ "Yes", .default = "No"),
    Cancer_type = case_when(

      str_detect(DX10_Combined, "C73\\d*") ~ "Thyroid",

      str_detect(DX10_Combined, "C0[0-9]\\d*|C1[0-4]\\d*|C69\\d*|C3[0-2]\\d*") ~ "Head and neck",

      str_detect(DX10_Combined, "C1[5-9]\\d*|C2[0-6]\\d*") ~ "Gastrointestinal tract",

      str_detect(DX10_Combined, "C34\\d*") ~ "Lung",

      str_detect(DX10_Combined, "C4[0-1]\\d*") ~ "Bone and articular cartilage",

      str_detect(DX10_Combined, "C4[5-9]\\d*") ~ "Mesothelium and soft tissue",

      str_detect(DX10_Combined, "C50\\d*") ~ "Breast",

      str_detect(DX10_Combined, "C5[1-8]\\d*|C6[0-3]\\d*") ~ "Genital organs",

      str_detect(DX10_Combined, "C6[4-8]\\d*") ~ "Renal",

      str_detect(DX10_Combined, "C7[0-2]\\d*") ~ "Central nervous system",

      str_detect(DX10_Combined, "C7[A-B]\\d*") ~ "Neuroendocrine",

      str_detect(DX10_Combined, "C81\\d*") ~ "Hodgkin Lymphoma",

      str_detect(DX10_Combined, "C8[2-6]\\d*") ~ "Non-Hodgkin Lymphoma",

      str_detect(DX10_Combined, "C9[1-6]\\d*") ~ "Leukemia",

      str_detect(DX10_Combined, "C90\\d*") ~ "Multiple Myeloma",

      str_detect(DX10_Combined, "C4[3-4]\\d*") ~ "Melanoma",

      str_detect(DX10_Combined, "C33\\d*|C3[7-9]\\d*|C4A\\d*|C7[4-5]\\d*|C88\\d*|C96\\d*") ~ "Others",
      .default = NA)
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
    Anemia,
    Alcohol,
    Obesity,
    pStroke,
    pCardiacSurg,
    Pacemaker_defib,
    Tobb,
    DIED,
    LOS,
    TOTCHG,
    adj_TOTCHG,
    CA,
    CA_Afib,
    A_Fib,
    AMI,
    AIS,
    VTE,
    MB,
    Cancer_type,
    PL_NCHS,
    VHD,
    Smoker,
    CatheterAblation,
    elixsum
  )

# Convert cleaned data to arrow dataset
dta_clean <- dta_clean |> to_arrow()

# Write cleaned data to parquet dataset
arrow::write_dataset(
  dta_clean,
  "./nis_data/NIS_CA_Afib.parquet",
  format = "parquet",
  partitioning = "YEAR"
)

toc()

