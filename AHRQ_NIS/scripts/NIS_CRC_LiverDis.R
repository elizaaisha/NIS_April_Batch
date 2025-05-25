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
    Residence = case_when(
      PL_NCHS %in% c("Central metro ≥1 million", "Fringe metro ≥1 million") ~ "Large metro",
      PL_NCHS %in% c("Metro 250,000-999,999", "Metro 50,000-249,999") ~ "Small metro",
      PL_NCHS %in% c("Micropolitan", "Other") ~ "Micropolitan"
    ),
    Insurance = case_when(
      PAY1 == "Self-pay" ~ "Other",
      PAY1 == "No charge" ~ "Other",
      .default = PAY1
    ),
    LiverDis = case_when(
      ynel14 == "Yes" ~ "with Liver Disease",
      .default = "without Liver Disease"
    ),
    ColorectalCancer = case_when(
      str_detect(DX10_Combined, isColorectalCancer) ~ "Yes",
      .default = "No"
    ),
    CRC_Resection = case_when(
      str_detect(PR10_Combined, CRC_Resection) ~ "Yes",
      .default = "No"
    ),
    AMI = case_when(
      str_detect(DX10_Combined, isAMI) ~ "Yes",
      .default = "No"
    ),
    CKD = case_when(
      str_detect(DX10_Combined, isCKD) ~ "Yes",
      .default = "No"
    ),
    IBD = case_when(
      str_detect(DX10_Combined, isIBD) ~ "Yes",
      .default = "No"
    ),
    HTN = case_when(
      (ynel6 == "Yes" | ynel31 == "Yes") ~ "Yes",
      .default = "No"
    ),
    Obesity = case_when(
      ynel22 == "Yes" ~ "Yes",
      .default = "No"
    ),
    DM = case_when(
      (ynel10 == "Yes" | ynel11 == "Yes") ~ "Yes",
      .default = "No"
    ),
    CPD = case_when(
      ynel9 == "Yes" ~ "Yes",
      .default = "No"
    ),
    PVD = case_when(
      ynel5 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Hyperlipidemia = case_when(
      str_detect(DX10_Combined, isHyperlipidemia) ~ "Yes",
      .default = "No"
    ),
    Arrhythmia = case_when(
      ynel2 == "Yes" ~ "Yes",
      .default = "No"
    ),
    CHF = case_when(
      ynel1 == "Yes" ~ "Yes",
      .default = "No"
    ),
    ObstructiveSleepApnea = case_when(
      ynel1 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Smoking = case_when(
      str_detect(DX10_Combined, isSmoking) ~ "Yes",
      .default = "No"
    ),
    PriorStroke = case_when(
      str_detect(DX10_Combined, isPriorStroke) ~ "Yes",
      .default = "No"
    ),
    Cancer = case_when(
      ynch14 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Sepsis = case_when(
      str_detect(DX10_Combined, isSepsis) ~ "Yes",
      .default = "No"
    ),
    WoundInfection = case_when(
      str_detect(DX10_Combined, isWoundInfection) ~ "Yes",
      .default = "No"
    ),
    Abcess = case_when(
      str_detect(DX10_Combined, isAbcess) ~ "Yes",
      .default = "No"
    ),
    Pneumonia = case_when(
      str_detect(DX10_Combined, isPneumonia) ~ "Yes",
      .default = "No"
    ),
    Hemorrhage = case_when(
      str_detect(DX10_Combined, isHemorrhage) ~ "Yes",
      .default = "No"
    ),
    DVT = case_when(
      str_detect(DX10_Combined, isDVT) ~ "Yes",
      .default = "No"
    ),
    PulmEmbolism = case_when(
      str_detect(DX10_Combined, isPulmEmbolism) ~ "Yes",
      .default = "No"
    ),
    RespFailure = case_when(
      str_detect(DX10_Combined, isRespFailure) ~ "Yes",
      .default = "No"
    ),
    MechanicalVentilation = case_when(
      str_detect(DX10_Combined, isMechanicalVentilation) ~ "Yes",
      .default = "No"
    ),
    CardiacArrest = case_when(
      str_detect(DX10_Combined, isCardiacArrest) ~ "Yes",
      .default = "No"
    ),
    Stroke = case_when(
      str_detect(DX10_Combined, isStroke) ~ "Yes",
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
    ZIPINC_QRTL,
    HOSP_REGION,
    HOSP_BEDSIZE,
    HOSP_LOCTEACH,
    charlindex,
    ELECTIVE,
    grpci,
    Residence,
    Insurance,
    PL_NCHS,
    # Study-specific variables
    AMI,
    ColorectalCancer,
    CKD,
    HTN,
    Obesity,
    DM,
    CPD,
    LiverDis,
    CRC_Resection,
    PVD,
    Hyperlipidemia,
    CHF,
    Arrhythmia,
    Smoking,
    PriorStroke,
    CardiacArrest,
    Stroke,
    IBD,
    ObstructiveSleepApnea,
    Cancer,
    WoundInfection,
    Abcess,
    Pneumonia,
    Hemorrhage,
    DVT,
    PulmEmbolism,
    RespFailure,
    MechanicalVentilation
  )

# Convert cleaned data to arrow dataset
dta_clean <- dta_clean |> to_arrow()

# Write cleaned data to parquet dataset
arrow::write_dataset(
  dta_clean,
  "./nis_data/NIS_CRC_LiverDis.parquet",
  format = "parquet",
  partitioning = "YEAR"
)

toc()

