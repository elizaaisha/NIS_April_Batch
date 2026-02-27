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
    Cirrhosis = case_when(
      str_detect(DX10_Combined, isCirrhosis) ~ "With liver cirrhosis",
      .default = "Without liver cirrhosis"
    ),
    ColorectalCancer = case_when(
      str_detect(I10_DX1, isColorectalCancer) ~ "Yes",
      .default = "No"
    ),
    Colectomy = case_when(
      str_detect(PR10_Combined, Colectomy) ~ "Yes",
      .default = "No"
    ),
    Colectomy_Type = case_when(
      str_detect(PR10_Combined, OpenColectomy) ~ "Open colectomy",
      str_detect(PR10_Combined, LapColectomy) ~ "Laparoscopic colectomy",
      str_detect(PR10_Combined, RoboticColectomy) ~ "Robotic colectomy",
      .default = "No"
    ),
    Anemia = case_when(ynel25 == "Yes" | ynel26 == "Yes" ~ "Yes",
                       .default = "No"
    ),
    HIV = case_when(ynel16 == "Yes" ~ "Yes",
                    .default = "No"
    ),
    Hypothyroidism = case_when(
      str_detect(DX10_Combined, isHypothyroidism) ~ "Yes",
      .default = "No"
    ),
    WeightLoss = case_when(ynel23 == "Yes" ~ "Yes",
                    .default = "No"
    ),
    Lymphoma = case_when(ynel17 == "Yes" ~ "Yes",
                           .default = "No"
    ),
    Hypothyroidism = case_when(ynel12 == "Yes" ~ "Yes",
                         .default = "No"
    ),
    AlcoholAbuse = case_when(ynel27 == "Yes" ~ "Yes",
                               .default = "No"
    ),
    DrugAbuse = case_when(ynel28 == "Yes" ~ "Yes",
                                 .default = "No"
    ),
    Depression = case_when(ynel30 == "Yes" ~ "Yes",
                          .default = "No"
    ),
    FluidDisorder = case_when(ynel24 == "Yes" ~ "Yes",
                          .default = "No"
    ),
    Coagulopathy = case_when(ynel21 == "Yes" ~ "Yes",
                              .default = "No"
    ),
    Psychoses = case_when(ynel29 == "Yes" ~ "Yes",
                              .default = "No"
    ),
    RheumatoidDisease = case_when(ynch7 == "Yes" ~ "Yes",
                          .default = "No"
    ),
    ValvularDisease = case_when(ynel3 == "Yes" ~ "Yes",
                                  .default = "No"
    ),
    CKD = case_when(
      str_detect(DX10_Combined, isCKD) ~ "Yes",
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
    Paralysis = case_when(
      ynel7 == "Yes" ~ "Yes",
      .default = "No"
    ),
    NeurologicalDisorders = case_when(
      ynel8 == "Yes" ~ "Yes",
      .default = "No"
    ),
    PVD = case_when(
      ynel5 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Dementia = case_when(ynch5 == "Yes" ~ "Yes", .default = "No"),
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
    Smoker = case_when(
      str_detect(DX10_Combined, isSmoking) ~ "Yes",
      .default = "No"
    ),
    PriorStroke = case_when(
      str_detect(DX10_Combined, isPriorStroke) ~ "Yes",
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
    GIHemorrhage = case_when(
      str_detect(DX10_Combined, isGIHemorrhage) ~ "Yes",
      .default = "No"
    ),
    VTE = case_when(
      str_detect(DX10_Combined, isVTE) ~ "Yes",
      .default = "No"
    ),
    AMI = case_when(
      str_detect(DX10_Combined, isAMI) ~ "Yes",
      .default = "No"
    ),
    AcuteRespiratoryFailure = case_when(
      str_detect(DX10_Combined, isAcuteRespiratoryFailure) ~ "Yes",
      .default = "No"
    ),
    MechanicalVentilation = case_when(
      str_detect(DX10_Combined, isMechanicalVentilation) ~ "Yes",
      .default = "No"
    ),
    Stroke = case_when(
      str_detect(DX10_Combined, isStroke) ~ "Yes",
      .default = "No"
    ),
    AcuteKidneyInjury  = case_when(
      str_detect(DX10_Combined, isAcuteKidneyInjury) ~ "Yes",
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
    Cirrhosis,
    ColorectalCancer,
    Colectomy,
    Colectomy_Type,
    Anemia,
    HIV,
    Hypothyroidism,
    WeightLoss,
    Lymphoma,
    Hypothyroidism,
    AlcoholAbuse,
    DrugAbuse,
    Depression,
    FluidDisorder,
    Psychoses,
    RheumatoidDisease,
    ValvularDisease,
    CKD,
    Paralysis,
    HTN,
    Obesity,
    DM,
    CPD,
    PVD,
    Dementia,
    Hyperlipidemia,
    Coagulopathy,
    CHF,
    NeurologicalDisorders,
    ObstructiveSleepApnea,
    Smoker,
    PriorStroke,
    Sepsis,
    WoundInfection,
    Abcess,
    Pneumonia,
    GIHemorrhage,
    VTE,
    AMI,
    AcuteRespiratoryFailure,
    MechanicalVentilation,
    Stroke,
    AcuteKidneyInjury,
    ELECTIVE,
    grpci,
    elixsum
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

