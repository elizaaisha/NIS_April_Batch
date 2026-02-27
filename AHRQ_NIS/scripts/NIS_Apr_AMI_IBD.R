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
    AMI = case_when(
      str_detect(I10_DX1, isAMI) ~ "Yes",
      .default = "No"
    ),
    CKD = case_when(
      str_detect(DX10_Combined, isCKD) ~ "Yes",
      .default = "No"
    ),
    IBD = case_when(
      str_detect(DX10_Combined, isIBD) ~ "AMI with IBD",
      .default = "AMI without IBD"
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
    LiverDis = case_when(
      ynel14 == "Yes" ~ "Yes",
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
    OSA = case_when(
      ynel1 == "Yes" ~ "Yes",
      .default = "No"
    ),
    ValvHD = case_when(
      ynel3 == "Yes" ~ "Yes",
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
    PreviousPCI = case_when(
      str_detect(DX10_Combined, isPreviousPCI) ~ "Yes",
      .default = "No"
    ),
    PreviousCABG = case_when(
      str_detect(DX10_Combined, isPreviousCABG) ~ "Yes",
      .default = "No"
    ),
    CABGProcedure = case_when(
      str_detect(PR10_Combined, isCABGProcedure) ~ "Yes",
      .default = "No"
    ),
    PCIProcedure = case_when(
      str_detect(PR10_Combined, isPCIProcedure) ~ "Yes",
      .default = "No"
    ),
    AKI = case_when(
      str_detect(DX10_Combined, isAcuteKidneyInjury) ~ "Yes",
      .default = "No"
    ),
    IschemicStroke = case_when(
      str_detect(DX10_Combined, isIschemicStroke) ~ "Yes",
      .default = "No"
    ),
    SubsequentAMI = case_when(
      str_detect(DX10_Combined, isSubsequentAMI) ~ "Yes",
      .default = "No"
    ),
    CardiacArrest = case_when(
      str_detect(DX10_Combined, isCardiacArrest) ~ "Yes",
      .default = "No"
    ),
    CardioShock = case_when(
      str_detect(DX10_Combined, isCardioShock) ~ "Yes",
      .default = "No"
    ),
    Stroke = case_when(
      str_detect(DX10_Combined, isStroke) ~ "Yes",
      .default = "No"
    ),
    TIA = case_when(
      str_detect(DX10_Combined, isTransientIschemicAttack) ~ "Yes",
      .default = "No"
    ),
    PostMIComplications = case_when(
      str_detect(DX10_Combined, isPostMIComplications) ~ "Yes",
      .default = "No"
    ),
    MACCE = case_when(
      (Stroke == "Yes" | TIA == "Yes" | PostMIComplications == "Yes") ~ "Yes",
      .default = "No"
    ),
    ObstructiveSleepApnea = case_when(
      str_detect(DX10_Combined, isObstructiveSleepApnea) ~ "Yes",
      .default = "No"
    ),
    Cancer = case_when(
      ynch14 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Dementia = case_when(
      ynch5 == "Yes" ~ "Yes",
      .default = "No"
      ),
    CAD = case_when(
      str_detect(DX10_Combined, isCAD) ~ "Yes",
      .default = "No"
    ),
    Anemia = case_when(
      str_detect(DX10_Combined, isAnemia) ~ "Yes",
      .default = "No"
    ),
    Favorable_Discharge = case_when(
      DISPUNIFORM %in% c("Routine discharge to home/self-care", "Home health care") ~ "Yes",
      .default = "No"
    ),
    CoroanryAngio = case_when(
      str_detect(PR10_Combined, isCoronaryAngiography) ~ "Yes",
      .default = "No"
    ),
    MVent = case_when(
      str_detect(PR10_Combined, isMechanicalVentilation) ~ "Yes",
      .default = "No"
    ),
    MCS = case_when(
      str_detect(PR10_Combined, isMechanicalCirculatorySupport) ~ "Yes",
      .default = "No"
    ),
    AKI = case_when(
      str_detect(DX10_Combined, isAcuteKidneyInjury) ~ "Yes",
      .default = "No"
    ),
    Sepsis = case_when(
      str_detect(DX10_Combined, isSepsis) ~ "Yes",
      .default = "No"
    ),
    GIHemorrhage = case_when(
      str_detect(DX10_Combined, isGIHemorrhage) ~ "Yes",
      .default = "No"
    ),
    CrohnsDisease = case_when(
      str_detect(DX10_Combined, isCrohnsDisease) ~ "Yes",
      .default = "No"
    ),
    UlcerativeColitis = case_when(
      str_detect(DX10_Combined, isUlcerativeColitis) ~ "Yes",
      .default = "No"
    ),
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
    elixsum,
    ELECTIVE,
    grpci,
    Residence,
    Insurance,
    PL_NCHS,
    # Study-specific variables
    AMI,
    CKD,
    HTN,
    Obesity,
    DM,
    CPD,
    LiverDis,
    PVD,
    Hyperlipidemia,
    CHF,
    Arrhythmia,
    ValvHD,
    Smoking,
    PriorStroke,
    PreviousPCI,
    PreviousCABG,
    CABGProcedure,
    PCIProcedure,
    AKI,
    IschemicStroke,
    CardioShock,
    CardiacArrest,
    SubsequentAMI,
    Stroke,
    IBD,
    ObstructiveSleepApnea,
    Cancer,
    MACCE,
    PostMIComplications,
    TIA,
    Favorable_Discharge,
    CAD,
    Dementia,
    Anemia,
    CoroanryAngio,
    MVent,
    AKI,
    Sepsis,
    GIHemorrhage,
    MCS,
    TRAN_IN,
    TRAN_OUT,
    CrohnsDisease,
    UlcerativeColitis
  )

# Convert cleaned data to arrow dataset
dta_clean <- dta_clean |> to_arrow()

# Write cleaned data to parquet dataset
arrow::write_dataset(
  dta_clean,
  "./nis_data/NIS_AMI_IBD.parquet",
  format = "parquet",
  partitioning = "YEAR"
)

toc()

