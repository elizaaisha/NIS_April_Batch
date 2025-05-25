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
    IE = case_when(
      str_detect(DX10_Combined, "I330|B376") ~ "Yes",
      .default = "No"
    ),
    HIV = case_when(ynch17 == "Yes" ~ "Yes",
      .default = "No"
    ),
    IE_HIV = case_when(
      HIV == "Yes" & IE == "Yes" ~ "Infective Endocarditis with HIV",
      HIV == "No" & IE == "Yes" ~ "Infective Endocarditis without HIV"
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
    pStroke = case_when(str_detect(DX10_Combined, "Z8673") | str_detect(DX10_Combined, "I693\\d*") ~ "Yes", .default = "No"),
    pCardiacSurg = case_when(str_detect(DX10_Combined, "Z95[1-4]") ~ "Yes", .default = "No"),
    Pacemaker_defib = case_when(str_detect(DX10_Combined, "Z950") | str_detect(DX10_Combined, "Z95810") ~ "Yes", TRUE ~ "No"),
    Tobb = case_when(str_detect(DX10_Combined, "Z720\\d*") ~ "Yes", .default = "No"),
    Sepsis = case_when(str_detect(DX10_Combined, "A021|A207|A227|A267|A327|A392|A393|A394|A40\\d*|A41\\d*|A427|A5486|B007|B377|I76|O0337|O0387|O0487|O0737|O0887|O85|O8604|P36\\d*|R6520|R6521|T8112XA|T8144XA") ~ "Yes", .default = "No"),
    ARF = case_when(str_detect(DX10_Combined, "N17\\d*") ~ "Yes", .default = "No"),
    AIHD = case_when(str_detect(DX10_Combined, "I2[0-4]\\d*") ~ "Yes", .default = "No"),
    ARF = case_when(str_detect(DX10_Combined, "N17\\d*") ~ "Yes", .default = "No"),
    AIS = case_when(str_detect(DX10_Combined, "I63\\d*") ~ "Yes", .default = "No"),
    AHF = case_when(str_detect(DX10_Combined, "I090|I119|I130|I135|I255|I40//d*|I41//d*|I50//d*") ~ "Yes", .default = "No"),
    # valve_type = case_when(str_detect(str_detect(DX10_Combined, "I34\\d*|105C0[0-9]\\d*|C1[0-4]\\d*|C69\\d*") ~ "Mitral",
    #                                   str_detect(DX10_Combined, "C1[5-9]\\d*|C2[0-6]\\d*") ~ "Aortic",
    #                                   str_detect(DX10_Combined, "C3[0,1,2,3,4,9]\\d*") ~ "Tricuspid",
    #                                   str_detect(DX10_Combined, "C4[0-1]\\d*") ~ "Pulmonic",
    #                                   str_detect(DX10_Combined, "C4[5-9]\\d*") ~ "Unspecified") ~ "Yes",
    #                        .default = "No")
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
    Sepsis,
    ARF,
    AIHD,
    ARF,
    AIS,
    AHF,
    Sepsis,
    DIED,
    LOS,
    IE_HIV,
    HIV,
    IE,
    TOTCHG,
    adj_TOTCHG
  )

# Convert cleaned data to arrow dataset
dta_clean <- dta_clean |> to_arrow()

# Write cleaned data to parquet dataset
arrow::write_dataset(
  dta_clean,
  "./nis_data/NIS_HIV_IE.parquet",
  format = "parquet",
  partitioning = "YEAR"
)

toc()

