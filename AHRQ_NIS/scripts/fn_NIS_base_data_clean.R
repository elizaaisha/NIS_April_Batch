# Load required libraries
library(arrow) # For handling Parquet files
library(dplyr) # For data manipulation
library(stringr) # For string manipulation
library(duckdb) # For DuckDB
library(dbplyr) # For DuckDB dplyr

# Define the function
fn_NIS_base_data_clean <- function(datasets) {
  # Define the vector columns
  icd_dx_columns <- paste0("I10_DX", 1:40)
  icd_pr_columns <- paste0("I10_PR", 1:25)
  icd_pr_day_columns <- paste0("PRDAY", 2:25)
  elix_dx_columns <- paste0("ynel", 1:31)
  charl_dx_columns <- paste0("ynch", 1:17)

  # Combine datasets and clean data
  dta_clean <- arrow::open_dataset(datasets, unify_schemas = TRUE, format = "parquet") |>
    to_duckdb() |>
    mutate(
      # Combine ICD DX & PR columns into one
      DX10_Combined = str_c(!!!syms(icd_dx_columns), sep = ", "),
      PR10_Combined = str_c(!!!syms(icd_pr_columns), sep = ", "),
      # Admission information
      AWEEKEND = case_when(
        AWEEKEND == 0 ~ "Monday-Friday",
        AWEEKEND == 1 ~ "Saturday-Sunday"
      ),
      AMONTH = case_when(
        AMONTH == 1 ~ "January",
        AMONTH == 2 ~ "February",
        AMONTH == 3 ~ "March",
        AMONTH == 4 ~ "April",
        AMONTH == 5 ~ "May",
        AMONTH == 6 ~ "June",
        AMONTH == 7 ~ "July",
        AMONTH == 8 ~ "August",
        AMONTH == 9 ~ "September",
        AMONTH == 10 ~ "October",
        AMONTH == 11 ~ "November",
        AMONTH == 12 ~ "December"
      ),
      TRAN_IN = case_when(
        TRAN_IN == 0 ~ "No transfer",
        TRAN_IN == 1 ~ "Transferred from acute-care",
        TRAN_IN == 2 ~ "Transferred from other"
      ),
      HCUP_ED = case_when(HCUP_ED == 0 ~ "No", HCUP_ED %in% c(1, 2, 3, 4, 5) ~ "Yes"),
      ELECTIVE = case_when(ELECTIVE == 0 ~ "Non-elective", ELECTIVE == 1 ~ "Elective"),
      # Patient demographic and location information
      AGE = as.numeric(AGE),
      AGE_NEONATE = case_when(
        AGE_NEONATE == 0 ~ "Non-neonatal age",
        AGE_NEONATE == 1 ~ "Neonatal age"
      ),
      FEMALE = case_when(FEMALE == 0 ~ "Male", FEMALE == 1 ~ "Female"),
      RACE = case_when(
        RACE == 1 ~ "White",
        RACE == 2 ~ "Black",
        RACE == 3 ~ "Hispanic",
        RACE == 4 ~ "Asian or Pacific Islander",
        RACE == 5 ~ "Native American",
        RACE == 6 ~ "Other"
      ),
      PL_NCHS = case_when(
        PL_NCHS == 1 ~ "Central metro ≥1 million",
        PL_NCHS == 2 ~ "Fringe metro ≥1 million",
        PL_NCHS == 3 ~ "Metro 250,000-999,999",
        PL_NCHS == 4 ~ "Metro 50,000-249,999",
        PL_NCHS == 5 ~ "Micropolitan",
        PL_NCHS == 6 ~ "Other"
      ),
      ZIPINC_QRTL = case_when(
        ZIPINC_QRTL == 1 ~ "$1 - $51,999",
        ZIPINC_QRTL == 2 ~ "$52,000 - $65,999",
        ZIPINC_QRTL == 3 ~ "$66,000 - $87,999",
        ZIPINC_QRTL == 4 ~ "$88,000 or more"
      ),
      # Payer information
      PAY1 = case_when(
        PAY1 == 1 ~ "Medicare",
        PAY1 == 2 ~ "Medicaid",
        PAY1 == 3 ~ "Private",
        PAY1 == 4 ~ "Self-pay",
        PAY1 == 5 ~ "No charge",
        PAY1 == 6 ~ "Other"
      ),
      # Diagnosis and procedure information
      I10_NDX = as.numeric(I10_NDX),
      I10_NPR = as.numeric(I10_NPR),
      PRDAY1 = as.numeric(PRDAY1),
      across(c(icd_pr_day_columns), as.numeric),
      # Resource use information
      TOTCHG = as.numeric(TOTCHG),
      LOS = as.numeric(LOS),
      # Discharge information
      DQTR = case_when(DQTR == 1 ~ "Q1", DQTR == 2 ~ "Q2", DQTR == 3 ~ "Q3", DQTR == 4 ~ "Q4"),
      YEAR = as.numeric(YEAR),
      DIED = case_when(DIED == 0 ~ "No", DIED == 1 ~ "Yes"),
      # Weights (to calculate national estimates)
      TRENDWT = as.numeric(TRENDWT),
      # Hospital information
      HOSPID = as.numeric(HOSPID),
      HOSP_DIVISION = case_when(
        HOSP_DIVISION == 1 ~ "New England",
        HOSP_DIVISION == 2 ~ "Middle Atlantic",
        HOSP_DIVISION == 3 ~ "East North Central",
        HOSP_DIVISION == 4 ~ "West North Central",
        HOSP_DIVISION == 5 ~ "South Atlantic",
        HOSP_DIVISION == 6 ~ "East South Central",
        HOSP_DIVISION == 7 ~ "West South Central",
        HOSP_DIVISION == 8 ~ "Mountain",
        HOSP_DIVISION == 9 ~ "Pacific"
      ),
      NIS_STRATUM = as.numeric(NIS_STRATUM),
      # NIS Hospital Files Elements
      # Hospital characteristics
      HOSP_BEDSIZE = case_when(
        HOSP_BEDSIZE == 1 ~ "Small",
        HOSP_BEDSIZE == 2 ~ "Medium",
        HOSP_BEDSIZE == 3 ~ "Large"
      ),
      H_CONTRL = case_when(
        H_CONTRL == 1 ~ "Government, non-federal",
        H_CONTRL == 2 ~ "Private, non-profit",
        H_CONTRL == 3 ~ "Private, investor-owned"
      ),
      HOSP_LOCTEACH = case_when(
        HOSP_LOCTEACH == 1 ~ "Rural",
        HOSP_LOCTEACH == 2 ~ "Urban, non-teaching",
        HOSP_LOCTEACH == 3 ~ "Urban, teaching"
      ),
      HOSP_REGION = case_when(
        HOSP_REGION == 1 ~ "Northeast",
        HOSP_REGION == 2 ~ "Midwest",
        HOSP_REGION == 3 ~ "South",
        HOSP_REGION == 4 ~ "West"
      ),
      # NIS Disease Severity Measures Files Elements
      APRDRG_Risk_Mortality = case_when(
        APRDRG_Risk_Mortality == 0 ~ "None specified",
        APRDRG_Risk_Mortality == 1 ~ "Minor",
        APRDRG_Risk_Mortality == 2 ~ "Moderate",
        APRDRG_Risk_Mortality == 3 ~ "Major",
        APRDRG_Risk_Mortality == 4 ~ "Extreme"
      ),
      APRDRG_Severity = case_when(
        APRDRG_Severity == 0 ~ "None specified",
        APRDRG_Severity == 1 ~ "Minor",
        APRDRG_Severity == 2 ~ "Moderate",
        APRDRG_Severity == 3 ~ "Major",
        APRDRG_Severity == 4 ~ "Extreme"
      ),
      # Elixhausen comorbidity index
      across(c(elix_dx_columns), ~ case_when(. == 0 ~ "No", . == 1 ~ "Yes")),
      elixsum = as.numeric(elixsum),
      # Charlson comorbidity index
      across(c(charl_dx_columns), ~ case_when(. == 0 ~ "No", . == 1 ~ "Yes")),
      charlindex = as.numeric(charlindex),
      grpci = case_when(
        grpci == 0 ~ "No comorbidities",
        grpci == 1 ~ "One comorbidity",
        grpci == 2 ~ "Two or more comorbidities"
      )
    )

  return(dta_clean)
}
