# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)

# Load external dependencies
source("./scripts/labels.R")

dta <- arrow::open_dataset("./nis_data/NIS_MitraClip.parquet/") |> collect()

# Set factor references
dta <- dta |>
  mutate(
    BMI = fct_relevel(BMI, BMI_order),
    DIED = fct_relevel(DIED, "No"),
    CV_Complications = fct_relevel(CV_Complications, "No"),
    LOS = as.numeric(LOS),
    adj_TOTCHG = as.numeric(adj_TOTCHG)
  )

# Set survey options for lonely PSUs
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

# Define survey design
dsgn <- svydesign(
  id = ~HOSPID,
  strata = ~NIS_STRATUM,
  weights = ~TRENDWT,
  data = dta,
  nest = TRUE
)

# Subset data for specific condition
dsgn <- subset(dsgn, A_Fib == "Yes" & !is.na(MitraClip_OMVR))

# Clean up memory
rm(dta)
gc()

# Mortality

#Create the regression model
reg_model_died <- svyglm(
  CV_Complications ~ MitraClip_OMVR + AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + charlindex + Anemia + CEVD + CKD + COPD + DM + Dementia + HF + HTN + Hyperlip + MI + PVD + Rheumatoid + Valvular_Disease,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_died, exponentiate = T)


# Length of Stay

# Create the regression model
reg_model_LOS <- svyglm(
  LOS ~ MitraClip_OMVR + AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + charlindex + Anemia + CEVD + CKD + COPD + DM + Dementia + HF + HTN + Hyperlip + MI + PVD + Rheumatoid + Valvular_Disease,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_LOS, exponentiate = F)


# Total Charge (Inflation Adjusted)

# Create the regression model
reg_model_charge <- svyglm(
  adj_TOTCHG ~ MitraClip_OMVR + AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + charlindex + Anemia + CEVD + CKD + COPD + DM + Dementia + HF + HTN + Hyperlip + MI + PVD + Rheumatoid + Valvular_Disease,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_charge, exponentiate = F)
