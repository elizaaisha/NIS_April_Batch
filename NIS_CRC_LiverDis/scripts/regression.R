# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)

# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- F

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./nis_data/NIS_CRC_LiverDis_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./nis_data/NIS_CRC_LiverDis.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = fct_relevel(DIED, "No"),
    LOS = as.numeric(LOS),
    adj_TOTCHG = as.numeric(adj_TOTCHG),
    Stroke = as.factor(Stroke),
    CardiacArrest = as.factor(CardiacArrest),
    WoundInfection = as.factor(WoundInfection),
    Abcess = as.factor(Abcess),
    Pneumonia = as.factor(Pneumonia),
    PulmEmbolism = as.factor(PulmEmbolism),
    DVT = as.factor(DVT),
    RespFailure = as.factor(RespFailure),
    Hemorrhage = as.factor(Hemorrhage),
    MechanicalVentilation = as.factor(MechanicalVentilation)
  )

# Set survey options for lonely PSUs
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

# Define survey design
dsgn <- svydesign(
  id = ~ HOSPID,
  strata = ~ NIS_STRATUM,
  weights = ~ TRENDWT,
  data = dta,
  nest = TRUE
)

# Subset data for specific condition
dsgn <- subset(dsgn,  CRC_Resection == "Yes" & ColorectalCancer == "Yes" & AGE >= 18)

# Clean up memory
rm(dta)
gc()

#Create the regression model
reg_model_DIED <- svyglm(
  DIED ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
    Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + Arrhythmia + Smoking + PriorStroke,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_DIED, exponentiate = T)

model_performance(reg_model_DIED)


# #Create the regression model
# reg_model_WoundInfection <- svyglm(
#   WoundInfection ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
#     Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + Arrhythmia + Smoking + PriorStroke,
#   design = dsgn,
#   family = "quasibinomial"
# )
#
# # Print regression table
# tbl_regression(reg_model_WoundInfection, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_CardiacArrest <- svyglm(
  CardiacArrest ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
    Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + Arrhythmia + Smoking + PriorStroke,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_CardiacArrest, exponentiate = T, label = reg_var_labels)

#Create the regression model ??????????????????????????????????????????
reg_model_Stroke <- svyglm(
  Stroke ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
    Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + Arrhythmia + Smoking + PriorStroke,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Stroke, exponentiate = T, label = reg_var_labels)

# #Create the regression model
# reg_model_Abcess <- svyglm(
#   Abcess ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
#     Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + Arrhythmia + Smoking + PriorStroke,
#   design = dsgn,
#   family = "quasibinomial"
# )
#
# # Print regression table
# tbl_regression(reg_model_Abcess, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_Pneumonia <- svyglm(
  Pneumonia ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
    Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + Arrhythmia + Smoking + PriorStroke,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Pneumonia, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_DVT <- svyglm(
  DVT ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
    Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + Arrhythmia + Smoking + PriorStroke,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_DVT, exponentiate = T, label = reg_var_labels)

# #Create the regression model
# reg_model_PulmEmbolism <- svyglm(
#   PulmEmbolism ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
#     Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + Arrhythmia + Smoking + PriorStroke,
#   design = dsgn,
#   family = "quasibinomial"
# )
#
# # Print regression table
# tbl_regression(reg_model_PulmEmbolism, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_RespFailure <- svyglm(
  RespFailure ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
    Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + Arrhythmia + Smoking + PriorStroke,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_RespFailure, exponentiate = T, label = reg_var_labels)

# #Create the regression model
# reg_model_Hemorrhage <- svyglm(
#   Hemorrhage ~ LiverDis + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + charlindex + HTN + CKD +
#     Obesity + DM + CPD + PVD + Hyperlipidemia + CHF + Cancer + Arrhythmia + Smoking + PriorStroke,
#   design = dsgn,
#   family = "quasibinomial"
# )
#
# # Print regression table
# tbl_regression(reg_model_Hemorrhage, exponentiate = T, label = reg_var_labels)

