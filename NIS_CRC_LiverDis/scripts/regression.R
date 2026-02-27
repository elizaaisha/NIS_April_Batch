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
    Sepsis = as.factor(Sepsis),
    WoundInfection = as.factor(WoundInfection),
    # Abcess = as.factor(Abcess),
    Pneumonia = as.factor(Pneumonia),
    VTE = as.factor(VTE),
    GIHemorrhage = as.factor(GIHemorrhage),
    # AMI = as.factor(AMI),
    AcuteRespiratoryFailure = as.factor(AcuteRespiratoryFailure),
    # MechanicalVentilation = as.factor(MechanicalVentilation),
    # Stroke = as.factor(Stroke),
    AcuteKidneyInjury = as.factor(AcuteKidneyInjury)
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
dsgn <- subset(dsgn,  ColorectalCancer == "Yes" & Colectomy == "Yes" & AGE >= 18)

# Clean up memory
rm(dta)
gc()

#Create the regression model
reg_model_DIED <- svyglm(
  DIED ~ Cirrhosis + Colectomy_Type + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + ELECTIVE +
    HTN + CKD + DM + CPD + PVD + Hyperlipidemia + CHF + Obesity + Anemia + HIV + Hypothyroidism + WeightLoss + Lymphoma + AlcoholAbuse + DrugAbuse +
    Depression + FluidDisorder + Psychoses + RheumatoidDisease + ValvularDisease + Paralysis + NeurologicalDisorders + Coagulopathy,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_DIED, exponentiate = T, label = reg_var_labels)

model_performance(reg_model_DIED)


#Create the regression model
reg_model_WoundInfection <- svyglm(
  WoundInfection ~ Cirrhosis + Colectomy_Type + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + ELECTIVE +
    HTN + CKD + DM + CPD + PVD + Hyperlipidemia + CHF + Obesity + Anemia + HIV + Hypothyroidism + WeightLoss + Lymphoma + AlcoholAbuse + DrugAbuse +
    Depression + FluidDisorder + Psychoses + RheumatoidDisease + ValvularDisease + Paralysis + NeurologicalDisorders + Coagulopathy,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_WoundInfection, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_Pneumonia <- svyglm(
  Pneumonia ~ Cirrhosis + Colectomy_Type + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + ELECTIVE +
    HTN + CKD + DM + CPD + PVD + Hyperlipidemia + CHF + Obesity + Anemia + HIV + Hypothyroidism + WeightLoss + Lymphoma + AlcoholAbuse + DrugAbuse +
    Depression + FluidDisorder + Psychoses + RheumatoidDisease + ValvularDisease + Paralysis + NeurologicalDisorders + Coagulopathy,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Pneumonia, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_GIHemorrhage <- svyglm(
  GIHemorrhage ~ Cirrhosis + Colectomy_Type + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + ELECTIVE +
    HTN + CKD + DM + CPD + PVD + Hyperlipidemia + CHF + Obesity + Anemia + HIV + Hypothyroidism + WeightLoss + Lymphoma + AlcoholAbuse + DrugAbuse +
    Depression + FluidDisorder + Psychoses + RheumatoidDisease + ValvularDisease + Paralysis + NeurologicalDisorders + Coagulopathy,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_GIHemorrhage, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_AcuteRespiratoryFailure <- svyglm(
  AcuteRespiratoryFailure ~ Cirrhosis + Colectomy_Type + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + ELECTIVE +
    HTN + CKD + DM + CPD + PVD + Hyperlipidemia + CHF + Obesity + Anemia + HIV + Hypothyroidism + WeightLoss + Lymphoma + AlcoholAbuse + DrugAbuse +
    Depression + FluidDisorder + Psychoses + RheumatoidDisease + ValvularDisease + Paralysis + NeurologicalDisorders + Coagulopathy,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_AcuteRespiratoryFailure, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_AcuteKidneyInjury <- svyglm(
  AcuteKidneyInjury ~ Cirrhosis + Colectomy_Type + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + ELECTIVE +
    HTN + CKD + DM + CPD + PVD + Hyperlipidemia + CHF + Obesity + Anemia + HIV + Hypothyroidism + WeightLoss + Lymphoma + AlcoholAbuse + DrugAbuse +
    Depression + FluidDisorder + Psychoses + RheumatoidDisease + ValvularDisease + Paralysis + NeurologicalDisorders + Coagulopathy,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_AcuteKidneyInjury, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_Sepsis <- svyglm(
  Sepsis ~ Cirrhosis + Colectomy_Type + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + ELECTIVE +
    HTN + CKD + DM + CPD + PVD + Hyperlipidemia + CHF + Obesity + Anemia + HIV + Hypothyroidism + WeightLoss + Lymphoma + AlcoholAbuse + DrugAbuse +
    Depression + FluidDisorder + Psychoses + RheumatoidDisease + ValvularDisease + Paralysis + NeurologicalDisorders + Coagulopathy,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Sepsis, exponentiate = T, label = reg_var_labels)

# Create the regression model
reg_model_LOS <- svyglm(
  LOS ~ Cirrhosis + Colectomy_Type + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + ELECTIVE +
    HTN + CKD + DM + CPD + PVD + Hyperlipidemia + CHF + Obesity + Anemia + HIV + Hypothyroidism + WeightLoss + Lymphoma + AlcoholAbuse + DrugAbuse +
    Depression + FluidDisorder + Psychoses + RheumatoidDisease + ValvularDisease + Paralysis + NeurologicalDisorders + Coagulopathy,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_LOS, exponentiate = F, label = reg_var_labels)

# Create the regression model for Total Charge (Inflation Adjusted)
reg_model_TOTCHG <- svyglm(
  adj_TOTCHG ~ Cirrhosis + Colectomy_Type + AGE + FEMALE + RACE + Insurance + ZIPINC_QRTL + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + ELECTIVE +
    HTN + CKD + DM + CPD + PVD + Hyperlipidemia + CHF + Obesity + Anemia + HIV + Hypothyroidism + WeightLoss + Lymphoma + AlcoholAbuse + DrugAbuse +
    Depression + FluidDisorder + Psychoses + RheumatoidDisease + ValvularDisease + Paralysis + NeurologicalDisorders + Coagulopathy,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_TOTCHG, exponentiate = F, label = reg_var_labels)

r2(reg_model_TOTCHG)
