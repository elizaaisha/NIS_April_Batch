# Load libraries
library(tidyverse)
library(arrow)
library(survey)
library(gtsummary)
library(car)
library(easystats)

# Load external dependencies
source("./scripts/labels.R")

# Global options
TESTING <- F

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./nis_data/NIS_AMI_IBD_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./nis_data/NIS_AMI_IBD.parquet/") |> collect()
}

# Set factor references
dta <- dta |>
  mutate(
    IBD = fct_relevel(IBD, "AMI without IBD"),
    Insurance = fct_relevel(Insurance, "Private"),
    RACE = fct_relevel(RACE, "White"),
    DIED = fct_relevel(DIED, "No"),
    LOS = as.numeric(LOS),
    adj_TOTCHG = as.numeric(adj_TOTCHG),
    CardioShock = as.factor(CardioShock),
    CardiacArrest = as.factor(CardiacArrest),
    Stroke = as.factor(Stroke),
    PCIProcedure = as.factor(PCIProcedure),
    MCS = as.factor(MCS),
    MVent = as.factor(MVent),
    AKI = as.factor(AKI),
    GIHemorrhage = as.factor(GIHemorrhage),
    CABGProcedure = as.factor(CABGProcedure)
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
dsgn <- subset(dsgn, AMI == "Yes" & AGE >= 18)

# Clean up memory
rm(dta)
gc()


#Create the regression model
reg_model_DIED <- svyglm(
  DIED ~ IBD + AGE + FEMALE + RACE + ZIPINC_QRTL + UlcerativeColitis + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
  obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_DIED, exponentiate = T)

vif(reg_model_DIED)

model_performance(reg_model_DIED)


#Create the regression model
reg_model_CardioShock <- svyglm(
  CardioShock ~ IBD + AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
    obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_CardioShock, exponentiate = T, label = reg_var_labels)

model_performance(reg_model_DIED)

vif()

#Create the regression model
reg_model_CardiacArrest <- svyglm(
  CardiacArrest ~ IBD + AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
    obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_CardiacArrest, exponentiate = T, label = reg_var_labels)

#Create the regression model
reg_model_Stroke <- svyglm(
  Stroke ~ IBD + AGE + FEMALE + RACE + ZIPINC_QRTL + UlcerativeColitis + CrohnsDisease + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
    obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_Stroke, exponentiate = T, label = reg_var_labels)

model_performance(reg_model_Stroke)

#Create the regression model
reg_model_PCIProcedure <- svyglm(
  PCIProcedure ~ IBD + AGE + FEMALE + UlcerativeColitis + CrohnsDisease + RACE + ZIPINC_QRTL + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
    obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_PCIProcedure, exponentiate = T, label = reg_var_labels)

model_performance(reg_model_PCIProcedure)

#Create the regression model
reg_model_CABGProcedure <- svyglm(
  CABGProcedure ~ IBD + AGE + FEMALE + RACE + ZIPINC_QRTL + UlcerativeColitis + CrohnsDisease + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
    obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_CABGProcedure, exponentiate = T, label = reg_var_labels)

model_performance(reg_model_CABGProcedure)

model_performance(reg_model_MACE)

# Create the regression model for Total Charge (Inflation Adjusted)
reg_model_TOTCHG <- svyglm(
  adj_TOTCHG ~ IBD + AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
    obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model_TOTCHG, exponentiate = F, label = reg_var_labels)

#Create the regression model
reg_model_PCI <- svyglm(
  PCIProcedure ~ IBD + AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
    obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_PCI, exponentiate = T, label = reg_var_labels)

#| label: reg_CABG

#Create the regression model
reg_model_CABG <- svyglm(
  CABGProcedure ~ IBD + AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
    obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_CABG, exponentiate = T, label = reg_var_labels)

reg_model_CABG <- svyglm(
  MCS ~ IBD + AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + PL_NCHS + HOSP_REGION + HOSP_BEDSIZE + HOSP_LOCTEACH + elixsum + htn + CKD +
    obesity + dm + CPD + Dementia + LiverDis + PVD + Hyperlipidemia + CHF + Cancer + ObstructiveSleepApnea + ValvHD + Smoking + PriorStroke + PreviousPCI + PreviousCABG,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model_CABG, exponentiate = T, label = reg_var_labels)
