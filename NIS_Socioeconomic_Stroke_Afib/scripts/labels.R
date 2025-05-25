baseline_var <-
  c(
    "AGE",
    "FEMALE",
    "RACE",
    "charlindex",
    "Hosp_Census_Region",
    "HOSP_BEDSIZE",
    "HOSP_LOCTEACH",
    "HTN",
    "DM",
    "CHF",
    "Hyperlip",
    "CAD",
    "PVD",
    "CKD",
    "Cancer",
    "COPD",
    "Dementia",
    "Liver_Dis",
    "Obesity",
    "pStroke",
    "pCardiacSurg",
    "Pacemaker_defib",
    "Fluid_disorders"
    )

baseline_var_labels <- list(
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  charlindex ~ "Charlson comorbidity index",
  Hosp_Census_Region ~ "Hospital region",
  HOSP_BEDSIZE ~ "Hospital bedsize",
  HOSP_LOCTEACH ~ "Hospital location and teaching status",
  HTN ~ "Hypertension",
  DM ~ "Diabetes mellitus",
  Hyperlip ~ "Hyperlipidemia",
  CAD ~ "Coronary artery disease",
  PVD ~ "Peripheral vascular disease",
  CHF ~ "Heart failure",
  Cancer ~ "Cancer",
  CKD ~ "Chronic kidney disease",
  COPD ~ "Chronic obstructive pulmonary disease",
  Dementia ~ "Dementia",
  Liver_Dis ~ "Liver disease",
  Obesity ~ "Obesity",
  pStroke ~ "Prior stroke",
  pCardiacSurg ~ "Prior cardiac surgery",
  Pacemaker_defib ~ "Pacemaker/ICD",
  Fluid_disorders ~ "Fluid disorders"
)

outcome_var <- c("DIED", "LOS", "adj_TOTCHG", "Endovascular_Thrombectomy", "ICH", "Sepsis")

outcome_var_labels <- list(
  DIED ~ "Died during hospitalization",
  LOS ~ "Length of stay (days)",
  adj_TOTCHG ~ "Inflation-adjusted total charge ($)",
  Endovascular_Thrombectomy ~ "Utilization of endovascular thrombectomy",
  ICH ~ "Intracranial hemorrhage",
  Sepsis ~ "Sepsis"
)

reg_var_labels <- list(
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  charlindex ~ "Charlson comorbidity index",
  ZIPINC_QRTL ~ "Residential income",
  Insurance ~ "Expected primary payer",
  Hosp_Census_Region ~ "Hospital region",
  HOSP_BEDSIZE ~ "Hospital bedsize",
  HOSP_LOCTEACH ~ "Hospital location and teaching status",
  HTN ~ "Hypertension",
  DM ~ "Diabetes mellitus",
  Hyperlip ~ "Hyperlipidemia",
  Cancer ~ "Cancer",
  CAD ~ "Coronary artery disease",
  PVD ~ "Peripheral vascular disease",
  CHF ~ "Heart failure",
  CKD ~ "Chronic kidney disease",
  COPD ~ "Chronic obstructive pulmonary disease",
  Dementia ~ "Dementia",
  Liver_Dis ~ "Liver disease",
  Obesity ~ "Obesity",
  pStroke ~ "Prior stroke",
  pCardiacSurg ~ "Prior cardiac surgery",
  Pacemaker_defib ~ "Pacemaker/ICD"
)
