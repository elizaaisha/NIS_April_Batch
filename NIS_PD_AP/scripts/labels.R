baseline_var <-
  c(
    "AGE",
    "FEMALE",
    "RACE",
    "elixsum",
    "ZIPINC_QRTL",
    "Insurance",
    "Hosp_Census_Region",
    "HOSP_BEDSIZE",
    "HOSP_LOCTEACH",
    "HTN",
    "DM",
    "Cancer",
    "CHF",
    "Hyperlip",
    "CAD",
    "PVD",
    "CKD",
    "COPD",
    "Cardiac_arrhythmia",
    "Dementia",
    "Liver_Dis",
    "Fluid_disorders"
  )

baseline_var_labels <- list(
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  elixsum ~ "Elixhauser comorbidity index",
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
  Dementia ~ "Dementia",
  CKD ~ "Chronic kidney disease",
  COPD ~ "Chronic obstructive pulmonary disease",
  Liver_Dis ~ "Liver disease",
  Cardiac_arrhythmia ~ "Cardiac arrhythmia",
  Fluid_disorders ~ "Fluid and electrolyte disorders"
)

outcome_var <- c("DIED", "LOS", "adj_TOTCHG", "Sepsis", "ARF", "MVent", "Favorable_Discharge", "Disp_long", "Disp_short", "Pall_Care")

outcome_var_labels <- list(
  DIED ~ "Died during hospitalization",
  LOS ~ "Length of stay (days)",
  adj_TOTCHG ~ "Inflation-adjusted total charge ($)",
  Sepsis ~ "Sepsis",
  ARF ~ "Acute respiratory failure",
  MVent ~ "Mechanical Ventilation",
  Favorable_Discharge ~ "Discharged to home",
  Disp_long ~ "Transferred to short-term facility",
  Disp_short ~ "Discharged to long-term facility",
  Pall_Care ~ "Palliative care utilization"
)

reg_var_labels <- list(
  PD_AP ~ "Categories",
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  elixsum ~ "Elixhauser comorbidity index",
  ZIPINC_QRTL ~ "Residential income",
  Insurance ~ "Expected primary payer",
  Hosp_Census_Region ~ "Hospital region",
  HTN ~ "Hypertension",
  DM ~ "Diabetes mellitus",
  Cancer ~ "Cancer",
  Hyperlip~ "Hyperlipidemia",
  CAD ~ "Coronary artery disease",
  Dementia ~ "Dementia",
  PVD ~ "Peripheral vascular disease",
  CHF ~ "Heart failure",
  CKD ~ "Chronic kidney disease",
  COPD ~ "Chronic obstructive pulmonary disease",
  Liver_Dis ~ "Liver disease",
  Cardiac_arrhythmia ~ "Cardiac arrhythmia",
  Fluid_disorders ~ "Fluid and electrolyte disorders"
)
