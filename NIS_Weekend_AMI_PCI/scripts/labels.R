baseline_var <-
  c(
    "AMI_type",
    "AGE",
    "FEMALE",
    "RACE",
    "Thrombolytic_Drugs",
    "PAY1",
    "ZIPINC_QRTL",
    "HOSP_REGION",
    "HOSP_BEDSIZE",
    "HOSP_LOCTEACH",
    "grpci"
  )

baseline_var_labels <- list(
  AMI_type ~ "Acute myocardial infarction",
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  Thrombolytic_Drugs ~ "Thrombolytic drugs given",
  PAY1 ~ "Insurance payer",
  ZIPINC_QRTL ~ "Income quartile",
  HOSP_REGION ~ "Hospital region",
  HOSP_BEDSIZE ~ "Hospital bedsize",
  HOSP_LOCTEACH ~ "Hospital location/teaching status",
  grpci ~ "No. of comorbidities"
)

outcome_var <- c(
  "DIED",
  "LOS",
  "adj_TOTCHG",
  "Mechanical_Ventilation",
  "Favorable_Discharge",
  "CS",
  "Cardaic_arrest",
  "AcuteKidneyInjury",
  "GIHemorrhage",
  "Stroke",
  "Pulm_catheter"
)

outcome_var_labels <- list(
  DIED ~ "In-hospital mortality",
  LOS ~ "Length of stay (days)",
  adj_TOTCHG ~ "Inflation-adjusted total charge ($)",
  Mechanical_Ventilation ~ "Mechanical ventilation use",
  Favorable_Discharge ~ "Favorable discharge",
  CS ~ "Cardiogenic Shock",
  Cardaic_arrest ~ "Cardaic arrest",
  AcuteKidneyInjury ~ "Acute kidney injury",
  GIHemorrhage ~ "GI hemorrhage",
  Stroke ~ "Stroke",
  Pulm_catheter ~ "Pulmonary artery catheterization"
)

reg_var_labels <- list(
  AWEEKEND ~ "Weekend admission",
  AMI_type ~ "Acute myocardial infarction",
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  Thrombolytic_Drugs ~ "Thrombolytic drugs given",
  PAY1_alt ~ "Insurance payer",
  ZIPINC_QRTL ~ "Income quartile",
  HOSP_REGION ~ "Hospital region",
  HOSP_BEDSIZE ~ "Hospital bedsize",
  HOSP_LOCTEACH ~ "Hospital location/teaching status",
  elixsum ~ "Elixhauser comorbidity index"
)
