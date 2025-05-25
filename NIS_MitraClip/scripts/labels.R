BMI_order <- c(
  "Normal weight (BMI 20.0-24.9kg/m²)",
  "Underweight (BMI ≤19.9kg/m²)",
  "Overweight (BMI 25.0-29.9kg/m²)",
  "Class I obesity (BMI 30.0-34.9kg/m²)",
  "Class II obesity (BMI 35.0-39.9kg/m²)",
  "Class III obesity (BMI ≥40.0kg/m²)"
)


baseline_var <-
  c(
    "AGE",
    "FEMALE",
    "RACE",
    "ZIPINC_QRTL",
    "Insurance",
    "grpci",
    "Arrhythmia",
    "Anemia",
    "CEVD",
    "CKD",
    "COPD",
    "DM",
    "Dementia",
    "HF",
    "HTN",
    "Hyperlip",
    "MI",
    "PVD",
    "Rheumatoid",
    "Valvular_Disease"
  )

baseline_var_labels <- list(
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  ZIPINC_QRTL ~ "Income quartile",
  Insurance ~ "Insurance payer",
  grpci ~ "No. of comorbidities",
  Arrhythmia ~ "Cardiac arrhythmia",
  Anemia ~ "Anemia",
  CEVD ~ "Cerebrovascular disease",
  CKD ~ "Chronic kidney disease",
  COPD ~ "Chronic obstructive pulmonary disease",
  DM ~ "Diabetes",
  Dementia ~ "Dementia",
  HF ~ "Heart failure",
  HTN ~ "Hypertension",
  Hyperlip ~ "Hyperlipidemia",
  MI ~ "Myocardial infarction",
  PVD ~ "Peripheral vascular disease",
  Rheumatoid ~ "Rheumatoid disease",
  Valvular_Disease ~ "Valvular heart disease"
)

outcome_var <- c(
  "DIED",
  "LOS",
  "adj_TOTCHG",
  "CV_Complications"
)

outcome_var_labels <- list(
  DIED ~ "Died during hospitalization",
  LOS ~ "Length of stay (days)",
  adj_TOTCHG ~ "Inflation-adjusted total charge ($)",
  CV_Complications ~
)

reg_var_labels <- list(
  BMI ~ "BMI category",
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  ZIPINC_QRTL ~ "Income quartile",
  Insurance ~ "Insurance payer",
  charlindex ~ "Charlson comorbidity index",
  Arrhythmia ~ "Cardiac arrhythmia",
  Anemia ~ "Anemia",
  CEVD ~ "Cerebrovascular disease",
  CKD ~ "Chronic kidney disease",
  COPD ~ "Chronic obstructive pulmonary disease",
  DM ~ "Diabetes",
  Dementia ~ "Dementia",
  HF ~ "Heart failure",
  HTN ~ "Hypertension",
  Hyperlip ~ "Hyperlipidemia",
  MI ~ "Myocardial infarction",
  PVD ~ "Peripheral vascular disease",
  Rheumatoid ~ "Rheumatoid disease",
  Valvular_Disease ~ "Valvular heart disease"
)
