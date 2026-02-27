baseline_var <- c(
  "AGE",
  "FEMALE",
  "RACE",
  "ZIPINC_QRTL",
  "Insurance",
  "HOSP_REGION",
  "HOSP_BEDSIZE",
  "HOSP_LOCTEACH",
  "elixsum"
)

baseline_var_labels <- list(
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  ZIPINC_QRTL ~ "Residential income",
  Insurance ~ "Expected primary payer",
  HOSP_REGION ~ "Hospital region",
  HOSP_BEDSIZE ~ "Hospital bedsize",
  HOSP_LOCTEACH ~ "Hospital location and teaching status",
  elixsum ~ "Elixhauser comorbidity index"
)

outcome_var <- c(
  "DIED",
  "LOS",
  "adj_TOTCHG",
  "Sepsis",
  "Pneumonia",
  "GIHemorrhage",
  "AcuteRespiratoryFailure",
  "AcuteKidneyInjury"
)

outcome_var_labels <- list(
  DIED ~ "In-hospital mortality",
  LOS ~ "Length of stay (days)",
  adj_TOTCHG ~ "Inflation-adjusted total hospital charges",
  Sepsis ~ "Sepsis",
  Pneumonia ~ "Pneumonia",
  GIHemorrhage ~ "Gastrointestinal Hemorrhage",
  AcuteRespiratoryFailure ~ "Acute respiratory failure",
  AcuteKidneyInjury ~ "Acute kidney injury"
)

reg_var_labels <- list(
  Cirrhosis ~ "Liver cirrhosis",
  Colectomy_Type ~ "Type of Colectomy",
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  ZIPINC_QRTL ~ "Residential income",
  Insurance ~ "Expected primary payer",
  HOSP_REGION ~ "Hospital region",
  HOSP_BEDSIZE ~ "Hospital bedsize",
  HOSP_LOCTEACH ~ "Hospital location and teaching status",
  ELECTIVE ~ "Type of admission",
  HTN ~ "Hypertension",
  CKD ~ "Chronic kidney disease",
  Obesity ~ "Obesity",
  DM ~ "Diabetes Mellitus",
  CPD ~ "Chronic pulmonary disease",
  PVD ~ "Peripheral vascular disease",
  Hyperlipidemia ~ "Hyperlipidemia",
  CHF ~ "Congestive heart failure",
  Anemia ~ "Anemia",
  FluidDisorder ~ "Fluid and electrolyte disorders",
  HIV ~ "HIV/AIDS",
  WeightLoss ~ "Weight loss",
  Lymphoma ~ "Lymphoma",
  AlcoholAbuse ~ "Alcohol abuse",
  DrugAbuse ~ "Drug abuse",
  Depression ~ "Depression",
  Psychoses ~ "Psychoses",
  RheumatoidDisease ~ "Rheumatoid disease",
  ValvularDisease ~ "Valvular disease",
  Paralysis ~ "Paralysis",
  NeurologicalDisorders ~ "Neurological disorders",
  Coagulopathy ~ "Coagulopathy"
)
