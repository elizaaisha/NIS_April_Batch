baseline_var <-
  c(
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
  "Favorable_Discharge"
)

outcome_var_labels <- list(
  DIED ~ "In-hospital mortality",
  LOS ~ "Length of stay (days)",
  adj_TOTCHG ~ "Inflation-adjusted total charge ($)",
  Mechanical_Ventilation ~ "Mechanical ventilation use",
  Favorable_Discharge ~ "Favorable discharge"
)

reg_var_labels <- list(
  AWEEKEND ~ "Weekend admission",
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
