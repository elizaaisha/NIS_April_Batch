baseline_var <-
  c(
    "AGE",
    "FEMALE",
    "RACE",
    "elixsum",
    "ZIPINC_QRTL",
    "Insurance",
    "Hosp_Census_Region",
    'HTN',
    "Hyperlip",
    "PVD",
    "COPD",
    "CAD",
    "Dementia",
    "Fluid_disorders",
    "Valv_HD",
    "Afib",
    "Obesity",
    "Pacemaker",
    "pMI",
    "pCardiacSurg",
    "Malnutrition",
    "Fluid_disorders",
    "CA"
    )

baseline_var_labels <- list(
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  elixsum ~ "Elixhauser comorbidity index",
  ZIPINC_QRTL ~ "Residential income",
  Insurance ~ "Expected primary payer",
  Hosp_Census_Region ~ "Hospital region",
  HTN ~ "Hypertension",
  Hyperlip ~ "Hyperlipidemia",
  PVD ~ "Peripheral vascular disease",
  COPD ~ "Chronic obstructive pulmonary disease",
  Dementia ~ "Dementia",
  pCardiacSurg ~ "Pior cardiac surgery",
  CAD ~ "Coronary artery disease",
  Valv_HD ~ "Valvular heart disease",
  Afib ~ "Atrial fibrillation",
  Obesity ~ "Obesity",
  Pacemaker ~ "Presence of pacemaker",
  pMI ~ "previous MI",
  Malnutrition ~ "Malnutrition",
  Fluid_disorders ~ "Fluid and electrolyte imbalance",
  CA ~ "Cancer"
  )

outcome_var <- c("DIED", "LOS", "adj_TOTCHG")

outcome_var_labels <- list(
  DIED ~ "Died during hospitalization",
  LOS ~ "Length of stay (days)",
  adj_TOTCHG ~ "Inflation-adjusted total charge ($)"
)

reg_var_labels <- list(
  HF_Categories ~ "Heart failure categories"
  )
