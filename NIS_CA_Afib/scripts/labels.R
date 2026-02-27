baseline_var <-
  c(
    "AGE",
    "FEMALE",
    "RACE",
    "elixsum",
    "ZIPINC_QRTL",
    "Insurance",
    "Hosp_Census_Region",
    "PL_NCHS",
    "HTN",
    "DM",
    "Hyperlip",
    "CAD",
    "PVD",
    "CHF",
    "CKD",
    "COPD",
    "VHD",
    "Liver_Dis",
    "OSA",
    "Anemia",
    "Alcohol",
    "Obesity",
    "pCardiacSurg",
    "Smoker"
    )

baseline_var_labels <- list(
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  elixsum ~ "Elixhauser comorbidity index",
  ZIPINC_QRTL ~ "Residential income",
  Insurance ~ "Expected primary payer",
  Hosp_Census_Region ~ "Hospital region",
  PL_NCHS ~ "Location of patient’s residence",
  HTN ~ "Hypertension",
  DM ~ "Diabetes mellitus",
  Hyperlip~ "Hyperlipidemia",
  CAD ~ "Coronary artery disease",
  PVD ~ "Peripheral vascular disease",
  CHF ~ "Heart failure",
  CKD ~ "Chronic kidney disease",
  COPD ~ "Chronic obstructive pulmonary disease",
  VHD ~ "Valvular heart disease",
  Liver_Dis ~ "Liver disease",
  OSA ~ "Obstructive sleep apnea",
  Anemia ~ "Anemia",
  Alcohol ~ "Alcohol use disorder",
  Obesity ~ "Obesity",
  pCardiacSurg ~ "Prior cardiac surgery",
  Smoker ~ "Smoker"
)

outcome_var <- c("DIED", "AIS", "VTE", "MB", "LOS", "adj_TOTCHG")

outcome_var_labels <- list(
  DIED ~ "Died during hospitalization",
  AIS ~ "Acute ischemic stroke",
  VTE ~ "Venous thromboembolism",
  MB ~ "Major bleeding",
  LOS ~ "Length of stay (days)",
  adj_TOTCHG ~ "Inflation-adjusted total charge ($)"
)

reg_var_labels <- list(
  CA_Afib ~ "Categories",
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  elixsum ~ "Elixhauser comorbidity index",
  ZIPINC_QRTL ~ "Residential income",
  Insurance ~ "Expected primary payer",
  Hosp_Census_Region ~ "Hospital region",
  PL_NCHS ~ "Location of patient’s residence",
  HTN ~ "Hypertension",
  DM ~ "Diabetes mellitus",
  Hyperlip~ "Hyperlipidemia",
  CAD ~ "Coronary artery disease",
  PVD ~ "Peripheral vascular disease",
  CHF ~ "Heart failure",
  CKD ~ "Chronic kidney disease",
  COPD ~ "Chronic obstructive pulmonary disease",
  VHD ~ "Valvular heart disease",
  Liver_Dis ~ "Liver disease",
  OSA ~ "Obstructive sleep apnea",
  Anemia ~ "Anemia",
  Alcohol ~ "Alcohol use disorder",
  Obesity ~ "Obesity",
  pCardiacSurg ~ "Prior cardiac surgery",
  Smoker ~ "Smoker"
)
