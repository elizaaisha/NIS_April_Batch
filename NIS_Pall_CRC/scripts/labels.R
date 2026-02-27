baseline_var <-
  c(
    "AGE1",
    "FEMALE",
    "RACE",
    "Insurance",
    "ZIPINC_QRTL",
    "Hosp_Census_Region",
    "HOSP_BEDSIZE",
    "HOSP_LOCTEACH",
    "APRDRG_Risk_Mortality",
    "grpci"
  )

baseline_var_labels <- list(
  AGE1 ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  Insurance ~ "Insurance payer",
  ZIPINC_QRTL ~ "Income quartile",
  Hosp_Census_Region ~ "Hospital region",
  HOSP_BEDSIZE ~ "Hospital bedsize",
  HOSP_LOCTEACH ~ "Hospital location/teaching status",
  APRDRG_Risk_Mortality ~ "APR-DRG risk of mortality",
  grpci ~ "No. of comorbidities"
)

reg_var_labels <- list(
  AGE ~ "Age, y",
  FEMALE ~ "Sex",
  RACE ~ "Race",
  Insurance ~ "Insurance payer",
  ZIPINC_QRTL ~ "Income quartile",
  Hosp_Census_Region ~ "Hospital region",
  HOSP_BEDSIZE ~ "Hospital bedsize",
  HOSP_LOCTEACH ~ "Hospital location/teaching status",
  APRDRG_Risk_Mortality ~ "APR-DRG risk of mortality",
  elixsum ~ "Elixhauser comorbidity index"
)
