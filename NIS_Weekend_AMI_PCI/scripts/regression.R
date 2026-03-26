# Mortality
#Create the regression model
reg_model <- svyglm(
  DIED ~
    AWEEKEND +
    AGE +
    FEMALE +
    RACE +
    Thrombolytic_Drugs +
    PAY1_alt +
    ZIPINC_QRTL +
    HOSP_REGION +
    HOSP_BEDSIZE +
    HOSP_LOCTEACH +
    elixsum,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model, exponentiate = T, label = reg_var_labels)

# Mechanical_Ventilation
#Create the regression model
reg_model <- svyglm(
  Mechanical_Ventilation ~
    AWEEKEND +
    AGE +
    FEMALE +
    RACE +
    Thrombolytic_Drugs +
    PAY1_alt +
    ZIPINC_QRTL +
    HOSP_REGION +
    HOSP_BEDSIZE +
    HOSP_LOCTEACH +
    elixsum,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model, exponentiate = T, label = reg_var_labels)

# Favorable_Discharge
#Create the regression model
reg_model <- svyglm(
  Favorable_Discharge ~
    AWEEKEND +
    AGE +
    FEMALE +
    RACE +
    Thrombolytic_Drugs +
    PAY1_alt +
    ZIPINC_QRTL +
    HOSP_REGION +
    HOSP_BEDSIZE +
    HOSP_LOCTEACH +
    elixsum,
  design = dsgn,
  family = "quasibinomial"
)

# Print regression table
tbl_regression(reg_model, exponentiate = T, label = reg_var_labels)


# Length of Stay
# Create the regression model
reg_model <- svyglm(
  LOS ~
    AWEEKEND +
    AGE +
    FEMALE +
    RACE +
    Thrombolytic_Drugs +
    PAY1_alt +
    ZIPINC_QRTL +
    HOSP_REGION +
    HOSP_BEDSIZE +
    HOSP_LOCTEACH +
    elixsum,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model, exponentiate = F, label = reg_var_labels)

# Total Charge (Inflation Adjusted)
# Create the regression model
reg_model <- svyglm(
  adj_TOTCHG ~
    AWEEKEND +
    AGE +
    FEMALE +
    RACE +
    Thrombolytic_Drugs +
    PAY1_alt +
    ZIPINC_QRTL +
    HOSP_REGION +
    HOSP_BEDSIZE +
    HOSP_LOCTEACH +
    elixsum,
  design = dsgn,
  family = "gaussian"
)

# Print regression table
tbl_regression(reg_model, exponentiate = F, label = reg_var_labels)
