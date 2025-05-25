###############################################################################
#  Propensity-score matching for NAFLD/NASH vs controls in the HCUP NIS       #
#  – keeps strata, hospital PSUs, and discharge weights intact               #
###############################################################################

# --------------------------------------------------------------------------- #
# 1.  Libraries                                                               #
# --------------------------------------------------------------------------- #
library(tidyverse)    # dplyr, ggplot2, forcats, etc.
library(arrow)        # fast Parquet import
library(MatchIt)      # propensity-score matching
library(cobalt)       # balance diagnostics
library(survey)       # complex-survey analysis
library(gtsummary)    # publication-ready tables

source("./scripts/labels.R")   # provides baseline_var, outcome_var, *_labels

# --------------------------------------------------------------------------- #
# 2.  Options & helper objects                                                #
# --------------------------------------------------------------------------- #
TESTING <- FALSE                # set TRUE to load the small sample

# Covariates that *must not* have missing values for the PS model
ps_covars <- c("AGE","FEMALE","RACE","ZIPINC_QRTL","Insurance","Hosp_Census_Region","elixsum","HTN",
               "Hyperlip","CAD","PVD","COPD","Dementia","OSA","Alcohol","Valv_HD", "Fluid_disorders","Pacemaker","Obesity",
               "CA","Malnutrition","pCardiacSurg", "NIS_STRATUM","TRENDWT")

# --------------------------------------------------------------------------- #
# 3.  Load data                                                               #
# --------------------------------------------------------------------------- #

#Import data
if (TESTING) {
  dta <- arrow::open_dataset("./data/NIS_HF_IDA_testing.parquet") |> collect()
} else {
  dta <- arrow::open_dataset("./data/NIS_HF_IDA_2.parquet/") |> collect()
}

# --------------------------------------------------------------------------- #
# 4.  Minimal cleaning & treatment flag                                       #
# --------------------------------------------------------------------------- #
dta <- dta %>%
  filter(!is.na(HF_Categories) & !is.na(AGE) & !is.na(FEMALE) & !is.na(RACE) & !is.na(ZIPINC_QRTL) & !is.na(Insurance)
         & !is.na(Hosp_Census_Region) & !is.na(HTN) & !is.na(Hyperlip) & !is.na(CAD)
         & !is.na(PVD) & !is.na(COPD) & !is.na(Dementia) & !is.na(OSA) & !is.na(Alcohol)
         & !is.na(Valv_HD) & !is.na(Dementia) & !is.na(CA) & !is.na(Fluid_disorders) & !is.na(Obesity) & !is.na(Pacemaker)
         & !is.na(pMI) & !is.na(Malnutrition) & !is.na(pCardiacSurg))

nis <- dta %>%
  mutate(
    # binary treatment indicator (factor with two levels)
    treat = factor(
      if_else(HF_Categories == "Heart failure with preserved ejection fraction", 1, 0),
      levels = c(0, 1),
      labels = c("Heart failure with reduced ejection fraction", "Heart failure with preserved ejection fraction")
    ),
    # reference levels & formats
    Insurance = fct_relevel(Insurance, "Private"),
    RACE      = fct_relevel(RACE, "White"),
    DIED      = factor(DIED)
  ) %>% drop_na(any_of(ps_covars))

# --------------------------------------------------------------------------- #
# 5.  Propensity-score matching (nearest‐neighbor, within strata)             #
# --------------------------------------------------------------------------- #
m.out <- matchit(
  treat ~ AGE + FEMALE + RACE + ZIPINC_QRTL + Insurance + Hosp_Census_Region + elixsum + HTN + Hyperlip + CAD + PVD + COPD + Dementia + OSA + Alcohol +
    Valv_HD + Fluid_disorders + Pacemaker + Obesity + CA + Obesity + Malnutrition + pCardiacSurg,
  data       = nis,
  method     = "nearest",
  distance   = "glm",
  exact      = ~ NIS_STRATUM,     # keep matches inside each stratum
  caliper    = 0.20,
  ratio      = 1,
  s.weights  = ~ TRENDWT          # survey weight enters PS model
)

# --------------------------------------------------------------------------- #
# 6.  Extract matched discharges & combine weights                            #
# --------------------------------------------------------------------------- #
matched <- match.data(m.out) %>%
  mutate(final_wt = weights * TRENDWT)  # `weights` is already present in `match.data(m.out)`

# --------------------------------------------------------------------------- #
# 7.  Post-match survey design                                                #
# --------------------------------------------------------------------------- #
options(survey.lonely.psu = "adjust")           # HCUP recommendation
dsgn <- svydesign(
  id      = ~HOSPID,
  strata  = ~NIS_STRATUM,
  weights = ~final_wt,
  data    = matched,
  nest    = TRUE
)

# --------------------------------------------------------------------------- #
# 8.  Covariate balance check                                                 #
# --------------------------------------------------------------------------- #
# print(bal.tab(m.out, un = TRUE, m.threshold = 0.10))

# --------------------------------------------------------------------------- #
# 9.  Outcomes table (optional)                                               #
# --------------------------------------------------------------------------- #
# tbl_outcomes <- tbl_svysummary(
#   dsgn,
#   by        = HF_Categories,
#   include   = outcome_var,
#   label     = outcome_var_labels,
#   statistic = list(all_continuous() ~ "{mean} ({sd})"),
#   missing   = "no"
# ) %>% add_p()
#
# tbl_outcomes

# --------------------------------------------------------------------------- #
# 10. Example survey-weighted regression (all-cause mortality)                #
# --------------------------------------------------------------------------- #
reg_model_died <- svyglm(
  DIED ~ NASH_NAFLD_Categories + AGE + ZIPINC_QRTL + Insurance +
    Hosp_Census_Region + HOSP_BEDSIZE + HOSP_LOCTEACH +
    PVD + Dementia + Alcohol + Valv_HD + Afib +  CA + Obesity,
  design  = dsgn,
  family  = quasibinomial()
)

tbl_regression(reg_model_died, exponentiate = TRUE, label = reg_var_labels)

# --------------------------------------------------------------------------- #
#  --- end of script ---                                                      #
# --------------------------------------------------------------------------- #

