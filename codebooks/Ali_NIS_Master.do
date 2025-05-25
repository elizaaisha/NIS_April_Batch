* Generating Variables

* Define primary diagnosis for acute heart failure with preserved ejection fraction (HFP)
generate acuteHFP = 0  // Initialize acuteHFP variable to 0
replace acuteHFP = 1 if I10_DX1 == "I5021"  // Set to 1 if primary diagnosis is I5021

* Define chronic and acute heart failure diagnosis (more than 1 diagnosis)
generate chronicacutHF = 0  // Initialize chronicacutHF variable to 0
replace chronicacutHF = 1 if inlist(I10_DX1, "I5021", "I5022")  // Set to 1 if primary diagnosis is either I5021 or I5022

* Define secondary diagnosis for acute heart failure
generate acuteHFS = 0  // Initialize acuteHFS variable to 0
foreach var of varlist I10_DX2-I10_DX40 {  // Loop through secondary diagnosis variables
    replace acuteHFS = 1 if `var' == "I5021"  // Set to 1 if any secondary diagnosis is I5021
}

* Define secondary procedures for Percutaneous Coronary Intervention (PCI)
generate PCI1 = 0  // Initialize PCI1 variable to 0
foreach var of varlist I10_PR1-I10_PR25 {  // Loop through procedure variables
    replace PCI1 = 1 if `var' == "02703ZZ"  // Set to 1 if any procedure is 02703ZZ
}

* Calculate time from admission to a specific procedure (PCI)
generate timetoPCI1 = .  // Initialize timetoPCI1 variable to missing
foreach var of numlist 1/25 {  // Loop through procedure days variables
    replace timetoPCI1 = PRDAY`var' if I10_PR`var' == "02703ZZ"  // Set time to PCI if procedure is 02703ZZ
}

* Calculate time from admission to the first occurrence of PCI
generate timetoPCIF = .  // Initialize timetoPCIF variable to missing
foreach var of numlist 1/25 {  // Loop through procedure days variables
    replace timetoPCIF = PRDAY`var' if missing(timetoPCIF) & (I10_PR`var' == "02703ZZ")  // Set to first occurrence of PCI
}

* Calculate time from admission to the last occurrence of PCI
generate timetoPCIL = .  // Initialize timetoPCIL variable to missing
foreach var of numlist 1/25 {  // Loop through procedure days variables
    replace timetoPCIL = PRDAY`var' if I10_PR`var' == "02703ZZ"  // Set to last occurrence of PCI
}

* Count the number of times a PCI procedure was repeated
generate PCI = 0  // Initialize PCI variable to 0
foreach var of varlist I10_PR1-I10_PR25 {  // Loop through procedure variables
    replace PCI = PCI + 1 if `var' == "02703ZZ"  // Increment count if procedure is 02703ZZ
}

* Define inclusion criteria: patients aged 18 or older, acute HFP, and specific hospital teaching status
generate include = 0  // Initialize include variable to 0
replace include = 1 if AGE >= 18 & acuteHFP == 1 & HOSP_LOCTEACH == 3  // Set to 1 if criteria are met

* Define variables for the fourth quarter (Oct, Nov, Dec)
generate q4 = 0  // Initialize q4 variable to 0
replace q4 = 1 if inlist(AMONTH, 10, 11, 12)  // Set to 1 if month is October, November, or December

* Define age category for adults (greater than or equal to 18)
generate adult = 0  // Initialize adult variable to 0
replace adult = 1 if AGE >= 18  // Set to 1 if age is 18 or older

* Define Length of Stay (LOS) of 24 hours or less
generate LOSof24hr = 0  // Initialize LOSof24hr variable to 0
replace LOSof24hr = 1 if LOS <= 1  // Set to 1 if length of stay is 24 hours or less

* Define non-white race category
generate non_white = 0  // Initialize non_white variable to 0
replace non_white = 1 if RACE != 1  // Set to 1 if race is not white

* Categorize a continuous variable (age) into groups
generate agecat = .  // Initialize agecat variable to missing
replace agecat = 0 if AGE < 18  // Set to 0 if age is less than 18
replace agecat = 1 if AGE >= 18 & AGE < 65  // Set to 1 if age is between 18 and 65
replace agecat = 2 if AGE >= 65  // Set to 2 if age is 65 or older

* Generate a categorical variable for January and February
generate janfeb = .  // Initialize janfeb variable to missing
replace janfeb = 0 if AMONTH == 1  // Set to 0 if month is January
replace janfeb = 1 if AMONTH == 2  // Set to 1 if month is February

* Calculate Charlson Comorbidity Index using ICD-10 codes from variables I10_DX1 to I10_DX40
charlson I10_DX1 - I10_DX40, index(10) assign0 noshow

* Calculate Elixhauser Comorbidity Index using ICD-10 codes from variables I10_DX1 to I10_DX40
elixhauser I10_DX1 - I10_DX40, index(10) noshow

* Analysis

* Set up survey data design
svyset [pweight=TRENDWT], strata(NIS_STRATUM) psu(HOSPID)  // Define survey design with weights, strata, and PSU

* Table 1: Descriptive Statistics

* Calculate totals for females
svy: total FEMALE  // Calculate total number of females

* Calculate totals for females in a subpopulation (acute HFP)
svy, subpop(acuteHFP): total FEMALE  // Calculate total number of females within acuteHFP subpopulation

* Calculate totals for females in a subpopulation (acute HFP), comparing weekends
svy, subpop(acuteHFP): total FEMALE, over(AWEEKEND)  // Calculate total number of females in acuteHFP by weekend status

* Calculate mean age
svy: mean AGE  // Calculate mean age

* Calculate mean age in a subpopulation (acute HFP)
svy, subpop(acuteHFP): mean AGE  // Calculate mean age within acuteHFP subpopulation

* Calculate mean age in a subpopulation (acute HFP), comparing weekends
svy, subpop(acuteHFP): mean AGE, over(AWEEKEND)  // Calculate mean age in acuteHFP by weekend status

* Calculate proportions of females
svy: proportion FEMALE  // Calculate proportion of females

* Calculate proportions of females in a subpopulation (acute HFP)
svy, subpop(acuteHFP): proportion FEMALE  // Calculate proportion of females within acuteHFP subpopulation

* Calculate proportions of females in a subpopulation (acute HFP), comparing elective status
svy, subpop(acuteHFP): proportion FEMALE, over(ELECTIVE)  // Calculate proportion of females in acuteHFP by elective status

* Compare continuous variables (age by gender)
svy, subpop(acuteHFP): mean AGE, over(FEMALE)  // Calculate mean age by gender in acuteHFP
svy, subpop(acuteHFP): regress AGE FEMALE  // Regress age on gender in acuteHFP
test FEMALE  // Test the significance of gender

* Compare categorical variables (race by gender)
svy, subpop(acuteHFP): tabulate RACE FEMALE, col  // Tabulate race by gender in acuteHFP

* Linear and Logistic Regression Analysis

* Linear univariate regression analysis (outcome: LOS, predictor: AGE)
svy, subpop(acuteHFP): regress LOS AGE  // Regress LOS on age in acuteHFP

* Linear univariate regression analysis with categorical predictor (outcome: LOS, predictor: HOSP_BEDSIZE)
svy, subpop(AWEEKEND): regress LOS i.HOSP_BEDSIZE  // Regress LOS on hospital bed size in weekend subpopulation

* Linear multivariate regression analysis
svy, subpop(AWEEKEND): regress LOS AGE FEMALE i.HOSP_BEDSIZE i.RACE i.PAY1  // Multivariate regression with multiple predictors

* Logistic univariate regression analysis (outcome: DIED, predictor: AGE)
svy, subpop(AWEEKEND): logistic DIED AGE  // Logistic regression of DIED on age in weekend subpopulation

* Logistic multivariate regression analysis
svy, subpop(AWEEKEND): logistic DIED AGE FEMALE i.HOSP_BEDSIZE i.RACE i.PAY1  // Logistic regression with multiple predictors

* Combine datasets from multiple years
append using "C:\Users\USER\Desktop\NIS\Dataset\NIS_2018_mega" "C:\Users\USER\Desktop\NIS\Dataset\NIS_2019_mega"  // Append datasets
save "C:\Users\USER\Desktop\NIS\Dataset\18-19.dta", replace  // Save combined dataset

* Trend analysis for logistic regression (outcome: DIED)
svy, subpop(acuteHFP if FEMALE == 0): logistic DIED i.YEAR AGE charlindex  // Logistic regression for males over years
margins YEAR  // Calculate margins for YEAR
svy, subpop(acuteHFP if FEMALE == 0): logistic DIED YEAR AGE charlindex  // Logistic regression for males over years

svy, subpop(acuteHFP if FEMALE == 1): logistic DIED i.YEAR AGE charlindex  // Logistic regression for females over years
margins YEAR  // Calculate margins for YEAR
svy, subpop(acuteHFP if FEMALE == 1): logistic DIED YEAR AGE charlindex  // Logistic regression for females over years

* Assess interaction effect between YEAR and FEMALE
generate interaction1 = YEAR*FEMALE  // Generate interaction term to assess interaction effect between YEAR and FEMALE
svy, subpop(acuteHFP): logistic DIED YEAR AGE charlindex interaction1  // Logistic regression including interaction term to test for interaction effect

* Miscellaneous
elabel remove ynlab

htmlcb, saving(codebook2020.html) title("Codebook: NIS 2020")