# Acute ischemic stroke (AIS) is defined within the main file as ICD-10 codes I63.x

# Endovascular thrombectomy captures all procedures (i.e., mechanical thrombectomy)
# performed percutaneously on major central nervous system arteries
# e.g. intracranial, carotid (common and internal), vertebral, basilar, etc.
isEVThrombectomy <- "03C[A-Z]3ZZ"

# PR CCSR:
# 1. Administration of thrombolytics and platelet inhibitors
isThrombolyticDrugs <- "3E03016|3E03017|3E030PZ|3E03316|3E03317|3E033PZ|3E04016|3E04017|3E040PZ|3E04316|3E04317|3E043PZ|3E05016|3E05017|3E050PZ|3E05316|3E05317|3E053PZ|3E06016|3E06017|3E060PZ|3E06316|3E06317|3E063PZ|3E07016|3E07017|3E070PZ|3E07316|3E07317|3E073PZ|3E08016|3E08017|3E080PZ|3E08316|3E08317|3E083PZ"

# PR CCSR:
# 1. Mechanical ventilation
isMechanicalVentilation <- "5A09B5K|5A09C5K|5A09D5K|5A1935Z|5A1945Z|5A1955Z"

# Intracranial hemorrhage (ICH) is classified under the broader ICD-10 categories of "Nontraumatic intracerebral hemorrhage" (I61.x)
# and "Other and unspecified nontraumatic intracranial hemorrhage" (I62.x), specifically excluding 
# "Nontraumatic subarachnoid hemorrhage" (I60.x) to restrict to secondary bleeds
isIntracranialHemorrhage <- "I61.*|I62.*"
