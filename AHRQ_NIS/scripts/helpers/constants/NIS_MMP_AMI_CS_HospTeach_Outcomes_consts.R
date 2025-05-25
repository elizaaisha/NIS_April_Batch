# Acute myocardial infarction (MI) is defined within the main file
# as encompassing ICD-10 codes I21.0 to I21.4, excluding I21.9 (unspecified AMI).

# Cardiogenic shock is classified under the broader ICD-10 category of "Shock, not elsewhere classified" (R57).
isCardioShock <- "R570.*"

# Acute kidney injury is classified under the broader ICD-10 category of "Acute kidney failure" (N17).
isAcuteKI <- "N17.*"

# DX CCSR:
# 1. Acute hemorrhagic cerebrovascular disease
# 2. Cerebral infarction
isStroke <- "I60.*|I61.*|I62.*|I63.*|G43601|G43609|G43611|G43619|I97810|I97811|I97820|I97821|R297.*"

# PR CCSR:
# 1. Cardiac assistance with balloon pump
# 2. Cardiac assistance with ventricular assist device
# 3. Extracorporeal membrane oxygenation
isMechanicalCirculatorySupport <- "5A02110|5A02210|5A02115|5A02116|5A0211D|5A02215|5A02216|5A0221D|5A0920Z|5A15223|5A1522F|5A1522G|5A1522H"

# PR CCSR:
# 1. Septicemia
isSepsis <- "A021|A207|A227|A267|A327|A392|A393|A394|A40.*|A41.*|A427|A5486|B007|B377|I76|O0337|O0387|O0487|O0737|O0882|O85|O8604|P360|P3610|P3619|P362|P3630|P3639|P364|P365|P368|P369|R6520|R6521|T8112XA|T8144XA"