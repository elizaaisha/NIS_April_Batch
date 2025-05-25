# In the United States, the flu season is considered October through May
isFluSeasonMonth <- c(
  "October",
  "November",
  "December",
  "January",
  "February",
  "March",
  "April",
  "May"
)

# Custom definition trying to best cover acute, infectious respiratory conditions
# that plausibly trigger hospitalizations and interact with HF pathophysiology
# consists of influenza, pneumonia, and acute bronchitis/bronchiolitis
isAcuteRespiratoryIllness <- "J09.*|J10.*|J11.*|J12.*|J13.*|J14.*|J15.*|J16.*|J17.*|J18.*|J20.*|J21.*"

# DX CCSR:
# 1. Heart failure
isHeartFailure <- "I50.*|I0981|I110|I130|I132|I97130|I97131|O29121|O29122|O29123|O29129|Z95811|Z95812"

# Immunization is defined by Z23 as a proxy for recent or updated vaccination status
isImmunization = "Z23.*"

# DX CCSR:
# 1. Septicemia
isSepsis <- "A40.*|A41.*|A021|A207|A227|A267|A327|A392|A393|A394|A427|A5486|B007|B377|I76|O0337|O0387|O0487|O0737|O0882|O85|O8604|P360|P3610|P3619|P362|P3630|P3639|P364|P365|P368|P369|R6520|R6521|T8112XA|T8144XA"

# DX CCSR:
# 1. Influenza
isInfluenza <- "J09.*|J10.*|J11.*"

# PR CCSR:
# 1. Mechanical ventilation
isMechanicalVentilation <- "5A09B5K|5A09C5K|5A09D5K|5A1935Z|5A1945Z|5A1955Z"
