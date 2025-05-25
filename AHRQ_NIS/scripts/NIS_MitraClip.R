# Import libraries
library(arrow) # For handling Parquet files
library(dplyr) # For data manipulation
library(stringr) # For string manipulation
library(duckdb) # For DuckDB
library(dbplyr) # For DuckDB dplyr
library(tictoc) # For timing

# Import datasets
datasets <- list(
  arrow::open_dataset("./nis_data/NIS_2018.parquet/"),
  arrow::open_dataset("./nis_data/NIS_2019.parquet/"),
  arrow::open_dataset("./nis_data/NIS_2020.parquet/")
)

# Import external functions
source("./scripts/fn_NIS_base_data_clean.R")

# Define global variables
# Create variables with CPI values for the relevant years
cpi_2020 <- 258.811
cpi_2019 <- 255.657
cpi_2018 <- 251.107

tic()
# Transform project-specific variables onto base cleaned data
dta_base <- fn_NIS_base_data_clean(datasets)

dta_clean <- dta_base |>
  mutate(
    # Inflation adjust to 2020 dollars
    adj_TOTCHG = case_when(
      YEAR == 2018 ~ round(TOTCHG * (cpi_2020 / cpi_2018)),
      YEAR == 2019 ~ round(TOTCHG * (cpi_2020 / cpi_2019)),
      YEAR == 2020 ~ round(TOTCHG * (cpi_2020 / cpi_2020))
    ),
    Insurance = case_when(
      PAY1 == "Self-pay" ~ "Other",
      PAY1 == "No charge" ~ "Other",
      .default = PAY1
    ),
    Residence = case_when(
      PL_NCHS %in% c("Central metro ≥1 million", "Fringe metro ≥1 million") ~ "Large metro",
      PL_NCHS %in% c("Metro 250,000-999,999", "Metro 50,000-249,999") ~ "Small metro",
      PL_NCHS %in% c("Micropolitan", "Other") ~ "Micropolitan"
    ),
    MitraClip = case_when(
      str_detect(PR10_Combined, "02UG3JZ|02UG3JZ|02UG4JZ") ~ "Yes",
      .default = "No"
    ),
    A_Fib = case_when(afib == 1 ~ "Yes", .default = "No"),
    MitraClip_Afib = case_when(
      MitraClip == "Yes" & A_Fib == "No" ~ "MitraClip without Atrial Fibrillation",
      MitraClip == "Yes" & A_Fib == "Yes" ~ "MitraClip with Atrial Fibrillation",
      .default = NA
    ),
    OMVR = case_when(
      str_detect(PR10_Combined, "02UG0JZ") ~ "Yes",
      .default = "No"
    ),
    CV_Complications = case_when(
      str_detect(DX10_Combined, "I970|I97110|I97120|I97130|I97190|I97418|I9751|I97618|I97638|I97648|I97820|I9788|I9789") ~ "Yes",
      .default = "No"
    ),
    MitraClip_OMVR = case_when(
      MitraClip == "Yes" ~ "MitraClip",
      OMVR == "Yes" ~ "OMVR",
      .default = NA
    ),
    BMI = case_when(
      str_detect(DX10_Combined, "Z681.*") ~ "Underweight (BMI ≤19.9kg/m²)",
      str_detect(DX10_Combined, "Z682[0-4].*") ~ "Normal weight (BMI 20.0-24.9kg/m²)",
      str_detect(DX10_Combined, "Z682[5-9].*") ~ "Overweight (BMI 25.0-29.9kg/m²)",
      str_detect(DX10_Combined, "Z683[0-4].*") ~ "Class I obesity (BMI 30.0-34.9kg/m²)",
      str_detect(DX10_Combined, "Z683[5-9].*") ~ "Class II obesity (BMI 35.0-39.9kg/m²)",
      str_detect(DX10_Combined, "Z684.*") ~ "Class III obesity (BMI ≥40.0kg/m²)"
    ),
    Arrhythmia = case_when(
      ynel2 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Anemia = case_when(
      ynel25 == "Yes" | ynel26 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Cancer = case_when(
      ynch14 == "Yes" | ynch16 == "Yes" ~ "Yes",
      .default = "No"
    ),
    CEVD = case_when(
      ynch4 == "Yes" ~ "Yes",
      .default = "No"
    ),
    CKD = case_when(
      str_detect(DX10_Combined, "N18*") ~ "Yes",
      .default = "No"
    ),
    COPD = case_when(
      ynch6 == "Yes" ~ "Yes",
      .default = "No"
    ),
    DM = case_when(
      ynel10 == "Yes" | ynel11 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Dementia = case_when(
      ynch5 == "Yes" ~ "Yes",
      .default = "No"
    ),
    HF = case_when(
      ynel1 == "Yes" ~ "Yes",
      .default = "No"
    ),
    HTN = case_when(
      ynel6 == "Yes" | ynel31 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Hyperlip = case_when(
      str_detect(DX10_Combined, "E78*") ~ "Yes",
      .default = "No"
    ),
    MI = case_when(
      ynch1 == "Yes" ~ "Yes",
      .default = "No"
    ),
    PVD = case_when(
      ynel5 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Rheumatoid = case_when(
      ynch7 == "Yes" ~ "Yes",
      .default = "No"
    ),
    Valvular_Disease = case_when(
      ynel3 == "Yes" ~ "Yes",
      .default = "No"
    )
    # HTN = case_when(str_detect(DX10_Combined, "H35031|H35032|H35033|I10|I119|I120|I129|I1310|I1311|I150|I151|I152|I158|I159|I160|I161|I169|I1A0|I674") ~ "Yes", .default = "No"),
    # Hyperlip = case_when(str_detect(DX10_Combined, "E78\\d*") ~ "Yes", .default = "No"),
    # CAD = case_when(str_detect(DX10_Combined, "I251\\d*") ~ "Yes", .default = "No"),
    # PVD = case_when(ynel5 == "Yes" ~ "Yes", .default = "No"),
    # COPD = case_when(ynch6 == "Yes" ~ "Yes", .default = "No"),
    # Dementia = case_when(ynch5 == "Yes" ~ "Yes", .default = "No"),
    # OSA = case_when(str_detect(DX10_Combined, "G4733") ~ "Yes", .default = "No"),
    # pMI = case_when(pMI == 1 ~ "Yes", .default = "No"),
    # Anemia = case_when(str_detect(DX10_Combined, "D500|D501|D508|D509|D510|D511|D512|D513|D518|D519|D520|D521|D528|D529|D530|D531|D532|D538|D539|D550|D551|D552|D5521|D5529|D553|D558|D559|D560|D561|D562|D563|D564|D565|D568|D569|D580|D581|D582|D588|D589|D590|D591|D5910|D5911|D5912|D5913|D5919|D592|D593|D5930|D5931|D5932|D5939|D594|D595|D596|D598|D599|D600|D601|D608|D609|D6101|D6102|D6109|D611|D612|D613|D61810|D61811|D61818|D6182|D6189|D619|D630|D631|D638|D640|D641|D642|D643|D644|D6481|D6489|D649|D62") ~ "Yes", .default = "No"),
    # Alcohol = case_when(ynel27 == "Yes" ~ "Yes", .default = "No"),
    # Valv_HD = case_when(str_detect(DX10_Combined, "I340|I341|I342|I348|I3481|I3489|I349|I350|I351|I352|I358|I359|I360|I361|I362|I368|I369|I370|I371|I372|I378|I379") ~ "Yes", .default = "No"),
    # Afib = case_when(afib == 1 ~ "Yes", .default = "No"),
    # CA = case_when(str_detect(DX10_Combined, "C000|C001|C002|C003|C004|C005|C006|C008|C009|C01|C020|C021|C022|C023|C024|C028|C029|C030|C031|C039|C040|C041|C048|C049|C050|C051|C052|C058|C059|C060|C061|C062|C0680|C0689|C069|C07|C080|C081|C089|C090|C091|C098|C099|C100|C101|C102|C103|C104|C108|C109|C110|C111|C112|C113|C118|C119|C12|C130|C131|C132|C138|C139|C140|C142|C148|C153|C154|C155|C158|C159|C160|C161|C162|C163|C164|C165|C166|C168|C169|C170|C171|C172|C173|C178|C179|C180|C181|C182|C183|C184|C185|C186|C187|C188|C189|C19|C20|C210|C211|C212|C218|C220|C221|C222|C223|C224|C227|C228|C229|C23|C240|C241|C248|C249|C250|C251|C252|C253|C254|C257|C258|C259|C260|C261|C269|C300|C301|C310|C311|C312|C313|C318|C319|C320|C321|C322|C323|C328|C329|C33|C3400|C3401|C3402|C3410|C3411|C3412|C342|C3430|C3431|C3432|C3480|C3481|C3482|C3490|C3491|C3492|C37|C380|C381|C382|C383|C384|C388|C390|C399|C4000|C4001|C4002|C4010|C4011|C4012|C4020|C4021|C4022|C4030|C4031|C4032|C4080|C4081|C4082|C4090|C4091|C4092|C410|C411|C412|C413|C414|C419|C430|C4310|C4311|C43111|C43112|C4312|C43121|C43122|C4320|C4321|C4322|C4330|C4331|C4339|C434|C4351|C4352|C4359|C4360|C4361|C4362|C4370|C4371|C4372|C438|C439|C4400|C4401|C4402|C4409|C44101|C44102|C441021|C441022|C44109|C441091|C441092|C44111|C44112|C441121|C441122|C44119|C441191|C441192|C44121|C44122|C441221|C441222|C44129|C441291|C441292|C44131|C441321|C441322|C441391|C441392|C44191|C44192|C441921|C441922|C44199|C441991|C441992|C44201|C44202|C44209|C44211|C44212|C44219|C44221|C44222|C44229|C44291|C44292|C44299|C44300|C44301|C44309|C44310|C44311|C44319|C44320|C44321|C44329|C44390|C44391|C44399|C4440|C4441|C4442|C4449|C44500|C44501|C44509|C44510|C44511|C44519|C44520|C44521|C44529|C44590|C44591|C44599|C44601|C44602|C44609|C44611|C44612|C44619|C44621|C44622|C44629|C44691|C44692|C44699|C44701|C44702|C44709|C44711|C44712|C44719|C44721|C44722|C44729|C44791|C44792|C44799|C4480|C4481|C4482|C4489|C4490|C4491|C4492|C4499|C450|C451|C452|C457|C459|C460|C461|C462|C463|C464|C4650|C4651|C4652|C467|C469|C470|C4710|C4711|C4712|C4720|C4721|C4722|C473|C474|C475|C476|C478|C479|C480|C481|C482|C488|C488|C490|C4910|C4911|C4912|C4920|C4921|C4922|C493|C494|C495|C496|C498|C499|C49A0|C49A1|C49A2|C49A3|C49A4|C49A5|C49A9|C4A0|C4A10|C4A11|C4A111|C4A112|C4A12|C4A121|C4A122|C4A20|C4A21|C4A22|C4A30|C4A31|C4A39|C4A4|C4A51|C4A52|C4A59|C4A60|C4A61|C4A62|C4A70|C4A71|C4A72|C4A8|C4A9|C50011|C50012|C50019|C50021|C50022|C50029|C50111|C50112|C50119|C50121|C50122|C50129|C50211|C50212|C50219|C50221|C50222|C50229|C50311|C50312|C50319|C50321|C50322|C50329|C50411|C50412|C50419|C50421|C50422|C50429|C50511|C50512|C50519|C50521|C50522|C50529|C50611|C50612|C50619|C50621|C50622|C50629|C50811|C50812|C50819|C50821|C50822|C50829|C50911|C50912|C50919|C50921|C50922|C50929|C510|C511|C512|C518|C519|C52|C530|C531|C538|C539|C540|C541|C542|C543|C548|C549|C55|C561|C562|C563|C569|C5700|C5701|C5702|C5710|C5711|C5712|C5720|C5721|C5722|C573|C574|C577|C578|C579|C58|C600|C601|C602|C608|C609|C61|C6200|C6201|C6202|C6210|C6211|C6212|C6290|C6291|C6292|C6300|C6301|C6302|C6310|C6311|C6312|C632|C637|C638|C639|C641|C642|C649|C651|C652|C659|C661|C662|C669|C670|C671|C672|C673|C674|C675|C676|C677|C678|C679|C680|C681|C688|C689|C6900|C6901|C6902|C6910|C6911|C6912|C6920|C6921|C6922|C6930|C6931|C6932") ~ "Yes", .default = "No"),
    # BMI = case_when(
    #   str_detect(DX10_Combined, "Z681\\d*") ~ "Underweight (BMI ≤19.9kg/m²)",
    #   str_detect(DX10_Combined, "Z682[0-4]\\d*") ~ "Normal weight (BMI 20.0-24.9kg/m²)",
    #   str_detect(DX10_Combined, "Z682[5-9]\\d*") ~ "Overweight (BMI 25.0-29.9kg/m²)",
    #   str_detect(DX10_Combined, "Z683[0-4]\\d*") ~ "Class I obesity (BMI 30.0-34.9kg/m²)",
    #   str_detect(DX10_Combined, "Z683[5-9]\\d*") ~ "Class II obesity (BMI 35.0-39.9kg/m²)",
    #   str_detect(DX10_Combined, "Z684\\d*") ~ "Class III obesity (BMI ≥40.0kg/m²)"
    # ),
    # IBD = case_when(str_detect(DX10_Combined, "K51\\d* | K51\\d*") ~ "Yes", .default = "No"),
    # Smoking = case_when(str_detect(DX10_Combined, "F17210 | F17213 | F17218 | F17219 | Z720") ~ "Yes", .default = "No"),
    # Obesity = case_when(ynel22 == "Yes" ~ "Yes", .default = "No"),
    # pStroke = case_when(str_detect(DX10_Combined, "Z8673") | str_detect(DX10_Combined, "I693\\d*") ~ "Yes", .default = "No"),
    # pCardiacSurg = case_when(str_detect(DX10_Combined, "Z95[1-4]") ~ "Yes", .default = "No"),
    # Pacemaker = case_when(str_detect(DX10_Combined, "Z950") | str_detect(DX10_Combined, "Z95810") ~ "Yes", TRUE ~ "No"),
    # Hypothyroidism = case_when(str_detect(DX10_Combined, "E000|E001|E002|E009|E010|E011|E012|E018|E02|E030|E031|E032|E033|E034|E035|E038|E039") ~ "Yes", .default = "No"),
    # Hyperthyroidism = case_when(str_detect(DX10_Combined, "E0500|E0501|E0510|E0511|E0520|E0521|E0530|E0531|E0540|E0541|E0580|E0581|E0590|E0591") ~ "Yes", .default = "No"),
    # Chronic_Pancreatitis = case_when(str_detect(DX10_Combined, "K860|K861") ~ "Yes", .default = "No"),
    # Malnutrition = case_when(str_detect(DX10_Combined, "E40|E41|E42|E43|E440|E441|E45|E46") ~ "Yes", .default = "No"),
    # Fluid_disorders = case_when(str_detect(DX10_Combined, "E860|E861|E869|E870|E871|E872|E8720|E8721|E8722|E8729|E873|E874|E875|E876|E8770|E8771|E8779|E878") ~ "Yes", .default = "No")
  ) |>
  select(
    # Base variables
    YEAR,
    HOSPID,
    NIS_STRATUM,
    TRENDWT,
    LOS,
    DIED,
    TOTCHG,
    adj_TOTCHG,
    # Additional variables
    AGE,
    FEMALE,
    RACE,
    ZIPINC_QRTL,
    ELECTIVE,
    HOSP_REGION,
    HOSP_BEDSIZE,
    HOSP_LOCTEACH,
    charlindex,
    grpci,
    # Study-specific variables
    Insurance,
    Residence,
    MitraClip,
    BMI,
    Arrhythmia,
    Anemia,
    Cancer,
    CEVD,
    CKD,
    COPD,
    DM,
    Dementia,
    HF,
    HTN,
    Hyperlip,
    MI,
    PVD,
    Rheumatoid,
    Valvular_Disease,
    MitraClip_Afib,
    A_Fib,
    MitraClip_OMVR,
    OMVR,
    CV_Complications
  )

# Convert cleaned data to arrow dataset
dta_clean <- dta_clean |> to_arrow()

# Write cleaned data to parquet dataset
arrow::write_dataset(
  dta_clean,
  "./nis_data/NIS_MitraClip.parquet",
  format = "parquet",
  partitioning = "YEAR"
)

toc()
