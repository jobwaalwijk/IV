# 04_baseline_characteristics.R
#
# Baseline characteristics and descriptive tables

# Load packages
source(here("R", "00_packages.R"))



# Load data
data <- readRDS(
  here(
    "data",
    "processed",
    "analysis_dataset_iv.rds"
  )
)


# Select one imputed dataset for descriptive analyse

# First row (.imp==0) contains original data
# Imputed datasets are used for regression analyses
#

baseline <- data %>%
  filter(.imp == 0)



# Create derived variables

baseline <- baseline %>%
  mutate(

    GPS_COMPLETE =
      ifelse(
        !is.na(DIFFERENTIAL_DISTANCE),
        1,
        0
      ),


    LEVEL_GROUP =
      ifelse(
        LEVEL1 == 1,
        "Level I",
        "Level II/III"
      ),


    ISS16 =
      ifelse(
        auto_ISS >=16,
        1,
        0
      ),


    HEADAIS3 =
      ifelse(
        AIS_1 >=3,
        1,
        0
      ),


    THORAXAIS3 =
      ifelse(
        AIS_4 >=3,
        1,
        0
      ),


    SEVERE_LOWER_EXTREMITY =
      ifelse(
        AIS_8 >=3,
        1,
        0
      )

  )

# Variables for Table 1

vars_table1 <- c(

  "LEEFTIJDSEH",

  "GESLACHTMAN",

  "ASA",

  "RRSYSTOLISCH",

  "ADEMFREQUENTIE",

  "GCS",

  "auto_ISS",

  "ISS16",

  "TRAFFIC",

  "MOTORIZED_VEHICLE_CRASH",

  "FALL",

  "VIOLENCE",

  "COMORB",

  "ECRU",

  "LEVEL1",

  "OVERLEDEN30D"

)



factor_vars <- c(

  "GESLACHTMAN",

  "ISS16",

  "TRAFFIC",

  "MOTORIZED_VEHICLE_CRASH",

  "FALL",

  "VIOLENCE",

  "COMORB",

  "ECRU",

  "LEVEL1",

  "OVERLEDEN30D"

)


# Table 1 overall population


table1 <- CreateTableOne(

  vars = vars_table1,

  factorVars = factor_vars,

  data = baseline

)



table1_output <- print(

  table1,

  smd = TRUE,

  nonnormal = TRUE

)



write.table(

  table1_output,

  here(
    "results",
    "tables",
    "Table1_baseline.csv"
  ),

  sep = ";",

  dec = ","

)


# Missing injury location comparison


gps_comparison <- CreateTableOne(

  vars = vars_table1,

  factorVars = factor_vars,

  strata = "GPS_COMPLETE",

  data = baseline,

  test = FALSE

)



gps_output <- print(

  gps_comparison,

  smd = TRUE,

  nonnormal = TRUE

)



write.table(

  gps_output,

  here(
    "results",
    "tables",
    "Supplementary_Table_GPS_missing.csv"
  ),

  sep = ";",

  dec = ","

)


# Level I vs Level II/III characteristics


level_comparison <- CreateTableOne(

  vars = vars_table1,

  factorVars = factor_vars,

  strata = "LEVEL_GROUP",

  data = baseline,

  test = FALSE

)



level_output <- print(

  level_comparison,

  smd = TRUE,

  nonnormal = TRUE

)



write.table(

  level_output,

  here(
    "results",
    "tables",
    "Table_Level_I_vs_Level_IIIII.csv"
  ),

  sep = ";",

  dec = ","

)


# Save summary objects

saveRDS(

  list(

    table1 = table1_output,

    gps = gps_output,

    level = level_output

  ),

  here(
    "results",
    "tables",
    "baseline_tables.rds"
  )

)
