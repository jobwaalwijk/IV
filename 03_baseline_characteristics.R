############################################################
# Baseline characteristics
#
# Generates:
# - Table 1
# - Supplementary Table: missing injury location comparison
############################################################


library(tidyverse)
library(tableone)
library(mice)


############################################################
# Load imputed dataset
############################################################

load("data/processed/Baseline_mids.rda")


############################################################
# Complete dataset for descriptive statistics
############################################################

baseline <- complete(
  Baseline_mids,
  action = "long",
  include = FALSE
)


############################################################
# Study cohort
############################################################

baseline <- baseline %>%
  filter(
    auto_ISS >= 16,
    GPSDATA_COMPLETE == 1,
    !is.na(DIFFERENTIAL_DISTANCE)
  )


############################################################
# Derived variables
############################################################

baseline <- baseline %>%
  mutate(

    HEADAIS3 = ifelse(AIS_1 >= 3,1,0),

    THORAXAIS3 = ifelse(AIS_4 >= 3,1,0),

    CRITICAL_INJURY = ifelse(auto_ISS >=25,1,0),

    ELDERLY = ifelse(LEEFTIJDSEH >=65,1,0)

  )



############################################################
# Table 1
# Level I vs level II/III trauma centers
############################################################


vars <- c(
  "LEEFTIJDSEH",
  "GESLACHTMAN",
  "ASA_CLASS",
  "RRSYSTOLISCH",
  "ADEMFREQUENTIE",
  "GCS",
  "auto_ISS",
  "MOTORIZED_VEHICLE_CRASH",
  "FALL_HIGH",
  "VIOLENCE",
  "HEADAIS3",
  "THORAXAIS3",
  "LEVEL1",
  "OVERLEDEN30D"
)


factorVars <- c(
  "GESLACHTMAN",
  "ASA_CLASS",
  "MOTORIZED_VEHICLE_CRASH",
  "FALL_HIGH",
  "VIOLENCE",
  "HEADAIS3",
  "THORAXAIS3",
  "LEVEL1",
  "OVERLEDEN30D"
)


table1 <- CreateTableOne(
  vars = vars,
  factorVars = factorVars,
  strata = "LEVEL1",
  data = baseline,
  test = FALSE
)


table1_output <- print(
  table1,
  smd = TRUE,
  nonnormal = TRUE
)


write.table(
  table1_output,
  "results/tables/Table1_baseline_characteristics.csv",
  sep=";",
  dec=","
)



############################################################
# Supplementary Table:
# patients with vs without injury location
############################################################


location_comparison <- complete(
  Baseline_mids,
  action="long",
  include=FALSE
)


location_comparison <- location_comparison %>%
  mutate(
    LOCATION_AVAILABLE =
      ifelse(
        GPSDATA_COMPLETE==1 &
        !is.na(DIFFERENTIAL_DISTANCE),
        1,
        0
      )
  )


supp_table <- CreateTableOne(
  vars = vars,
  factorVars = factorVars,
  strata = "LOCATION_AVAILABLE",
  data = location_comparison,
  test = FALSE
)


supp_output <- print(
  supp_table,
  smd = TRUE,
  nonnormal = TRUE
)


write.table(
  supp_output,
  "results/tables/Supplementary_Table_location_missing.csv",
  sep=";",
  dec=","
)
