############################################################
# 01_import_and_clean_data.R
#
# Import and preprocessing of Dutch National Trauma Registry
# data (2015-2019)
#
# Output:
#   data/processed/analysis_population.rds
#
############################################################


############################################################
# Packages
############################################################

source(here("R", "00_packages.R"))


############################################################
# Import DNTR data
############################################################

db <- read_csv(
  here("data", "raw", "DNTR.csv"),
  col_types = cols(
    MMT_ = col_double()
  )
)


############################################################
# Basic preprocessing
############################################################

db <- db %>%
  mutate(

    # Missing values
    OORZAAKCATEGORIEID =
      ifelse(is.na(OORZAAKCATEGORIEID),
             11,
             OORZAAKCATEGORIEID),

    INTENTIE =
      ifelse(is.na(INTENTIE),
             18,
             INTENTIE),

    INTUBATIEPREHOSP =
      ifelse(is.na(INTUBATIEPREHOSP),
             0,
             INTUBATIEPREHOSP),

    LETSELAARDWAARDEID =
      ifelse(is.na(LETSELAARDWAARDEID),
             0,
             LETSELAARDWAARDEID),

    MMT_ =
      ifelse(is.na(MMT_),
             0,
             MMT_),

    INTERVENTIETYPE =
      ifelse(is.na(INTERVENTIETYPE),
             0,
             INTERVENTIETYPE),


    # Outcome timing
    TIJDTOTOVERLIJDEN =
      DATUMOVERLEDEN - DATUMAANKOMST,


    OVERLEDEN24H =
      ifelse(
        TIJDTOTOVERLIJDEN > 0 &
          TIJDTOTOVERLIJDEN < 1440,
        1,
        0
      ),

    OVERLEDEN24H =
      ifelse(
        is.na(OVERLEDEN24H),
        0,
        OVERLEDEN24H
      ),


    # Emergency interventions
    SPOEDINTERVENTIE =
      ifelse(
        INTERVENTIETYPE %in% 1:10,
        1,
        0
      ),


    SEH_ICU =
      ifelse(
        is.na(OVERPLBESTID),
        0,
        ifelse(
          OVERPLBESTID == 4,
          1,
          0
        )
      ),


    ECRU =
      ifelse(
        SEH_ICU == 1 |
          INTUBATIEPREHOSP == 1 |
          SPOEDINTERVENTIE == 1,
        1,
        0
      ),


    JAAR =
      year(IDAABA),


    # Trauma region for clustering/imputation
    REGIO =
      factor(TRAUMAREGIOID)

  )


############################################################
# Study population
############################################################

analysis_population <- db %>%
  filter(
    JAAR %in% 2015:2019,
    VERKEERWAARDEID %in% c(1,3,4,5),
    HERKOMSTWAARDEID == 1
  )


############################################################
# Derived analysis variables
############################################################

analysis_population <- analysis_population %>%
  mutate(

    # Injury severity
    ISS16 =
      ifelse(auto_ISS >= 16,1,0),


    # AIS regions
    AIS_1_3 = ifelse(AIS_1 >=3,1,0),
    AIS_2_3 = ifelse(AIS_2 >=3,1,0),
    AIS_3_3 = ifelse(AIS_3 >=3,1,0),
    AIS_4_3 = ifelse(AIS_4 >=3,1,0),
    AIS_5_3 = ifelse(AIS_5 >=3,1,0),
    AIS_6_3 = ifelse(AIS_6 >=3,1,0),
    AIS_7_3 = ifelse(AIS_7 >=3,1,0),
    AIS_8_3 = ifelse(AIS_8 >=3,1,0),
    AIS_9_3 = ifelse(AIS_9 >=3,1,0),


    # Mechanism of injury
    FALL =
      ifelse(INTENTIE %in% c(10,11),1,0),

    FALL_LOW =
      ifelse(INTENTIE == 10,1,0),

    FALL_HIGH =
      ifelse(INTENTIE == 11,1,0),


    TRAFFIC =
      ifelse(INTENTIE %in% 1:6,1,0),


    TRAFFIC_CAR =
      ifelse(INTENTIE == 1,1,0),

    TRAFFIC_MOTOR =
      ifelse(INTENTIE == 2,1,0),

    TRAFFIC_SCOOTER =
      ifelse(INTENTIE == 3,1,0),

    TRAFFIC_BIKE =
      ifelse(INTENTIE == 4,1,0),


    MOTORIZED_VEHICLE_CRASH =
      ifelse(INTENTIE %in% c(1,2),1,0),


    VIOLENCE =
      ifelse(INTENTIE %in% c(7,8,9),1,0),


    OTHER_MECHANISM =
      ifelse(
        INTENTIE %in% c(12,13,14,15,18),
        1,
        0
      ),


    # Physiology
    SBP90 =
      ifelse(RRSYSTOLISCH <90,1,0),


    GCS =
      as.numeric(EYEOPENINGWAARDEID) +
      as.numeric(MOTORRESPONSEWAARDEID) +
      as.numeric(VERBALRESPONSEWAARDEID),


    GCS14 =
      ifelse(GCS <14,1,0),


    RR1029 =
      ifelse(
        ADEMFREQUENTIE <10 |
          ADEMFREQUENTIE >29,
        1,
        0
      ),


    # Age
    LEEFTIJD65 =
      ifelse(LEEFTIJDSEH >=65,1,0),


    # Missing injury location indicator
    GPSDATA_COMPLETE =
      ifelse(
        !is.na(GPSDATA),
        1,
        0
      ),


    # Outcome
    OVERLEDEN =
      ifelse(
        is.na(OVERLEDEN),
        0,
        OVERLEDEN
      )

  )


############################################################
# Save cleaned dataset
############################################################

saveRDS(
  analysis_population,
  here(
    "data",
    "processed",
    "analysis_population.rds"
  )
)


message(
  "Data cleaning completed. Dataset contains ",
  nrow(analysis_population),
  " patients."
)
