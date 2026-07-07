############################################################
# 03_geospatial_processing.R
#
# Creation of differential distance instrumental variable
#
# Input:
#   data/processed/imputed_dataset.rds
#   postcode_coordinates.csv
#   hospitals.csv
#
# Output:
#   data/processed/analysis_dataset_iv.rds
#
############################################################


############################################################
# Load packages
############################################################

source(here("R", "00_packages.R"))



############################################################
# Load data
############################################################


data <- readRDS(
  here(
    "data",
    "processed",
    "imputed_dataset.rds"
  )
)



############################################################
# Load postcode coordinates
############################################################

postcode <- read_csv(
  here(
    "data",
    "raw",
    "postcode_coordinates.csv"
  )
)


postcode <- postcode %>%
  transmute(
    Patient_Postcode = postcode,
    Patient_Latitude = latitude,
    Patient_Longitude = longitude
  ) %>%
  distinct(
    Patient_Postcode,
    .keep_all = TRUE
  )



############################################################
# Clean postcode variable
############################################################

data <- data %>%
  mutate(

    GPSDATA = str_replace_all(
      GPSDATA,
      "[^[:alnum:]]",
      ""
    ),

    Patient_Postcode =
      str_extract(
        GPSDATA,
        "^[0-9]{4}[A-Z]{2}$"
      )

  )



############################################################
# Add coordinates
############################################################

data <- data %>%
  left_join(
    postcode,
    by = "Patient_Postcode"
  )



############################################################
# Load hospital data
############################################################


hospitals <- read_csv(
  here(
    "data",
    "raw",
    "hospitals.csv"
  )
)



hospitals <- hospitals %>%
  transmute(

    ID,

    Hospital,

    Level,

    Latitude,

    Longitude

  ) %>%
  distinct(ID, .keep_all = TRUE)



############################################################
# Function: nearest hospital
############################################################


nearest_hospital_distance <- function(
    lat,
    lon,
    hospital_data
){

  if(
    is.na(lat) |
    is.na(lon)
  ){
    return(NA)
  }


  distances <- distHaversine(

    matrix(
      c(
        hospital_data$Longitude,
        hospital_data$Latitude
      ),
      ncol = 2
    ),

    c(
      lon,
      lat
    )

  )


  min(distances)

}



############################################################
# Calculate distance to nearest Level I
############################################################


level1 <- hospitals %>%
  filter(Level == 1)



level23 <- hospitals %>%
  filter(Level %in% c(2,3))



############################################################
# Unique postcode calculation
############################################################


postcode_unique <- data %>%
  select(
    Patient_Postcode,
    Patient_Latitude,
    Patient_Longitude
  ) %>%
  distinct()



############################################################
# Distance calculation
############################################################


postcode_unique <- postcode_unique %>%
  rowwise() %>%
  mutate(

    DIST_LEVEL1 =
      nearest_hospital_distance(
        Patient_Latitude,
        Patient_Longitude,
        level1
      ),


    DIST_LEVEL23 =
      nearest_hospital_distance(
        Patient_Latitude,
        Patient_Longitude,
        level23
      )

  ) %>%
  ungroup()



############################################################
# Create instrumental variable
############################################################


postcode_unique <- postcode_unique %>%
  mutate(

    DIFFERENTIAL_DISTANCE =
      DIST_LEVEL1 -
      DIST_LEVEL23

  )



############################################################
# Merge back
############################################################


data <- data %>%
  left_join(

    postcode_unique %>%
      select(
        Patient_Postcode,
        DIST_LEVEL1,
        DIST_LEVEL23,
        DIFFERENTIAL_DISTANCE
      ),

    by = "Patient_Postcode"

  )



############################################################
# Save
############################################################


saveRDS(

  data,

  here(
    "data",
    "processed",
    "analysis_dataset_iv.rds"
  )

)


message(
  "Geospatial processing completed."
)
