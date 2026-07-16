
# 02_geospatial_processing.R
#
# Creation of differential distance instrumental variable
#

# Load packages

source(here("R", "00_packages.R"))

# Load patient data

data <- readRDS(
  here(
    "data",
    "processed",
    "imputed_dataset.rds"
  )
)


# Load postcode coordinates

postcode <- read_csv(
  here(
    "data",
    "raw",
    "postcode_coordinates.csv"
  ),
  show_col_types = FALSE
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



# Extract patient postcode from GPS data

data <- data %>%
  mutate(

    Patient_Postcode =
      GPSDATA %>%
      str_extract(
        "[0-9]{4}\\s?[A-Z]{2}"
      ) %>%
      str_replace_all(
        " ",
        ""
      )

  )


# Add postcode coordinates
data <- data %>%
  left_join(
    postcode,
    by = "Patient_Postcode"
  )


# Load hospital data

hospitals <- read_csv(
  here(
    "data",
    "raw",
    "hospitals.csv"
  ),
  show_col_types = FALSE
)


hospitals <- hospitals %>%
  transmute(

    Hospital_ID = ID,

    Hospital,

    Level,

    Latitude,

    Longitude

  ) %>%
  distinct(
    Hospital_ID,
    .keep_all = TRUE
  )


# Split trauma centers by level

level1 <- hospitals %>%
  filter(
    Level == 1
  )


level23 <- hospitals %>%
  filter(
    Level %in% c(2,3)
  )


# Function: calculate distance to nearest hospital

nearest_hospital_distance <- function(
    lat,
    lon,
    hospital_data
){

  if(
    is.na(lat) |
    is.na(lon)
  ){
    return(NA_real_)
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


# Calculate distances by unique postcode

postcode_unique <- data %>%
  select(
    Patient_Postcode,
    Patient_Latitude,
    Patient_Longitude
  ) %>%
  distinct()



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



# Create instrumental variable
#
# Positive values indicate that Level I is farther away
# Negative values indicate that Level I is closer
#

postcode_unique <- postcode_unique %>%
  mutate(

    DIFFERENTIAL_DISTANCE =
      DIST_LEVEL1 -
      DIST_LEVEL23

  )


# Merge IV back to patient dataset

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


# Quality checks
message(
  "Missing postcode coordinates: ",
  round(
    mean(is.na(data$Patient_Latitude)) * 100,
    1
  ),
  "%"
)


print(
  quantile(
    data$DIFFERENTIAL_DISTANCE,
    probs = c(
      0,
      0.25,
      0.50,
      0.75,
      1
    ),
    na.rm = TRUE
  )
)


# Save dataset

saveRDS(

  data,

  here(
    "data",
    "processed",
    "analysis_dataset_iv.rds"
  )

)
