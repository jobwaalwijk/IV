############################################################
# Creation of instrumental variable:
# Differential distance
#
# Description:
# Calculates the distance from injury location to the nearest
# level I trauma center and nearest lower-level trauma center.
#
# Instrument:
# Differential distance =
# distance to nearest level I trauma center -
# distance to nearest level II/III trauma center
#
############################################################


##############################
# 0. Setup
##############################

rm(list = ls())

source("R/00_packages.R")
source("R/config.R")



##############################
# 1. Load imputed dataset
##############################

data <- readRDS(
  file.path(
    paths$output,
    "multiple_imputed_dataset.rds"
  )
)



##############################
# 2. Load hospital coordinates
##############################

hospitals <- read.csv(
  file.path(
    paths$data,
    "hospitals.csv"
  )
)


# Required variables:
# ID
# Latitude
# Longitude
# Level


hospitals <- hospitals %>%
  select(
    ID,
    Latitude,
    Longitude,
    Level
  )



##############################
# 3. Link injury location
#    to coordinates
##############################

postcode_map <- read.csv(
  file.path(
    paths$data,
    "postcode_coordinates.csv"
  )
)


# Required variables:
# Patient_Postcode
# Patient_Latitude
# Patient_Longitude


data <- data %>%
  left_join(
    postcode_map,
    by = "Patient_Postcode"
  )



##############################
# 4. Function calculating
#    nearest hospital distance
##############################

nearest_distance <- function(
    latitude,
    longitude,
    hospital_data
){

  distances <- geosphere::distHaversine(
    cbind(
      hospital_data$Longitude,
      hospital_data$Latitude
    ),
    c(
      longitude,
      latitude
    )
  )

  min(distances)

}



##############################
# 5. Unique injury locations
##############################

unique_locations <- data %>%
  filter(
    !is.na(Patient_Latitude),
    !is.na(Patient_Longitude)
  ) %>%
  distinct(
    Patient_Postcode,
    Patient_Latitude,
    Patient_Longitude
  )



##############################
# 6. Calculate distances
##############################

unique_locations <- unique_locations %>%
  rowwise() %>%
  mutate(

    # nearest level I center

    distance_level_I =
      nearest_distance(
        Patient_Latitude,
        Patient_Longitude,
        hospitals %>%
          filter(Level == 1)
      ),


    # nearest level II/III center

    distance_level_II_III =
      nearest_distance(
        Patient_Latitude,
        Patient_Longitude,
        hospitals %>%
          filter(Level %in% c(2,3))
      )

  ) %>%
  ungroup()



##############################
# 7. Create instrumental
#    variable
##############################

unique_locations <- unique_locations %>%
  mutate(

    DIFFERENTIAL_DISTANCE =
      distance_level_I -
      distance_level_II_III

  )



##############################
# 8. Merge instrument
#    back to patient data
##############################

data_IV <- data %>%
  left_join(
    unique_locations %>%
      select(
        Patient_Postcode,
        distance_level_I,
        distance_level_II_III,
        DIFFERENTIAL_DISTANCE
      ),
    by = "Patient_Postcode"
  )



##############################
# 9. Save
##############################

saveRDS(
  data_IV,
  file.path(
    paths$output,
    "dataset_with_instrument.rds"
  )
)



############################################################
# End script
############################################################
