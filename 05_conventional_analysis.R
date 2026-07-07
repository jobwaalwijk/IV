############################################################
# 05_conventional_analysis.R
#
# Conventional logistic regression analysis
#
# Exposure:
#   Level I trauma center admission
#
# Outcome:
#   30-day mortality
#
# Input:
#   data/processed/analysis_dataset_iv.rds
#
# Output:
#   results/models/conventional_analysis.rds
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
    "analysis_dataset_iv.rds"
  )
)



############################################################
# Select complete cases for conventional analysis
############################################################

analysis <- data %>%

  filter(

    .imp == 0,

    !is.na(LEVEL1),

    !is.na(OVERLEDEN30D)

  )



############################################################
# Define analysis variables
############################################################


analysis <- analysis %>%

  mutate(

    MECHANISM =
      factor(
        INTENTIE
      ),

    SEX =
      factor(
        GESLACHTMAN
      )

  )



############################################################
# Unadjusted model
############################################################


model_unadjusted <- glm(

  OVERLEDEN30D ~ LEVEL1,

  family = binomial,

  data = analysis

)



############################################################
# Adjusted model
############################################################


model_adjusted <- glm(

  OVERLEDEN30D ~

    LEVEL1 +

    LEEFTIJDSEH +

    SEX +

    ASA +

    RRSYSTOLISCH +

    ADEMFREQUENTIE +

    GCS +

    auto_ISS +

    MECHANISM,


  family = binomial,


  data = analysis

)



############################################################
# Extract odds ratios
############################################################


extract_OR <- function(model){

  est <- summary(model)$coefficients


  output <- data.frame(

    Variable = rownames(est),

    OR = exp(est[,1]),

    CI_lower =
      exp(est[,1] -
            1.96*est[,2]),

    CI_upper =
      exp(est[,1] +
            1.96*est[,2]),

    p =
      est[,4]

  )


  return(output)

}



OR_unadjusted <-
  extract_OR(model_unadjusted)



OR_adjusted <-
  extract_OR(model_adjusted)



############################################################
# Save results
############################################################


write.csv(

  OR_unadjusted,

  here(
    "results",
    "tables",
    "conventional_unadjusted_OR.csv"
  ),

  row.names = FALSE

)



write.csv(

  OR_adjusted,

  here(
    "results",
    "tables",
    "conventional_adjusted_OR.csv"
  ),

  row.names = FALSE

)



saveRDS(

  list(

    unadjusted = model_unadjusted,

    adjusted = model_adjusted,

    OR_unadjusted = OR_unadjusted,

    OR_adjusted = OR_adjusted

  ),

  here(
    "results",
    "models",
    "conventional_analysis.rds"
  )

)



message(
  "Conventional logistic regression completed."
)
