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


# Select complete cases for conventional analysis

analysis <- data %>%

  filter(

    .imp == 0,

    !is.na(LEVEL1),

    !is.na(OVERLEDEN30D)

  )


# Check required variables

required_vars <- c(

  "LEVEL1",
  "OVERLEDEN30D",
  "LEEFTIJDSEH",
  "GESLACHTMAN",
  "ASA",
  "RRSYSTOLISCH",
  "ADEMFREQUENTIE",
  "GCS",
  "auto_ISS",
  "INTENTIE"

)


missing_vars <- setdiff(
  required_vars,
  names(analysis)
)


if(length(missing_vars) > 0){

  stop(
    paste(
      "Missing variables:",
      paste(
        missing_vars,
        collapse = ", "
      )
    )
  )

}


# Create analysis variables

analysis <- analysis %>%

  mutate(

    SEX =
      factor(
        GESLACHTMAN
      ),


    ASA =
      factor(
        ASA
      ),


    MECHANISM =
      factor(
        INTENTIE
      )

  )


# Define rms distribution object

dd <- datadist(
  analysis
)

options(
  datadist = "dd"
)


# Descriptive information

message(
  "Complete case population: ",
  nrow(analysis)
)


message(
  "Number of deaths: ",
  sum(
    analysis$OVERLEDEN30D
  )
)


# Unadjusted logistic regression

model_unadjusted <- glm(

  OVERLEDEN30D ~

    LEVEL1,


  family = binomial,


  data = analysis

)



# Adjusted logistic regression

model_adjusted <- glm(

  OVERLEDEN30D ~

    LEVEL1 +

    rcs(
      LEEFTIJDSEH,
      4
    ) +

    SEX +

    ASA +

    RRSYSTOLISCH +

    ADEMFREQUENTIE +

    GCS +

    rcs(
      auto_ISS,
      4
    ) +

    MECHANISM,


  family = binomial,


  data = analysis

)


# Extract odds ratios and confidence intervals

extract_OR <- function(model){

  est <- summary(model)$coefficients


  data.frame(

    Variable =
      rownames(est),


    OR =
      exp(est[,1]),


    CI_lower =
      exp(
        est[,1] -
          1.96 * est[,2]
      ),


    CI_upper =
      exp(
        est[,1] +
          1.96 * est[,2]
      ),


    p =
      est[,4],


    row.names = NULL

  )

}



OR_unadjusted <-

  extract_OR(
    model_unadjusted
  )


OR_adjusted <-

  extract_OR(
    model_adjusted
  )


# Save tables

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


# Save models


saveRDS(

  list(

    analysis_population = analysis,

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
