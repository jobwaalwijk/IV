############################################################
# 06_iv_analysis.R
#
# Instrumental variable analysis
#
# Instrument:
#   Differential distance
#
# Exposure:
#   Level I trauma center admission
#
# Outcome:
#   30-day mortality
#
# Clustering:
#   Trauma region
#
# Input:
#   data/processed/analysis_dataset_iv.rds
#
# Output:
#   results/models/iv_analysis.rds
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
# Select IV population
############################################################

#
# Severe trauma population
# ISS >=16
#

iv_data <- data %>%

  filter(

    .imp > 0,

    auto_ISS >= 16,

    !is.na(DIFFERENTIAL_DISTANCE),

    !is.na(LEVEL1),

    !is.na(OVERLEDEN30D)

  )



############################################################
# Store results
############################################################


iv_models <- list()

estimates <- list()

ses <- list()



############################################################
# Run IV model in each imputed dataset
############################################################


for(i in 1:30){


  cat(
    "Running imputation:",
    i,
    "\n"
  )


  dat_i <- iv_data %>%

    filter(
      .imp == i
    )



  ##########################################################
  # First stage:
  #
  # Probability of Level I admission
  #
  ##########################################################


  fitX <- glm(

    LEVEL1 ~

      DIFFERENTIAL_DISTANCE +

      LEEFTIJDSEH +

      GESLACHTMAN +

      auto_ISS,


    family = binomial,


    data = dat_i

  )



  ##########################################################
  # Second stage:
  #
  # Mortality model
  #
  ##########################################################


  fitY <- glm(

    OVERLEDEN30D ~

      LEVEL1 +

      LEEFTIJDSEH +

      GESLACHTMAN +

      auto_ISS,


    family = binomial,


    data = dat_i

  )



  ##########################################################
  # IV model
  ##########################################################


  iv_models[[i]] <-

    ivglm(

      estmethod = "ts",


      fitX.LZ = fitX,


      fitY.LX = fitY,


      data = dat_i,


      clusterid = "TRAUMAREGIO"

    )



  estimates[[i]] <-

    iv_models[[i]]$est



  ses[[i]] <-

    sqrt(
      diag(
        iv_models[[i]]$vcov
      )
    )

}



############################################################
# Pool estimates across imputations
############################################################


estimates <- do.call(
  rbind,
  estimates
)


ses <- do.call(
  rbind,
  ses
)



iv_results <-

  mi.meld(

    q = estimates,

    se = ses

  )



############################################################
# Confidence intervals
############################################################


iv_results <- iv_results %>%

  mutate(

    CI_lower =
      q.mi -
      1.96*se.mi,


    CI_upper =
      q.mi +
      1.96*se.mi,


    OR =
      exp(q.mi),


    OR_lower =
      exp(CI_lower),


    OR_upper =
      exp(CI_upper)

  )



############################################################
# First stage instrument strength
############################################################


first_stage <- glm(

  LEVEL1 ~

    DIFFERENTIAL_DISTANCE +

    LEEFTIJDSEH +

    GESLACHTMAN +

    auto_ISS,


  family = binomial,


  data =
    iv_data %>%
    filter(.imp==1)

)



wald_test <- summary(first_stage)$coefficients[

  "DIFFERENTIAL_DISTANCE",

  "z value"

]^2



############################################################
# Save results
############################################################


saveRDS(

  list(

    iv_models = iv_models,

    pooled_results = iv_results,

    first_stage_wald = wald_test


  ),


  here(

    "results",

    "models",

    "iv_analysis.rds"

  )

)



write.csv(

  iv_results,


  here(

    "results",

    "tables",

    "IV_results.csv"

  ),


  row.names = FALSE

)



message(
  "IV analysis completed with clustering by trauma region."
)
