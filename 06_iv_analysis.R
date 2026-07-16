############################################################
# 06_iv_analysis.R

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


# Select IV population

iv_data <- data %>%

  filter(

    .imp > 0,

    auto_ISS >= 16,

    !is.na(DIFFERENTIAL_DISTANCE),

    !is.na(LEVEL1),

    !is.na(OVERLEDEN30D),

    !is.na(TRAUMAREGIO)

  ) %>%

  mutate(

    SEX =
      factor(
        GESLACHTMAN
      )

  )


# Storage objects

iv_models <- list()

iv_estimates <- list()

iv_SE <- list()

first_stage_info <- list()



# Run IV analysis in each imputed dataset

for(i in 1:30){


  message(
    "Running imputation ",
    i
  )


  dat_i <- iv_data %>%

    filter(
      .imp == i
    )

  # Restricted cubic spline setup

  dd <- datadist(dat_i)

  options(
    datadist = "dd"
  )


  # First stage

  fitX <- glm(

    LEVEL1 ~

      DIFFERENTIAL_DISTANCE +

      rcs(
        LEEFTIJDSEH,
        4
      ) +

      SEX +

      rcs(
        auto_ISS,
        4
      ),


    family = binomial,


    data = dat_i

  )


# Second stage

  fitY <- glm(

    OVERLEDEN30D ~

      LEVEL1 +

      rcs(
        LEEFTIJDSEH,
        4
      ) +

      SEX +

      rcs(
        auto_ISS,
        4
      ),


    family = binomial,


    data = dat_i

  )

  # Instrumental variable model
  iv_models[[i]] <-

    ivglm(

      estmethod = "ts",

      fitX.LZ = fitX,

      fitY.LX = fitY,

      data = dat_i,

      clusterid = "TRAUMAREGIO"

    )


# Store estimates

  iv_estimates[[i]] <-

    iv_models[[i]]$est



  iv_SE[[i]] <-

    sqrt(
      diag(
        iv_models[[i]]$vcov
      )
    )

# Pool estimates across imputations

estimates <- do.call(
  rbind,
  iv_estimates
)


ses <- do.call(
  rbind,
  iv_SE
)



iv_results <- mitools::MIcombine(

  results = estimates,

  variances = ses^2

)


# Convert IV estimates to OR and CI

iv_table <- data.frame(

  Variable =
    names(iv_results$coefficients),


  Estimate =
    iv_results$coefficients,


  SE =
    sqrt(
      diag(
        iv_results$variance
      )
    )

)



iv_table <- iv_table %>%

  mutate(

    CI_lower =
      Estimate -
      1.96 * SE,


    CI_upper =
      Estimate +
      1.96 * SE,


    OR =
      exp(Estimate),


    OR_lower =
      exp(CI_lower),


    OR_upper =
      exp(CI_upper)

  )
