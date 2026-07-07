############################################################
# Instrumental variable analysis
#
# Exposure:
#   Direct admission to level I trauma center
#
# Instrument:
#   Differential distance
#
# Outcome:
#   30-day mortality
#
# Method:
#   Two-stage  ivglm
#
############################################################


##############################
# 0. Setup
##############################

rm(list = ls())

source("R/00_packages.R")
source("R/config.R")



##############################
# 1. Load dataset
##############################

data <- readRDS(
  file.path(
    paths$output,
    "dataset_with_instrument.rds"
  )
)



##############################
# 2. Study population
##############################

analysis_data <- data %>%
  filter(
    auto_ISS >= 16,
    !is.na(DIFFERENTIAL_DISTANCE)
  )



##############################
# 3. Function for IV analysis
##############################

run_IV_analysis <- function(
    dataset,
    outcome_variable
){


  fit_first_stage <- list()

  fit_second_stage <- list()

  estimates <- NULL

  standard_errors <- NULL



  #################################
  # Loop over imputed datasets
  #################################

  for(i in 1:30){


    current_data <- dataset %>%
      filter(.imp == i)



    #################################
    # First stage:
    #
    # Differential distance predicts
    # admission to level I center
    #################################

    fit_first_stage[[i]] <-
      glm(
        LEVEL1 ~
          DIFFERENTIAL_DISTANCE +
          LEEFTIJDSEH +
          GESLACHTMAN +
          auto_ISS,

        family = binomial,

        data = current_data
      )



    #################################
    # Second stage:
    #
    # Level I admission predicts
    # mortality
    #################################

    fit_second_stage[[i]] <-
      glm(
        as.formula(
          paste0(
            outcome_variable,
            " ~ LEVEL1 + ",
            "LEEFTIJDSEH + ",
            "GESLACHTMAN + ",
            "auto_ISS"
          )
        ),

        family = binomial,

        data = current_data
      )



    #################################
    # IV model
    #################################

    iv_model <-
      ivtools::ivglm(
        estmethod = "ts",

        fitX.LZ =
          fit_first_stage[[i]],

        fitY.LX =
          fit_second_stage[[i]],

        data =
          current_data
      )



    estimates <-
      rbind(
        estimates,
        iv_model$est
      )


    standard_errors <-
      rbind(
        standard_errors,
        sqrt(
          diag(iv_model$vcov)
        )
      )


  }



  #################################
  # Pool multiple imputations
  #################################

  pooled <-
    mice::pool.scalar(
      Q = estimates[,2],
      U = standard_errors[,2]^2
    )



  result <-
    data.frame(

      estimate =
        pooled$qbar,

      standard_error =
        pooled$tau,

      CI_lower =
        pooled$qbar -
        1.96*pooled$tau,

      CI_upper =
        pooled$qbar +
        1.96*pooled$tau

    )


  return(result)

}



##############################
# 4. Run primary analysis
##############################

IV_30day_mortality <-

  run_IV_analysis(
    dataset = analysis_data,
    outcome_variable = "OVERLEDEN30D"
  )



##############################
# 5. Save output
##############################

write.csv(
  IV_30day_mortality,

  file.path(
    paths$output,
    "IV_primary_result.csv"
  ),

  row.names = FALSE
)



############################################################
# End script
############################################################
