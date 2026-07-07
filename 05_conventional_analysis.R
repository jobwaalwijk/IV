############################################################
# Conventional multivariable logistic regression
# Outcome: 30-day mortality
# Exposure: Direct admission to level I trauma center
############################################################


library(tidyverse)
library(mice)
library(broom)
library(sandwich)
library(lmtest)
library(mitools)


############################################################
# Load data
############################################################

load("data/processed/Baseline_mids.rda")


############################################################
# Prepare analysis dataset
############################################################

analysis_data <- complete(
  Baseline_mids,
  action = "long",
  include = FALSE
) %>%
  filter(
    auto_ISS >= 16,
    GPSDATA_COMPLETE == 1,
    !is.na(DIFFERENTIAL_DISTANCE)
  )


############################################################
# Conventional logistic regression
############################################################
#
# Outcome:
# OVERLEDEN30D
#
# Exposure:
# LEVEL1
#
# Adjusted for:
# age
# sex
# ISS
#
############################################################


models <- list()


for(i in unique(analysis_data$.imp)) {


  data_i <- analysis_data %>%
    filter(.imp == i)


  models[[i]] <- glm(
    OVERLEDEN30D ~ 
      LEVEL1 +
      LEEFTIJDSEH +
      GESLACHTMAN +
      auto_ISS +
      COMORB + 
      RRSYSTOLISCH +
      ADEMFREQUENTIE +
      GCS +
      auto_ISS +
      MOTORIZED_VEHICLE_CRASH +
      FALL_HIGH +
      VIOLENCE,
    family = binomial(link = "logit"),
    data = data_i
  )

}model <- glm(
  OVERLEDEN30D ~ 
   
  family = binomial(link="logit"),
  data = data_i
)


############################################################
# Pool estimates across imputations
############################################################


estimates <- map_df(
  models,
  ~tidy(.x, exponentiate = TRUE, conf.int = TRUE),
  .id = "imputation"
)


conventional_results <- estimates %>%
  filter(term == "LEVEL1") %>%
  summarise(
    OR = mean(estimate),
    CI_low = mean(conf.low),
    CI_high = mean(conf.high)
  )


print(conventional_results)


############################################################
# Save results
############################################################


write_csv(
  conventional_results,
  "results/conventional_analysis.csv"
)
