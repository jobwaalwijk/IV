############################################################
# 02_multiple_imputation.R
#
# Multilevel multiple imputation of missing covariates
# Accounting for clustering within Dutch trauma regions
#
# Input:
#   data/processed/clean_dataset.rds
#
# Output:
#   data/processed/imputed_dataset.rds
#
############################################################


############################################################
# Load packages
############################################################

source(here("R", "00_packages.R"))


############################################################
# Load cleaned dataset
############################################################

data <- readRDS(
  here(
    "data",
    "processed",
    "clean_dataset.rds"
  )
)



############################################################
# Define trauma region clustering variable
############################################################

# TRAUMAREGIO should contain the 11 Dutch trauma regions
# If the variable has another name in DNTR, rename here

data <- data %>%
  mutate(
    TRAUMAREGIO = as.factor(TRAUMAREGIO)
  )


############################################################
# Select variables for imputation
############################################################

#
# Variables not imputed:
# - identifiers
# - outcomes
# - exposure
# - instrumental variable components
#

id_vars <- c(
  "ID",
  "IDAABA",
  "DATUMAANKOMST",
  "DATUMOVERLEDEN",
  "JAAR"
)


analysis_vars <- data %>%
  select(
    -all_of(id_vars)
  )


############################################################
# Prepare predictor matrix
############################################################

# micemd uses a predictor matrix similar to mice
# Positive values indicate predictors
#
# Trauma region is included as clustering variable
#

pred <- make.predictorMatrix(
  analysis_vars
)


############################################################
# Remove variables that should not predict others
############################################################

pred[, "ID"] <- 0
pred[, "IDAABA"] <- 0



############################################################
# Specify multilevel structure
############################################################

#
# In micemd:
# -2 = cluster variable
#  1 = predictor
#  0 = not used
#

pred[, "TRAUMAREGIO"] <- -2



############################################################
# Variables included in imputation model
############################################################

# Important clinical predictors

predictors <- c(

  # demographics
  "LEEFTIJDSEH",
  "GESLACHTMAN",

  # physiology
  "RRSYSTOLISCH",
  "ADEMFREQUENTIE",
  "EYEOPENINGWAARDEID",
  "MOTORRESPONSEWAARDEID",
  "VERBALRESPONSEWAARDEID",

  # injury severity
  "auto_ISS",
  "AIS_1",
  "AIS_2",
  "AIS_3",
  "AIS_4",
  "AIS_5",
  "AIS_6",
  "AIS_7",
  "AIS_8",
  "AIS_9",

  # mechanism
  "INTENTIE",

  # comorbidity
  "COMORB",

  # treatment indicators
  "MMT",
  "INTUBATIEPREHOSP",

  # cluster
  "TRAUMAREGIO"

)



############################################################
# Restrict predictor matrix
############################################################

pred[,] <- 0

pred[predictors, predictors] <- 1


# cluster variable

pred[, "TRAUMAREGIO"] <- -2



############################################################
# Define imputation methods
############################################################

method <- make.method(
  analysis_vars
)


#
# Continuous variables
#

method[c(
  "LEEFTIJDSEH",
  "RRSYSTOLISCH",
  "ADEMFREQUENTIE",
  "auto_ISS"
)] <- "2l.norm"


#
# Binary variables
#

binary_vars <- c(
  "GESLACHTMAN",
  "COMORB",
  "INTUBATIEPREHOSP",
  "MMT",
  "OVERLEDEN",
  "ISS16"
)


method[binary_vars] <- "2l.bin"



############################################################
# Run multilevel multiple imputation
############################################################

set.seed(2025)


imputed <- mice(
  
  analysis_vars,
  
  m = 30,
  
  maxit = 10,
  
  predictorMatrix = pred,
  
  method = method,
  
  printFlag = TRUE
  
)



############################################################
# Convert to long format
############################################################

imputed_long <- complete(
  imputed,
  action = "long",
  include = TRUE
)



############################################################
# Add identifiers back
############################################################

imputed_long <- data %>%
  select(
    all_of(id_vars)
  ) %>%
  bind_cols(
    imputed_long
  )



############################################################
# Save
############################################################


saveRDS(
  imputed_long,
  here(
    "data",
    "processed",
    "imputed_dataset.rds"
  )
)


message(
  "Multiple imputation completed: ",
  30,
  " datasets created with clustering by trauma region."
)
