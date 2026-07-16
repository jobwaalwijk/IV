# 02_multiple_imputation.R
# Multilevel multiple imputation of missing covariates
# Accounting for clustering within Dutch trauma regions

# Packages
source(here("R", "00_packages.R"))


############################################################
# Load cleaned dataset
############################################################

data <- readRDS(
  here(
    "data",
    "processed",
    "analysis_population.rds"
  )
)

# Clustering variable
data <- data %>%
  mutate(
    REGIO = factor(REGIO)
  )

# Variables not included in imputation

id_vars <- c(
  "ID",
  "IDAABA",
  "DATUMAANKOMST",
  "DATUMOVERLEDEN",
  "JAAR"
)


analysis_vars <- data %>%
  select(
    -any_of(id_vars)
  )

# Predictor matrix

pred <- make.predictorMatrix(
  analysis_vars
)

# Reset predictor matrix

pred[,] <- 0

# Variables used in imputation model

predictors <- c(

  # Demographics
  "LEEFTIJDSEH",
  "GESLACHTMAN",

  # Physiology
  "RRSYSTOLISCH",
  "ADEMFREQUENTIE",
  "EYEOPENINGWAARDEID",
  "MOTORRESPONSEWAARDEID",
  "VERBALRESPONSEWAARDEID",

  # Injury severity
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

  # Mechanism
  "INTENTIE",

  # Comorbidity
  "COMORB",

  # Prehospital treatment
  "MMT_",
  "INTUBATIEPREHOSP",

  # Cluster
  "REGIO"

)


predictors <- predictors[
  predictors %in% names(analysis_vars)
]


pred[predictors, predictors] <- 1

# Specify cluster variable

pred[, "REGIO"] <- -2

# Imputation methods

method <- make.method(
  analysis_vars
)

# Continuous variables

continuous_vars <- c(
  "LEEFTIJDSEH",
  "RRSYSTOLISCH",
  "ADEMFREQUENTIE",
  "auto_ISS"
)

method[continuous_vars] <- "2l.norm"



# Binary variable

binary_vars <- c(
  "GESLACHTMAN",
  "COMORB",
  "INTUBATIEPREHOSP",
  "MMT_"
)


binary_vars <- binary_vars[
  binary_vars %in% names(analysis_vars)
]


method[binary_vars] <- "2l.bin"


# Do not impute derived variables or outcomes

no_impute <- c(
  "OVERLEDEN",
  "ISS16",
  "AIS_1_3",
  "AIS_2_3",
  "AIS_3_3",
  "AIS_4_3",
  "AIS_5_3",
  "AIS_6_3",
  "AIS_7_3",
  "AIS_8_3",
  "AIS_9_3",
  "SBP90",
  "GCS14",
  "RR1029",
  "ECRU",
  "SPOEDINTERVENTIE",
  "SEH_ICU"
)

no_impute <- no_impute[
  no_impute %in% names(method)
]

method[no_impute] <- ""


# Multiple imputation

set.seed(2025)


imputed <- mice(
  analysis_vars,
  m = 30,
  maxit = 10,
  predictorMatrix = pred,
  method = method,
  printFlag = TRUE
)


# Convert to long format

imputed_long <- complete(
  imputed,
  action = "long",
  include = TRUE
)


# Add identifiers back

imputed_long <- data %>%
  select(
    any_of(id_vars)
  ) %>%
  bind_cols(
    imputed_long
  )


# Save

saveRDS(
  imputed_long,
  here(
    "data",
    "processed",
    "imputed_dataset.rds"
  )
)
