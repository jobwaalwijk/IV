############################################################
# Multiple imputation using multilevel chained equations
#
# Patients are clustered within trauma regions (REGIO)
#
# Input:
#   cleaned_trauma_data.rds
#
# Output:
#   multiple_imputed_dataset.rds
#
############################################################


##############################
# 0. Setup
##############################

rm(list = ls())

source("R/00_packages.R")
source("R/config.R")


##############################
# 1. Load cleaned dataset
##############################

data <- readRDS(
  file.path(
    paths$output,
    "cleaned_trauma_data.rds"
  )
)


##############################
# 2. Define clustering variable
##############################

# Trauma region is the clustering level
# in the Dutch regionalized trauma system

data$REGIO <- as.factor(data$REGIO)



##############################
# 3. Variables not included
#    in imputation
##############################

# Variables that are already complete,
# are outcomes/exposures, or define the IV

not_imputed <- c(
  "ID",
  "IDAABA",
  "ONGEVALDT",
  "DATUMAANKOMST",
  "DATUMOVERLEDEN",

  # exposure
  "LEVEL1",
  "LEVEL2",
  "LEVEL3",

  # outcomes
  "OVERLEDEN",
  "OVERLEDEN30D",
  "OVERLEDEN24H",

  # instrumental variable
  "DIFFERENTIAL_DISTANCE",

  # clustering variable
  "REGIO",

  # geographic variables
  "GPSDATA",
  "Patient_Postcode",
  "Patient_Latitude",
  "Patient_Longitude"
)



##############################
# 4. Dataset for imputation
##############################

imp_data <- data %>%
  select(
    -all_of(not_imputed)
  )


# Add cluster variable back
# because mice requires it in the dataset

imp_data$REGIO <- data$REGIO



##############################
# 5. Missing data overview
##############################

missing_table <- data.frame(
  variable = names(imp_data),
  missing_n = sapply(
    imp_data,
    function(x) sum(is.na(x))
  ),
  missing_percentage = sapply(
    imp_data,
    function(x) mean(is.na(x))*100
  )
)


write.csv(
  missing_table,
  file.path(
    paths$output,
    "missing_data_summary.csv"
  ),
  row.names = FALSE
)



##############################
# 6. Predictor matrix
##############################

predictor_matrix <- mice::make.predictorMatrix(
  imp_data
)


# REGIO defines clustering
# It should not be imputed itself

predictor_matrix["REGIO", ] <- 0


# All variables are nested within REGIO

predictor_matrix[, "REGIO"] <- -2



##############################
# 7. Imputation methods
##############################

method <- mice::make.method(
  imp_data
)


# Multilevel continuous variables

method[method == "norm"] <- "2l.norm"


# Multilevel binary variables

method[method == "logreg"] <- "2l.bin"


# Multilevel categorical variables

method[method == "polyreg"] <- "2l.polyreg"



# Cluster variable remains unchanged

method["REGIO"] <- ""



##############################
# 8. Run multilevel MI
##############################

set.seed(2025)


imp <- mice::mice(
  imp_data,
  m = 30,
  maxit = 10,
  method = method,
  predictorMatrix = predictor_matrix,
  printFlag = TRUE
)



##############################
# 9. Export long dataset
##############################

imputed_long <- mice::complete(
  imp,
  action = "long",
  include = TRUE
)



##############################
# 10. Add non-imputed variables
##############################

non_imputed <- data %>%
  select(
    all_of(not_imputed)
  )


non_imputed_long <- do.call(
  rbind,
  replicate(
    31,
    non_imputed,
    simplify = FALSE
  )
)



final_dataset <- bind_cols(
  imputed_long,
  non_imputed_long
)



##############################
# 11. Save
##############################

saveRDS(
  final_dataset,
  file.path(
    paths$output,
    "multiple_imputed_dataset.rds"
  )
)


saveRDS(
  imp,
  file.path(
    paths$output,
    "mice_multilevel_object.rds"
  )
)


############################################################
# End script
############################################################
