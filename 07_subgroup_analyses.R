############################################################
# 07_subgroup_analyses.R
#
# Prespecified subgroup analyses
#
# Subgroups:
#   1. Elderly patients (age ≥65 years)
#   2. Traumatic brain injury (AIS Head ≥3)
#   3. Critical injuries (ISS ≥25)
#
# Output:
#   results/subgroup_results.csv
#
############################################################

source(here("R", "00_packages.R"))

############################################################
# Load dataset
############################################################

Baseline <- readRDS(
  here("data", "processed", "baseline_dataset.rds")
)

############################################################
# Complete imputed datasets
############################################################

Baseline_IV <- complete(
  Baseline,
  action = "long",
  include = FALSE
)

############################################################
# Create subgroup variables
############################################################

Baseline_IV <- Baseline_IV %>%
  mutate(

    ELDERLY =
      ifelse(LEEFTIJDSEH >= 65, 1, 0),

    TBI =
      ifelse(AIS_1 >= 3, 1, 0),

    CRITICAL_INJURY =
      ifelse(auto_ISS >= 25, 1, 0)

  )

############################################################
# Function for IV analysis
############################################################

run_iv <- function(data){

  fits <- vector("list",30)

  for(i in 1:30){

    dat <- filter(data,.imp==i)

    fitX <- glm(
      LEVEL1 ~ DIFFERENTIAL_DISTANCE +
        LEEFTIJDSEH +
        GESLACHTMAN +
        auto_ISS,
      family=binomial,
      data=dat
    )

    fitY <- glm(
      OVERLEDEN30D ~ LEVEL1 +
        LEEFTIJDSEH +
        GESLACHTMAN +
        auto_ISS,
      family=binomial,
      data=dat
    )

    fits[[i]] <-
      ivglm(
        estmethod="ts",
        fitX.LZ=fitX,
        fitY.LX=fitY,
        data=dat,
        clusterid=dat$REGIO
      )

  }

  est <- do.call(
    rbind,
    lapply(fits,function(x)x$est)
  )

  se <- do.call(
    rbind,
    lapply(fits,function(x)sqrt(diag(x$vcov)))
  )

  out <- mi.meld(est,se)

  out$OR <- exp(out$q.mi)
  out$Lower95 <- exp(out$q.mi-1.96*out$se.mi)
  out$Upper95 <- exp(out$q.mi+1.96*out$se.mi)

  out
}

############################################################
# Run subgroup analyses
############################################################

elderly_results <-
  run_iv(
    filter(Baseline_IV,ELDERLY==1)
  )

tbi_results <-
  run_iv(
    filter(Baseline_IV,TBI==1)
  )

critical_results <-
  run_iv(
    filter(Baseline_IV,CRITICAL_INJURY==1)
  )

############################################################
# Combine results
############################################################

Results <-
  bind_rows(

    elderly_results %>%
      mutate(Subgroup="Age ≥65 years"),

    tbi_results %>%
      mutate(Subgroup="Traumatic brain injury"),

    critical_results %>%
      mutate(Subgroup="Critical injury (ISS ≥25)")

  )

############################################################
# Save results
############################################################

write_csv(
  Results,
  here(
    "results",
    "subgroup_results.csv"
  )
)

message("Subgroup analyses completed.")
