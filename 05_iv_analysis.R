###############################################################################
# 05_iv_analysis.R
# Instrumental variable analyses
###############################################################################

library(tidyverse)
library(ivtools)
library(micemd)
library(rms)

#------------------------------------------------------------------------------
# Load analysis dataset
#------------------------------------------------------------------------------

load("data/derived/imputed_dataset.rda")

analysis <- imputed_dataset %>%
  filter(
    auto_ISS >= 16,
    GPSDATA_COMPLETE == 1,
    !is.na(DIFFERENTIAL_DISTANCE)
  )

###############################################################################
# Two-stage IV analysis
###############################################################################

iv_models <- vector("list", 30)

for(i in 1:30){

  dat <- analysis %>%
    filter(.imp == i)

  fitX <- glm(
    LEVEL1 ~ DIFFERENTIAL_DISTANCE +
      LEEFTIJDSEH +
      GESLACHTMAN +
      auto_ISS,
    family = binomial(),
    data = dat
  )

  fitY <- glm(
    OVERLEDEN ~
      LEVEL1 +
      LEEFTIJDSEH +
      GESLACHTMAN +
      auto_ISS,
    family = binomial(),
    data = dat
  )

  iv_models[[i]] <- ivglm(
    estmethod = "ts",
    fitX.LZ = fitX,
    fitY.LX = fitY,
    data = dat,
    clusterid = dat$REGIO
  )

}

###############################################################################
# Pool estimates
###############################################################################

mi.est <- do.call(
  rbind,
  lapply(iv_models, function(x) x$est)
)

mi.se <- do.call(
  rbind,
  lapply(iv_models, function(x) sqrt(diag(x$vcov)))
)

iv_results <- mi.meld(
  q = mi.est,
  se = mi.se
)

iv_results$CI_lower <- iv_results$q.mi -
  1.96 * iv_results$se.mi

iv_results$CI_upper <- iv_results$q.mi +
  1.96 * iv_results$se.mi

print(iv_results)
