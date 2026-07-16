# 07_subgroup_analyses.R

source(here("R", "00_packages.R"))


# Load imputed IV dataset

data <- readRDS(
  here(
    "data",
    "processed",
    "analysis_dataset_iv.rds"
  )
)


# Select IV population

data <- data %>%

  filter(

    .imp > 0,

    auto_ISS >=16,

    !is.na(DIFFERENTIAL_DISTANCE),

    !is.na(LEVEL1),

    !is.na(OVERLEDEN30D),

    !is.na(TRAUMAREGIO)

  ) %>%

  mutate(

    SEX =
      factor(GESLACHTMAN)

  )

# Create subgroup variables

data <- data %>%

  mutate(

    ELDERLY =
      ifelse(
        LEEFTIJDSEH >=65,
        1,
        0
      ),


    TBI =
      ifelse(
        AIS_1 >=3,
        1,
        0
      ),


    CRITICAL_INJURY =
      ifelse(
        auto_ISS >=25,
        1,
        0
      )

  )


# IV function

run_iv <- function(dataset){


  estimates <- list()

  ses <- list()



  for(i in 1:30){


    dat <- dataset %>%

      filter(
        .imp==i
      )


    dd <- datadist(dat)

    options(
      datadist="dd"
    )



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


      family=binomial,


      data=dat

    )



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


      family=binomial,


      data=dat

    )



    iv_fit <- ivglm(

      estmethod="ts",

      fitX.LZ=fitX,

      fitY.LX=fitY,

      data=dat,

      clusterid="TRAUMAREGIO"

    )



    estimates[[i]] <-

      iv_fit$est



    ses[[i]] <-

      sqrt(
        diag(
          iv_fit$vcov
        )
      )

  }



  pooled <- mi.meld(

    do.call(rbind,estimates),

    do.call(rbind,ses)

  )



  data.frame(

    Estimate =
      pooled$q.mi,


    SE =
      pooled$se.mi,


    OR =
      exp(pooled$q.mi),


    Lower95 =
      exp(
        pooled$q.mi -
          1.96*pooled$se.mi
      ),


    Upper95 =
      exp(
        pooled$q.mi +
          1.96*pooled$se.mi
      )

  )

}

# Run analyses

elderly_results <-

  run_iv(
    filter(
      data,
      ELDERLY==1
    )
  ) %>%

  mutate(
    Subgroup="Age >=65 years"
  )



tbi_results <-

  run_iv(
    filter(
      data,
      TBI==1
    )
  ) %>%

  mutate(
    Subgroup="Traumatic brain injury"
  )



critical_results <-

  run_iv(
    filter(
      data,
      CRITICAL_INJURY==1
    )
  ) %>%

  mutate(
    Subgroup="Critical injury (ISS >=25)"
  )

Results <- bind_rows(

  elderly_results,

  tbi_results,

  critical_results

)
