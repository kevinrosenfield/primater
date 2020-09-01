setupABM <- function(Dimensions = 2) {
  model <-  list(
    "numberAgents" = sample(1:100, 1),
    "Territorial" = sample(c(FALSE, TRUE), 1),
    "worldDiameterMeters" = runif(1, 25, 1000),
    "dailyActivityProp" = abs(rnorm(1, 12, 2.5)))
  model <- c(model,
    if( Dimensions == 2) {
      area = pi * (model$worldDiameterMeters / 2)^2
    } else {
      area = 0.75 * pi * (model$worldDiameterMeters / 2)^3
      },
    "worldSizeMetersDim" = area,
    "meanMaleDayRange" = abs(rnorm(1, model$worldDiameterMeters / 10, (model$worldDiameterMeters / 50))),
    "meanFemaleDayRange" = abs(rnorm(1, model$worldDiameterMeters / 10,
                                      (model$worldDiameterMeters / 50))))
  model <- c(model,
    "meanMaleRangeMetersDim" = abs(rnorm(1, model$worldSizeMetersDim / model$numberAgents,
                                          (model$worldSizeMetersDim / model$numberAgents) * 0.25)),
    "meanFemaleRangeMetersDim" = abs(rnorm(1, model$worldSizeMetersDim / model$numberAgents,
                                            (model$worldSizeMetersDim / model$numberAgents) * 0.25)),
    "meanMaleMetersPerHour" = abs(rnorm(1, model$meanMaleDayRange / model$dailyActivityProp,
                                         (model$meanMaleDayRange / model$dailyActivityProp / 3))),
    "meanFemaleMetersPerHour" = abs(rnorm(1, model$meanFemaleDayRange / model$dailyActivityProp,
                                         (model$meanFemaleDayRange / model$dailyActivityProp / 3))),
    "sdMaleDayRange" = abs(runif(1, 0, model$meanMaleDayRange / 10)),
    "sdFemaleDayRange" = abs(runif(1, 0, model$meanFemaleDayRange / 10)))
  model <- c(model,
    "sdMaleRangeMetersDim" = abs(runif(1, 0, model$meanMaleRangeMetersDim / 2)),
    "sdFemaleRangeMetersDim" = abs(runif(1, 0, model$meanFemaleRangeMetersDim / 20)),
    "sdFemaleMetersPerHour" = abs(runif(1, 0, model$meanFemaleMetersPerHour / 3)),
    "sdMaleMetersPerHour" = abs(runif(1, 0, model$meanMaleMetersPerHour / 3))
  )
  model <-  model[-5]
  return(model)
}
