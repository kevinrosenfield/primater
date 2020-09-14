setupABM <- function(dimensions, numberAgents, worldDiameter, liveInGroup, maleRangeProp, dayRangeProp, refractory, fleeTime) {
  model <-  list(
    "numberAgents" = numberAgents,
    "Territorial" = sample(c(FALSE, TRUE), 1),
    "groupLiving" = liveInGroup,
    "worldDiameterMeters" = worldDiameter,
    "dimensions" = dimensions,
    "dailyActivityProp" = abs(rnorm(1, 0.12, 0.025)),
    "dayRangeProp" = dayRangeProp,
    "refractory" = refractory,
    "fleeTime" = fleeTime)
  model <- c(
    model,
    "worldRadius" = model$worldDiameterMeters / 2,
    "worldSizeMetersDim" = ifelse(
      dimensions == 2, pi * (model$worldDiameterMeters / 2)^2, 0.75 * pi * (model$worldDiameterMeters / 2)^3))
  model <- c(model,
     "meanMaleRangeMetersDim" = ifelse(liveInGroup == T, model$worldSizeMetersDim,
                                       abs(rnorm(1, (model$worldSizeMetersDim / model$numberAgents) * maleRangeProp,
                                          (model$worldSizeMetersDim / model$numberAgents) * (maleRangeProp / 2.5)))),
     "meanFemaleRangeMetersDim" = ifelse(liveInGroup == T, model$worldSizeMetersDim,
                                         abs(rnorm(1, (model$worldSizeMetersDim / model$numberAgents) * 0.25,
                                                   (model$worldSizeMetersDim / model$numberAgents) * 0.10))))
  model <- c(model,
             "sdMaleRangeMetersDim" = ifelse(liveInGroup == T, 0, abs(runif(1, 0, model$meanMaleRangeMetersDim / 2))),
             "sdFemaleRangeMetersDim" = ifelse(liveInGroup == T, 0, abs(runif(1, 0, model$meanFemaleRangeMetersDim / 20)))
  )
  return(model)
}
