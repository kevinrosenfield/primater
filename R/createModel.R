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



setupAgents <- function(df = dfABM, numberMales) {
  dfAgents <- data.frame(1:df$numberAgents)
  dfAgents$agentID <- seq(1:df$numberAgents)
  dfAgents <- dfAgents[-1]
  dfAgents$dimensions <- df$dimensions
  dfAgents$xCor <- runif(df$numberAgents, 0 - df$worldDiameterMeters / 2, df$worldDiameterMeters / 2)
  dfAgents$yCor <- runif(df$numberAgents, 0 - df$worldDiameterMeters / 2, df$worldDiameterMeters / 2)
  if (df$dimensions == 3) {
    dfAgents$zCor <- runif(df$numberAgents, 0 - df$worldDiameterMeters / 2, df$worldDiameterMeters / 2)
  } else {
    dfAgents$zCor <- NA
  }
  if (dfABM$groupLiving == T) { dfAgents[c("xCor", "yCor", "zCor")] <- dfAgents[c("xCor", "yCor", "zCor")] / 2 }
  dfAgents$xCorOrigin <- dfAgents$xCor
  dfAgents$yCorOrigin <- dfAgents$yCor
  dfAgents$zCorOrigin <- dfAgents$zCor
  if (dfABM$groupLiving == T) { dfAgents[c("xCorOrigin", "yCorOrigin", "zCorOrigin")] <- 0 }
  dfAgents$distFromHome <- 0
  dfAgents$Age <- abs(rnorm(df$numberAgents, 9, 4))
  if (is.numeric(numberMales)) {
    dfAgents$Sex = as.factor(c(rep("M", numberMales), rep("F", dfABM$numberAgents - numberMales)))
  } else {
    dfAgents$Sex = as.factor(ifelse(sample(1:2, df$numberAgents, replace = TRUE) == 1, "M", "F"))
  }
  dfAgents$Mass <- abs(rnorm(df$numberAgents, 20, 5))
  dfAgents$Attractiveness <- abs(rnorm(df$numberAgents, 5, 1.66))
  dfAgents$Wins <- rep(0, df$numberAgents)
  dfAgents$Losses <- rep(0, df$numberAgents)
  dfAgents$winRatio <- 0
  dfAgents$Mates <- rep(0, dfABM$numberAgents)
  dfAgents$potentialMate <- rep(0, dfABM$numberAgents)
  dfAgents$sinceLastMate <- 1000000
  dfAgents$chasing <- F
  dfAgents$fleeTimeLeft <- rep(0, dfABM$numberAgents)
  dfAgents$Heading1 <- runif(df$numberAgents, 0, 360)
  dfAgents$Heading2 <- runif(df$numberAgents, 0, 360)
  dfAgents$homeRangeMetersDim <- ifelse(dfAgents$Sex == "F",
                                        abs(rnorm(df$numberAgents, df$meanFemaleRangeMetersDim, df$sdFemaleRangeMetersDim)),
                                        abs(rnorm(df$numberAgents, df$meanMaleRangeMetersDim, df$sdMaleRangeMetersDim)))
  dfAgents$homeRangeRadius <- sqrt(dfAgents$homeRangeMetersDim/pi)
  dfAgents$dayRangeMeters <- dfAgents$homeRangeRadius * 2 * df$dayRangeProp
  dfAgents$metersPerHour <- dfAgents$dayRangeMeters * df$dailyActivityProp
  
  cexSizes <- list()
  cexConstant <- ifelse(dfABM$groupLiving == T, 20.1, 0.8)
  cexConstant <- ifelse((Sys.info()[['sysname']] == 'Windows') == T,
                        ifelse(dfABM$groupLiving == T, cexConstant * 1.8, cexConstant * 1.4), cexConstant)
  
  fig <<- plot(NA, xlim =  c(0 - dfABM$worldRadius, dfABM$worldRadius), ylim = c(0 - dfABM$worldRadius, dfABM$worldRadius))
  #rm(fig, pos=".GlobalEnv")
  cexSizes <<- (dfAgents$homeRangeRadius / (par("cin")[2] / par("pin")[1]) /
                  (par("usr")[2] -par("usr")[1]) / par("cex") / 0.1875) * 1.1 * cexConstant
  
  print(paste("This ", df$dimensions, "-dimensional model world is ", round(df$worldSizeMetersDim,2), " m^",
              df$dimensions,". Its ", length(dfAgents$Sex[dfAgents$Sex == "F"]), " female residents' home ranges are ",
              round(df$meanFemaleRangeMetersDim ,2), " m^", df$dimensions, " on average.", sep = ""), 1)
  print(paste("This ", df$dimensions, "-dimensional model world is ", round(df$worldSizeMetersDim,2), " m^", df$dimensions,
              ". Its ", length(dfAgents$Sex[dfAgents$Sex == "M"]), " male residents' home ranges are ",
              round(df$meanMaleRangeMetersDim ,2), " m^", df$dimensions, " on average.", sep = ""), 1)
  
  return(dfAgents)
}


