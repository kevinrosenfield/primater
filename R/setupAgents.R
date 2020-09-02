setupAgents <- function(df = dfABM) {
  dfAgents <- data.frame(1:df$numberAgents)
  dfAgents$agentID <- seq(1:df$numberAgents)
  dfAgents <- dfAgents[-1]
  dfAgents$dimensions <- df$dimensions
  dfAgents$xCor <- runif(df$numberAgents, 0 - df$worldDiameterMeters / 2, df$worldDiameterMeters / 2)
  dfAgents$yCor <- runif(df$numberAgents, 0 - df$worldDiameterMeters / 2, df$worldDiameterMeters / 2)
  dfAgents$xCorOrigin <- dfAgents$xCor
  dfAgents$yCorOrigin <- dfAgents$yCor
  dfAgents$zCor <- ifelse(df$dimensions == 3,
                          runif(df$numberAgents, 0 - df$worldDiameterMeters / 2, df$worldDiameterMeters / 2), NA)
  dfAgents$zCorOrigin <- ifelse(df$dimensions == 3, dfAgents$zCor, NA)
  dfAgents$Age <- abs(rnorm(df$numberAgents, 9, 4))
  dfAgents$Sex = as.factor(ifelse(sample(1:2, 1) == 1, "M", "F"))
  dfAgents$Mass <- abs(rnorm(df$numberAgents, 20, 5))
  dfAgents$Heading1 <- runif(df$numberAgents, 0, 360)
  dfAgents$Heading2 <- runif(df$numberAgents, 0, 360)
  dfAgents$homeRangeMetersDim <- ifelse(dfAgents$Sex == "F",
                                        rnorm(df$numberAgents, df$meanFemaleRangeMetersDim, df$sdFemaleRangeMetersDim),
                                        rnorm(df$numberAgents, df$meanMaleRangeMetersDim, df$sdMaleRangeMetersDim))
  dfAgents$homeRangeRadius <- sqrt(dfAgents$homeRangeMetersDim/pi)
  dfAgents$dayRangeMeters <- ifelse(dfAgents$Sex == "F",
                                    rnorm(df$numberAgents, df$meanFemaleDayRange, df$sdFemaleDayRange),
                                   rnorm(df$numberAgents, df$meanMaleDayRange, df$sdMaleDayRange))
  dfAgents$metersPerHour <- ifelse(dfAgents$Sex == "F",
                                   rnorm(df$numberAgents, df$meanFemaleMetersPerHour, df$sdFemaleMetersPerHour),
                                    rnorm(df$numberAgents, df$meanMaleMetersPerHour, df$sdMaleMetersPerHour))
  print(paste("This ", df$dimensions, "-dimensional model world is ", round(df$worldSizeMetersDim,2), " m^",
                   df$dimensions,". Its ", length(dfAgents$Sex[dfAgents$Sex == "F"]), " female residents' home ranges are ",
                   round(df$meanFemaleRangeMetersDim ,2), " m^", df$dimensions, " on average.", sep = ""), 1)
  print(paste("This ", df$dimensions, "-dimensional model world is ", round(df$worldSizeMetersDim,2), " m^", df$dimensions,
             ". Its ", length(df$Sex[dfAgents$Sex == "M"]), " male residents' home ranges are ",
             round(df$meanMaleRangeMetersDim ,2), " m^", df$dimensions, " on average.", sep = ""), 1)

  return(dfAgents)
}
