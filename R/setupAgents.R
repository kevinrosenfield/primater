setupAgents <- function(df = dfABM) {
  dfAgent <- data.frame(
    "agentID" = seq(1:numberAgents),
    "dimensions" = df$dimensions,
    "xCor" = runif(df$numberAgents, 0 - df$worldDiameterMeters / 2, df$worldDiameterMeters / 2),
    "yCor" = runif(df$numberAgents, 0 - df$worldDiameterMeters / 2, df$worldDiameterMeters / 2)
    )
  dfAgent$xCorOrigin = dfAgent$xCor
  dfAgent$yCorOrigin = dfAgent$yCor
  if (dfParams$Dimensions == 3)  {
    dfAgent$zCor = runif(df$numberAgents, 0 - df$worldDiameterMeters / 2, df$worldDiameterMeters / 2)
    dfAgent$zCorOrigin = dfAgent$zCor
    }
  dfAgent$Age <- abs(rnorm(df$numberAgents, 9, 4))
  dfAgent$Sex = as.factor(ifelse(sample(1:2, 1) == 1, "M", "F"))
  dfAgent$Mass <- abs(rnorm(df$numberAgents, 20, 5))
  dfAgent$Heading1 <- runif(df$numberAgents, 0, 360)
  dfAgent$Heading2 <- runif(df$numberAgents, 0, 360)
  dfAgent$homeRangeMetersDim <- ifelse(dfAgent$Sex == "F", rnorm(1, df$meanFemaleRangeMetersDim, df$sdFemaleRangeMetersDim),
                                                                 rnorm(1, df$meanMaleRangeMetersDim, df$sdMaleRangeMetersDim))
  dfAgent$homeRangeRadius <- sqrt(dfAgent$homeRangeMetersDim/pi)
  dfAgent$dayRangeMeters <- ifelse(dfAgent$Sex[i] == "F", rnorm(1, df$meanFemaleDayRange, df$sdFemaleDayRange),
                                   rnorm(1, df$meanMaleDayRange, df$sdMaleDayRange))
  dfAgent$metersPerHour <- ifelse(dfAgent$Sex[i] == "M", rnorm(1, df$meanMaleMetersPerHour, df$sdMaleMetersPerHour),
                                    rnorm(1, df$meanFemaleMetersPerHour, df$sdFemaleMetersPerHour))
  head(paste("This ", df$Dimensions, "-dimensional model world is ", round(dfModel$worldSizeMetersDim,2), " m^", df$dimensions,
             ". Its ", length(dfAgent$Sex[dfAgent$Sex == "F"]), " female residents' home ranges are ",
             round(dfModel$meanFemaleRangeMetersDim ,2), " m^", df$Dimensions, " on average.", sep = ""), 1)
  head(paste("This ", df$Dimensions, "-dimensional model world is ", round(dfModel$worldSizeMetersDim,2), " m^", df$dimensions,
             ". Its ", length(dfAgent$Sex[dfAgent$Sex == "M"]), " male residents' home ranges are ",
             round(dfModel$meanMaleRangeMetersDim ,2), " m^", df$Dimensions, " on average.", sep = ""), 1)
}
