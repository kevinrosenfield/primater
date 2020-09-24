checkHunger <- function(df = dfAgents, resources = dfResources, sight) {
  xCorsCompete <<- list()
  yCorsCompete <<- list()
  for (a in 1:dfABM$numberAgents) {
    hoursRemaining <- 24 - dfABM$hour - (df$siteMeanTravelDist[a] / df$metersPerHour[a])
    needsPerHour <- df$energyNeedsRemaining[a] /  ifelse(hoursRemaining < 0, 0, hoursRemaining)
    seekFoodProb <- ifelse(needsPerHour < 0, 0, needsPerHour) / dfABM$siteHourlyEnergy
    seekFoodProb[seekFoodProb > 1]  <- 1
    # print(paste(a, " has a ", round(seekFoodProb*100,2), "% chance of looking for food"))
    df$potentialFeedingSite[a] <- ifelse(sample(c(1,2), 1, prob = c(seekFoodProb, 1 - seekFoodProb)) == 1,
                                         checkFoodSite(df, resources, sight, a), NA)
    # print(paste(a, " wants to feed at site ", df$potentialFeedingSite[a]))
    if(!is.na(df$potentialFeedingSite[a])) {
      df$xCorFood[a] <- dfResources$x[df$potentialFeedingSite[a]]
      df$yCorFood[a] <- dfResources$y[df$potentialFeedingSite[a]]
      distFood <- sqrt((df$xCorFood[a] - df$xCor[a])^2 + (df$yCorFood[a] - df$yCor[a])^2)
      # print(paste(df$potentialFeedingSite[a], " is ", distFood, " meters away."))
      if (distFood < df$metersPerHour[a]) {
        df$currentFeedingSite[a] <-df$potentialFeedingSite[a]
        # print(paste(a, " is ready to feed at site ", df$potentialFeedingSite[a]))
        } else {
          # print(paste(a, " is facing site ", df$potentialFeedingSite[a], "which is located at ", df$xCorFood[a], ", ", df$yCorFood[a]))
          df <- faceFood(df, a)
          }
    } else {
      df$xCorFood[a] <- NA
      df$yCorFood[a] <- NA
    }
  }
  for (f in 1:length(resources$agentFeeding)) {
    potentialFeeders <- df$agentID[df$currentFeedingSite == f & !is.na(df$currentFeedingSite)]
    # print(potentialFeeders)
    if (length(potentialFeeders) > 1) {
      xCorsCompete <<- append(xCorsCompete, resources$x[f])
      yCorsCompete <<- append(yCorsCompete, resources$y[f])
    }

    resources$agentFeeding[f] <- ifelse(length(potentialFeeders) < 1, NA, ifelse(length(unlist(potentialFeeders) == 1),
                                               unlist(potentialFeeders), sample(potentialFeeders, 1, prob = df$Mass[potentialFeeders])))
    if (!is.na(resources$agentFeeding[f])) {
      df <-feed(df, resources, feedingSite = f)
      }
  }
  return(df)
}



checkFoodSite <- function(df, resources, sight, a) {
  euclideanDist <- sqrt((dfResources$x - df$xCor[a])^2 + (dfResources$y - df$yCor[a])^2)
  euclideanDist[euclideanDist > sight] <- NA
  euclideanDist[euclideanDist == 0] <- 0.00000000001
  displaceFeederProb <- ifelse(is.na(dfResources$agentFeeding), 1, df$Mass[a] / (df$Mass[dfResources$agentFeeding] + df$Mass[a]))
  potentialFeedingSite <- ifelse(!all(is.na(euclideanDist)), which.min(euclideanDist / (displaceFeederProb * dfResources$energyRemaining)), NA)
  resources$energyRemaining[resources$energyRemaining == 0] <- NA
  return(potentialFeedingSite)
}


faceFood <- function(df, a) {
  angle <- complex(real = df$xCorFood[a] - df$xCor[a], imaginary = df$yCorFood[a] - df$yCor[a])
  df$Heading1[a] <- Arg(angle) / base::pi * 180
  return(df)
}


feed <- function(df, resources, feedingSite) {
  agentFeeding <- resources$agentFeeding[feedingSite]
  df$xCor[agentFeeding] <- df$xCorFood[agentFeeding]
  df$yCor[agentFeeding] <- df$yCorFood[agentFeeding]
  # print(paste(agentFeeding, " eats ", dfABM$siteHourlyEnergy, " from ", feedingSite))
  df$energyNeedsRemaining[agentFeeding] <- df$energyNeedsRemaining[agentFeeding] - dfABM$siteHourlyEnergy
  # print(paste(agentFeeding, " still needs ", df$energyNeedsRemaining[agentFeeding]))
  df$feeding[agentFeeding] <- ifelse(df$energyNeedsRemaining[agentFeeding] >= 0, T, F)
  resources$energyRemaining[feedingSite] <- resources$energyRemaining[feedingSite] - dfABM$siteHourlyEnergy
  resources$energyRemaining[resources$energyRemaining < 0] <- 0
  dfResources$resources$agentFeeding[feedingSite] <- NA
  df$currentFeedingSite[agentFeeding] <- NA
  df$potentialFeedingSite[agentFeeding] <- NA
  dfResources <<- resources
  return(df)
}
