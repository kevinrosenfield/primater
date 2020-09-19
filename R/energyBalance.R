seekFood <- function(df = dfAgents, resources = dfResources, sight) {
  for (a in 1:dfABM$numberAgents) {
    hoursRemaining <- (24 - dfABM$hour - (df$siteMeanTravelDist[a] / df$metersPerHour[a]))
    hoursRemaining[hoursRemaining < 0] <- 0
    needsPerHour <- (df$energyNeedsRemaining[a] / hoursRemaining)
    needsPerHour[needsPerHour < 0]  <- 0
    seekFoodProb <-  needsPerHour / dfABM$siteHourlyEnergy
    seekFoodProb[seekFoodProb > 1]  <- 1

    if (sample(c(1,2), 1,prob = c(seekFoodProb, 1 - seekFoodProb)) == 1) {
      euclideanDist <- sqrt((dfResources$x - df$xCor[a])^2 + (dfResources$y - df$yCor[a])^2)
      euclideanDist[euclideanDist > sight] <- NA
      euclideanDist[euclideanDist == 0] <- 0.00000000001
      displaceFeederProb <- ifelse(is.na(dfResources$agentFeeding), 1,
                                   df$Mass[a] / (df$Mass[dfResources$agentFeeding] + df$Mass[a]))
      if (!all(is.na(euclideanDist))) {
        df$potentialFeedingSite[a] <- which.min(euclideanDist / (displaceFeederProb * dfResources$energyRemaining))
        df$xCorFood[a] <- dfResources$x[df$potentialFeedingSite[a]]
        df$yCorFood[a] <- dfResources$y[df$potentialFeedingSite[a]]
        df$potentialFeedingSite[a] <- ifelse(dfResources$energyRemaining[df$potentialFeedingSite[a]] == 0,
                                             NA, df$potentialFeedingSite[a])
        } else {
          df$potentialFeedingSite[a] <- NA
        }
      } else {
      df$potentialFeedingSite[a] <- NA
      }
    if (!is.na(df$potentialFeedingSite[a])) {
      angle <- complex(real = df$xCorFood[a] - df$xCor[a], imaginary = df$yCorFood[a] - df$yCor[a])
      df$Heading1[a] <- Arg(angle) / base::pi * 180
      distFood <- sqrt((df$xCorFood[a] - df$xCor[a])^2 + (df$yCorFood[a] - df$yCor[a])^2)
      if (distFood <= df$metersPerHour[a]) {
        feedingSite = df$potentialFeedingSite[a]
        #print(paste(a, " feeds at ", feedingSite))
        df <- feed(df, resources, distFood, feedingSite, a)
        }
      }
    }
  return(df)
  }

feed <- function(df, resources, distFood, feedingSite, a) {
  df$xCor[a] <- df$xCorFood[a]
  df$yCor[a] <- df$yCorFood[a]
  #print(paste(a, " is ", distFood, " from food, and every hour can move ", df$metersPerHour[a]))
  feedProp <- 1 - (distFood / df$metersPerHour[a]) # proportion of the next hour spent feeding
  #print(paste(a, " eats for ", feedProp, " of an hour"))
  feedProp[feedProp > 1] <- 1 # can't be higher than 1
  consumed <- feedProp * dfABM$siteHourlyEnergy
  #print(paste(a, " eats ", consumed))
  df$energyNeedsRemaining[a] <- df$energyNeedsRemaining[a] - consumed
  #print(paste(a, " still needs ", df$energyNeedsRemaining[a]))
  df$feeding[a] <- ifelse(df$energyNeedsRemaining[a] >= 0, T, F)
  resources$energyRemaining[feedingSite] <- resources$energyRemaining[feedingSite] - consumed
  resources$energyRemaining[resources$energyRemaining < 0] <- 0
  dfResources <<- resources
  return(df)
}

expendEnergy <- function() {

}

