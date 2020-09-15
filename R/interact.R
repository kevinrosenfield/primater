# findContests() - males determine which other male is closest to themselves, and challenge them if under a specified threshold
# i tracks agentIds, min(d[d > 0]) find the smallest distance in the list of distances for a given individual
# round(match(min(d[d > 0]), d), 0) gives the position number (agentID) corresponding to the smallest distance
# if statement excludes nearest neighbors if either individual is female, or if the distance is above a specified threshold
# challenge stores relevant information. Here, agentID and Mass for both competitions
# challengers makes a lists of all contests to process; further code cleans up the format and removes duplicate contests


findNeighbors <- function(df = dfAgents, numberAgents = dfABM$numberAgents) {
  if (dfABM$dimensions == 2) {
    neighbors <- matrix(c(df$agentID, df$xCor, df$yCor), numberAgents, 3)
  } else {
    neighbors <- matrix(c(df$agentID, df$xCor, df$yCor, df$zCor), numberAgents, 4)
  }
  distances = matrix(ncol = numberAgents)
  for (i in 1:numberAgents) {
    if (dfABM$dimensions == 2) {
      distancesAgent <- c(sqrt((neighbors[,2][i] - neighbors[,2])^2 + (neighbors[,3][i] - neighbors[,3])^2))
    } else {
      distancesAgent <- c(sqrt((neighbors[,2][i] - neighbors[,2])^2 + (neighbors[,3][i] - neighbors[,3])^2 +
                                 (neighbors[,4][i] - neighbors[,4])^2))
    }
    distances <- rbind(distances, distancesAgent)
  }
  distances <- distances[-1,]
  row.names(distances) <- c(1:numberAgents)
  colnames(distances) <- c(1:numberAgents)
  return(distances)
}



interact <- function(df = dfAgents, numberAgents = dfABM$numberAgents, reach = reach) {
distances <<- findNeighbors(df = dfAgents, numberAgents = dfABM$numberAgents)
dfAgents <<- chooseMate(reach = reach)
dfAgents <<- compete(reach = reach)
}


compete <- function(dist.mat = distances, df = dfAgents, reach = reach) {
  dist.mat[upper.tri(dist.mat, diag = TRUE) == TRUE] <- NA
  dist.mat[dist.mat > reach] <- NA
  xCorsCompete <- list()
  yCorsCompete <- list()
  for (a in 1:dfABM$numberAgents) {
    if (df$Sex[a] == "F" | df$fleeTimeLeft[a] > 0) {
      dist.mat[a,] <- NA
      dist.mat[,a] <- NA
    }
    if (!all(is.na(dist.mat[a,]))) {
      opponent <- which.min(dist.mat[a,])[[1]]
      winner <- sample(c(a, opponent), 1, prob = c(df$Mass[a], df$Mass[opponent]))
      loser <- ifelse(winner == a, opponent, a)
      df$Wins[winner] <- df$Wins[winner] + 1
      df$Losses[loser] <- df$Losses[loser] + 1
      df$fleeTimeLeft[loser] <- 1
      if (df$chasing[loser] == T) {
        df$chasing[loser] <- F
        df$Heading1[loser] <- df$Heading1[loser] - 180
        df$potentialMate[loser] <- NA
        }
      xCorsCompete <- append(xCorsCompete, df$xCor[a])
      yCorsCompete <- append(yCorsCompete, df$yCor[a])
      dist.mat[c(winner,loser),] <- NA
      dist.mat[,c(winner,loser)] <- NA
    }
    xCorsCompete <<- xCorsCompete
    yCorsCompete <<- yCorsCompete
  }
  df$winRatio <- df$Wins / (df$Wins + df$Losses)
  return(df)
}

seekMate <- function(dist.mat = distances, df = dfAgents, sight = sight) {
  print('go')
  dist.mat[dist.mat > sight | dist.mat == 0] <- NA
  for (a in 1:dfABM$numberAgents) {
    if (df$Sex[a] == "M") {
      dist.mat[,a] <- NA }
    if (df$Sex[a] == "F" | dfAgents$fleeTimeLeft[a] > 0) {
      dist.mat[a,] <- NA
    }
  }
  for (a in 1:dfABM$numberAgents) {
    
    if (dfAgents$chasing[a] == F & dfAgents$fleeTimeLeft[a] < 0 & !all(is.na(dist.mat[a,])) &
        sample(1:2, 1, prob = c(1 / (dfABM$refractory * 24),
                                (abs(dfABM$refractory - (1 / (dfABM$refractory * 24)))))) == 1) {
      df$potentialMate[a] <- which.min(dist.mat[a,])[[1]]
      df$chasing[a] <- T
      print(paste("I am agent ", df$agentID[a], ", and my fleeTimeLeft is "  dfAgents$fleeTimeLeft[a]))
    }
    if (dfAgents$chasing[a] == T) {
      xCorGoal <- df$xCor[df$potentialMate[a]]
      yCorGoal <- df$yCor[df$potentialMate[a]]
      zCorGoal <- df$zCor[df$potentialMate[a]]
      angle <- complex(real = xCorGoal - df$xCor[a], imaginary = yCorGoal - df$yCor[a]) # need to adapt this to 3D
      df$Heading1[a] <- Arg(angle) / base::pi * 180
    }
  }
  return(df)
}


# sample from 0 with prob = 0 because when there is only 1 caller, sample is set tp 1:caller

chooseMate <- function(dist.mat = distances, df = dfAgents, reach = reach) {
  dist.mat[dist.mat > reach | dist.mat == 0] <- NA
  for (a in 1:dfABM$numberAgents) {
    if (df$Sex[a] == "F") {
      dist.mat[,a] <- NA
    } else {
      dist.mat[a,] <- NA
      if (df$fleeTimeLeft[a] >= 0) { dist.mat[,a] <- NA }
    }
  }
  for (a in 1:dfABM$numberAgents) {
    if (!all(is.na(dist.mat[a,]))) {
      callers <- match(dist.mat[a,][!is.na(dist.mat[a,])], dist.mat[a,])
      mate <- sample(c(0, callers), size = 1, prob = c(0, df$Attractiveness[callers]))
      rejects <- callers[-match(mate, callers)]
      df$Mates[a] <- df$Mates[a] + 1
      df$Mates[mate] <- df$Mates[mate] + 1
      df$sinceLastMate[mate] <- 0
      df$chasing[mate] <- F
    }
  }
  return(df)
}



