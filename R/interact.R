# findContests() - males determine which other male is closest to themselves, and challenge them if under a specified threshold
# i tracks agentIds, min(d[d > 0]) find the smallest distance in the list of distances for a given individual
# round(match(min(d[d > 0]), d), 0) gives the position number (agentID) corresponding to the smallest distance
# if statement excludes nearest neighbors if either individual is female, or if the distance is above a specified threshold
# challenge stores relevant information. Here, agentID and Mass for both competitions
# challengers makes a lists of all contests to process; further code cleans up the format and removes duplicate contests


compete <- function(dist.mat = distances, df = dfAgents, reach = 1000) {
  dist.mat[upper.tri(dist.mat, diag = TRUE) == TRUE] <- NA
  dist.mat[dist.mat > reach] <- NA
  for (a in 1:dfABM$numberAgents) {
    if (df$Sex[a] == "F") {
      dist.mat[a,] <- NA
      dist.mat[,a] <- NA
    }
    if (!all(is.na(dist.mat[a,]))) {
      opponent <- which.min(dist.mat[a,])[[1]]
      winner <- sample(c(a, opponent), 1, prob = c(df$Mass[a], df$Mass[opponent]))
      loser <- ifelse(winner == a, opponent, a)
      df$Wins[winner] <- df$Wins[winner] + 1
      df$Losses[loser] <- df$Losses[loser] + 1
    }
  }
  return(df)
}

# sample from 0 with prob = 0 because when there is only 1 caller, sample is set tp 1:caller

chooseMate <- function(dist.mat = distances, df = dfAgents, reach = 1000) {
  dist.mat[dist.mat > reach | dist.mat == 0] <- NA
  for (a in 1:dfABM$numberAgents) {
    if (df$Sex[a] == "F") {
      dist.mat[,a] <- NA
    } else {
      dist.mat[a,] <- NA
    }
  }
  for (a in 1:dfABM$numberAgents) {
    if (!all(is.na(dist.mat[a,]))) {
      callers <- match(dist.mat[a,][!is.na(dist.mat[a,])], dist.mat[a,])
      mate <- sample(c(0, callers), size = 1, prob = c(0, df$Attractiveness[callers]))
      rejects <- callers[-match(mate, callers)]
      df$Mates[a] <- df$Mates[a] + 1
      df$Mates[mate] <- df$Mates[mate] + 1
    }
  }
  return(df)
}



