seekMate <- function(dist.mat = distances, df = dfAgents, sight = sight) {
  dist.mat[dist.mat > sight | dist.mat == 0] <- NA
  for (a in 1:dfABM$numberAgents) {
    if (df$Sex[a] == "M") {
      dist.mat[,a] <- NA
    } else {
      dist.mat[a,] <- NA
    }
  }
  for (a in 1:dfABM$numberAgents) {

    if (dfAgents$chasing[a] == F & !all(is.na(dist.mat[a,])) &
        sample(1:2, 1, prob = c(1 - dfAgents$fleeTimeLeft[a] / (dfABM$refractory * 24),
                                (abs(dfABM$refractory - (1 / (dfABM$refractory * 24)))))) == 1) {
      df$potentialMate[a] <- which.min(dist.mat[a,])[[1]]
      df$chasing[a] <- T
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

