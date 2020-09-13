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
    if (!all(is.na(dist.mat[a,])) &
        sample(1:2, 1, prob = c(df$sinceLastMate[a] / dfABM$refractory,
                              (abs(dfABM$refractory - df$sinceLastMate[a] / dfABM$refractory)))) == 1) {
      potentialMate <- which.min(dist.mat[a,])[[1]]
      xCorGoal <- df$xCor[potentialMate]
      yCorGoal <- df$yCor[potentialMate]
      zCorGoal <- df$yCor[potentialMate]
      angle <- complex(real = xCorGoal - df$xCor[a], imaginary = yCorGoal - df$yCor[a]) # need to adapt this to 3D
      df$Heading1[a] <- Arg(angle) / base::pi * 180
      df$chasing[a] <- T
    } else {
      df$chasing[a] <- F
    }
  }
  return(df)
}

