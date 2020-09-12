seekMate <- function() {
  dist.mat[dist.mat > reach | dist.mat == 0] <- NA
  for (a in 1:dfABM$numberAgents) {
    if (df$Sex[a] == "M") {
      dist.mat[,a] <- NA
    } else {
      dist.mat[a,] <- NA
    }
  }
  for (a in 1:dfABM$numberAgents) {
    if (!all(is.na(dist.mat[a,]))) {
      potentialMate <- which.min(dist.mat[a,])[[1]]
    }
  }
  return(df)
}

