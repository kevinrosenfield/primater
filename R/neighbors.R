
distances2D <- function(df = dfAgents, numberAgents = dfABM$numberAgents) {
  neighbors <- matrix(c(df$agentID, df$xCor, df$yCor), numberAgents, 3)
  distances = matrix(ncol = numberAgents)
  for (i in 1:numberAgents) {
    distancesAgent <- c(sqrt((neighbors[,2][i] - neighbors[,2])^2 + (neighbors[,3][i] - neighbors[,3])^2))
    distances <- rbind(distances, distancesAgent)
  }
  distances <- distances[-1,]
  row.names(distances) <- c(1:numberAgents)
  colnames(distances) <- c(1:numberAgents)
  return(distances)
}

distances3D <- function(df = dfAgents, numberAgents = dfABM$numberAgents) {
  neighbors <- matrix(c(df$agentID, df$xCor, df$yCor, df$zCor), numberAgents, 4)
  distances = list()
  for (i in 1:numberAgents) {
    distancesAgent <- list(sqrt((neighbors[,2][i] - neighbors[,2])^2 + (neighbors[,3][i] - neighbors[,3])^2 +
                                  (neighbors[,4][i] - neighbors[,4])^2))
    distances <- c(distances, distancesAgent)
    }
  return(distances)
  }
