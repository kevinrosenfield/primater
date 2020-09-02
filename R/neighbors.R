
distances2D <- function(df = dfAgents, numberAgents = model$numberAgents) {
  neighbors <- matrix(c(df$agentID, df$xCor, df$yCor), numberAgents, 3)
  for (i in 1:numberAgents) {
    sqrt((neighbors[,2][i] - neighbors[,2])^2 + (neighbors[,3][i] - neighbors[,3])^2)
  }
  return(neighbors)
}

distances3D <- function(df = dfAgent, numberAgents = model$numberAgents) {
  neighbors <- matrix(c(df$agentID, df$xCor, df$yCor, df$zCor), numberAgents, 4)
  for (i in 1:numberAgents) {
    sqrt((neighbors[,2][i] - neighbors[,2])^2 + (neighbors[,3][i] - neighbors[,3])^2 + (neighbors[,4][i] - neighbors[,4])^2)
    }
  return(neighbors)
  }
