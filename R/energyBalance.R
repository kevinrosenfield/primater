seekFood <- function() {
  for (a in 1:numberAgents) {
    dfResources$euclideanDist <- sqrt((dfResources$x - dfAgents$xCor[a])^2 + (dfResources$y - dfAgents$yCor[a])^2)
    dfResources$euclideanDist[dfResources$euclideanDist > sight] <- NA
    dfResources$displaceFeederProb <- 
  closestSite <- 
}

consumeEnergy <- function() {
  
}

expendEnergy <- function() {
  
}