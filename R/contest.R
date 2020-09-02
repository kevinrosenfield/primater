
challenge <- function(df = distances) {
  matches = list()
  for (d in df) {
  newMatches = c(min(d[d > 0]), match(min(d[d > 0]), d))
  matches <- c(matches, newMatches)
  print(newMatches)
  }
  #return(matches)
}

contest <- function(agent1 = agent1, agent2 = agent2) {
  winProbs <- c((agent1$Mass + agent2$Mass) / agent1$Mass, (agent1$Mass + agent2$Mass) / agent2$Mass)
  return(paste("agent", sample(1:2, 1, prob = c(winProbs)), sep = ""))
}
