
findContests <- function(dist = distances, df = dfAgents) {
  challengers = list()
  i = 1
  for (d in dist) {
    newChallengers = c(i, min(d[d > 0]), round(match(min(d[d > 0]), d), 0))
    if(newChallengers[2] < 50)  {
      challenge <- c(dfAgents[dfAgents$agentID == i,c("agentID","Mass")], dfAgents[newChallengers[3], c("agentID","Mass")])
      challengers <- append(challengers, list(challenge))
      }
    i = i + 1
  }
  challengers <- data.frame(t(matrix(unlist(challengers), ncol = length(challengers))))
  colnames(challengers) <- c("agent1", "Mass1", "agent2", "Mass2")
  dupes <- data.frame(t(apply(challengers[c(1,3)], 1, sort))) %>% unique() %>% select(X2)
  challengers <- challengers %>% filter(agent1 %in% dupes$X2)
  return(challengers)
}

contests <- function(dfChallengers = challengers, df = dfAgents) {
  winners = list()
  losers = list
  winProbs <- data.frame(dfChallengers$Mass1 / (dfChallengers$Mass1 + dfChallengers$Mass2),
                         dfChallengers$Mass2 / (dfChallengers$Mass1 + dfChallengers$Mass2))
  for (n in 1:length(dfChallengers$agent1)) {
    winners <- append(winners, ifelse(sample(1:2, 1, prob = winProbs[n,]) == 1, dfChallengers$agent1[n], dfChallengers$agent2[n]))
  }
  winners <- unlist(winners)
  losers <- ifelse(winners == dfChallengers$agent1, dfChallengers$agent2, dfChallengers$agent1)
  outcomes <- data.frame(winners, losers)
  for (w in winners) {df$Wins[dfAgents$agentID == w] <- df$Wins[df$agentID == w] + 1}
  for (l in losers) {df$Losses[dfAgents$agentID == l] <- df$Losses[df$agentID == l] + 1}
  return(df)
}
