# findContests() - males determine which other male is closest to themselves, and challenge them if under a specified threshold
# i tracks agentIds, min(d[d > 0]) find the smallest distance in the list of distances for a given individual
# round(match(min(d[d > 0]), d), 0) gives the position number (agentID) corresponding to the smallest distance
# if statement excludes nearest neighbors if either individual is female, or if the distance is above a specified threshold
# challenge stores relevant information. Here, agentID and Mass for both competitions
# challengers makes a lists of all contests to process; further code cleans up the format and removes duplicate contests

findContests <- function(dist = distances, df = dfAgents) {
  challengers = list()
  i = 1
  for (d in dist) {
    newChallengers = c(i, min(d[d > 0]), round(match(min(d[d > 0]), d), 0))
    if(newChallengers[2] < 300 &
       dfAgents$Sex[dfAgents$agentID == newChallengers[1]] == "M" &
       dfAgents$Sex[dfAgents$agentID == newChallengers[3]] == "M")  {
      challenge <- c(dfAgents[dfAgents$agentID == i,c("agentID","Mass")], dfAgents[newChallengers[3], c("agentID","Mass")])
      challengers <- append(challengers, list(challenge))
      }
    i = i + 1
  }
  if (length(challengers) < 1) {
    stop()
  } else {
    challengers <- data.frame(t(matrix(unlist(challengers), ncol = length(challengers))))
    colnames(challengers) <- c("agent1", "Mass1", "agent2", "Mass2")
    dupes <- data.frame(t(apply(challengers[c(1,3)], 1, sort))) %>% unique()
    print(dupes[2])
    print(challengers) # something is wrong btwn here and end
    challengers %>% filter(agent1 %in% dupes[2])
    challengers <- challengers %>% filter(agent1 %in% dupes[2])
    return(challengers)
  }
}

# contests - contests in the challengers list are processed
# the variable(s) that influence contest outcome, here Mass, are weighted and entered as outcome probabilities
# wins and losses are applied to their respective columns in dfAgents and an updated dfAgents is returned

contests <- function(dfChallengers = challengers, df = dfAgents) {
  winners = list()
  losers = list()
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
