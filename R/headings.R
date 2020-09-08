

setHeading2D <- function(df = dfAgents, numberAgents = dfABM$numberAgents) {
  df$Heading1 <- ifelse(df$distFromHome < df$homeRangeRadius - df$metersPerHour,
                        df$Heading1 + rnorm(numberAgents, 0, 20), setHeadingHome(df$Heading1))
  df$Heading1 = ifelse(df$Heading1 >= 360 | df$Heading1 < 0, abs(abs(df$Heading1) - 360), df$Heading1)
  return(df)
}

setHeading3D <- function(df = dfAgent, numberAgents = dfABM$numberAgents) {
  df$Heading1 = df$Heading1 + rnorm(numberAgents, 0, 20)
  df$Heading2 = df$Heading2 + rnorm(numberAgents, 0, 20)
  df$Heading1 = ifelse(df$Heading1 > 360 | df$Heading1 < 0, abs(abs(df$Heading1) - 360), df$Heading1)
  df$Heading2 = ifelse(df$Heading2 > 360 | df$Heading2 < 0, abs(abs(df$Heading2) - 360), df$Heading2)
  return(df)
}

# turn around if agent has left the world

reverseHeading2D <- function(df = dfAgents) {
  for (agent in df$agentID) {
    while (df$homeRangeRadius[agent] < abs(df$xCorOrigin[agent] - df$xCor[agent]) |
           df$homeRangeRadius[agent] < abs(df$yCorOrigin[agent] - df$yCor[agent])) {
      print("true")
      df$xCor[agent] <-  df$xCor[agent] - (df$metersPerHour[agent] * cos(df$Heading1[agent]))
      df$yCor[agent] <-  df$yCor[agent] - (df$metersPerHour[agent] * sin(df$Heading1[agent]))
      df$Heading1[agent] <- ifelse(df$Heading1[agent] > 180, df$Heading1[agent] - 180, df$Heading1[agent] + 180)
    }
  }
  return(df)
}

reverseHeading3D <- function(df = dfAgents) {
  df$Heading1 <- ifelse(df$homeRangeRadius < abs(df$xCorOrigin - df$xCor) | abs(df$homeRangeRadius) < abs(df$yCorOrigin - df$yCor),
                        ifelse(df$Heading1 > 180, df$Heading1 - 180, df$Heading1 + 180), df$Heading1)
  df$Heading2 <- ifelse(df$homeRangeRadius < abs(df$xCorOrigin - df$xCor) | abs(df$homeRangeRadius) < abs(df$yCorOrigin - df$yCor),
                        ifelse(df$Heading2 > 180, df$Heading2 - 180, df$Heading2 + 180), df$Heading2)
  return(df)
}

setHeadingHome <- function(heading = df$Heading) {

}
