
setHeading <- function(df = dfAgents, numberAgents = dfABM$numberAgents){
  df <- ifelse(dfABM$dimensions == 2, setHeading2D(df, numberAgents), setHeading3D(df, numberAgents))
  return(df)
}

setHeading2D <- function(df = dfAgents, numberAgents = dfABM$numberAgents) {
  df$Heading1 <- ifelse(df$distFromHome < df$homeRangeRadius - df$metersPerHour,
                        df$Heading1 + rnorm(numberAgents, 0, 20), df$Heading1 - 180)
  df$Heading1 = ifelse(df$Heading1 >= 360 | df$Heading1 < 0, abs(abs(df$Heading1) - 360), df$Heading1)
  return(df)
}

setHeading3D <- function(df = dfAgents, numberAgents = dfABM$numberAgents) {
  df$Heading1 <- ifelse(df$distFromHome < df$homeRangeRadius - df$metersPerHour,
                        df$Heading1 + rnorm(numberAgents, 0, 20), df$Heading1 - 180)
  df$Heading1 = ifelse(df$Heading1 >= 360 | df$Heading1 < 0, abs(abs(df$Heading1) - 360), df$Heading1)
  df$Heading2 <- ifelse(df$distFromHome < df$homeRangeRadius - df$metersPerHour,
                        df$Heading2 + rnorm(numberAgents, 0, 20), df$Heading2 - 180)
  df$Heading2 = ifelse(df$Heading2 >= 360 | df$Heading2 < 0, abs(abs(df$Heading2) - 360), df$Heading2)
  return(df)
}
