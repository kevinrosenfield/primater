

setHeading2D <- function(df = dfAgent, numberAgents) {
  df$Heading1 = df$Heading1 + rnorm(numberAgents, 0, 20)
  df$Heading1 = ifelse(df$Heading1 > 360 | df$Heading1 < 0, abs(abs(df$Heading1) - 360), df$Heading1)
  return(df$Heading1)
}

setHeading3D <- function(df = dfAgent, numberAgents) {
  df$Heading1 = df$Heading1 + rnorm(numberAgents, 0, 20)
  df$Heading2 = df$Heading2 + rnorm(numberAgents, 0, 20)
  df$Heading1 = ifelse(df$Heading1 > 360 | df$Heading1 < 0, abs(abs(df$Heading1) - 360), df$Heading1)
  df$Heading2 = ifelse(df$Heading2 > 360 | df$Heading2 < 0, abs(abs(df$Heading2) - 360), df$Heading2)
  return(c(df$Heading1, df$Heading2))
}

# turn around if agent has left the world

reverseHeading2D <- function(df = dfAgent) {
  df$Heading1 = ifelse(df$Heading1 > 180, df$Heading1 - 180, df$Heading1 + 180)
  return(df$Heading1)
}

reverseHeading3D <- function(df = dfAgent, i. = i) {
  df$Heading1 = ifelse(df$Heading1 > 180, df$Heading1 - 180, df$Heading1 + 180)
  df$Heading2 = ifelse(df$Heading2 > 180, df$Heading2 - 180, df$Heading2 + 180)
  return(c(df$Heading1, df$Heading2))
}
