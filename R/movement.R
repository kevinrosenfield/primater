# direct agents to move according to the XY direction they are facing (heading1) and their hourly rate of movement
# agents' age increases by 1 hour (1/365/24) during every time step

move2D <- function(df = dfAgents) {
  df$xCor = df$xCor + (df$metersPerHour * cos(df$Heading1))
  df$yCor = df$yCor + (df$metersPerHour * sin(df$Heading1))
  df$Age = df$Age + 0.0001141553
  return(df)
}

# movement in three dimensions requires two angles: XY and a YZ headings, but still only one distance

move3D <- function(df = dfAgents) {
  df$xCor = df$xCor + (df$metersPerHour * cos(df$Heading2) * sin(df$Heading1))
  df$yCor = df$yCor + (df$metersPerHour * sin(df$Heading2))
  df$zCor = df$zCor + (df$metersPerHour * cos(df$Heading2) * cos(df$Heading1))
  df$Age = df$Age + 0.0001141553
  return(df)
  }
