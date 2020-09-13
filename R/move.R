# direct agents to move according to the XY direction they are facing (heading1) and their hourly rate of movement
# agents' age increases by 1 hour (1/365/24) during every time step

move <- function(df = dfAgents, sinuosity = sinuosity){
  df$distFromHome <- lookHome(df = df)
  df <- setHeading(df = df, numberAgents = dfABM$numberAgents, sinuosity = sinuosity)
  df <- if (dfABM$dimensions == 2) { move2D(df) } else { move3D(df) }
  xCorsMale <<- append(xCors, dfAgents$xCor[dfAgents$Sex == "M"])
  yCorsMale <<- append(yCors, dfAgents$yCor[dfAgents$Sex == "M"])
  zCorsMale <<- append(zCors, dfAgents$zCor[dfAgents$Sex == "M"])
  xCorsFemale <<- append(xCors, dfAgents$xCor[dfAgents$Sex == "F"])
  yCorsFemale <<- append(yCors, dfAgents$yCor[dfAgents$Sex == "F"])
  zCorsFemale <<- append(zCors, dfAgents$zCor[dfAgents$Sex == "F"])
  df$Age = df$Age + 0.0001141553
  df$sinceLastMate <- df$sinceLastMate + 0.0001141553
  return(df)
}

lookHome <- function(df = dfAgents){
  df$distFromHome <- if(dfABM$dimensions == 2) { lookHome2D(df) } else { lookHome3D(df) }
  return(df$distFromHome)
}

move2D <- function(df = dfAgents) {
  df$xCor = df$xCor + (df$metersPerHour * cos((df$Heading1) * (pi / 180)))
  df$yCor = df$yCor + (df$metersPerHour * sin((df$Heading1) * (pi / 180)))
  return(df)
}

# movement in three dimensions requires two angles: XY and a YZ headings, but still only one distance

move3D <- function(df = dfAgents) {
  df$xCor = df$xCor + (df$metersPerHour * cos((df$Heading2) * sin(df$Heading1) * (pi / 180)))
  df$yCor = df$yCor + (df$metersPerHour * sin((df$Heading2) * (pi / 180)))
  df$zCor = df$zCor + (df$metersPerHour * cos((df$Heading2) * cos(df$Heading1) * (pi / 180)))
  return(df)
}

lookHome2D <- function(df = dfAgents) {
  df$distFromHome <- sqrt((dfAgents$xCor - dfAgents$xCorOrigin)^2 + (dfAgents$yCor - dfAgents$yCorOrigin)^2)
  return(df$distFromHome)
}

lookHome3D <- function(df = dfAgents) {
  df$distFromHome <- sqrt((dfAgents$xCor - dfAgents$xCorOrigin)^2 + (dfAgents$yCor - dfAgents$yCorOrigin)^2 +
                            (dfAgents$zCor - dfAgents$zCorOrigin)^2)
  return(df$distFromHome)
}
