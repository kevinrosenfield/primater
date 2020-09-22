# direct agents to move according to the XY direction they are facing (heading1) and their hourly rate of movement
# agents' age increases by 1 hour (1/365/24) during every time step

move <- function(df = dfAgents, sinuosity = sinuosity){
  df$distFromHome <- lookHome(df)
  df <- setHeading(df, dfABM$numberAgents, sinuosity)
  df <- if (dfABM$dimensions == 2) { move2D(df) } else { move3D(df) }
  df$fleeTimeLeft <- df$fleeTimeLeft - (1 / (dfABM$fleeTime * 24))
  df$Age = df$Age + 0.0001141553
  df$sinceLastMate <- df$sinceLastMate + 0.0001141553
  return(df)
}

move2D <- function(df) {
  df$xCor = ifelse(df$feeding == F, df$xCor + (df$metersPerHour * cos((df$Heading1) * (pi / 180))), df$xCor)
  df$yCor = ifelse(df$feeding == F, df$yCor + (df$metersPerHour * sin((df$Heading1) * (pi / 180))), df$yCor)
  df$feeding <- F
  return(df)
}

# movement in three dimensions requires two angles: XY and a YZ headings, but still only one distance

move3D <- function(df = dfAgents) {
  df$xCor = df$xCor + (df$metersPerHour * cos((df$Heading2) * sin(df$Heading1) * (pi / 180)))
  df$yCor = df$yCor + (df$metersPerHour * sin((df$Heading2) * (pi / 180)))
  df$zCor = df$zCor + (df$metersPerHour * cos((df$Heading2) * cos(df$Heading1) * (pi / 180)))
  return(df)
}


setHeading <- function(df = dfAgents, numberAgents = dfABM$numberAgents, sinuosity = sinuosity){
  if (dfABM$dimensions == 2) {
    df <- setHeading2D(df, numberAgents, sinuosity)
  } else {
    df <- setHeading3D(df, numberAgents, sinuosity) }
  return(df)
}

setHeading2D <- function(df = dfAgents, numberAgents = dfABM$numberAgents, sinuosity = sinuosity) {
  df$Heading1 <- ifelse(df$distFromHome < df$homeRangeRadius - df$metersPerHour,
                        ifelse(df$chasing == T | !is.na(df$potentialFeedingSite), df$Heading1,
                               df$Heading1 + rnorm(numberAgents, 0, sinuosity)),
                        Arg(complex(real = df$xCorOrigin - df$xCor, imaginary = df$yCorOrigin - df$yCor)) /base::pi * 180)
  df$Heading1 <- ifelse(df$Heading1 >= 360 | df$Heading1 < 0, abs(abs(df$Heading1) - 360), df$Heading1)
  return(df)
}

setHeading3D <- function(df = dfAgents, numberAgents = dfABM$numberAgents, sinuosity = sinuosity) {

  df$Heading1 <- ifelse(df$distFromHome < df$homeRangeRadius - df$metersPerHour,
                        ifelse(df$chasing == T, df$Heading1,
                               df$Heading1 + rnorm(numberAgents, 0, sinuosity)), df$Heading1 - 180)
  df$Heading1 <- ifelse(df$Heading1 >= 360 | df$Heading1 < 0, abs(abs(df$Heading1) - 360), df$Heading1)
  df$Heading2 <- ifelse(df$distFromHome < df$homeRangeRadius - df$metersPerHour,
                        ifelse(df$chasing == T, df$Heading1,
                               df$Heading2 + rnorm(numberAgents, 0, sinuosity)), df$Heading2 - 180)
  df$Heading2 <- ifelse(df$Heading2 >= 360 | df$Heading2 < 0, abs(abs(df$Heading2) - 360), df$Heading2)
  return(df)
}


lookHome <- function(df = dfAgents){
  df$distFromHome <- if(dfABM$dimensions == 2) { lookHome2D(df) } else { lookHome3D(df) }
  return(df$distFromHome)
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


faceHome <- function(xCor = df$xCor, xCorOrigin = df$xCorOrigin, yCor = df$yCor, yCorOrigin = df$yCorOrigin) {
  angle <- complex(real = xCorOrigin - xCor, imaginary = yCorOrigin - yCor)
  df$Heading1 <- Arg(angle) / base::pi * 180
  return(df$Heading1)
}


