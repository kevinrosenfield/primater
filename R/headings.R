
setHeading <- function(df = dfAgents, numberAgents = dfABM$numberAgents, sinuosity = sinuosity){
  if (dfABM$dimensions == 2) {
    df <- setHeading2D(df, numberAgents, sinuosity)
  } else {
    df <- setHeading3D(df, numberAgents, sinuosity) }
  return(df)
}

setHeading2D <- function(df = dfAgents, numberAgents = dfABM$numberAgents, sinuosity = sinuosity) {
  df$Heading1 <- ifelse(df$distFromHome < df$homeRangeRadius - df$metersPerHour,
                        ifelse(df$chasing == T, df$Heading1,
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

faceHome <- function(xCor = df$xCor, xCorOrigin = df$xCorOrigin, yCor = df$yCor, yCorOrigin = df$yCorOrigin) {
  angle <- complex(real = xCorOrigin - xCor, imaginary = yCorOrigin - yCor)
  df$Heading1 <- Arg(angle) / base::pi * 180
  return(df$Heading1)
}
