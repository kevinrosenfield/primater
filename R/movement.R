move2D <- function(df = dfAgent) {
  df$xCor = df$xCor + (df$metersPerHour * cos(df$Heading1))
  df$yCor = df$yCor + (df$metersPerHour * sin(df$Heading1))
  return(c(df$xCor, df$yCor))
}

move3D <- function(df = dfAgent) {
  df$xCor = df$xCor + (df$metersPerHour * cos(df$Heading2 * (pi / 180)) * sin(df$Heading1 * (pi / 180)))
  df$yCor = df$yCor + (df$metersPerHour * sin(df$Heading2 * (pi / 180)))
  df$zCor = df$zCor + (df$metersPerHour * cos(df$Heading2 * (pi / 180)) * cos(df$Heading1 * (pi / 180)))
  return(c(df$xCor, df$yCor, df$zCor))
  }
