
setup <- function(dimensions = 2, numberAgents =  sample(2:100, 1), worldDiameter = runif(1, 25, 1000), liveInGroup = T) {
  dfABM <<- setupABM(dimensions = dimensions, numberAgents = numberAgents,
                     worldDiameter = worldDiameter, liveInGroup = liveInGroup)
  dfAgents <<- setupAgents(df = dfABM)

  reps <<-  10
  xCors <<- list()
  yCors <<- list()
  zCors <<- list()
}

go <- function(reps = reps, GIF = F, plot = F, contestPlot = F, reach = 100) {
  if (GIF == T) {
    wd <- getwd()
    setwd("/Users/kevinrosenfield/Box/PSU/Dissertation/New dissertation/Figures")
  }
  if (contestPlot == T) {
    quartz()
  }
  if (plot == T ) {
    quartzPoint <- ifelse(dfABM$groupLiving == T, 1, 18)
    quartz(pointsize = quartzPoint)
  }
  for (i in 1:reps) {
    distances <<- findNeighbors()
    dfAgents <<- move()
    dfAgents <<- interact(reach = reach)
    if (contestPlot == T) {
      cPlot <- plot(dfAgents$winRatio ~ dfAgents$Mass)
    }
    if (plot == T) {
      omit <- length(xCors) - dfABM$numberAgents
      fig <- plot(c(dfAgents$xCorOrigin, xCors[-c(1:omit)]),
                  c(dfAgents$yCorOrigin, yCors[-c(1:omit)]),
                  pch = 21,
                  cex = c(cexSizes,rep(.2, length(xCors[-c(1:omit)]))),
                  col = c(rep(rgb(0, 0, 1, alpha = 0.5), dfABM$numberAgents),
                          rep(rgb(1, 0, 0, alpha = 0.5),  length(xCors[-c(1:omit)]))),
                  bg = c(rep(rgb(0, 1, 0, alpha = 0.5), dfABM$numberAgents),
                         rep(rgb(0, 1, 0, alpha = 0.5),  length(xCors[-c(1:omit)]))),
                  xlim=c(0 - dfABM$worldRadius, dfABM$worldRadius), ylim=c(0 - dfABM$worldRadius, dfABM$worldRadius))
    }
    if (GIF == T) {
      png(file = paste("fig", i, ".png", sep = ""))
      dev.off()
    }
  }
  if (GIF == T) {
    system("convert -delay 10 *.png example_2_reduced.gif")
    setwd(wd)
  }
}
