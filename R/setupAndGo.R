
setup <- function(dimensions = 2, numberAgents = sample(2:100, 1), worldDiameter = runif(1, 25, 1000),
                  liveInGroup = T, maleRangeProp = 0.25, dayRangeProp = 0.1, refractory = 1/365, fleeTime = 1) {
  dfABM <<- setupABM(dimensions, numberAgents, worldDiameter, liveInGroup, maleRangeProp, dayRangeProp, refractory, fleeTime)
  dfAgents <<- setupAgents(df = dfABM)
}

go <- function(reps = 10, GIF = F, plot = F, contestPlot = F, matingPlot = F, reach = 10, sight = 100, sinuosity = 20) {
  if (GIF == T) {
    wd <- getwd()
    setwd("/Users/kevinrosenfield/Box/PSU/Dissertation/New dissertation/Figures")
  }
  if (contestPlot == T | matingPlot == T) {
    if (Sys.info()[['sysname']] == 'Windows') {
      x11(pointsize = quartzPoint)
    } else {
      quartz(height = 5, width = 5)
    }
  }
  if (plot == T ) {
    quartzPoint <- ifelse(dfABM$groupLiving == T, 1, 13)
    print(Sys.info()[['sysname']])
    if (Sys.info()['sysname'] == 'Windows') {
      x11(pointsize = quartzPoint)
    } else {
      quartz(pointsize = quartzPoint)
    }
  }
  for (i in 1:reps) {
    distances <<- findNeighbors()
    dfAgents <<- move(sinuosity = sinuosity)
    dfAgents <<- seekMate(sight = sight)
    dfAgents <<- interact(reach = reach)
    if (contestPlot == T) {
      cPlot <- plot(dfAgents$winRatio[dfAgents$Sex == "M"] ~ dfAgents$Mass[dfAgents$Sex == "M"])
      abline(lm(dfAgents$winRatio[dfAgents$Sex == "M"] ~ dfAgents$Mass[dfAgents$Sex == "M"]))

    }
    if (matingPlot == T) {
      mPlot <- plot(dfAgents$Mates[dfAgents$Sex == "M"] ~ dfAgents$Attractiveness[dfAgents$Sex == "M"])
      abline(lm(dfAgents$Mates[dfAgents$Sex == "M"] ~ dfAgents$Attractiveness[dfAgents$Sex == "M"]))
    }
    palette(c("red", "blue"))
    agentConstant <- ifelse(dfABM$groupLiving == T, 10, .5)
    legendConstant <- ifelse(dfABM$groupLiving == T, 15, 1)
    axisConstant <- ifelse(dfABM$groupLiving == T, 1, 1.5)
    shapes = c(21, 21, 1)
    shapes <- shapes[sign(dfAgents$fleeTimeLeft) + 2]
    if (plot == T) {
      fig <- plot(c(dfAgents$xCorOrigin, dfAgents$xCor, xCorsCompete),
                  c(dfAgents$yCorOrigin, dfAgents$yCor, yCorsCompete),
                  pch = c(rep(21, length(cexSizes)), shapes, rep(4, length(xCorsCompete))),
                  cex = c(cexSizes,
                          rep(agentConstant, dfABM$numberAgents), rep(agentConstant * 2, length(xCorsCompete))),
                  col = c(rep(rgb(0, 1, 0, alpha = 0.5), dfABM$numberAgents),
                          dfAgents$Sex, rep("black", length(xCorsCompete))),
                  bg = c(rep(rgb(0, 1, 0, alpha = 0.5), dfABM$numberAgents),
                         dfAgents$Sex, rep(NA, length(xCorsCompete))),
                  xlim=c(0 - (dfABM$worldRadius * axisConstant), (dfABM$worldRadius * axisConstant)),
                  ylim=c(0 - (dfABM$worldRadius * axisConstant), (dfABM$worldRadius * axisConstant)))
      legend("topright", legend=c("Female", "Male"), col=c("red", "blue"), lty=1:1, cex=1 * legendConstant)
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

clearModel <- function() {
  suppressWarnings({rm(dfAgents, pos = ".GlobalEnv")})
  suppressWarnings({rm(dfABM, pos = ".GlobalEnv")})
  if (length(dev.list()) > 0) {for (d in 1:length(dev.list()) - 1) { dev.off() } }
}
