
setup <- function(dimensions = 2, numberAgents = sample(2:100, 1), worldDiameter = runif(1, 25, 1000), liveInGroup = T,
                  maleRangeProp = 0.25, dayRangeProp = 0.1, refractory = 1/365, fleeTime = .25, numberMales = NA,
                  numberPatches = runif(n = 1, min = 1, max = 10), sitesPerPatch = round(runif(n = 1, min = 1, max = 10)),
                  patchSpread = runif(1, 0, 100)) {
  dfABM <<- setupABM(dimensions, numberAgents, worldDiameter, liveInGroup, maleRangeProp, dayRangeProp, refractory, fleeTime)
  dfResources <<- setupResources(numberPatches, sitesPerPatch, patchSpread)
  dfAgents <<- setupAgents(df = dfABM, numberMales)
}


setupResources <- function(patchNumber, sitesPerPatch, patchSpread) {
  xFeedingSites <- c()
  yFeedingSites <- c()
  patchRadius <- dfABM$worldRadius * patchSpread
  for (p in 1:patchNumber) {
    xPatch <- runif(1, 0 - dfABM$worldRadius, 0 + dfABM$worldRadius)
    yPatch <- runif(1, 0 - dfABM$worldRadius, 0 + dfABM$worldRadius)
    while (sqrt((0 - xPatch)^2 + (0 - yPatch)^2) > dfABM$worldRadius - patchRadius) {
      xPatch <- runif(1, 0 - dfABM$worldRadius, 0 + dfABM$worldRadius)
      yPatch <- runif(1, 0 - dfABM$worldRadius, 0 + dfABM$worldRadius)
    }
    sitesInThisPatch <- abs(round(rnorm(1, sitesPerPatch, sitesPerPatch / 3)))
    for (s in 1:sitesPerPatch) {
      xFeedingSite <- runif(1, xPatch - patchRadius, xPatch + patchRadius)
      yFeedingSite <- runif(1, yPatch - patchRadius, yPatch + patchRadius)
      while (sqrt((xPatch - xFeedingSite)^2 + (yPatch - yFeedingSite)^2) > patchRadius) {
        xFeedingSite <- runif(1, xPatch - patchRadius, xPatch + patchRadius)
        yFeedingSite <- runif(1, yPatch - patchRadius, yPatch + patchRadius)
      }
      xFeedingSites <- append(xFeedingSites, xFeedingSite)
      yFeedingSites <- append(yFeedingSites, yFeedingSite)
    }
  }
  feedingSites <- as.data.frame(bind_cols("x" = xFeedingSites, "y" = yFeedingSites))
}


go <- function(reps = 10, GIF = F, plot = F, contestPlot = F, matingPlot = F, reach = 10, sight = 100, sinuosity = 20) {
  if (GIF == T) {
    wd <- getwd()
    setwd("/Users/kevinrosenfield/Box/PSU/Dissertation/New dissertation/Figures")
  }
  if (contestPlot == T | matingPlot == T) {
    if (Sys.info()[['sysname']] == 'Windows') {
      x11(height = 5, width = 5)
    } else {
      quartz(height = 5, width = 5)
    }
  }
  if (plot == T ) {
    quartzPoint <- ifelse(dfABM$groupLiving == T, 1, 13)
    if (Sys.info()[['sysname']] == 'Windows') {
      x11(pointsize = quartzPoint * 1.4)
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
      fig <- plot(c(dfAgents$xCorOrigin, dfAgents$xCor, xCorsCompete, FeedingSites$x),
                  c(dfAgents$yCorOrigin, dfAgents$yCor, yCorsCompete, FeedingSites$y),
                  pch = c(rep(21, length(cexSizes)), shapes, rep(4, length(xCorsCompete)), 21, length(FeedingSites$x))),
                  cex = c(cexSizes,
                          rep(agentConstant, dfABM$numberAgents), rep(agentConstant * 2, length(xCorsCompete)),
                          rep(agentConstant * 2, length(FeedingSites$x)),
                  col = c(rep(rgb(0, 1, 0, alpha = 0.5), dfABM$numberAgents),
                          dfAgents$Sex, rep("black", length(xCorsCompete)), rep(agentConstant * 2, length(FeedingSites$x)),
                  bg = c(rep(rgb(0, 1, 0, alpha = 0.5), dfABM$numberAgents),
                         dfAgents$Sex, rep(NA, length(xCorsCompete)), rep(NA * 2, length(FeedingSites$x)),
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
