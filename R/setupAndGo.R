
setup <- function(dimensions = 2, numberAgents = sample(2:100, 1), worldDiameter = runif(1, 25, 1000), liveInGroup = T,
                  maleRangeProp = 0.25, dayRangeProp = 0.1, refractory = 1/365, fleeTime = .25, numberMales = NA,
                  numberPatches = runif(n = 1, min = 3, max = 10), sitesPerPatch = round(runif(n = 1, min = 5, max = 10)),
                  patchSpread = runif(1, 0, 100) / 100, siteHourlyEnergy = 200, siteMaxEnergy = 1000,
                  energyNeedsPerKilo = 25, siteHourlyEnergyGrowth = 2) {
  dfABM <<- setupABM(dimensions, numberAgents, worldDiameter, liveInGroup, maleRangeProp,
                     dayRangeProp, refractory, fleeTime, siteHourlyEnergy, energyNeedsPerKilo,
                     siteHourlyEnergyGrowth, siteMaxEnergy)
  dfResources <<- setupResources(numberPatches, sitesPerPatch, patchSpread, dfABM$siteMaxEnergy)
  dfAgents <<- setupAgents(df = dfABM, numberMales, energyNeedsPerKilo)
}


go <- function(reps = 100, GIF = F, plot = T, contestPlot = F, matingPlot = F, reach = 10, sight = 100, sinuosity = 30, siteMaxEnergy = dfABM$siteMaxEnergy) {
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
    dfAgents$energyNeedsRemaining <<- ifelse(rep(dfABM$hour == 0, dfABM$numberAgents), dfAgents$energyNeedsRemaining +
                                              dfAgents$myDailyEnergyNeeds, dfAgents$energyNeedsRemaining)
    distances <<- findNeighbors()
    dfAgents <<- move(sinuosity = sinuosity)
    dfAgents <<- seekFood(sight = sight)
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
    palette(c("white", "blue"))
    agentConstant <- ifelse(dfABM$groupLiving == T, 10, .5)
    legendConstant <- ifelse(dfABM$groupLiving == T, 15, 1)
    axisConstant <- ifelse(dfABM$groupLiving == T, 1, 1.5)
    shapes = c(21, 21, 1)
    shapes <- shapes[sign(dfAgents$fleeTimeLeft) + 2]
    if (plot == T) {
      fig <- plot(c(dfAgents$xCorOrigin, dfResources$x, dfAgents$xCor, xCorsCompete),
                  c(dfAgents$yCorOrigin, dfResources$y,  dfAgents$yCor, yCorsCompete),
                  pch = c(rep(21, length(cexSizes)), rep(21, length(dfResources$x)), shapes, rep(4, length(xCorsCompete))),
                  cex = c(cexSizes, rep(agentConstant, length(dfResources$x)),
                          rep(agentConstant, dfABM$numberAgents), rep(agentConstant * 2, length(xCorsCompete))),
                  col = c(rep(rgb(0.5, 0.3, 0, alpha = 0.5), dfABM$numberAgents), rep("green", length(dfResources$x)),
                          dfAgents$Sex, rep("black", length(xCorsCompete))),
                  bg = c(rep(rgb(0.5, 0.3, 0, alpha = 0.5), dfABM$numberAgents), rep("green", length(dfResources$x)),
                         dfAgents$Sex, rep(NA, length(xCorsCompete))),
                  xlim=c(0 - (dfABM$worldRadius * axisConstant), (dfABM$worldRadius * axisConstant)),
                  ylim=c(0 - (dfABM$worldRadius * axisConstant), (dfABM$worldRadius * axisConstant)))
      #points(dfABM$worldRadius, 0, cex = agentConstant * 5)
      legend("topright", legend=c("Female", "Male", "Food"), col=c("white", "blue", "green"),
             lty=1:1, cex=1 * legendConstant)
    }
    if (GIF == T) {
      png(file = paste("fig", i, ".png", sep = ""))
      dev.off()
    }
    dfABM$hour <<-  ifelse(dfABM$hour == 23, 0, dfABM$hour + 1)
    dfABM$day <<- ifelse(dfABM$hour == 0, ifelse(dfABM$day == 364, 0, dfABM$day + 1), dfABM$day)
    dfABM$year <<- ifelse(dfABM$day == 0, dfABM$year + 1, dfABM$year)
    dfResources$energyRemaining <<- dfResources$energyRemaining + dfABM$siteHourlyEnergyGrowth
    dfResources$energyRemaining[dfResources$energyRemaining > dfABM$siteMaxEnergy] <<- dfABM$siteMaxEnergy
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
