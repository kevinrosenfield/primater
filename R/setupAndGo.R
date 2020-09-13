
setup <- function(dimensions = 2, numberAgents =  sample(2:100, 1), worldDiameter = runif(1, 25, 1000),
                  liveInGroup = T, maleRangeProp = 0.25, dayRangeProp = 0.1, refractory = 1/365) {
  dfABM <<- setupABM(dimensions, numberAgents, worldDiameter, liveInGroup, maleRangeProp, dayRangeProp, refractory)
  dfAgents <<- setupAgents(df = dfABM)

  reps <<-  10
  xCors <<- list()
  yCors <<- list()
  zCors <<- list()
}

go <- function(reps = reps, GIF = F, plot = F, contestPlot = F, matingPlot = F, reach = 10, sight = 100, sinuosity = 20) {
  if (GIF == T) {
    wd <- getwd()
    setwd("/Users/kevinrosenfield/Box/PSU/Dissertation/New dissertation/Figures")
  }
  if (contestPlot == T | matingPlot == T) {
    quartz(height = 5, width = 5)
  }
  if (plot == T ) {
    quartzPoint <- ifelse(dfABM$groupLiving == T, 1, 18)
    quartz(pointsize = quartzPoint)
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
    agentConstant <- ifelse(dfABM$groupLiving == T, 5, .1)
    if (plot == T) {
      omitMale <- length(xCorsMale) - length(dfABM$numberAgents[dfAgents$Sex == "M"])
      omitFemale <- length(xCorsFemale) - length(dfABM$numberAgents[dfAgents$Sex == "F"])
      fig <- plot(c(dfAgents$xCorOrigin, xCorsMale[-c(1:omitMale)], xCorsFemale[-c(1:omitFemale)]),
                  c(dfAgents$yCorOrigin, yCorsMale[-c(1:omitMale)], yCorsFemale[-c(1:omitFemale)]),
                  pch = 21,
                  cex = c(cexSizes,
                          rep(agentConstant, length(xCorsMale[-c(1:omitMale)])),
                          rep(agentConstant, length(xCorsFemale[-c(1:omitFemale)]))),
                  col = c(rep(rgb(0, 1, 0, alpha = 0.5), dfABM$numberAgents),
                          rep(rgb(0, 0, 1),  length(xCorsMale[-c(1:omitMale)])),
                          rep(rgb(1, 0, 0),  length(xCorsFemale[-c(1:omitFemale)]))),
                  bg = c(rep(rgb(0, 1, 0, alpha = 0.5), dfABM$numberAgents),
                         rep(rgb(0, 1, 0),  length(xCorsMale[-c(1:omitMale)])),
                         rep(rgb(0, 1, 0),  length(xCorsFemale[-c(1:omitFemale)]))),
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

