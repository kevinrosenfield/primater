
setup <- function(dimensions = 2, numberAgents =  sample(2:100, 1), worldDiameter = runif(1, 25, 1000), liveInGroup = T) {
  dfABM <<- setupABM(dimensions, numberAgents, worldDiameter, liveInGroup)
  dfAgents <<- setupAgents()

  reps <<-  10
  xCors <<- list()
  yCors <<- list()
  zCors <<- list()
}

go <- function(reps = reps, GIF = F, plot = F) {
  if (GIF == T) {
    wd <- getwd()
    setwd("/Users/kevinrosenfield/Box/PSU/Dissertation/New dissertation/Figures")
  }
  if (plot == T) {
    #x11()
    quartz(title = "agents", width = 5, height = 5)
  }
  for (i in 1:reps) {
    distances <<- findNeighbors()
    dfAgents <<- chooseMate(reach = 100)
    dfAgents <<- compete(reach = 100)
    dfAgents <<- move()
    dfAgents <<- interact()
    if (plot == T) {
      omit <- length(xCors) - dfABM$numberAgents
      fig <- plot(c(dfAgents$xCorOrigin, xCors[-c(1:omit)]),
         c(dfAgents$yCorOrigin, yCors[-c(1:omit)]),
         pch = 21,
         cex = c(cexSizes,rep(.2, length(xCors[-c(1:omit)]))),
         col = c(rep("blue", dfABM$numberAgents), rep("red",  length(xCors[-c(1:omit)]))),
         bg = c(rep("green", dfABM$numberAgents), rep("green",  length(xCors[-c(1:omit)]))),
         xlim =  c(0 - dfABM$worldRadius, dfABM$worldRadius), ylim = c(0 - dfABM$worldRadius, dfABM$worldRadius))
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
