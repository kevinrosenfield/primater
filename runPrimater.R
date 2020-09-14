# the following workflow installs and attaches primater,
# setupABM() creates dfABM; sets 2- or 3-dimensional model parameters, many of which are drawn from normal or uniform distributions
# setupAgents() creates dfAgents; brings agents to life, based on the parameters of dfABM
# a scatter plot shows the starting positions of all agents
# the working directory is temporarily pointed elsewhere so that a GIF file can be produced for visualization
# the model is run a number of times; agents find their neighbors, engage in contests, set new headings, and move forward
# you can see that agents accumulate wins or losses, currently based solely on body mass (with built-in stochasticity)
# one plot is produced for each tick, and plots are stitched together into a GIF file

devtools::install_github('kevinrosenfield/primater', force = T)
detach('package:primater', unload = TRUE)
library(primater); library(plotly); library(rethinking); library(tidyverse); library(png)

setup(dimensions = 2, liveInGroup = T, numberAgents = 4, maleRangeProp = .2,
      dayRangeProp = 0.05, worldDiameter = 700, refractory = 365/365)

go(reps = 1000, GIF = F, plot = T, contestPlot = T, matingPlot = F, reach = 20, sight = 100, sinuosity = 20)

clearModel()

plot(c(dfAgents$xCorOrigin, xCors), c(dfAgents$yCorOrigin, yCors), pch = 21,
     cex = c(cexSizes,rep(.2, length(xCors))),
     col = c(rep("blue", dfABM$numberAgents), rep("red",  length(xCors))),
     bg = c(rep("green", dfABM$numberAgents), rep("green",  length(xCors))),
     xlim =  c(0 - dfABM$worldRadius, dfABM$worldRadius), ylim = c(0 - dfABM$worldRadius, dfABM$worldRadius))

dfAgents %>%
  filter(Sex == "M") %>%
  dplyr::select(Mates, Attractiveness, winRatio, Wins, Mass) %>%
  pairs()

summary(lm(dfAgents$Mates[dfAgents$Sex == "M"] ~ dfAgents$Attractiveness[dfAgents$Sex == "M"]))
summary(lm(dfAgents$Mates[dfAgents$Sex == "M"] ~ dfAgents$Mass[dfAgents$Sex == "M"]))
summary(lm(dfAgents$Mates[dfAgents$Sex == "M"] ~ dfAgents$winRatio[dfAgents$Sex == "M"]))
summary(lm((dfAgents$Wins[dfAgents$Sex == "M"] ~ dfAgents$Mass[dfAgents$Sex == "M"])))
summary(lm((dfAgents$Losses[dfAgents$Sex == "M"] ~ dfAgents$Mass[dfAgents$Sex == "M"])))
summary(lm((dfAgents$winRatio[dfAgents$Sex == "M"] ~ dfAgents$Mass[dfAgents$Sex == "M"])))
summary(lm(dfAgents$Mass[dfAgents$Sex == "M"] ~ dfAgents$Losses[dfAgents$Sex == "M"] + dfAgents$Wins[dfAgents$Sex == "M"]))

plot3D <- plotly::plot_ly(type = 'scatter3d', mode = 'markers')
plot3D <- plot3D %>%
  add_trace(x = xCors, y = yCors,  z = zCors,
            marker = list(
              color = 'rgb(255, 0, 250)',
              size = 1,
              line = list(
                color = 'rgb(231, 99, 250)',
                width = .5
              )
            ),
            showlegend = F
  )  %>%
  add_trace(x = dfAgents$xCorOrigin, y = dfAgents$yCorOrigin,  z = dfAgents$zCorOrigin,
            opacity = 0.2,
            marker = list(
              color = ~dfAgents$agentID,
              colors = 'Set1',
              size = 300,
              line = list(
                color = 'rgb(231, 99, 250)',
                width = 10
              )
            ),
            showlegend = F
  )

