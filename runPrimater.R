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

betas <- list()
fleeTimes <- seq(0, 0.02, .001)
for (f in fleeTimes) {
  setup(dimensions = 2, liveInGroup = F, numberAgents = 5, maleRangeProp = 2,
        dayRangeProp = 0.1, worldDiameter = 70, refractory = .1, fleeTime = 0)

  go(reps = 1, GIF = F, plot = T, contestPlot = F, matingPlot = F, reach = 10, sight = 75, sinuosity = 0)

  betas <- append(betas, summary(lm(dfAgents$Mates[dfAgents$Sex == "M"] ~
                                      dfAgents$Mass[dfAgents$Sex == "M"]))[4]$coefficients[2])

}

plot(unlist(betas) ~ fleeTimes)
summary(lm(unlist(betas) ~ fleeTimes))

  dfAgents %>%
    filter(Sex == "M") %>%
    dplyr::select(Mates, Attractiveness, winRatio, Wins, Mass) %>%
    pairs()

clearModel()

palette(c("red", "blue"))
agentConstant <- ifelse(dfABM$groupLiving == T, 5, .2)
fig <- plot(c(dfAgents$xCorOrigin, dfAgents$xCor),
            c(dfAgents$yCorOrigin, dfAgents$yCor),
            pch = 21,
            cex = c(cexSizes,
                    rep(agentConstant, dfABM$numberAgents)),
            col = c(rep(rgb(0, 1, 0, alpha = 0.5), dfABM$numberAgents),
                    dfAgents$Sex),
            bg = c(rep(rgb(0, 1, 0, alpha = 0.5), dfABM$numberAgents),
                   dfAgents$Sex),
            xlim=c(0 - dfABM$worldRadius, dfABM$worldRadius), ylim=c(0 - dfABM$worldRadius, dfABM$worldRadius))
legend("topright", legend=c("Female", "Male"),
       col=c("red", "blue"), lty=1:1, cex=15)

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

