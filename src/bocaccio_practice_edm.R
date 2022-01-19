# Practice simplex EDM with bocaccio
library(rEDM)


# NN Simplex Forecasting --------------------------------------------------
# going to follow the example from the package tutorial with bocaccio data
bocaccio <- bocaccio %>% 
  filter(Yr >= 1960) %>% 
  filter(Yr <= 2015) # remove model run in time

plot(bocaccio$Yr, bocaccio$Recruit_0, type = "l")

ts <- bocaccio$Recruit_0 # recruitment time series
str(ts)

lib <- c(1, 20) # training data
pred <- c(21, 30) # test data

simplex_output <- simplex(ts, lib, pred)
str(simplex_output)

plot(simplex_output$E, simplex_output$rho, type = "l",
     xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)") # peak of rho is with 9 embedding dimensions (??)

simplex_output2 <- simplex(ts, lib, pred, E = 4, tp = c(1,2,3,4,5))
plot(simplex_output2$tp, simplex_output2$rho, type = "l",
     xlab = "Time to Prediction (E)", ylab = "Forecast Skill (rho)")
