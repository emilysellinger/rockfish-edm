# Practice simplex EDM with bocaccio
library(rEDM)
library(tidyverse)
library(here)

# NN Simplex Forecasting Test 1--------------------------------------------------
# going to follow the example from the package tutorial with bocaccio data
bocaccio <- read_table(here("data", "bocaccio_2017.txt"))
bocaccio <- bocaccio %>%
  select(Area, Yr, Era, SpawnBio, Recruit_0) %>% 
  filter(Yr >= 1960) %>% 
  filter(Yr <= 2015) # remove model run in time

plot(bocaccio$Yr, bocaccio$Recruit_0, type = "l")
acf(bocaccio$Recruit_0) # autocorrelation w/in the time series was an important factor in 
# recruit forecasting skill in Van Beveren et al. 2021
# there is some evidence of autocorrelation at lags 4 at 12 - significant but weak

# I'm going to follow the method used in the github tutorial. They first check forecast skill across the entire time series
# to determine the embedding dimension
ts <- bocaccio$Recruit_0 # recruitment time series
str(ts)

lib <- c(1, length(ts)) # training data
pred <- c(1, length(ts)) # test data

simplex_output <- simplex(ts, lib, pred)
str(simplex_output)

plot(simplex_output$E, simplex_output$rho, type = "l",
     xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)") #
simplex_output$rho # when using the whole time series, the peak of the embedding dimension is 7
# there is also a peak at E = 2, so I'll try out both embedding dimensions

# next we test for nonlinearity - if the forecast skill is better when theta > 0, a sign of nonlinearity
smap_output <- list(s_map(ts, lib, pred, E = 2, silent = TRUE), s_map(ts, lib, pred, E = 7, silent = TRUE))

plot(smap_output[[1]]$theta, smap_output[[1]]$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
plot(smap_output[[2]]$theta, smap_output[[2]]$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
# it seems like with a smaller embedding dimension, there is more evidence of nonlinear dynamics (peak is theta = 1.5)
# with E = 7, a nonlinear forecast method works best


# NN Simplex Forecasting Test 2 ------------------------------------------------------------------
# I want to repeat this with only a portion of the data (3/4 for training, 1/4 for test)
n <- length(ts)
lib <- c(1, floor(3/4*n)) # training data
pred <- c(floor(3/4*n)+1, floor(3/4*n)+3) # test data

simplex_output <- simplex(ts, lib, pred)
str(simplex_output)

plot(simplex_output$E, simplex_output$rho, type = "l",
     xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)") #
simplex_output$rho 



# next we test for nonlinearity - if the forecast skill is better when theta > 0, a sign of nonlinearity
smap_output <- list(s_map(ts, lib, pred, E = 4, silent = TRUE), s_map(ts, lib, pred, E = 7, silent = TRUE), 
                    s_map(ts, lib, pred, E = 10, silent = TRUE))

plot(smap_output[[1]]$theta, smap_output[[1]]$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
plot(smap_output[[2]]$theta, smap_output[[2]]$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
plot(smap_output[[3]]$theta, smap_output[[3]]$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")

# NN Simplex Forecasting Test 3 -------------------------------------------
# I want to maybe try doing some sort of loop to repeatedly forecast 5 years ahead
n <- length(ts)
# if I start with 3/4 of the data for training, and I predict 5 years concurrently, I need my loop to run 9 times
# note: would find a better way to do this later
for(i in 0:9){
  
  end_train <- floor(3/4*n) + i
  lib <- c(1, end_train) # training data
  pred <- c(end_train + 1, end_train + 5) # test data
  
  simplex_output <- simplex(ts, lib, pred, E = 1:5)
  plot(simplex_output$E, simplex_output$rho, type = "l",
       xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)",
       main = pred)
}

# so the issue with this is that the embedding dimension changes during each of the prediction periods
# Since we are interested in the idea of a moving window, what I will try next is to train the simplex
# model on 1/2 of the data, use 1/4 of the data to determine the embedding dimension, then predict the last 1/4 of the data
# moving window style
n <- length(ts)
lib <- c(1, floor(0.5*n))
pred <- c(floor(0.5*n) + 1, floor(3/4*n))

simplex_output <- simplex(ts, lib, pred)
plot(simplex_output$E, simplex_output$rho, type = "l",
     xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)")
simplex_output$rho # it says the peak is 3

for(i in 0:9){
  
  end_train <- floor(3/4*n) + i
  lib <- c(1, end_train) # training data
  pred <- c(end_train + 1, end_train + 5) # test data
  
  simplex_output <- simplex(ts, lib, pred, E = 3, stats_only = FALSE)
  predictions <- na.omit(simplex_output$model_output[[1]])
  
  plot(bocaccio$Yr, ts, type = "l", xlab = "year", ylab = "recruits")
  lines(predictions$Index + 1960, predictions$Predictions, col = "blue", lty = 2)
}


# not sure if that is actually the method for Van Beveren going to try it slightly different
n <- length(ts)
lib <- c(1, n-20)
pred <- c(n-20+1, n)

simplex_output <- simplex(ts, lib, pred)
plot(simplex_output$E, simplex_output$rho, type = "l",
     xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)")
simplex_output$rho # E = 7

simplex_output <- simplex(ts, lib, pred, E = 7, stats_only = FALSE)
predictions <- na.omit(simplex_output$model_output[[1]])

plot(bocaccio$Yr, ts, type = "l", xlab = "year", ylab = "recruits")
lines(predictions$Index + 1960, predictions$Predictions, col = "blue", lty = 2)
polygon(c(predictions$Index + 1960, rev(predictions$Index + 1960)),
        c(predictions$Predictions - sqrt(predictions$Pred_Variance), 
          rev(predictions$Predictions + sqrt(predictions$Pred_Variance))),
        col = rgb(0, 0, 1, 0.5), border = NA)

# for second round
lib2 <- c(1, n-10)
pred2 <- c(n-10+1, n)

output2 <- simplex(ts, lib2, pred2)
output2$rho # E = 1

output2 <- simplex(ts, lib2, pred2, E = 1, stats_only = FALSE)
predictions2 <- na.omit(output2$model_output[[1]])

# full plot
plot(bocaccio$Yr, ts, type = "l", xlab = "year", ylab = "recruits")
lines(predictions$Index + 1960, predictions$Predictions, col = "blue", lty = 2)
lines(predictions2$Index + 1960, predictions2$Predictions, col = "red", lty = 2)
polygon(c(predictions$Index + 1960, rev(predictions$Index + 1960)),
        c(predictions$Predictions - sqrt(predictions$Pred_Variance), 
          rev(predictions$Predictions + sqrt(predictions$Pred_Variance))),
        col = rgb(0, 0, 1, 0.3), border = NA)
polygon(c(predictions2$Index + 1960, rev(predictions2$Index + 1960)),
        c(predictions2$Predictions - sqrt(predictions2$Pred_Variance), 
          rev(predictions2$Predictions + sqrt(predictions2$Pred_Variance))),
        col = rgb(1, 0, 0, 0.3), border = NA)

# going to look at if there is evidence of nonlinearity in this scenario, given the t-20
# 20 year forecast didn't seem to do a good job of predicting the peaks around 2000 and 2013
smap_output1 <- s_map(ts, lib, pred, E = 7)
plot(smap_output1$theta, smap_output1$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")

smap_output2 <- s_map(ts, lib2, pred2, E = 1)
plot(smap_output2$theta, smap_output2$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")

# I don't think there is any evidence of nonlinearity at with bocaccio