library(stats4)
library(tidyverse)
spawn_ts <- seq(10, 5000, length.out = 199)
rec_ts <- (1.1*spawn_ts/(1 + 0.002*spawn_ts))*exp(rnorm(length(spawn_ts), 0, 0.2))


plot(spawn_ts, rec_ts)

# Forecast Method Functions ---------------------------------------------------------------
# x = test set vector, df1 = recruitment time series, df2 = spawning biomass time series

rec_mean <- function(x, df1){
  # subset data to window size
  dat <- df1[1:(x-1)]
  
  # calculate mean and standard deviation of time series window
  mu <- mean(log(dat))
  #print(mu)
  sigmaR <- sd(log(dat))
  
  # calculate prediction
  pred <- exp(mu + rnorm(1, 0, sigmaR))
  return(pred)

}

rec_AR <- function(x, df1){
  # subset data to window size
  dat <- df1[1:(x-1)]
  
  # fit AR model and calculate standard deviation (atm doing separately)
  mod <- arima0(log(dat), order = c(1,0,0))
  sigmaR <- sd(log(dat))
  
  one_ahead <- predict(mod, n.head = 1)
  pred <- exp(one_ahead$pred[1] + rnorm(1, 0, sigmaR))
  return(pred)
}

rec_BH <- function(x, df1, df2){
  # subset spawning and recruit data to window size
  datr <- df1[1:(x-1)]
  dats <- df2[1:(x-1)]
  s_x <- df2[x]
  
  # Likelihood function
  BHminusLL <- function(loga, logb, logsigmaR){
    # extract parameters
    a <- exp(loga); b <- exp(logb); sigmaR <- exp(logsigmaR)
    
    # make predictions
    pred <- log(a*dats/(1 + b*dats))
    
    #calculate negative log like
    NegLogL <- (-1)*sum(dnorm(log(datr), pred, sigmaR, log = TRUE))
    return(NegLogL)
  }
  
  
  starts <- list(loga = log(2), logb = log(0.2), logsigmaR = 5)
  mle_out <- mle(BHminusLL, start = starts)
  
  # extract parameters
  a <- exp(coef(mle_out)[1])
  b <- exp(coef(mle_out)[2])
  sigmaR <- exp(coef(mle_out)[3])
  
  # predict one step ahead
  one_ahead <- a*s_x/(1 + b*s_x)*exp(rnorm(1,0,sigmaR))
  
  return(unname(one_ahead))
}


# Projection Function -----------------------------------------------------
# A function that passes in the data files and calculates 1-step forecasts for an 
# expanding window. For each simulation, the function returns a matrix of the observed 
# recruitment and 1-step ahead forecasts for each of the methods above at time t in 
# the test set.

# nsims = # of simulations with the given test set of recruits and sbiomass
# time_vec = vector of test data for 1 step ahead forecasts
# recruits = recruitment time series
# sbiomass = spawning biomass time series

expanding_window <- function(nsims, time_vec, recruits, sbiomass){
  
  sim_preds <- array(NA, dim = c(length(time_vec), 4, nsims))
  
  for(i in 1:nsims){
    preds <- matrix(NA, nrow = length(time_vec), ncol = 4)
    
    for(j in 1:length(time_vec)){
      # first column is observed recruitment
      preds[j, 1] <- recruits[time_vec[j]]
      
      # columns for each of the forecasting methods
      preds[j, 2] <- rec_mean(time_vec[j], recruits)
      preds[j, 3] <- rec_AR(time_vec[j], recruits)
      preds[j, 4] <- rec_BH(time_vec[j], recruits, sbiomass)
    }
    
    sim_preds[,,i] <- preds
  }

  return(sim_preds)
}

# Predictions -------------------------------------------------------------









