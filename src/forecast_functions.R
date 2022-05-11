library(stats4)
library(tidyverse)
library(forecast)
spawn_ts <- seq(10, 5000, length.out = 50)
rec_ts <- (1.1*spawn_ts/(1 + 0.002*spawn_ts))*exp(rnorm(length(spawn_ts), 0, 0.2))


plot(spawn_ts, rec_ts)

time_vec <- seq(40, 50, 1)
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
  
  # fit AR model, will use sd estimate from model estimates
  mod <- Arima(log(dat), order = c(1,0,0))
  sigmaR <- sqrt(mod$sigma2)
  
  # calculate prediction
  pred <- exp((forecast(mod, h = 1)$mean)[1] + rnorm(1, 0, sigmaR))
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

expanding_window <- function(fmethods, nsims, time_vec, recruits, sbiomass){
  
  sim_preds <- array(NA, dim = c(length(time_vec), (nsims+1), length(fmethods)))
  
  for(i in 1:length(fmethods)){
    fmethod <- fmethods[i]
    
    preds <- matrix(NA, nrow = length(time_vec), ncol = (nsims+1))
    # first column is observed recruitment
    preds[,1] <- recruits[time_vec]
    
    for(j in 2:(nsims+1)){
      for(k in 1:length(time_vec)){
        preds[k, j] <- switch(
          fmethod,
          "m" = rec_mean(time_vec[k], recruits),
          "ar" = rec_AR(time_vec[k], recruits),
          "bh" = rec_BH(time_vec[k], recruits, sbiomass))
      }
      
    }
    
    sim_preds[,,i] <- preds
  }

  return(sim_preds)
}

# Predictions -------------------------------------------------------------
sims1 <- expanding_window(fmethods = c("m", "ar", "bh"), 100, time_vec, rec_ts, spawn_ts)

m_preds <- sims1[,,1]
ar_preds <- sims1[,,2]
bh_preds <- sims1[,,3]


# Performance Stat Functions ----------------------------------------------
sim_mae <- function(sim_results){
  mae_df <- rep(NA, (dim(sim_results)[2]) - 1)
  for(i in 1:(dim(sim_results)[2] - 1)){
    mae_df[i] <- mean(abs(sim_results[,(i+1)] - sim_results[,1]))
  }
  
  hist(mae_df)
  mae_quants <- quantile(mae_df, probs = c(0.025, 0.975))
  return(mae_quants)
  
}

# NOT WORKING YET
sim_CI_prob <- function(sim_results, ci){
  # initialize counter
  ci_prob <- 0
  
  # calculate values for quantile function
  ci1 <- ci - (1 - ci)/2
  ci2 <- ci + (1 - ci)/2
  
  for(i in 1:dim(sim_results)[1]){
    sim_quants <- quantile(sim_results[i,2:dim(sim_results)[2]], probs = c(ci1, ci2))
    
    # add 1 to counter if the observed recruitment at year t is within the specified quantile
    # across simulation results for year t
    if(sim_results[i, 1] >= sim_quants[1] && sim_results[i, 1] <= sim_quants[2]){
      ci_prob <- ci_prob + 1
    }
  }
  
  prob <- ci_prob/dim(sim_results)[1]
  return(prob)
}






