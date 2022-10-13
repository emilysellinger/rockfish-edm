# Load packages -----------------------------------------------------------
library(stats4)
library(forecast)
library(depmixS4)
library(NatParksPalettes)
library(here)
library(tidyverse)
library(dplyr)
library(truncnorm)
library(rEDM)
# Short-term Forecast Functions ---------------------------------------------------------------
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

# functions for sampling method
run.pred.mc.sim <- function(P, num.iters = 2, final_state){
  
  # number of possible states
  num.states <- nrow(P)
  # create vector for states
  states <- rep(NA, num.iters)
  
  # initialize variable for first state 
  states[1] <- final_state
  
  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    ## draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}
# sampling method recruitment forecast
pred.rec <- function(future_states, mu1, mu2, sd1, sd2){
  preds <- rep(NA, length(future_states))
  
  for(i in 1:length(future_states)){
    if(future_states[i] == 1){
      preds[i] <- rtruncnorm(1, a = 10, b= Inf, mu1, sd1)
    }else{
      preds[i] <- rtruncnorm(1, a = 10, b = Inf, mu2, sd2)
    }
  }
  
  return(preds)
}

rec_HMM_sample <- function(x, df1, df2){
  # subset spawning and recruit data to window size
  datr <- df1[1:(x-1)]
  dats <- df2[1:(x-1)]
  
  # make a data frame for depmix package
  a <- tibble(rec = datr,
              spawn = dats,
              logRS = log(datr/dats))
  # fit hidden markov model
  mod <- depmix(logRS ~ spawn, data = a, nstates = 2, family = gaussian())
  fit_mod <- fit(mod)
  #summary(fit_mod)
  fit_post <- posterior(fit_mod)
  
  # update data frame with posterior state classification
  a <- a %>% 
    add_column(est_state = fit_post$state)
  # Filter dataframe by state
  est_state_1 <- a %>% 
    filter(est_state == 1)
  est_state_2 <- a %>% 
    filter(est_state == 2)
  
  # Determine the estimated state at the last time step
  final_state <- pull(a[nrow(a), "est_state"])
  
  # Extract estimated transition matrix
  est_P <- t(matrix(getpars(fit_mod)[3:6], nrow = 2, ncol = 2))
  
  # predict states for forecast years
  future_states <- run.pred.mc.sim(est_P, num.iters = 2, final_state)
  
  # forecast recruitment
  rec_preds <- pred.rec(future_states, mu1 = mean(est_state_1$rec), mu2 = mean(est_state_2$rec),
                        sd1 = sd(est_state_1$rec), sd2 = sd(est_state_2$rec))
  
  # return forecasts
  return(rec_preds[2])
}

rec_simplex <- function(x, df1){
  
  sim_lib <- c(1, x-1)
  # limit on updating E, simplex needs at least 2 data points to predict
  if((x-1) >= (length(df1) - 1)){
    sim_lib <- c(1, (length(df1) - 2))
  }
  
  sim_pred <- c(x-1, length(df1))
  
  # calculate standard deviation for subsetted data frame
  dat <- df1[1:(x-1)]
  sigmaR <- sd(log(dat))
  
  # determine optimal embedding dimension
  simplex_output <- simplex(log(df1), sim_lib, sim_pred)
  
  rho_vals <- unlist(simplex_output$rho)
  E_val <- unname(which.max(rho_vals))
  
  # forecast recruitment
  simplex_output2 <- simplex(log(df1), sim_lib, sim_pred, E = E_val, stats_only = FALSE)
  
  preds <- na.omit(simplex_output2$model_output[[1]])
  
  # return forecast
  pred <- exp(preds[1, 3] + rnorm(1, 0, sigmaR))
  return(pred)
}
# Long-term forecast functions -------------------------------------------------------------
lrec_mean <- function(x, df1){
  # subset data to window size
  dat <- df1[1:(x-1)]
  
  # calculate mean and standard deviation of time series window
  mu <- mean(log(dat))
  #print(mu)
  sigmaR <- sd(log(dat))
  
  # calculate prediction
  preds <- exp(mu + rnorm(5, 0, sigmaR))
  return(preds)
}

lrec_AR <- function(x, df1){
  
  # subset data to window size
  dat <- df1[1:(x-1)]
  
  # fit AR model, will use sd estimate from model estimates
  mod <- Arima(log(dat), order = c(1,0,0))
  sigmaR <- sqrt(mod$sigma2)
  
  # calculate prediction
  preds <- exp((forecast(mod, h = 5)$mean)[1:5] + rnorm(5, 0, sigmaR))
  return(preds)
}

lrec_BH <- function(x, df1, df2){
  # subset spawning and recruit data to window size
  datr <- df1[1:(x-1)]
  dats <- df2[1:(x-1)]
  s_x <- df2[x:(x+4)]
  
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
  five_ahead <- a*s_x/(1 + b*s_x)*exp(rnorm(5,0,sigmaR))
  
  return(unname(five_ahead))
}

lrec_HMM_sample <- function(x, df1, df2){
  # subset spawning and recruit data to window size
  datr <- df1[1:(x-1)]
  dats <- df2[1:(x-1)]
  
  # make a data frame for depmix package
  a <- tibble(rec = datr,
              spawn = dats,
              logRS = log(datr/dats))
  # fit hidden markov model
  mod <- depmix(logRS ~ spawn, data = a, nstates = 2, family = gaussian())
  fit_mod <- fit(mod)
  #summary(fit_mod)
  fit_post <- posterior(fit_mod)
  
  # update data frame with posterior state classification
  a <- a %>% 
    add_column(est_state = fit_post$state)
  # Filter dataframe by state
  est_state_1 <- a %>% 
    filter(est_state == 1)
  est_state_2 <- a %>% 
    filter(est_state == 2)
  
  # Determine the estimated state at the last time step
  final_state <- pull(a[nrow(a), "est_state"])
  
  # Extract estimated transition matrix
  est_P <- t(matrix(getpars(fit_mod)[3:6], nrow = 2, ncol = 2))
  
  # predict states for forecast years
  future_states <- run.pred.mc.sim(est_P, num.iters = 6, final_state)
  
  # forecast recruitment
  rec_preds <- pred.rec(future_states, mu1 = mean(est_state_1$rec), mu2 = mean(est_state_2$rec),
                        sd1 = sd(est_state_1$rec), sd2 = sd(est_state_2$rec))
  
  # return forecasts
  return(rec_preds[-1])
}

lrec_simplex <- function(x, df1){
  
  sim_lib <- c(1, x-1)
  sim_pred <- c(x-1, length(df1))
  
  # calculate standard deviation for subsetted data frame
  dat <- df1[1:(x-1)]
  sigmaR <- sd(log(dat))
  
  # determine optimal embedding dimension
  simplex_output1 <- simplex(log(df1), sim_lib, sim_pred)
  
  rho_vals <- unlist(simplex_output1$rho)
  E_val <- unname(which.max(rho_vals))
  
  # forecast recruitment
  simplex_output2 <- simplex(log(df1), sim_lib, sim_pred, E = E_val, stats_only = FALSE)
  
  preds <- na.omit(simplex_output2$model_output[[1]])
  
  # return forecast
  pred <- exp(preds[1:5, 3] + rnorm(5, 0, sigmaR))
  return(pred)
}
# Projection Functions -----------------------------------------------------
# A function that passes in the data files and calculates 1-step forecasts for an 
# expanding window. For each simulation, the function returns a matrix of the observed 
# recruitment and 1-step (or 5-step) ahead forecasts for each of the methods above at time t in 
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
          "bh" = rec_BH(time_vec[k], recruits, sbiomass),
          "hmm" = rec_HMM_sample(time_vec[k],recruits, sbiomass),
          "simplex" = rec_simplex(time_vec[k], recruits))
      }
      
    }
    
    sim_preds[,,i] <- preds
  }

  return(sim_preds)
}

expanding_window_5yr <- function(fmethods, nsims, time_vec, time_vec2, recruits, sbiomass){
  
  sim_preds <- array(NA, dim = c(length(time_vec), (nsims+1), length(fmethods)))
  
  for(i in 1:length(fmethods)){
    fmethod <- fmethods[i]
    
    preds <- matrix(NA, nrow = length(time_vec), ncol = (nsims+1))
    # first column is observed recruitment
    preds[,1] <- recruits[time_vec]
    
    for(j in 2:(nsims+1)){
      # create a matrix for each 5 year expanding window sim
      raw_preds <- matrix(data = NA, nrow = length(time_vec), ncol = length(time_vec2))
      for(k in 1:length(time_vec2)){
        
        if(fmethod == "m"){
          raw_preds[k:(k+4), k] <- lrec_mean(time_vec2[k], recruits)
        }else if(fmethod == "ar"){
          raw_preds[k:(k+4), k] <- lrec_AR(time_vec2[k], recruits)
        }else if(fmethod == "bh"){
          raw_preds[k:(k+4), k] <- lrec_BH(time_vec2[k], recruits, sbiomass)
        }else if(fmethod == "hmm"){
          raw_preds[k:(k+4), k] <- lrec_HMM_sample(time_vec2[k],recruits, sbiomass)
        }else {
          raw_preds[k:(k+4), k] <- lrec_simplex(time_vec2[k], recruits)
        }
        
      }
      #print(raw_preds)
      # take mean of overlapping predictions, save simulation to sim dataframe
      preds[,j] <- apply(raw_preds, 1, median, na.rm = TRUE)
    }
    
    sim_preds[,,i] <- preds
  }
  
  return(sim_preds)
}


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


# Actually fine, called coverage probability
sim_CI_prob <- function(sim_results, ci){
  # initialize counter
  ci_prob <- 0
  
  # calculate values for quantile function
  ci1 <- (1 - ci)/2
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


# Mean absolute relative error - need to update draft
sim_mare <- function(sim_results, df, time_vec){
  # data frame for mare stats
  mare_df <- rep(NA, dim(sim_results)[1])
  
  for(i in 1:length(time_vec)){
    # subset to training data
    dat <- df[1:(time_vec[i]-1)]
    train_n <- (time_vec[i] - 1)
    # calculate denominator
    denom <- sum(abs(diff(dat)))/(train_n - 1)
    
    # calculate numerator vector
    num_vec <- abs(sim_results[i, 2:dim(sim_results)[2]] - sim_results[i, 1])
    
    # calculate value
    mare_df[i] <- mean(num_vec/denom)
  }
  return(mare_df)
}

sim_5yr_trend <- function(sim_results, time_vec){
  # think I want to fit a lm to the 5 year data, then return slope estimate
}
