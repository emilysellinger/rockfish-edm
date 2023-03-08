# Load packages -----------------------------------------------------------
library(stats4)
library(forecast)
library(depmixS4)
library(NatParksPalettes)
library(here)
library(tidyverse)
library(dplyr)
library(rEDM)
library(gridExtra)
library(zoo)
library(grid)
library(changepoint)
library(patchwork)
# Short-term Forecast Functions ---------------------------------------------------------------
# x = test set vector, df1 = recruitment time series, df2 = spawning biomass time series, 
# nsims = number of simulations

rec_mean <- function(x, df1, nsims){
  # subset data to window size
  dat <- df1[1:(x-1)]
  
  # calculate mean and standard deviation of time series window
  mu <- mean(log(dat))
  #print(mu)
  sigmaR <- sd(log(dat))
  
  # calculate predictions for each simulation
  pred <- exp(mu + rnorm(nsims, 0, sigmaR))
  return(pred)
  
}

rec_AR <- function(x, df1, nsims){
  
  # subset data to window size
  dat <- df1[1:(x-1)]
  
  # fit AR model, will use sd estimate from model estimates
  mod <- Arima(log(dat), order = c(1,0,0))
  sigmaR <- sqrt(mod$sigma2)
  
  # calculate prediction
  pred <- exp((forecast(mod, h = 1)$mean)[1] + rnorm(nsims, 0, sigmaR))
  return(pred)
}

rec_BH <- function(x, df1, df2, nsims){
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
  one_ahead <- a*s_x/(1 + b*s_x)*exp(rnorm(nsims,0,sigmaR))
  
  return(unname(one_ahead))
}

rec_simplex <- function(x, df1, nsims){
  
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
  simplex_output <- simplex(log(df1), sim_lib, sim_pred, E = 1:9)
  #print(simplex_output)
  
  rho_vals <- unlist(simplex_output$rho)
  #print(rho_vals)
  E_val <- unname(which.max(rho_vals))
  
  # forecast recruitment
  simplex_output2 <- simplex(log(df1), sim_lib, sim_pred, E = E_val, stats_only = FALSE)
  
  preds <- na.omit(simplex_output2$model_output[[1]])
  
  # return forecast
  pred <- exp(preds[1, 3] + rnorm(nsims, 0, sigmaR))
  return(pred)
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
pred.rec <- function(future_state, mu1, mu2, sd1, sd2, df2, x){
  
  if(future_state == 1){
    logRS <- rnorm(1,mu1,sd1)
    pred <- exp(logRS + log(df2[x]))
  }else{
    logRS <- rnorm(1, mu2, sd2)
    pred <- exp(logRS + log(df2[x]))
  }
  
  return(pred)
}

rec_HMM_sample <- function(x, df1, df2, nsims){
  # subset spawning and recruit data to window size
  datr <- df1[1:(x-1)]
  dats <- df2[1:(x-1)]
  
  # make a data frame for depmix package
  a <- tibble(rec = datr,
              spawn = dats,
              logRS = log(datr/dats),
              logR = log(datr))
  # fit hidden markov model
  mod1 <- depmix(logRS ~ spawn, data = a, nstates = 2, family = gaussian(), instart = c(0.9,0.1))
  fit_mod1 <- fit(mod1, em = em.control(maxit = 500))
  
  
  fit_post <- posterior(fit_mod1, type = "viterbi")
  
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
  est_P <- t(matrix(getpars(fit_mod1)[3:6], nrow = 2, ncol = 2))
  
  # predict states for forecast years
  future_state <- tail(run.pred.mc.sim(est_P, num.iters = 2, final_state), n = 1)
  
  rec_preds <- rep(NA, nsims)  
  for(i in 1:nsims){
    rec_preds[i] <- pred.rec(future_state, mu1 = mean(est_state_1$logRS), mu2 = mean(est_state_2$logRS),
                             sd1 = sd(est_state_1$logRS), sd2 = sd(est_state_2$logRS),df2, x)
  }  
  
  # return forecasts
  return(rec_preds)
  
}

rec_chpt_sample <- function(x, df1, nsims){
  # subset data to window size
  dat <- df1[1:(x-1)]
  
  # fit changepoints
  fitPelt <- cpt.meanvar(log(dat),method="PELT",test.stat="Normal",penalty="Manual",
                         pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=7)
  changes	<- fitPelt@cpts
  
  # use estimates of mean and variance of final regime to predict next year
  mu <- tail(fitPelt@param.est$mean, n = 1)
  sigmaR <- sqrt(tail(fitPelt@param.est$variance, n = 1))
  pred <- exp(mu + rnorm(nsims, 0, sigmaR))
  
  # return forecasts
  return(pred)
}


# Long-term forecast functions -------------------------------------------------------------
# x = test set vector, df1 = recruitment time series, df2 = spawning biomass time series, 
# nsims = number of simulations

lrec_mean <- function(x, df1, nsims){
  # subset data to window size
  dat <- df1[1:(x-5)]
  
  # calculate mean and standard deviation of time series window
  mu <- mean(log(dat))
  #print(mu)
  sigmaR <- sd(log(dat))
  
  # calculate prediction
  preds <- exp(mu + rnorm(nsims, 0, sigmaR))
  return(preds)
}

lrec_AR <- function(x, df1, nsims){
  
  # subset data to window size
  dat <- df1[1:(x-5)]
  
  # fit AR model, will use sd estimate from model estimates
  mod <- Arima(log(dat), order = c(1,0,0))
  sigmaR <- sqrt(mod$sigma2)
  
  # calculate prediction
  preds <- exp(forecast(mod, h = 5)$mean[5] + rnorm(nsims, 0, sigmaR))
  
  return(preds)
}

lrec_BH <- function(x, df1, df2, nsims){
  # subset spawning and recruit data to window size
  datr <- df1[1:(x-5)]
  dats <- df2[1:(x-5)]
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
  
  # predict
  five_ahead <- a*s_x/(1 + b*s_x)*exp(rnorm(nsims,0,sigmaR))
  
  return(unname(five_ahead))
}

lrec_HMM_sample <- function(x, df1, df2, nsims){
  # subset spawning and recruit data to window size
  datr <- df1[1:(x-5)]
  dats <- df2[1:(x-5)]
  
  # make a data frame for depmix package
  a <- tibble(rec = datr,
              spawn = dats,
              logRS = log(datr/dats))
  # fit hidden markov model
  mod <- depmix(logRS ~ spawn, data = a, nstates = 2, family = gaussian())#, instart = c(0.9, 0.1))
  fit_mod <- fit(mod)
  #summary(fit_mod)
  fit_post <- posterior(fit_mod, type = "viterbi")
  
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
  
  # predict states for forecast years, only save state for terminal year
  future_state <- tail(run.pred.mc.sim(est_P, num.iters = 6, final_state), n = 1)
  
  rec_preds <- rep(NA, nsims)  
  for(i in 1:nsims){
    rec_preds[i] <- pred.rec(future_state, mu1 = mean(est_state_1$logRS), mu2 = mean(est_state_2$logRS),
                             sd1 = sd(est_state_1$logRS), sd2 = sd(est_state_2$logRS),df2, x)
  }  
  
  # return forecasts
  return(rec_preds)
}

lrec_simplex <- function(x, df1, nsims){
  
  sim_lib <- c(1, x-5)
  sim_pred <- c(x-5, length(df1))
  
  # calculate standard deviation for subsetted data frame
  dat <- df1[1:(x-5)]
  sigmaR <- sd(log(dat))
  
  # determine optimal embedding dimension
  simplex_output1 <- simplex(log(df1), sim_lib, sim_pred, E = 1:7)
  #print(simplex_output1)
  
  rho_vals <- unlist(simplex_output1$rho)
  E_val <- unname(which.max(rho_vals))
  
  # forecast recruitment
  simplex_output2 <- simplex(log(df1), sim_lib, sim_pred, E = E_val, stats_only = FALSE)
  
  preds <- na.omit(simplex_output2$model_output[[1]])
  pred_row <- which(preds$Index == x)
  # return forecast
  pred <- exp(preds[pred_row, 3] + rnorm(nsims, 0, sigmaR))
  return(pred)
}

lrec_chpt_sample <- function(x, df1, nsims){
  # subset data to window size
  dat <- df1[1:(x-5)]
  
  # fit changepoints
  fitPelt <- cpt.meanvar(log(dat),method="PELT",test.stat="Normal",penalty="Manual",
                         pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=7)
  changes	<- fitPelt@cpts
  
  # use estimates of mean and variance of final regime to predict next year
  mu <- tail(fitPelt@param.est$mean, n = 1)
  sigmaR <- sqrt(tail(fitPelt@param.est$variance, n = 1))
  pred <- exp(mu + rnorm(nsims, 0, sigmaR))
  
  # return forecasts
  return(pred)
}


# Projection Functions -----------------------------------------------------
# A function that passes in the data files and calculates 1-step forecasts for an 
# expanding window. For each simulation, the function returns a matrix of the observed 
# recruitment and 1-step (or 5-step) ahead forecasts for each of the methods above at time t in 
# the test set.
# fmethods = vector of which forcasting methods to use ("m" = mean, "ar" = AR(1), "bh" = Beverton-Holt,
# "hmm" = hidden Markov model sampling, "simplex" = simplex projection, "chpt" = PELT change point sampling)
# nsims = number of simulations
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
    
    for(k in 1:length(time_vec)){
      preds[k, 2:(nsims+1)] <- switch(
        fmethod,
        "m" = rec_mean(time_vec[k], recruits, nsims),
        "ar" = rec_AR(time_vec[k], recruits, nsims),
        "bh" = rec_BH(time_vec[k], recruits, sbiomass, nsims),
        "hmm" = rec_HMM_sample(time_vec[k],recruits, sbiomass, nsims),
        "simplex" = rec_simplex(time_vec[k], recruits, nsims),
        "chpt" = rec_chpt_sample(time_vec[k], recruits, nsims))
    }
    
    sim_preds[,,i] <- preds
  }
  
  return(sim_preds)
}


expanding_window_5yr <- function(fmethods, nsims, time_vec, recruits, sbiomass){
  
  sim_preds <- array(NA, dim = c(length(time_vec), (nsims+1), length(fmethods)))
  
  for(i in 1:length(fmethods)){
    fmethod <- fmethods[i]
    
    preds <- matrix(NA, nrow = length(time_vec), ncol = (nsims+1))
    # first column is observed recruitment
    preds[,1] <- recruits[time_vec]
    
    for(k in 1:length(time_vec)){
      preds[k, 2:(nsims+1)] <- switch(
        fmethod,
        "m" = lrec_mean(time_vec[k], recruits, nsims),
        "ar" = lrec_AR(time_vec[k], recruits, nsims),
        "bh" = lrec_BH(time_vec[k], recruits, sbiomass, nsims),
        "hmm" = lrec_HMM_sample(time_vec[k],recruits, sbiomass, nsims),
        "simplex" = lrec_simplex(time_vec[k], recruits, nsims),
        "chpt" = lrec_chpt_sample(time_vec[k], recruits, nsims))
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



# sim_5yr_trend <- function(sim_results, time_vec){
#   # create matrix for 5 yr slopes
#   yr_trend <- matrix(NA, nrow = length(time_vec), ncol = (dim(sim_results)[2] - 1))
#   
#   for(i in 1:nrow(yr_trend)){
#     for(j in 1:ncol(yr_trend)){
#       
#       rec_lm <- lm(sim_results[i:(i+4), (j+1)] ~ seq(1,5))
#       
#       yr_trend[i,j] <- unname(rec_lm$coefficients[2])
#     }
#   }
#   # calculate quantiles
#   yr_trend_quants <- apply(yr_trend, 1, quantile, p = c(0.0275, 0.5, 0.975))
#   return(yr_trend_quants)
# }


sim_5yr_trend <- function(sim_results, time_vec){
  # create matrix for 5 yr slopes
  yr_trend <- matrix(NA, nrow = (dim(sim_results)[1] - 4), ncol = (dim(sim_results)[2] - 1))
  
  for(i in 1:ncol(yr_trend)){
    yr_trend[,i] <- rollmean(sim_results[,i+1], 5)
  }
  # calculate quantiles
  yr_trend_quants <- apply(yr_trend, 1, quantile, p = c(0.0275, 0.5, 0.975))
  return(yr_trend_quants)
}
