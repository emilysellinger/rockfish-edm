library(stats4)
library(forecast)
library(depmixS4)
library(NatParksPalettes)
library(here)
library(tidyverse)
library(dplyr)
library(truncnorm)

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
          "bh" = rec_BH(time_vec[k], recruits, sbiomass),
          "hmm" = rec_HMM_sample(time_vec[k],recruits, sbiomass))
      }
      
    }
    
    sim_preds[,,i] <- preds
  }

  return(sim_preds)
}

# Predictions -------------------------------------------------------------
sims1 <- expanding_window(fmethods = c("m"), 100, time_vec, rec_ts, spawn_ts)#"ar", "bh"), 100, time_vec, rec_ts, spawn_ts)

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


# Actually fine, called bayesian coverage probability - should double check
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

# going to test
mare_results <- sim_mare(m_preds, rec_ts, time_vec)


# want to get quantiles for bh
bh_quants <- matrix(NA, nrow = 11, ncol = 3)
for(i in 1:dim(bh_preds)[1]){
  sim_quants <- quantile(bh_preds[i,2:dim(bh_preds)[2]], probs = c(0.25, 0.75))
  
  bh_quants[i, 1] <- bh_preds[i,1]
  bh_quants[i, 2] <- unname(sim_quants[1])
  bh_quants[i, 3] <- unname(sim_quants[2])
}

bh_quants <- as.data.frame(bh_quants)
ggplot(data = bh_quants) + geom_point(aes(x = seq(1, 11, 1), y = V1)) +
  geom_line(aes(x = seq(1, 11, 1), y = V2), color = "blue") +
  geom_line(aes(x = seq(1, 11, 1), y = V3), color = "blue")

