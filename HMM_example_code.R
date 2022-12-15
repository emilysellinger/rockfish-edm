## Load Packages-----------------------------------------------
library(tidyverse)
library(truncnorm)
library(here)
library(depmixS4)

## Load Data -------------------------------------------------
# set working directory

black_wa <- read_csv("black_wa.csv")
## Functions -------------------------------------------------
# takes estimated transition matrix and projects state for the following year
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

# samples recruitment for following year using the predicted state 
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

# uses training data to fit HMM
rec_HMM_sample <- function(x, df1, df2){
  # subset spawning and recruit data to window size
  datr <- df1[1:(x-1)]
  dats <- df2[1:(x-1)]
  
  # make a data frame for depmix package
  a <- tibble(rec = datr,
              spawn = dats,
              logRS = log(datr/dats),
              logR = log(datr))
  # fit hidden markov model
  mod1 <- depmix(logRS ~ spawn, data = a, nstates = 2, family = gaussian())
  fit_mod1 <- fit(mod1, em = em.control(maxit = 500))
  
  mod2 <- depmix(logRS ~ spawn, data = a, nstates = 1, family = gaussian())
  fit_mod2 <- fit(mod2)
  
  if(AIC(fit_mod2) < AIC(fit_mod1)){
    #print("one state best")
    mu1 <- mean(a$rec)
    sd1 <- sd(a$spawn)
    pred <- rtruncnorm(1, a = 10, b= Inf, mu1, sd1)
    
    # return forecasts
    return(pred)
    
  }else{
    #print("two state best")
    fit_post <- posterior(fit_mod1)
    
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
    future_states <- run.pred.mc.sim(est_P, num.iters = 2, final_state)
    
    
    rec_preds <- pred.rec(future_states, mu1 = mean(est_state_1$rec), mu2 = mean(est_state_2$rec),
                          sd1 = sd(est_state_1$rec), sd2 = sd(est_state_2$rec))
    # return forecasts
    return(rec_preds[2])
    
  }
}

# Projection function that passes in the data files and calculates 1-step forecasts for an 
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
      print((j-1))
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

## HMM Simulation ----------------------------------------------------
set.seed(110)

# create recruitment/spawning biomass vectors
rec_ts <- black_wa$Recruit_0
spawn_ts <- black_wa$SpawnBio
# create time vector
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts

black_wa_hmm_sims <- expanding_window(fmethods = c("hmm"), 1000, time_vec1, rec_ts, spawn_ts)


# get error "Error in if ((LL >= LL.old)) { : missing value where TRUE/FALSE needed"
# around simulation #41