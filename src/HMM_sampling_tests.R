# practice with HMMs in depmix
library(depmixS4)
library(Rlab)
library(tidyverse)

# simulate data
# I'm going to simulate data 2 different ways, the first will just be a recruitment
# time series using normal - got simulation function from 

# simulate discrete Markov chains according to transition matrix P
run.mc.sim <- function( P, num.iters = 50 ){
  
  # number of possible states
  num.states <- nrow(P)
  # create vector for states
  states <- rep(NA, num.iters)
  
  # initialize variable for first state 
  states[1] <- 1
  
  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    ## draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}
  

P <- t(matrix(c(0.8, 0.2, 0.15, 0.85), nrow = 2, ncol = 2))
states <- run.mc.sim(P, num.iters = 50)
plot(states)


# vector for spawning biomass and recruitment
spawn_ts <- rep(NA, length(states))
rec_ts <- rep(NA, length(states))

# fill in based on state
for(i in 1:length(states)){
  if(states[i] == 1){
    spawn_ts[i] <- runif(1, 10, 500)
    rec_ts[i] <- 5*spawn_ts[i]*exp(-0.002*spawn_ts[i])*exp(rnorm(1,0,0.2))
  }else{
    spawn_ts[i] <- runif(1, 10, 500)
    rec_ts[i] <- 2*spawn_ts[i]*exp(-0.004*spawn_ts[i])*exp(rnorm(1,0,0.2))
  }
}

# plot
a <- tibble(state = states, rec = rec_ts, spawn = spawn_ts, logRS = log(rec_ts/spawn_ts))

ggplot(a) + geom_point(aes(x = seq(1,50), y = rec, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,50), y = rec)) + 
  labs(y = "recruitment", x = "year") +
  scale_color_discrete(name = "state")


ggplot(a) + geom_point(aes(x = seq(1,50), y = spawn, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,50), y = spawn)) + 
  labs(y = "spawning biomass", x = "year") +
  scale_color_discrete(name = "state")

ggplot(a) + geom_point(aes(x = spawn, y = logRS, color = as.factor(state)), size = 3) +
  labs(x = "spawning biomass", y = "recruitment") +
  scale_color_discrete(name = "state")




# fit HMM
mod <- depmix(logRS ~ spawn, data = a, nstates = 2, family = gaussian())
fit_mod <- fit(mod)
summary(fit_mod)
fit_post <- posterior(fit_mod)

df <- tibble(state = states,
             est_state = fit_post$state)

ggplot(data = df) + geom_point(aes(x = seq(1,50), y = state), col = "red") +
  geom_point(aes(x = seq(1,50), y = est_state), col = "blue", alpha = 0.3)

# Note: seems that some of the initial problems with label switching were because
# the parameters for the two regimes were too similar, changing the b value improved
# model fitting. Something to keep in mind when fitting data

# retrieve parameters
pars <- getpars(fit_mod)

est_t_matrix <- t(matrix(c(pars[3], pars[4], pars[5], pars[6]), nrow = 2, ncol = 2))
state1_params <- c(exp(pars[7]), pars[8], pars[9])
state2_params <- c(exp(pars[10]), pars[11], pars[12])

# determine state at last time step
current_state <- fit_post$state[50]


# Second simulation -------------------------------------------------------
# I'm going to try out a second way to simulate the data, where
# spawning biomass varies by regime
# Note: I don't think that this is right, I'm going to talk to Andre on Monday
# vector for spawning biomass and recruitment
spawn_ts2 <- rep(NA, length(states))
rec_ts2 <- rep(NA, length(states))

# fill in based on state
for(i in 1:length(states)){
  if(states[i] == 1){
    spawn_ts2[i] <- runif(1, 100, 500)
    rec_ts2[i] <- 2*spawn_ts[i]*exp(-0.002*spawn_ts[i])*exp(rnorm(1,0,0.2))
  }else{
    spawn_ts2[i] <- runif(1, 10, 100)
    rec_ts2[i] <- 2*spawn_ts[i]*exp(-0.002*spawn_ts[i])*exp(rnorm(1,0,0.2))
  }
}

# plot
b <- tibble(state = states, rec = rec_ts2, spawn = spawn_ts2, logRS = log(rec_ts2/spawn_ts2))
ggplot(b) + geom_point(aes(x = seq(1,50), y = rec, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,50), y = rec)) + 
  labs(y = "recruitment", x = "year") +
  scale_color_discrete(name = "state")


ggplot(b) + geom_point(aes(x = seq(1,50), y = spawn, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,50), y = spawn)) + 
  labs(y = "spawning biomass", x = "year") +
  scale_color_discrete(name = "state")

ggplot(b) + geom_point(aes(x = spawn, y = logRS, color = as.factor(state)), size = 3) +
  labs(x = "spawning biomass", y = "log recruitment") +
  scale_color_discrete(name = "state")

ggplot(b) + geom_point(aes(x = spawn, y = rec, color = as.factor(state)), size = 3) +
  labs(x = "spawning biomass", y = "recruitment") +
  scale_color_discrete(name = "state")

# fit HMM
mod2 <- depmix(logRS ~ spawn, data = b, nstates = 2, family = gaussian())
fit_mod <- fit(mod2)
summary(fit_mod2)
fit_post <- posterior(fit_mod)

df <- tibble(state = states,
             est_state = fit_post$state)

ggplot(data = df) + geom_point(aes(x = seq(1,50), y = state), col = "red") +
  geom_point(aes(x = seq(1,50), y = est_state), col = "blue", alpha = 0.3)