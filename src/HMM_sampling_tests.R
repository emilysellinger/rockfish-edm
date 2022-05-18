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
  

P <- t(matrix(c(0.9, 0.1, 0.15, 0.85), nrow = 2, ncol = 2))
states <- run.mc.sim(P, num.iters = 50)
plot(states)


# vector for spawning biomass and recruitment
spawn_ts <- rep(NA, length(states))
rec_ts <- rep(NA, length(states))

# fill in based on state
for(i in 1:length(states)){
  if(states[i] == 1){
    spawn_ts[i] <- runif(1, 1000, 5000)
    rec_ts[i] <- (1.1*spawn_ts[i]/(1 + 0.002*spawn_ts[i]))
  }else{
    spawn_ts[i] <- runif(1, 1000, 5000)
    rec_ts[i] <- (0.9*spawn_ts[i]/(1 + 0.002*spawn_ts[i]))
  }
}

# plot
a <- tibble(state = states, rec = rec_ts, spawn = spawn_ts)

ggplot(a) + geom_point(aes(x = seq(1,50), y = rec, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,50), y = rec)) + 
  labs(y = "recruitment", x = "year") +
  scale_color_discrete(name = "state")


ggplot(a) + geom_point(aes(x = seq(1,50), y = spawn, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,50), y = spawn)) + 
  labs(y = "spawning biomass", x = "year") +
  scale_color_discrete(name = "state")

ggplot(a) + geom_point(aes(x = spawn, y = rec, color = as.factor(state)), size = 3) +
  labs(x = "spawning biomass", y = "recruitment") +
  scale_color_discrete(name = "state")


# fit HMM
mod <- depmix(rec ~ 1, data = a, nstates = 2, family = gaussian())
fit_mod <- fit(mod)
summary(fit_mod)
fit_post <- posterior(fit_mod)

df <- tibble(state = states,
             est_state = fit_post$state)

ggplot(data = df) + geom_point(aes(x = seq(1,50), y = state), col = "red") +
  geom_point(aes(x = seq(1,50), y = est_state), col = "blue", alpha = 0.3)
