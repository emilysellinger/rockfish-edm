# forecast sampling procedure development

# Load packages -----------------------------------------------------------
library(depmixS4)
library(Rlab)
library(tidyverse)
library(truncnorm)
library(gridExtra)

# Simulate data -----------------------------------------------------------
# to simulate the data I will use a function to determine the state 
# of the system and a simple age structured model

# simulate discrete Markov chains according to transition matrix P
run.mc.sim <- function(P, num.iters = 50){
  
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
  

P <- t(matrix(c(0.95, 0.05, 0.1, 0.9), nrow = 2, ncol = 2))
states <- run.mc.sim(P, num.iters = 50)
plot(states)


# Age Matrix
# I adapted this code from Trevor's 458 class
nages <- 10      #number of ages in the model
nyears <- 50    #number of years in the model

Nat <- matrix(nrow=nyears, ncol=nages)
# egg production each year
Et <- vector(length = nyears)

# alpha and beta parameters for S-R function
alpha1 <- 6.8
beta1 <- 0.001
alpha2 <- 6.8
beta2 <- 0.001

# survival by age
sa <- c(0.7,0.85,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9)
# vulnerability by age
va <- c(0.0,0.2, 0.5,0.8,1.0,1.0,1.0,1.0,1.0,1.0)
# fecundity by age
feca <- c(0,0,0.1,0.5,0.9,1,1,1,1,1)
# exploitation rate by year
ut <- c(rep(x=0.1, times=nyears))

# plot of parameters for Andre
parameter_df <- tibble(age = seq(1, 10),
                       survival = sa,
                       vulnerability = va,
                       fecundity = feca)

figA <- ggplot(parameter_df) + geom_line(aes(x = age, y = survival)) + 
  labs(subtitle = "(a)", x = "Age", y = "Survival")
figB <- ggplot(parameter_df) + geom_line(aes(x = age, y = vulnerability)) + 
  labs(subtitle = "(b)", x = "Age", y = "Vulnerability to fishery")
figC <- ggplot(parameter_df) + geom_line(aes(x = age, y = fecundity)) + 
  labs(subtitle = "(c)", x = "Age", y = "Fecundity")
grid.arrange(figA, figB, figC, nrow = 3)


# Age projection
#initial recruitment
Nat[1,1] <- 1500

#initial numbers at ages 2 to n-1
for (a in 1:(nages-2)) {
  Nat[1,a+1] <- (1-va[a]*ut[1])*sa[a]*Nat[1,a]
}

#plus group age n
Nat[1,nages] <- (1-va[nages]*ut[1])*sa[nages] / 
  (1-(1-va[nages]*ut[1])*sa[nages]) * Nat[1,nages-1]

#egg production
Et[1] <- sum(feca*Nat[1,])

# Fill in remaining years
for (yr in 1:(nyears-1)) {
  #recruits
  if(states[yr+1] == 1){
    Nat[yr+1,1] <- alpha1*Et[yr]*exp(-beta1*Et[yr])*exp(rnorm(1,0,0.2))
  }else{
    Nat[yr+1,1] <- alpha2*Et[yr]*exp(-beta2*Et[yr])*exp(rnorm(1,0,0.2))
  }
  
  
  #numbers at other age groups except plus group
  for (a in 1:(nages-2)) {
    Nat[yr+1,a+1] <- (1-va[a]*ut[yr])*sa[a]*Nat[yr,a]
  }
  
  #numbers in plus group
  Nat[yr+1,nages] <- (1-va[nages]*ut[yr])*sa[nages]*
    (Nat[yr,nages]+Nat[yr,nages-1])
  
  #egg production
  Et[yr+1] <- sum(feca*Nat[yr+1,])
}


# Calculate biomass
# weight at age 
weighta <- c(0.16,1.6,3.2,4.4,5.6,7.2,8.8,10,11.2,12)
Btot <- vector(length=nyears)

for (yr in 1:nyears) {  
  Btot[yr] <- sum(Nat[yr,] * weighta)
}

# Plot spawning biomass by recruits
recs <- Nat[,1]

# plot
a <- tibble(state = states, rec = recs, spawn = Btot, logRS = log(recs/Btot))

ggplot(a) + geom_point(aes(x = seq(1,nyears), y = rec, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,nyears), y = rec)) + 
  labs(y = "recruitment", x = "year") +
  scale_color_discrete(name = "state")


ggplot(a) + geom_point(aes(x = seq(1,nyears), y = spawn, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,nyears), y = spawn)) + 
  labs(y = "spawning biomass", x = "year") +
  scale_color_discrete(name = "state")

ggplot(a) + geom_point(aes(x = spawn, y = rec, color = as.factor(state)), size = 3) +
  labs(x = "spawning biomass", y = "recruitment") +
  scale_color_discrete(name = "state")

ggplot(a) + geom_point(aes(x = spawn, y = logRS, color = as.factor(state)), size = 3) +
  labs(x = "spawning biomass", y = "log recruitment") +
  scale_color_discrete(name = "state")


#a_short <- a[-c(1:200),]


# fit HMM
mod <- depmix(logRS ~ spawn, data = a, nstates = 2, family = gaussian())
fit_mod <- fit(mod)
summary(fit_mod)
fit_post <- posterior(fit_mod)

df <- tibble(state = states,
             est_state = fit_post$state,
             year = seq(1, nyears))

df2 <- df %>% pivot_longer(cols = !(year), names_to = c("generation"), values_to = "state")
# this code is if the labels appear switched
df2 <- df2 %>% 
  mutate(state = ifelse(state == 2, 1, 2))
df2 <- df2 %>% 
  mutate(generation = ifelse(generation == "state", "actual", "predicted"))

ggplot(data = df2) + geom_point(aes(x = year, y = state, color = generation)) + theme(legend.title = element_blank())

# Seems to do a reasonable job at recovering parameters, so I'm going to start working on a sampling method
a <- a %>% 
  add_column(est_state = df$est_state)

ggplot(a) + geom_histogram(aes(x = spawn, fill = as.factor(state)), binwidth = 1000)
ggplot(a) + geom_histogram(aes(x = rec, fill = as.factor(state)), binwidth = 100)

ggplot(a) + geom_density(aes(x = rec, fill = as.factor(est_state)), alpha = 0.8) +
  labs(x = "recruitment", fill = "predicted state")
ggplot(a) + geom_point(aes(x = state, y = spawn, color = as.factor(est_state)))



# Sampling Procedure ------------------------------------------------------
est_state_1 <- a %>% 
  filter(est_state == 1)

est_state_2 <- a %>% 
  filter(est_state == 2)
# first step - determine the estimated state at the last time step
final_state <- pull(a[nrow(a), "est_state"])

# will use estimated transition matrix to determine the state
est_P <- t(matrix(getpars(fit_mod)[3:6], nrow = 2, ncol = 2))

# new function to predict states for n years 
run.pred.mc.sim <- function(P, num.iters = 50){
  
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

future_states <- run.pred.mc.sim(est_P, num.iters = 50)

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


rec_preds <- pred.rec(future_states, mu1 = mean(est_state_1$rec), mu2 = mean(est_state_2$rec),
                      sd1 = sd(est_state_1$rec), sd2 = sd(est_state_2$rec))

rec_pred_df <- tibble(year = seq(1, 100),
                      recruitment = c(a$rec, rec_preds),
                      state = c(a$state, future_states),
                      yr_type = c(rep("simulated", 50), rep("forecasted", 50)))

ggplot(rec_pred_df, aes(x = year, y = recruitment)) +
  geom_line() +
  geom_point(aes(color = as.factor(state), shape = yr_type), size = 3) +
  labs(shape = "data generation", color = "state")
