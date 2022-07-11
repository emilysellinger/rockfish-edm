# practice with HMMs

# Load packages -----------------------------------------------------------
library(depmixS4)
library(Rlab)
library(tidyverse)

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
states <- run.mc.sim(P, num.iters = 500)
plot(states)


# Age Matrix
# I adapted this code from Trevor's 458 class
nages <- 10      #number of ages in the model
nyears <- 500    #number of years in the model

Nat <- matrix(nrow=nyears, ncol=nages)
# egg production each year
Et <- vector(length = nyears)

# alpha and beta parameters for S-R function
alpha1 <- 6.8
beta1 <- 0.001
alpha2 <- 3
beta2 <- 0.0004

# survival by age
sa <- c(0.7,0.85,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9)
# vulnerability by age
va <- c(0.0,0.2, 0.5,0.8,1.0,1.0,1.0,1.0,1.0,1.0)
# fecundity by age
feca <- c(0,0,0.1,0.5,0.9,1,1,1,1,1)
# exploitation rate by year, first year is 0 (unfished)
ut <- c(0, rep(x=0.1, times=nyears-1))

# Age projection
#initial recruitment
Nat[1,1] <- 400

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

plot(Btot, recs)
plot(seq(201,500), Btot[-c(1:200)], type = "l")
plot(seq(1, 500), recs, type = "l")


# plot
a <- tibble(state = states, rec = recs, spawn = Btot, logRS = log(recs/Btot))

ggplot(a) + geom_point(aes(x = seq(1,500), y = rec, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,500), y = rec)) + 
  labs(y = "recruitment", x = "year") +
  scale_color_discrete(name = "state")


ggplot(a) + geom_point(aes(x = seq(1,500), y = spawn, color = as.factor(state)), size = 3) + 
  geom_line(aes(x= seq(1,500), y = spawn)) + 
  labs(y = "spawning biomass", x = "year") +
  scale_color_discrete(name = "state")

ggplot(a) + geom_point(aes(x = spawn, y = rec, color = as.factor(state)), size = 3) +
  labs(x = "spawning biomass", y = "recruitment") +
  scale_color_discrete(name = "state")

ggplot(a) + geom_point(aes(x = spawn, y = logRS, color = as.factor(state)), size = 3) +
  labs(x = "spawning biomass", y = "log recruitment") +
  scale_color_discrete(name = "state")


a_short <- a[-c(1:200),]


# fit HMM
mod <- depmix(logRS ~ spawn, data = a_short, nstates = 2, family = gaussian())
fit_mod <- fit(mod)
summary(fit_mod)
fit_post <- posterior(fit_mod)

df <- tibble(state = states[-c(1:200)],
             est_state = fit_post$state)

ggplot(data = df) + geom_point(aes(x = seq(1,300), y = state), col = "red") +
  geom_point(aes(x = seq(1,300), y = est_state), col = "blue", alpha = 0.3)

# Seems to do a reasonable job at recovering parameters, so I'm going to start working on a sampling method
library(cluster)

sbclus <- kmeans(Btot, centers = 2)
#clusplot(Btot, sbclus$cluster, color = T, shade = T, labels = 2, lines = 0)

plot(sbclus$cluster, Btot)

recclus <- kmeans(recs, centers = 2)
