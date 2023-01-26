# Beverton Holt S-R code

library(stats4)
library(FSA)
# log likelihood for Beverton-Holt S-R
minusLL <- function(loga, logb, logsigma){
  # extract parameters
  a <- exp(loga); b <- exp(logb); sigma <- exp(logsigma)
  # make predictions
  pred <- a*Spawn/(1 + b*Spawn)
  
  #calculate negative log like
  NegLogL <- (-1)*sum(dnorm(Recruit, pred, sigma, log = TRUE))
  return(NegLogL)
}


# going to test function w/ aurora data
Recruit <- aurora$Recruit_0[50:99]
Spawn <- aurora$SpawnBio[50:99]
plot(Spawn, Recruit)

starts <- list(loga = log(0.6), logb = log(1), logsigma = 1)
mle_out <- mle(minusLL, start = starts)
summary(mle_out)

