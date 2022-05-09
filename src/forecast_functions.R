library(stats4)
spawn_ts <- seq(0.1, 5000, length.out = 199)
rec_ts <- (1.1*spawn_ts/(1 + 0.002*spawn_ts))*exp(rnorm(length(spawn_ts), 0, 0.2))


plot(spawn_ts, rec_ts)

# Functions ---------------------------------------------------------------
# going to first define the forecasting functions

rec_mean <- function(x,df){
  # subset data to window size
  dat <- df[1:x, 1]
  
  # calculate mean and standard deviation of time series window
  mu <- mean(dat)
  sigmaR <- sd(dat)
  
  # calculate prediction
  pred <- mu + rnorm(1, 0, sigmaR)
  return(pred)

}

rec_AR <- function(x, df){
  # subset data to window size
  dat <- df[1:x, 1]
  
  # fit AR model and calculate standard deviation (atm doing separately)
  mod <- arima0(dat, order = c(1,0,0))
  sigmaR <- sd(dat)
  
  one_ahead <- predict(mod, n.head = 1)
  pred <- one_ahead$pred[1] + rnorm(1, 0, sigmaR)
  return(pred)
}


BHminusLL <- function(loga, logb, logsigmaR){
  # extract parameters
  a <- exp(loga); b <- exp(logb); sigma <- exp(logsigmaR)
  # make predictions
  pred <- a*dats/(1 + b*dats)
  
  #calculate negative log like
  NegLogL <- (-1)*sum(dnorm(datr, pred, sigmaR, log = TRUE))
  return(NegLogL)
}

rec_BH <- function(x, df1, df2){
  # subset spawning and recruit data to window size
  dats <- df1[1:x, 1]
  datr <- df2[1:x, 1]
  
  starts <- list(loga = log(0.6), logb = log(1), logsigmaR = 1)
  mle_out <- mle(BHminusLL, start = starts)
  
  return(mle_out)
}

# Predictions -------------------------------------------------------------
test <- seq(40, length(rec_ts$y),1)
rec_preds <- rep(NA, length(test))

for(i in 1:length(test)){
  rec_preds[i] <- rec_AR(test[i], rec_ts)
}



BHminusLL <- function(loga, logb, logsigmaR){
  # extract parameters
  a <- exp(loga); b <- exp(logb); sigmaR <- exp(logsigmaR)
  
  # make predictions
  pred <- a*spawn_ts/(1 + b*spawn_ts)
  
  #calculate negative log like
  NegLogL <- (-1)*sum(dnorm(rec_ts, pred, sigmaR, log = TRUE))
  return(NegLogL)
}

starts <- list(loga = log(1.1), logb = log(0.02), logsigmaR = 1)
mle_out <- mle(BHminusLL, start = starts)

a <- exp(coef(mle_out)[1])
b <- exp(coef(mle_out)[2])

preds <- a*spawn_ts/(1+b*spawn_ts)
dat <- tibble(biomass = spawn_ts,
              recruits = rec_ts,
              preds = preds)

ggplot(data = dat)+
  geom_point(aes(x = biomass, y = recruits)) +
  geom_line(aes(x = biomass, y = preds), color = "red")

library(FSA)

bevholt <- log(rec_ts)~log((a*spawn_ts)/(1+b*spawn_ts))
bhstarts <- srStarts(rec_ts~spawn_ts, type = "BevertonHolt", param = 1)
bhfit <- nls(bevholt, start = bhstarts)
