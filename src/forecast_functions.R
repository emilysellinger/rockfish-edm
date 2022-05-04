library(stats4)
spawn_ts <- data.frame(y = runif(60, 50, 150))
rec_ts <- 0.8*spawn_ts/(1 + 1.2*spawn_ts)


plot(rec_ts$y, type = "b")

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



