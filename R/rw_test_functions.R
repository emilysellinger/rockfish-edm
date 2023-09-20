rec_RW <- function(x, df1){
  # subset data to window size
  dat <- df1[1:(x-1)]
  
  # fit RW model
  mod <- Arima(log(dat), order = c(0,1,0))
  sigmaR <- sqrt(mod$sigma2)
  
  # calculate prediction, won't make random, because I'm just testing the plots
  pred <- exp((forecast(mod, h = 1)$mean)[1] + rnorm(1, 0, sigmaR))
  return(pred)
}

lrec_RW <- function(x, df1){
  
  # subset data to window size
  dat <- df1[1:(x-5)]
  
  # fit RW model, won't use sd b/c only interested in point estimate for this test
  mod <- Arima(log(dat), order = c(0,1,0))

  # calculate prediction
  preds <- exp(forecast(mod, h = 5)$mean[5])
  
  return(preds)
}


short_RW_preds <- rep(NA, length(time_vec1))

for(i in 1:length(time_vec1)){
  short_RW_preds[i] <- rec_RW(time_vec1[i], rec_ts)
}

long_RW_preds <- rep(NA, length(time_vec1))

for(i in 1:length(time_vec1)){
  long_RW_preds[i] <- lrec_RW(time_vec1[i], rec_ts)
}


RW_preds <- tibble(year = pop_goa$year,
                   obs = pop_goa$recruits,
                   short = c(rep(NA, 19), short_RW_preds),
                   long = c(rep(NA, 19), long_RW_preds))



ggplot(data = RW_preds) + geom_line(aes(x = year, y = (obs/1000)), alpha = 0.5) +
  geom_point(aes(x = year, y = (obs/1000))) +
  geom_point(aes(x = year, y = (long/1000)), color = "#00A1B7") +
  geom_line(aes(x = year, y = (long/1000)), color = "#00A1B7", alpha = 0.5) +
  xlab("Year") + ylab("Recruitment ('000s)") +
  theme_minimal()
