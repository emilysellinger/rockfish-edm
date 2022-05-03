
rec_ts <- data.frame(y = rnorm(60, 100, 10))
plot(rec_ts$y, type = "b")

# Functions ---------------------------------------------------------------
# going to first define the forecasting functions

rec_mean <- function(x, ts){
  df <- ts[1:x, 1]
  mu <- mean(ts)
  sigmaR <- sd(ts)
  
}


rec_mean <- lm(y~1, data = rec_ts)
