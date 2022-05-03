
rec_ts <- data.frame(y = rnorm(60, 100, 10))
plot(rec_ts$y, type = "b")

# Functions ---------------------------------------------------------------
# going to first define the forecasting functions

rec_mean <- function(X){
  df <- rec_ts[1:X, 1]
  mu <- mean(df)
  sigmaR <- sd(df)
  
  pred <- mu + rnorm(1, 0, sigmaR)
  
  return(pred)
}

test <- lapply(seq(40, length(rec_ts$y),1), rec_mean())


# Example code
X1 <- runif(50, 0, 1)
X2 <- runif(50, 0, 10) # I included another variable just for a better demonstration
Y <- runif(50, 0, 1)
df <- data.frame(X1,X2,Y)


rolling_lms <- lapply( seq(20,nrow(df) ), function(x) lm( Y ~ X1+X2, data = df[1:x , ]) )