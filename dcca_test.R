library(tseries)

ts <- c(rnorm(100, mean = 10, sd = 2), rnorm(50, mean = 5, sd = 1), 
        rnorm(75, mean = 11, sd = 3))
plot(ts, type = "l")

acf(ts)
adf.test(ts)

ts2 <- c(rnorm(100, mean = 10, sd = 2), rnorm(50, mean = 5, sd = 2), 
         rnorm(75, mean = 11, sd = 2))
plot(ts2, type = "l")
adf.test(ts2)


cor(ts, ts2, method = "spearman")

DCCA_CC=function(x,y,k){
  ## calculate cumulative sum profile of all t
  xx<- cumsum(x - mean(x))  ## Equation 2
  yy<- cumsum(y - mean(y))  ## Equation 2
  
  ## Divide in to overlapping boxes of size k
  
  slide_win_xx = mat_sliding_window(xx,k)
  slide_win_yy = mat_sliding_window(yy,k)
  ## calculate linear fit value in each box 
  x_hat = t(apply(slide_win_xx,1,function(n) (lm(n~seq(1:length(n)))$fitted.values)))
  y_hat = t(apply(slide_win_yy,1,function(n) (lm(n~seq(1:length(n)))$fitted.values)))
  
  ##  Get detrend variance in each box with linear fit value (detrend by local trend).
  F2_dfa_x = c()
  F2_dfa_y = c()
  for(i in 1:nrow(x_hat)){
    ## Equation 4
    F2_dfa_x = c(F2_dfa_x,mean((xx[i:(i+k-1)]-x_hat[i,])^2))
  }
  for(i in 1:nrow(y_hat)){
    ## Equation 4
    F2_dfa_y = c(F2_dfa_y,mean((yy[i:(i+k-1)]-y_hat[i,])^2))
  }
  ## Average detrend variance over all boxes to obtain fluctuation
  F2_dfa_x = mean(F2_dfa_x) ## Equation 3
  F2_dfa_y = mean(F2_dfa_y) ## Equation 3
  
  ## Get detrended covariance of two profile
  F2_dcca = c()
  for(i in 1:nrow(x_hat)){
    ## Equation 5
    F2_dcca = c(F2_dcca,mean((xx[i:(i+k-1)]-x_hat[i,]) * (yy[i:(i+k-1)]-y_hat[i,]) ))
  }
  
  ## Equation 6
  F2_dcca = mean(F2_dcca)
  
  ## Calculate correlation coefficient 
  rho = F2_dcca / sqrt(F2_dfa_x * F2_dfa_y) ## Equation 1
  return(rho)
}

mat_sliding_window = function(xx,k){
  ## Function to generate boxes given dataset(xx) and box size (k)
  slide_mat=c()
  for (i in 1:(length(xx)-k+1)){
    slide_mat = rbind(slide_mat,xx[i:(i+k-1)] )
  }
  return(slide_mat)
}

DCCA_CC(ts,ts2,k) ##This give me 3.392



# going to test with real data from aurora stock assessment
k=5
DCCA_CC(aurora$Recruit_0, aurora$SpawnBio, k)

cor(aurora$Recruit_0, aurora$SpawnBio, method = "pearson")
cor(aurora$Recruit_0, aurora$SpawnBio, method = "spearman")
