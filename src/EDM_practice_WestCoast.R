# going to make some functions to make this process a bit quicker


# Functions ---------------------------------------------------------------
EDM_graph <- function(data, stock){
  
  n <- length(data)
  lib1 <- c(1, n-20)
  pred1 <- c(n-20 +1, n)
  lib2 <- c(1, n-10)
  pred2 <- c(n-10+1, n)
  
  # Plot for forecasts beginning at n - 20 years
  
  simplex_output1 <- simplex(data, lib1, pred1)
  plot(simplex_output1$E, simplex_output1$rho, type = "l",
       xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)", main = paste(stock, "20 yr test set"))
  
  
  # Plot for forecasts beginning at n - 10 years
  simplex_output2 <- simplex(data, lib2, pred2)
  plot(simplex_output2$E, simplex_output2$rho, type = "l",
       xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)", main = paste(stock, "10 yr test set"))
  
  # return a list
  a <- list(section1 = simplex_output1$rho, section2 = simplex_output2$rho)
  return(a)
}

SMAP_graph <- function(data, stock, E1, E2){
  
  n <- length(data)
  lib1 <- c(1, n-20)
  pred1 <- c(n-20 +1, n)
  lib2 <- c(1, n-10)
  pred2 <- c(n-10+1, n)
  
  smap_output1 <- s_map(data, lib1, pred1, E = E1)
  plot(smap_output1$theta, smap_output1$rho, type = "l", 
       xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)", main = paste(stock, "20 yr test set"),
       sub = paste("Embedding dimestion = ", E1))
  
  smap_output2 <- s_map(data, lib2, pred2, E = E2)
  plot(smap_output2$theta, smap_output2$rho, type = "l", 
       xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)", main = paste(stock, "10 yr test set"),
       sub = paste("Embedding dimension =", E2))
}

EDM_forecasts <- function(data, stock, E1, E2, year){
  
  ts <- data$Recruit_0
  n <- length(ts)
  lib1 <- c(1, n-20)
  pred1 <- c(n-20 +1, n)
  lib2 <- c(1, n-10)
  pred2 <- c(n-10+1, n)
  
  simplex_output1 <- simplex(ts, lib1, pred1, E = E1, stats_only = FALSE)
  simplex_output2 <- simplex(ts, lib2, pred2, E = E2, stats_only = FALSE)
  
  preds1 <- na.omit(simplex_output1$model_output[[1]])
  preds2 <- na.omit(simplex_output2$model_output[[1]])
  
  
  plot(data$Yr, data$Recruit_0, type = "l", xlab = "year", ylab = "recruits", main = stock,
       sub = paste("E1 =", E1, ", E2 =", E2))
  lines(preds1$Index + year - 1, preds1$Predictions, col = "blue", lty = 2)
  lines(preds2$Index + year - 1, preds2$Predictions, col = "red", lty = 2)
  polygon(c(preds1$Index + year - 1, rev(preds1$Index + year - 1)),
          c(preds1$Predictions - sqrt(preds1$Pred_Variance), 
            rev(preds1$Predictions + sqrt(preds1$Pred_Variance))),
          col = rgb(0, 0, 1, 0.3), border = NA)
  polygon(c(preds2$Index + year - 1, rev(preds2$Index + year - 1)),
          c(preds2$Predictions - sqrt(preds2$Pred_Variance), 
            rev(preds2$Predictions + sqrt(preds2$Pred_Variance))),
          col = rgb(1, 0, 0, 0.3), border = NA)
  
}

# Aurora ------------------------------------------------------------------
index <- which(years$stock_names == "aurora")
stock <- "Aurora"
min_year <- years$min_yr[index]

aurora <- aurora %>% 
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(aurora$Yr, aurora$Recruit_0, type = "l")
acf(aurora$Recruit_0)

EDM_graph(aurora$Recruit_0, stock) # E1 = 2 (7 also has a peak), E2 = 7
SMAP_graph(aurora$Recruit_0, stock, E1 = 2, E2 = 7)
EDM_forecasts(aurora, stock, E1 = 2, E2 = 7, min_year)

# Black CA ----------------------------------------------------------------
index <- which(years$stock_names == "black_ca")
stock <- "Black CA"
min_year <- years$min_yr[index]

black_ca <- black_ca %>% 
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(black_ca$Yr, black_ca$Recruit_0, type = "l")
acf(black_ca$Recruit_0) # only at lag 1

EDM_graph(black_ca$Recruit_0, stock) # E1 = 2 or 5, E2 = 2
SMAP_graph(black_ca$Recruit_0, stock, E1 = 2, E2 = 2) # doesn't look like nonlinearity in 20 year
EDM_forecasts(black_ca, stock, E1 = 2, E2 = 2, min_year) # not a great forecast


# Black WA ----------------------------------------------------------------
index <- which(years$stock_names == "black_wa")
stock <- "Black WA"
min_year <- years$min_yr[index]

black_wa <- black_wa %>% 
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(black_wa$Yr, black_wa$Recruit_0, type = "l")
acf(black_wa$Recruit_0) # no autocorrelation

EDM_graph(black_wa$Recruit_0, stock) # E1 = 1, E2 = 2 (doesn't look nonlinear in long term)
SMAP_graph(black_wa$Recruit_0, stock, E1 = 1, E2 = 2) # doesn't look nonlinear in 20 year
EDM_forecasts(black_wa, stock, E1 = 1, E2 = 2, min_year) # also not great forecast


# Bocaccio ----------------------------------------------------------------
index <- which(years$stock_names == "bocaccio")
stock <- "Bocaccio"
min_year <- years$min_yr[index]

bocaccio <- bocaccio %>% 
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(bocaccio$Yr, bocaccio$Recruit_0, type = "l")
acf(bocaccio$Recruit_0) # slight autocorrelation at lag = 4

EDM_graph(bocaccio$Recruit_0, stock) # E1 = 7, E2 = 1 (rho for 20 yr not great, slightly better for 10 yr)
SMAP_graph(bocaccio$Recruit_0, stock, E1 = 7, E2 = 1) # doesn't look nonlinear in either test set
EDM_forecasts(bocaccio, stock, E1 = 7, E2 = 1, min_year) # doesn't do great at getting the peaks

# Cabezon NCS -------------------------------------------------------------
index <- which(years$stock_names == "cabezon_ncs")
stock <- "Cabezon NCS"
min_year <- years$min_yr[index]

cabezon_ncs <- cabezon_ncs %>% 
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(cabezon_ncs$Yr, cabezon_ncs$Recruit_0, type = "l")
acf(cabezon_ncs$Recruit_0) # slight autocorrelation at lag = 1

EDM_graph(cabezon_ncs$Recruit_0, stock) # E1 = 1, E2 = 2 (doesn't look nonlinear)
SMAP_graph(cabezon_ncs$Recruit_0, stock, E1 = 1, E2 = 2) # doesn't look nonlinear in 20 year
EDM_forecasts(cabezon_ncs, stock, E1 = 1, E2 = 2, min_year) # also not great forecast


# Cabezon ORS -------------------------------------------------------------
index <- which(years$stock_names == "cabezon_ors")
stock <- "Cabezon ORS"
min_year <- years$min_yr[index]

cabezon_ors <- cabezon_ors %>% 
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(cabezon_ors$Yr, cabezon_ors$Recruit_0, type = "l")
acf(cabezon_ors$Recruit_0) # no autocorrelation

EDM_graph(cabezon_ors$Recruit_0, stock) # E1 = 4, E2 = 8
SMAP_graph(cabezon_ors$Recruit_0, stock, E1 = 4, E2 = 8) # doesn't look nonlinear in 20 year
EDM_forecasts(cabezon_ors, stock, E1 = 4, E2 = 8, min_year) # seems to be focused around average, high sd


# Cabezon SCS -------------------------------------------------------------
index <- which(years$stock_names == "cabezon_scs")
stock <- "Cabezon SCS"
min_year <- years$min_yr[index]

cabezon_scs <- cabezon_scs %>% 
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(cabezon_scs$Yr, cabezon_scs$Recruit_0, type = "l")
acf(cabezon_scs$Recruit_0) # no autocorrelation

EDM_graph(cabezon_scs$Recruit_0, stock) # E1 = 4 (higher peak at 9, but going with simpler), E2 = 1
# also has another peak at 9, but going with simpler model
SMAP_graph(cabezon_scs$Recruit_0, stock, E1 = 4, E2 = 1) # theta > 0, but not by a lot
EDM_forecasts(cabezon_scs, stock, E1 = 4, E2 = 1, min_year) # really bad at 10 years, high sd


# Canary ------------------------------------------------------------------
# going to only do area 1 for canary
index <- which(years$stock_names == "canary")
stock <- "Canary"
min_year <- years$min_yr[index]

canary1 <- canary %>%
  filter(Area == 1) %>% 
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(canary1$Yr, canary1$Recruit_0, type = "l")
acf(canary1$Recruit_0) # some autocorrelation at lags 4, 6, 7

EDM_graph(canary1$Recruit_0, stock) # E1 = 5, E2 = 4 (rho in both cases is bad)
SMAP_graph(canary1$Recruit_0, stock, E1 = 5, E2 = 4) # no evidence of nonlinear dynamics
EDM_forecasts(canary1, stock, E1 = 5, E2 = 4, min_year) # bad


# Chilipepper -------------------------------------------------------------
index <- which(years$stock_names == "chilipepper")
stock <- "Chilipepper"
min_year <- years$min_yr[index]

chilipepper <- chilipepper %>%
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(chilipepper$Yr, chilipepper$Recruit_0, type = "l")
acf(chilipepper$Recruit_0) # some autocorrelation at lag = 15

EDM_graph(chilipepper$Recruit_0, stock) #E1 = 5 (rho bad), E2 = 6, 9 (rho better, will go with 6)
SMAP_graph(chilipepper$Recruit_0, stock, E1 = 5, E2 = 6) # not great evidence at 20 years, better at 10 years
EDM_forecasts(chilipepper, stock, E1 = 5, E2 = 6, min_year) # relatively good


# Darkblotched ------------------------------------------------------------
index <- which(years$stock_names == "darkblotched")
stock <- "Darkblotched"
min_year <- years$min_yr[index]

darkblotched <- darkblotched %>%
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(darkblotched$Yr, darkblotched$Recruit_0, type = "l")
acf(darkblotched$Recruit_0) # no autocorrelation

EDM_graph(darkblotched$Recruit_0, stock) #E1 = 4, E2 = 2
SMAP_graph(darkblotched$Recruit_0, stock, E1 = 4, E2 = 2) # theta >0, not great rho
EDM_forecasts(darkblotched, stock, E1 = 4, E2 = 2, min_year) # didn't do great for the peaks


# Dover Sole --------------------------------------------------------------
index <- which(years$stock_names == "dover_sole")
stock <- "Dover Sole"
min_year <- years$min_yr[index]

dover_sole <- dover_sole %>%
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(dover_sole$Yr, dover_sole$Recruit_0, type = "l")
acf(dover_sole$Recruit_0) # fair amount of autocorrelation, especially lag = 5

EDM_graph(dover_sole$Recruit_0, stock) #E1 = 5 (went w/ simpler model), E2 = 5
SMAP_graph(dover_sole$Recruit_0, stock, E1 = 5, E2 = 5) # no nonlinearity at 20 year, not great for 10 year
EDM_forecasts(dover_sole, stock, E1 = 5, E2 = 5, min_year) # high sd


# Kelp Greenling ----------------------------------------------------------
index <- which(years$stock_names == "kelp_greenling")
stock <- "Kelp Greenling"
min_year <- years$min_yr[index]

kelp_greenling <- kelp_greenling %>%
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(kelp_greenling$Yr, kelp_greenling$Recruit_0, type = "l")
acf(kelp_greenling$Recruit_0) # autocorrelation at lag = 1

EDM_graph(kelp_greenling$Recruit_0, stock) #E1 = 3, E2 = 1
SMAP_graph(kelp_greenling$Recruit_0, stock, E1 = 3, E2 = 1) # not great evidence of nonlinearity
EDM_forecasts(kelp_greenling, stock, E1 = 3, E2 = 1, min_year) # high sd, better at following shape 


# Lingcod North -----------------------------------------------------------
index <- which(years$stock_names == "lingcod_n")
stock <- "Lingcod North"
min_year <- years$min_yr[index]

lingcod_n <- lingcod_n %>%
  filter(Yr >= years$min_yr[index]) %>% 
  filter(Yr <= years$max_yr[index])

plot(lingcod_n$Yr, lingcod_n$Recruit_0, type = "l")
acf(lingcod_n$Recruit_0) # autocorrelation at lag = 1

EDM_graph(lingcod_n$Recruit_0, stock) #E1 = 2, E2 = 2 (rho really bad for both)
SMAP_graph(lingcod_n$Recruit_0, stock, E1 = 2, E2 = 2) # not great evidence of nonlinearity
EDM_forecasts(lingcod_n, stock, E1 = 2, E2 = 2, min_year) # high sd, better at following shape 


