# initial data processing for Alaska

library(tidyverse)
library(here)


# load data ---------------------------------------------------------------
# Note: the BSAI stock of blackspotted and rougheye was initially included, but after
# trimming off model-run-in years, there were not enough years of data to forecast

arrowtooth_flounder_bsai <- read_csv(here("data/alaska_stocks", "arrowtooth_flounder_bsai.csv"))
atka_mackerel_bsai <- read_csv(here("data/alaska_stocks", "atka_mackerel_bsai.csv"))
blackspotted_rougheye_goa <- read_csv(here("data/alaska_stocks", "blackspotted_rougheye_goa.csv"))
dusky_goa <- read_csv(here("data/alaska_stocks", "dusky_goa.csv"))
greenland_turbot <- read_csv(here("data/alaska_stocks", "greenland_turbot.csv"))
kamchatka_flounder <- read_csv(here("data/alaska_stocks", "kamchatka_flounder.csv"))
northern_rock_sole <- read_csv(here("data/alaska_stocks", "n_rock_sole.csv"))
ns_rock_sole_goa <- read_csv(here("data/alaska_stocks", "n_s_rock_sole_goa.csv"))
northern_rockfish_goa <- read_csv(here("data/alaska_stocks", "northern_rockfish_goa.csv"))
pacific_cod_ebs <- read_csv(here("data/alaska_stocks", "pacific_cod_ebs.csv"))
pacific_cod_goa <- read_csv(here("data/alaska_stocks", "pacific_cod_goa.csv"))
pollock_ebs <- read_csv(here("data/alaska_stocks", "pollock_ebs.csv"))
pollock_goa <- read_csv(here("data/alaska_stocks", "pollock_goa.csv"))
pop_bsai <- read_csv(here("data/alaska_stocks", "pop_bsai.csv"))
pop_goa <- read_csv(here("data/alaska_stocks", "pop_goa.csv"))
sablefish_alaska <- read_csv(here("data/alaska_stocks", "sablefish.csv"))
yellowfin_sole_bsai <- read_csv(here("data/alaska_stocks", "yellowfin_sole_bsai.csv"))


# list of alaska stocks
snames_alaska <- c("arrowtooth_flounder_bsai", "atka_mackerel_bsai", "blackspotted_rougheye_bsai", "blackspotted_rougheye_goa",
                   "dusky_goa", "greenland_turbot", "kamchatka_flounder", "northern_rock_sole", "ns_rock_sole_goa", 
                   "northern_rockfish_goa", "pacific_cod_ebs", "pacific_cod_goa", "pollock_ebs", "pollock_goa",
                   "pop_bsai", "pop_goa", "sablefish_alaska", "yellowfin_sole_bsai")

add_name <- function(df){
  name <- deparse(substitute(df))
  df <- df %>% 
    add_column(stock_name = name)
  return(df)
}

arrowtooth_flounder_bsai <- add_name(arrowtooth_flounder_bsai)
atka_mackerel_bsai <- add_name(atka_mackerel_bsai) 
#blackspotted_rougheye_bsai <- add_name(blackspotted_rougheye_bsai) 
blackspotted_rougheye_goa <- add_name(blackspotted_rougheye_goa)
dusky_goa <- add_name(dusky_goa)
greenland_turbot <- add_name(greenland_turbot) 
kamchatka_flounder <- add_name(kamchatka_flounder)
northern_rock_sole <- add_name(northern_rock_sole)
ns_rock_sole_goa <- add_name(ns_rock_sole_goa) 
northern_rockfish_goa <- add_name(northern_rockfish_goa)
pacific_cod_ebs <- add_name(pacific_cod_ebs)
pacific_cod_goa <- add_name(pacific_cod_goa)
pollock_ebs <- add_name(pollock_ebs)
pollock_goa <- add_name(pollock_goa)
pop_bsai <- add_name(pop_bsai)
pop_goa <- add_name(pop_goa)
sablefish_alaska <- add_name(sablefish_alaska) 
yellowfin_sole_bsai <- add_name(yellowfin_sole_bsai)




alaska_stocks <- rbind(arrowtooth_flounder_bsai, atka_mackerel_bsai, blackspotted_rougheye_bsai, blackspotted_rougheye_goa,
                       dusky_goa, greenland_turbot, kamchatka_flounder, northern_rock_sole[,-c(3,5)], ns_rock_sole_goa[,-c(3,5)], 
                       northern_rockfish_goa, pacific_cod_ebs[,-c(3,5)], pacific_cod_goa, pollock_ebs[,-c(3,5)], pollock_goa[,-c(3,5)],
                       pop_bsai[,-c(3,5)], pop_goa, sablefish_alaska, yellowfin_sole_bsai)
# filter time series ------------------------------------------------------
pdf("data/raw_recruitment_time_series_alaska.pdf")
for(i in snames_alaska){
  df <- alaska_stocks %>% filter(stock_name == i)
  ts_plot <- ggplot(df, aes(x = year, y = recruits)) + geom_line() + 
    xlab("year") + ylab("recruits") + ggtitle(i)
  print(ts_plot)
}
dev.off()

alaska_years <- tibble(stock_name = snames_alaska,
                       min_yr = c(1976,1977,1995,1977,1979,1980,1991,1975,1977,1977,1977,1977,
                                  1964,1977,1977,1977,1977,1954),
                       max_yr = c(2019,2020,2014,2021,2020,2020,2020,2020,2021,2020,2020,2019,
                                  2020,2020,2017,2020,2018,2020))

# Time Series Characteristics ---------------------------------------------

filter_alaska_sr_data <- function(df){
  stock_name <- pull(df[1, "stock_name"])
  row_num <- which(alaska_years[, "stock_name"] == stock_name)
  
  min_year <- pull(alaska_years[row_num, "min_yr"])
  max_year <- pull(alaska_years[row_num, "max_yr"])
  
  df <- df %>% 
    filter(year >= min_year) %>% 
    filter(year <= max_year)
  
  return(df)
}

ts_range <- alaska_years[,"max_yr"] - alaska_years[,"min_yr"]
# not enough years for BSAI blackspotted rougheye
quantile(ts_range[,"max_yr"], probs = c(0.25, 0.5, 0.75))

alaska_ts_characteristics <- tibble(stock_name = snames_alaska,
                                    depletion = rep(NA, length(snames_alaska)),
                                    autocorrS = rep(NA, length(snames_alaska)),
                                    autocorrR = rep(NA, length(snames_alaska)),
                                    num_yrs = ts_range[,"max_yr"],
                                    log_sigmaR_full = rep(NA, length(snames_alaska)),
                                    log_sigmaR_test = rep(NA, length(snames_alaska)),
                                    regime_shift = rep(NA, length(snames_alaska)),
                                    detectable_SR = rep(NA, length(snames_alaska)))

for(i in snames_alaska){
  row1 <- which(alaska_ts_characteristics[, "stock_name"] == i)
  stock <- alaska_stocks %>% 
    filter(stock_name == i)
  
  stock <- filter_alaska_sr_data(stock)
  
  quants <- stock %>% 
    summarise(dep = quantile(sbiomass, probs = 0.05)/quantile(sbiomass, probs = 0.95),
              acS = pacf(sbiomass, plot = F)$acf[1],
              acR = pacf(recruits, plot = F)$acf[1],
              sigmaR_full = sd(log(recruits)),
              sigmaR_test = sd(log(recruits[20:length(recruits)])))
  
  
  
  alaska_ts_characteristics[row1, "depletion"] <- quants$dep
  alaska_ts_characteristics[row1, "autocorrS"] <- quants$acS
  alaska_ts_characteristics[row1, "autocorrR"] <- quants$acR
  alaska_ts_characteristics[row1, "log_sigmaR_full"] <- quants$sigmaR_full
  alaska_ts_characteristics[row1, "log_sigmaR_test"] <- quants$sigmaR_test
  
  # regime shifts
  # calculate change points for full time series
  fitPelt <- cpt.mean(log(stock$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                      pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
  changes	<- fitPelt@cpts
  
  if(length(changes)> 1){
    alaska_ts_characteristics[row1, "regime_shift"] <- 1
  }else{
    alaska_ts_characteristics[row1, "regime_shift"] <- 0
  }
  
  # determine if SR relationship is present
  alaska_ts_characteristics[row1, "detectable_SR"] <- find_SR_relationship(stock)
  
}

alaska_ts_characteristics$region <- c("BSAI", "BSAI", "BSAI", "GOA", "GOA", "BSAI","BSAI",
                                      "BSAI", "GOA", "GOA","BSAI", "GOA", "BSAI", "GOA", "BSAI",
                                      "GOA", "BSAI", "BSAI")

write_csv(alaska_ts_characteristics, here("data", "alaska_ts_characteristics.csv"))


## SR functions ---------------------------------------------------------------
# find starting values for beverton holt and ricker curves
find_SR_relationship <- function(stock){
  ricker_starts <- srStarts(recruits~sbiomass, data = stock, type = "Ricker", param = 1)
  bevholt_starts <- srStarts(recruits~sbiomass, data = stock, type = "BevertonHolt", param = 1)
  
  if(ricker_starts[2] < 0){ricker_starts[2] <- 0.000001} # make sure ricker b param is positive
  if(bevholt_starts[1] < 0){bevholt_starts[1] <- 0.000001} # make sure bevholt a param is positive
  if(bevholt_starts[2] < 0){bevholt_starts[2] <- 0.000001} # make sure bevholt b param is positive
  
  # fit ricker and beverton holt models to stock
  ricker_fit <- ricker(stock, ricker_starts)
  bevholt_fit <- bevholt(stock, bevholt_starts)
  
  # calculate AICc for both models
  params <- 3
  n <- nrow(stock)
  ricker_AICc <- AIC(ricker_fit) + ((2*params*(params+1)) /(n - params - 1))
  bevholt_AICc <- AIC(bevholt_fit) + ((2*params*(params+1)) /(n - params - 1))
  
  # calculate relative likelihood
  if(ricker_AICc < bevholt_AICc){
    rel_likelihood <- 1/(1 + exp((ricker_AICc - bevholt_AICc)/2))
    min_model <- "ricker"
  }else{
    rel_likelihood <- 1/(1 + exp((bevholt_AICc - ricker_AICc)/2))
    min_model <- "bevholt"
  }
  
  # correlation analysis
  if(min_model == "ricker" && rel_likelihood > 0.75){
    # find max SpBio
    ricker_b <-  exp(unname(ricker_fit@coef[2]))
    max_sb <- 1/ricker_b
    
    # find S-R pairs greater than max & remove from dataset
    stock <- stock %>% 
      filter(sbiomass <= 1.5*max_sb)
    # some of the ricker curves don't fit as well, will add a workaround
    # since the correlation will be negative (i.e. environmentally influenced)
    if(nrow(stock) <= 3){
      stock <- alaska_stocks %>% 
        filter(stock_name == i)
    }
    # find spearman's correlation
    corrr <- cor.test(rank(stock$recruits), rank(stock$sbiomass))
    
    # determine driver - if lag0 is positive and significant, the stock has some evidence of SR relationship
    if(corrr$estimate > 0 && corrr$p.value < 0.05){
      SR_relationship <- 1
    }else{
      SR_relationship <- 0
    }
  }else{
    # get ranks for recruitment and spawning biomass
    stock <- stock %>%
      mutate(rec_rank = rank(recruits)) %>%
      mutate(sb_rank = rank(sbiomass))
    
    
    # find cross correlation values
    stock_ccf <- ccf(stock$rec_rank, stock$sb_rank, lag.max = 10)
    stock_ccf_df <- data.frame(lag = stock_ccf$lag,
                               ccf = stock_ccf$acf)
    
    
    
    # save zero-lagged correlation value and threshold significance value
    zero_lag <- stock_ccf_df[11,2]
    
    stock_ccf_df <- stock_ccf_df %>% 
      filter(lag < 0)
    
    # Correlation
    corrr <- cor.test(rank(stock$rec_rank), rank(stock$sb_rank))
    
    if(zero_lag > 0 && corrr$p.value < 0.05){
      if(all(stock_ccf_df$ccf < zero_lag)){
        SR_relationship <- 1 
      }else{
        SR_relationship <- 2
      }
    }else{
      SR_relationship <- 0
    }
  }
  
  return(SR_relationship)
}

bevholt <- function(stock, starts_df){
  dats <- stock$sbiomass
  datr <- stock$recruits
  
  BHminusLL <- function(loga, logb, logsigmaR){
    # extract parameters
    a <- exp(loga); b <- exp(logb); sigmaR <- exp(logsigmaR)
    
    # make predictions
    pred <- log(a*dats/(1 + b*dats))
    
    #calculate negative log like
    NegLogL <- (-1)*sum(dnorm(log(datr), pred, sigmaR, log = TRUE))
    return(NegLogL)
  }
  
  
  starts <- list(loga = log(starts_df$a), logb = log(starts_df$b), logsigmaR = 5)
  mle_out <- mle(BHminusLL, start = starts)
  
  # return output
  return(mle_out)
}

ricker <- function(stock, starts_df){
  dats <- stock$sbiomass
  datr <- stock$recruits
  
  RminusLL <- function(loga, logb, logsigmaR){
    # extract parameters
    a <- exp(loga); b <- exp(logb); sigmaR <- exp(logsigmaR)
    
    # make predictions
    pred <- log(a*dats*exp(-b*dats))
    
    #calculate negative log like
    NegLogL <- (-1)*sum(dnorm(log(datr), pred, sigmaR, log = TRUE))
    return(NegLogL)
  }
  
  
  starts <- list(loga = log(starts_df$a), logb = log(starts_df$b), logsigmaR = 5)
  mle_out <- mle(RminusLL, start = starts)
  
  # return output
  return(mle_out)
}


