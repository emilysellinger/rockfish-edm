# Load data ---------------------------------------------------------------

aurora <- read_table(here("data/west_coast_stocks", "aurora_2013.txt"))
black_ca <- read_table(here("data/west_coast_stocks", "black_ca_2015.txt"))
black_wa <- read_table(here("data/west_coast_stocks", "black_wa_2015.txt"))
bocaccio <- read_table(here("data/west_coast_stocks", "bocaccio_2017.txt"))
cabezon_ncs <- read_table(here("data/west_coast_stocks", "cabezon_ncs_2019.txt"))
cabezon_ors <- read_table(here("data/west_coast_stocks", "cabezon_ors_2019.txt"))
cabezon_scs <- read_table(here("data/west_coast_stocks", "cabezon_scs_2019.txt"))
canary <- read_table(here("data/west_coast_stocks", "canary_2015.txt"))
chilipepper <- read_table(here("data/west_coast_stocks", "chilipepper_2015.txt"))
darkblotched <- read_table(here("data/west_coast_stocks", "darkblotched_2017.txt"))
dover_sole <- read_table(here("data/west_coast_stocks", "dover_sole_2021.txt"))
kelp_greenling <- read_table(here("data/west_coast_stocks", "kelp_greenling_2015.txt"))
lingcod_n <- read_table(here("data/west_coast_stocks", "lingcod_north_2021.txt"))
lingcod_s <- read_table(here("data/west_coast_stocks", "lingcod_south_2021.txt"))
petrale_sole <- read_table(here("data/west_coast_stocks", "petrale_sole_2019.txt"))
sablefish <- read_table(here("data/west_coast_stocks", "sablefish_2021.txt"))
splitnose <- read_table(here("data/west_coast_stocks", "splitnose_2009.txt"))
widow <- read_table(here("data/west_coast_stocks", "widow_2019.txt"))
yelloweye <- read_table(here("data/west_coast_stocks", "yelloweye_2017.txt"))


# filter data -------------------------------------------------------------

snames <- c("aurora", "black_ca", "black_wa", "bocaccio", "cabezon_ncs", "cabezon_ors", "cabezon_scs",
            "canary", "chilipepper", "darkblotched", "dover_sole", "kelp_greenling", "lingcod_n", "lingcod_s",
            "petrale_sole", "sablefish", "splitnose", "widow", "yelloweye")

remove_excess <- function(df){
  name <- deparse(substitute(df))
  df <- df %>% 
    dplyr::select(Area, Yr, Era, SpawnBio, Recruit_0) %>% 
    add_column(stock_name = name)
  return(df)
}

aurora <- remove_excess(aurora)
black_ca <- remove_excess(black_ca)
black_wa <- remove_excess(black_wa)
bocaccio <- remove_excess(bocaccio)
cabezon_ncs <- remove_excess(cabezon_ncs)
cabezon_ors <- remove_excess(cabezon_ors)
cabezon_scs <- remove_excess(cabezon_scs)
canary <- remove_excess(canary)
chilipepper <- remove_excess(chilipepper)
darkblotched <- remove_excess(darkblotched)
dover_sole <- remove_excess(dover_sole)
kelp_greenling <- remove_excess(kelp_greenling)
lingcod_n <- remove_excess(lingcod_n)
lingcod_s <- remove_excess(lingcod_s)
petrale_sole <- remove_excess(petrale_sole)
sablefish <- remove_excess(sablefish)
splitnose <- remove_excess(splitnose)
widow <- remove_excess(widow)
yelloweye <- remove_excess(yelloweye)


stocks <- rbind(aurora, black_ca, black_wa, bocaccio, cabezon_ncs, cabezon_ors, cabezon_scs, canary, chilipepper,
                darkblotched, dover_sole, kelp_greenling, lingcod_n, lingcod_s, petrale_sole, sablefish, splitnose,
                widow, yelloweye)


# filter model run in time ------------------------------------------------
# For West Coast stocks, the first year used in this analysis will be the 
# start year for the main_rec_dev period. The main_rec_dev period can be
# found in each stock's Report.sso file. The final year used in this anaylsis will
# be the last year of recruiment data from each stock assessment

# main rec start data
rec_dev_start <- read_csv(here('data/west_coast_stocks/west_coast_main_rec_dev_start.csv'))
west_coast_years <- tibble(stock_name = snames,
                min_yr = rec_dev_start$main_rec_dev_start,
                max_yr = c(2012, 2014, 2014, 2015, 2018, 2018, 2018, 2014, 2014, 2016, 2020, 2014, 2020, 2020,
                           2018, 2020, 2008, 2018, 2016))

filter_sr_data <- function(df){
  stock_name <- pull(df[1, "stock_name"])
  row_num <- which(west_coast_years[, "stock_name"] == stock_name)
  
  min_year <- pull(west_coast_years[row_num, "min_yr"])
  max_year <- pull(west_coast_years[row_num, "max_yr"])
  
  df <- df %>% 
    filter(Yr >= min_year) %>% 
    filter(Yr <= max_year)
  
  return(df)
}

# Time Series Characteristics -------------------------------------------------------------------------
library(FSA)

ts_range <- west_coast_years[,"max_yr"] - west_coast_years[,"min_yr"]
quantile(ts_range[,"max_yr"], probs = c(0.25, 0.5, 0.75))

west_coast_ts_characteristics <- tibble(stock_name = snames,
                    depletion = rep(NA, length(snames)),
                    autocorrS = rep(NA, length(snames)),
                    autocorrR = rep(NA, length(snames)),
                    num_yrs = ts_range[,"max_yr"],
                    log_sigmaR_full = rep(NA, length(snames)),
                    log_sigmaR_test = rep(NA, length(snames)),
                    regime_shift = rep(NA, length(snames)),
                    detectable_SR = rep(NA, length(snames)))

for(i in snames){
  # find row for stock in summary csv file
  row1 <- which(west_coast_ts_characteristics[, "stock_name"] == i)
  # filter stock to single df
  stock <- stocks %>% 
    filter(stock_name == i)
  # remove excess years
  stock <- filter_sr_data(stock)
  
  
  # summarize characteristics by assessment area
  quants <- stock %>% 
    group_by(Area) %>% 
    summarise(dep = quantile(SpawnBio, probs = 0.05)/quantile(SpawnBio, probs = 0.95),
              acS = pacf(SpawnBio, plot = F)$acf[1],
              acR = pacf(Recruit_0, plot = F)$acf[1],
              sigmaR_full = sd(log(Recruit_0)),
              sigmaR_test = sd(log(Recruit_0[30:length(Recruit_0)])))
  
  dep <- mean(quants$dep)
  acS <- mean(quants$acS)
  acR <- mean(quants$acR)
  sigmaR_full <- mean(quants$sigmaR_full)
  sigmaR_test <- mean(quants$sigmaR_test)
  
  # save values to csv file
  west_coast_ts_characteristics[row1, "depletion"] <- dep
  west_coast_ts_characteristics[row1, "autocorrS"] <- acS
  west_coast_ts_characteristics[row1, "autocorrR"] <- acR
  west_coast_ts_characteristics[row1, "log_sigmaR_full"] <- sigmaR_full
  west_coast_ts_characteristics[row1, "log_sigmaR_test"] <- sigmaR_test
  
  
  # regime shifts
  stock <- stock %>% filter(Area == 1)
  
  # calculate change points for full time series
  fitPelt <- cpt.mean(log(stock$Recruit_0),method="PELT",test.stat="Normal",penalty="Manual",
                       pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
  changes	<- fitPelt@cpts
  
  if(length(changes)> 1){
    west_coast_ts_characteristics[row1, "regime_shift"] <- 1
  }else{
    west_coast_ts_characteristics[row1, "regime_shift"] <- 0
  }
  
  # determine if SR relationship is present
  west_coast_ts_characteristics[row1, "detectable_SR"] <- find_SR_relationship(stock)
  
  
}

# Save time series characteristics to data file
write_csv(west_coast_ts_characteristics, here("data", "west_coast_stock_characteristics.csv"))

## SR functions ---------------------------------------------------------------
# find starting values for beverton holt and ricker curves
find_SR_relationship <- function(stock){
  ricker_starts <- srStarts(Recruit_0~SpawnBio, data = stock, type = "Ricker", param = 1)
  bevholt_starts <- srStarts(Recruit_0~SpawnBio, data = stock, type = "BevertonHolt", param = 1)
  
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
      filter(SpawnBio <= 1.5*max_sb)
    # some of the ricker curves don't fit as well, will add a workaround
    # since the correlation will be negative (i.e. environmentally influenced)
    if(nrow(stock) <= 3){
      stock <- stocks %>% 
        filter(stock_name == i) %>% 
        filter(Area == 1)
    }
    # find spearman's correlation
    corrr <- cor.test(rank(stock$Recruit_0), rank(stock$SpawnBio))
    
    # determine driver - if lag0 is positive and significant, the stock has some evidence of SR relationship
    if(corrr$estimate > 0 && corrr$p.value < 0.05){
      SR_relationship <- 1
    }else{
      SR_relationship <- 0
    }
  }else{
    # get ranks for recruitment and spawning biomass
    stock <- stock %>%
      mutate(rec_rank = rank(Recruit_0)) %>%
      mutate(sb_rank = rank(SpawnBio))
    
    
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
  dats <- stock$SpawnBio
  datr <- stock$Recruit_0
  
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
  dats <- stock$SpawnBio
  datr <- stock$Recruit_0
  
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


# Save trimmed time series ------------------------------------------------
write_csv(filter_sr_data(aurora), here('data/west_coast_stocks/trimmed_time_series/aurora_trimmed.csv'))
write_csv(filter_sr_data(black_ca), here('data/west_coast_stocks/trimmed_time_series/black_ca_trimmed.csv'))
write_csv(filter_sr_data(black_wa), here('data/west_coast_stocks/trimmed_time_series/black_wa_trimmed.csv'))
write_csv(filter_sr_data(bocaccio), here('data/west_coast_stocks/trimmed_time_series/bocaccio_trimmed.csv'))
write_csv(filter_sr_data(cabezon_ncs), here('data/west_coast_stocks/trimmed_time_series/cabezon_ncs_trimmed.csv'))
write_csv(filter_sr_data(cabezon_ors), here('data/west_coast_stocks/trimmed_time_series/cabezon_ors_trimmed.csv'))
write_csv(filter_sr_data(cabezon_scs), here('data/west_coast_stocks/trimmed_time_series/cabezon_scs_trimmed.csv'))
write_csv(filter_sr_data(canary), here('data/west_coast_stocks/trimmed_time_series/canary_trimmed.csv'))
write_csv(filter_sr_data(chilipepper), here('data/west_coast_stocks/trimmed_time_series/chilipepper_trimmed.csv'))
write_csv(filter_sr_data(darkblotched), here('data/west_coast_stocks/trimmed_time_series/darkblotched_trimmed.csv'))
write_csv(filter_sr_data(dover_sole), here('data/west_coast_stocks/trimmed_time_series/dover_sole_trimmed.csv'))
write_csv(filter_sr_data(kelp_greenling), here('data/west_coast_stocks/trimmed_time_series/kelp_greenling_trimmed.csv'))
write_csv(filter_sr_data(lingcod_n), here('data/west_coast_stocks/trimmed_time_series/lingcod_n_trimmed.csv'))
write_csv(filter_sr_data(lingcod_s), here('data/west_coast_stocks/trimmed_time_series/lingcod_s_trimmed.csv'))
write_csv(filter_sr_data(petrale_sole), here('data/west_coast_stocks/trimmed_time_series/petrale_sole_trimmed.csv'))
write_csv(filter_sr_data(sablefish), here('data/west_coast_stocks/trimmed_time_series/sablefish_trimmed.csv'))
write_csv(filter_sr_data(splitnose), here('data/west_coast_stocks/trimmed_time_series/splitnose_trimmed.csv'))
write_csv(filter_sr_data(widow), here('data/west_coast_stocks/trimmed_time_series/widow_trimmed.csv'))
write_csv(filter_sr_data(yelloweye), here('data/west_coast_stocks/trimmed_time_series/yelloweye_trimmed.csv'))


