# initial data processing for Alaska

library(tidyverse)
library(here)


# load data ---------------------------------------------------------------

arrowtooth_flounder_bsai <- read_csv(here("data", "arrowtooth_flounder_bsai.csv"))
atka_mackerel_bsai <- read_csv(here("data", "atka_mackerel_bsai.csv"))
blackspotted_rougheye_bsai <- read_csv(here("data", "blackspotted_rougheye_bsai.csv"))
blackspotted_rougheye_goa <- read_csv(here("data", "blackspotted_rougheye_goa.csv"))
dusky_goa <- read_csv(here("data", "dusky_goa.csv"))
greenland_turbot <- read_csv(here("data", "greenland_turbot.csv"))
kamchatka_flounder <- read_csv(here("data", "kamchatka_flounder.csv"))
northern_rock_sole <- read_csv(here("data", "n_rock_sole.csv"))
ns_rock_sole_goa <- read_csv(here("data", "n_s_rock_sole_goa.csv"))
northern_rockfish_goa <- read_csv(here("data", "northern_rockfish_goa.csv"))
pacific_cod_ebs <- read_csv(here("data", "pacific_cod_ebs.csv"))
pacific_cod_goa <- read_csv(here("data", "pacific_cod_goa.csv"))
pollock_ebs <- read_csv(here("data", "pollock_ebs.csv"))
pollock_goa <- read_csv(here("data", "pollock_goa.csv"))
pop_bsai <- read_csv(here("data", "pop_bsai.csv"))
pop_goa <- read_csv(here("data", "pop_goa.csv"))
sablefish_alaska <- read_csv(here("data", "sablefish.csv"))
yellowfin_sole_bsai <- read_csv(here("data", "yellowfin_sole_bsai.csv"))


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
blackspotted_rougheye_bsai <- add_name(blackspotted_rougheye_bsai) 
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
quantile(ts_range[,"max_yr"], probs = c(0.25, 0.5, 0.75))

alaska_ts_characteristics <- tibble(stock_name = snames_alaska,
                                        depletion = rep(NA, length(snames_alaska)),
                                        autocorrS = rep(NA, length(snames_alaska)),
                                        autocorrR = rep(NA, length(snames_alaska)),
                                        num_yrs = ts_range[,"max_yr"])

for(i in snames_alaska){
  row1 <- which(alaska_ts_characteristics[, "stock_name"] == i)
  stock <- alaska_stocks %>% 
    filter(stock_name == i)
  
  stock <- filter_alaska_sr_data(stock)
  
  quants <- stock %>% 
    summarise(dep = quantile(sbiomass, probs = 0.05)/quantile(sbiomass, probs = 0.95),
              acS = pacf(sbiomass, plot = F)$acf[1],
              acR = pacf(recruits, plot = F)$acf[1])
  
  dep <- quants$dep
  acS <- quants$acS
  acR <- quants$acR
  
  alaska_ts_characteristics[row1, "depletion"] <- dep
  alaska_ts_characteristics[row1, "autocorrS"] <- acS
  alaska_ts_characteristics[row1, "autocorrR"] <- acR
}

alaska_ts_characteristics$region <- c("BSAI", "BSAI", "BSAI", "GOA", "GOA", "BSAI","BSAI",
                                      "BSAI", "GOA", "GOA","BSAI", "GOA", "BSAI", "GOA", "BSAI",
                                      "GOA", "BSAI", "BSAI")

write_csv(alaska_ts_characteristics, here("data", "alaska_ts_characteristics.csv"))
