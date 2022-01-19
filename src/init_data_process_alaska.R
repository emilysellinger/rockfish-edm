# initial data processing for 

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
                       dusky_goa, greenland_turbot, kamchatka_flounder, northern_rock_sole, ns_rock_sole_goa, 
                       northern_rockfish_goa, pacific_cod_ebs, pacific_cod_goa, pollock_ebs, pollock_goa,
                       pop_bsai, pop_goa, sablefish_alaska, yellowfin_sole_bsai)
# filter time series ------------------------------------------------------
pdf("data/raw_recruitment_time_series_alaska.pdf")
for(i in snames_alaska){
  df <- alaska_stocks %>% filter(stock_name == i)
  ts_plot <- ggplot(df, aes(x = year, y = recruits)) + geom_line() + 
    xlab("year") + ylab("recruits") + ggtitle(i)
  print(ts_plot)
}
dev.off()


