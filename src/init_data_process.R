library(tidyverse)
library(here)

# load data ---------------------------------------------------------------

aurora <- read_table(here("data", "aurora_2013.txt"))
black_ca <- read_table(here("data", "black_ca_2015.txt"))
black_wa <- read_table(here("data", "black_wa_2015.txt"))
bocaccio <- read_table(here("data", "bocaccio_2017.txt"))
cabezon_ncs <- read_table(here("data", "cabezon_ncs_2019.txt"))
cabezon_ors <- read_table(here("data", "cabezon_ors_2019.txt"))
cabezon_scs <- read_table(here("data", "cabezon_scs_2019.txt"))
canary <- read_table(here("data", "canary_2015.txt"))
chilipepper <- read_table(here("data", "chilipepper_2015.txt"))
darkblotched <- read_table(here("data", "darkblotched_2017.txt"))
dover_sole <- read_table(here("data", "dover_sole_2021.txt"))
kelp_greenling <- read_table(here("data", "kelp_greenling_2015.txt"))
lingcod_n <- read_table(here("data", "lingcod_north_2021.txt"))
lingcod_s <- read_table(here("data", "lingcod_south_2021.txt"))
petrale_sole <- read_table(here("data", "petrale_sole_2019.txt"))
sablefish <- read_table(here("data", "sablefish_2021.txt"))
splitnose <- read_table(here("data", "splitnose_2009.txt"))
widow <- read_table(here("data", "widow_2019.txt"))
yelloweye <- read_table(here("data", "yelloweye_2017.txt"))


# filter data -------------------------------------------------------------

snames <- c("aurora", "black_ca", "black_wa", "bocaccio", "cabezon_ncs", "cabezon_ors", "cabezon_scs",
            "canary", "chilipepper", "darkblotched", "dover_sole", "kelp_greenling", "lingcod_n", "lingcod_s",
            "petrale_sole", "sablefish", "splitnose", "widow", "yelloweye")

remove_excess <- function(df){
  name <- deparse(substitute(df))
  df <- df %>% 
    select(Area, Yr, Era, SpawnBio, Recruit_0) %>% 
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

# plot time series --------------------------------------------------------

pdf("data/raw_recruitment_time_series.pdf")
for(i in snames){
  df <- stocks %>% filter(stock_name == i)
  ts_plot <- ggplot(df, aes(x = Yr, y = Recruit_0, colour = as.factor(Area))) + geom_line() + 
    xlab("year") + ylab("recruits") + labs(color = "Area") + ggtitle(i)
  print(ts_plot)
}
dev.off()


# filter model run in time ------------------------------------------------
# for the EDM, going to need to remove model run in time, as well as the forecasting period for each
# species
# will likely prioritize the species with the longest time series for analysis

years <- tibble(stock_names = snames,
                min_yr = c(1963, 1963, 1963, 1960, 1962, 1980, 1975, 1960, 1966, 1975, 1975, 1980, 1960, 1970,
                           1950, 1960, 1975, 1965, 1960),
                max_yr = c(2012, 2014, 2014, 2015, 2018, 2018, 2018, 2014, 2014, 2016, 2020, 2014, 2020, 2020,
                           2018, 2020, 2008, 2018, 2016))
