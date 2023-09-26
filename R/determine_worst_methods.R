# coverage probability
# Load in forecast performance data files ---------------------------------------------

# Alaska
arrowtooth_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/arrowtooth_stats.Rds"))
atka_mackerel_bsai_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/atka_mackerel_bsai_stats.Rds"))
blackspotted_rougheye_goa_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/blackspotted_rougheye_goa_stats.Rds"))
dusky_goa_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/dusky_goa_stats.Rds"))
greenland_turbot_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/greenland_turbot_stats.Rds"))
kamchatka_flounder_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/kamchatka_flounder_stats.Rds"))
northern_rock_sole_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/northern_rock_sole_stats.Rds"))
northern_rockfish_goa_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/northern_rockfish_goa_stats.Rds"))
ns_rock_sole_goa_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/ns_rock_sole_goa_stats.Rds"))
pacific_cod_ebs_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/pacific_cod_ebs_stats.Rds"))
pacific_cod_goa_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/pacific_cod_goa_stats.Rds"))
pollock_ebs_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/pollock_ebs_stats.Rds"))
pollock_goa_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/pollock_goa_stats.Rds"))
pop_bsai_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/pop_bsai_stats.Rds"))
pop_goa_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/pop_goa_stats.Rds"))
sablefish_alaska_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/sablefish_alaska_stats.Rds"))
yellowfin_sole_bsai_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/yellowfin_sole_bsai_stats.Rds"))

# West Coast
aurora_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/aurora_stats.Rds"))
black_ca_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/black_ca_stats.Rds"))
black_wa_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/black_wa_stats.Rds"))
bocaccio_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/bocaccio_stats.Rds"))
cabezon_ncs_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/cabezon_ncs_stats.Rds"))
cabezon_ors_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/cabezon_ors_stats.Rds"))
cabezon_scs_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/cabezon_scs_stats.Rds"))
canary_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/canary1_stats.Rds"))
chilipepper_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/chilipepper_stats.Rds"))
darkblotched_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/darkblotched_stats.Rds"))
dover_sole_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/dover_sole_stats.Rds"))
kelp_greenling_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/kelp_greenling_stats.Rds"))
lingcod_n_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/lingcod_n_stats.Rds"))
lingcod_s_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/lingcod_s_stats.Rds"))
petrale_sole_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/petrale_sole_stats.Rds"))
sablefish_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/sablefish_stats.Rds"))
splitnose_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/splitnose_stats.Rds"))
widow_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/widow_stats.Rds"))
yelloweye_stats <- readRDS(here("results/simulation_results/west_coast/performance_stats/yelloweye1_stats.Rds"))



# Time series characteristics ---------------------------------------------
alaska_ts_characteristics <- read_csv(here("data/alaska_ts_characteristics.csv"))
west_coast_ts_characteristics <- read_csv(here("data/west_coast_stock_characteristics.csv"))

# Coverage probability data frames -------------------------------------------------
# Alaska
coverage_probs_short <- rbind(arrowtooth_stats$short_cov_prob, atka_mackerel_bsai_stats$short_cov_prob,
                              blackspotted_rougheye_goa_stats$short_cov_prob, dusky_goa_stats$short_cov_prob,
                              greenland_turbot_stats$short_cov_prob, kamchatka_flounder_stats$short_cov_prob,
                              northern_rock_sole_stats$short_cov_prob, northern_rockfish_goa_stats$short_cov_prob,
                              ns_rock_sole_goa_stats$short_cov_prob, pacific_cod_ebs_stats$short_cov_prob,
                              pacific_cod_goa_stats$short_cov_prob, pollock_ebs_stats$short_cov_prob,
                              pollock_goa_stats$short_cov_prob, pop_bsai_stats$short_cov_prob,
                              pop_goa_stats$short_cov_prob, sablefish_alaska_stats$short_cov_prob,
                              yellowfin_sole_bsai_stats$short_cov_prob)

coverage_probs_short$stock_name <- c(rep("arrowtooth_flounder_bsai", nrow(arrowtooth_stats$short_cov_prob)), rep("atka_mackerel_bsai", nrow(atka_mackerel_bsai_stats$short_cov_prob)),
                                     rep("blackspotted_rougheye_goa", nrow(blackspotted_rougheye_goa_stats$short_cov_prob)), rep("dusky_goa", nrow(dusky_goa_stats$short_cov_prob)),
                                     rep("greenland_turbot", nrow(greenland_turbot_stats$short_cov_prob)), rep("kamchatka_flounder", nrow(kamchatka_flounder_stats$short_cov_prob)),
                                     rep("northern_rock_sole", nrow(northern_rock_sole_stats$short_cov_prob)), rep("northern_rockfish_goa", nrow(northern_rockfish_goa_stats$short_cov_prob)),
                                     rep("ns_rock_sole_goa", nrow(ns_rock_sole_goa_stats$short_cov_prob)), rep("pacific_cod_ebs", nrow(pacific_cod_ebs_stats$short_cov_prob)),
                                     rep("pacific_cod_goa", nrow(pacific_cod_goa_stats$short_cov_prob)), rep("pollock_ebs", nrow(pollock_ebs_stats$short_cov_prob)),
                                     rep("pollock_goa", nrow(pollock_goa_stats$short_cov_prob)), rep("pop_bsai", nrow(pop_bsai_stats$short_cov_prob)),
                                     rep("pop_goa", nrow(pop_goa_stats$short_cov_prob)), rep("sablefish_alaska", nrow(sablefish_alaska_stats$short_cov_prob)),
                                     rep("yellowfin_sole_bsai", nrow(yellowfin_sole_bsai_stats$short_cov_prob)))

coverage_probs_long <- rbind(arrowtooth_stats$long_cov_prob, atka_mackerel_bsai_stats$long_cov_prob,
                             blackspotted_rougheye_goa_stats$long_cov_prob, dusky_goa_stats$long_cov_prob,
                             greenland_turbot_stats$long_cov_prob, kamchatka_flounder_stats$long_cov_prob,
                             northern_rock_sole_stats$long_cov_prob, northern_rockfish_goa_stats$long_cov_prob,
                             ns_rock_sole_goa_stats$long_cov_prob, pacific_cod_ebs_stats$long_cov_prob,
                             pacific_cod_goa_stats$long_cov_prob, pollock_ebs_stats$long_cov_prob,
                             pollock_goa_stats$long_cov_prob, pop_bsai_stats$long_cov_prob,
                             pop_goa_stats$long_cov_prob, sablefish_alaska_stats$long_cov_prob,
                             yellowfin_sole_bsai_stats$long_cov_prob)

coverage_probs_long$stock_name <- c(rep("arrowtooth_flounder_bsai", nrow(arrowtooth_stats$long_cov_prob)), rep("atka_mackerel_bsai", nrow(atka_mackerel_bsai_stats$long_cov_prob)),
                                    rep("blackspotted_rougheye_goa", nrow(blackspotted_rougheye_goa_stats$long_cov_prob)), rep("dusky_goa", nrow(dusky_goa_stats$long_cov_prob)),
                                    rep("greenland_turbot", nrow(greenland_turbot_stats$long_cov_prob)), rep("kamchatka_flounder", nrow(kamchatka_flounder_stats$long_cov_prob)),
                                    rep("northern_rock_sole", nrow(northern_rock_sole_stats$long_cov_prob)), rep("northern_rockfish_goa", nrow(northern_rockfish_goa_stats$long_cov_prob)),
                                    rep("ns_rock_sole_goa", nrow(ns_rock_sole_goa_stats$long_cov_prob)), rep("pacific_cod_ebs", nrow(pacific_cod_ebs_stats$long_cov_prob)),
                                    rep("pacific_cod_goa", nrow(pacific_cod_goa_stats$long_cov_prob)), rep("pollock_ebs", nrow(pollock_ebs_stats$long_cov_prob)),
                                    rep("pollock_goa", nrow(pollock_goa_stats$long_cov_prob)), rep("pop_bsai", nrow(pop_bsai_stats$long_cov_prob)),
                                    rep("pop_goa", nrow(pop_goa_stats$long_cov_prob)), rep("sablefish_alaska", nrow(sablefish_alaska_stats$long_cov_prob)),
                                    rep("yellowfin_sole_bsai", nrow(yellowfin_sole_bsai_stats$long_cov_prob)))


# west coast coverage probs
coverage_probs_short_wc <- rbind(aurora_stats$short_cov_prob, black_ca_stats$short_cov_prob, black_wa_stats$short_cov_prob,
                                 bocaccio_stats$short_cov_prob, cabezon_ncs_stats$short_cov_prob, cabezon_ors_stats$short_cov_prob,
                                 cabezon_scs_stats$short_cov_prob, canary_stats$short_cov_prob, chilipepper_stats$short_cov_prob,
                                 darkblotched_stats$short_cov_prob, dover_sole_stats$short_cov_prob, kelp_greenling_stats$short_cov_prob,
                                 lingcod_n_stats$short_cov_prob, lingcod_s_stats$short_cov_prob, petrale_sole_stats$short_cov_prob,
                                 sablefish_stats$short_cov_prob, splitnose_stats$short_cov_prob, widow_stats$short_cov_prob,
                                 yelloweye_stats$short_cov_prob)

coverage_probs_short_wc$stock_name <- c(rep("aurora", nrow(aurora_stats$short_cov_prob)),
                                        rep("black_ca", nrow(black_ca_stats$short_cov_prob)),
                                        rep("black_wa", nrow(black_wa_stats$short_cov_prob)),
                                        rep("bocaccio", nrow(bocaccio_stats$short_cov_prob)),
                                        rep("cabezon_ncs", nrow(cabezon_ncs_stats$short_cov_prob)),
                                        rep("cabezon_ors", nrow(cabezon_ors_stats$short_cov_prob)), 
                                        rep("cabezon_scs", nrow(cabezon_scs_stats$short_cov_prob)),
                                        rep("canary", nrow(canary_stats$short_cov_prob)),
                                        rep("chilipepper", nrow(chilipepper_stats$short_cov_prob)),
                                        rep("darkblotched", nrow(darkblotched_stats$short_cov_prob)), 
                                        rep("dover_sole", nrow(dover_sole_stats$short_cov_prob)),
                                        rep("kelp_greenling", nrow(kelp_greenling_stats$short_cov_prob)), 
                                        rep("lingcod_n", nrow(lingcod_n_stats$short_cov_prob)),
                                        rep("lingcod_s", nrow(lingcod_s_stats$short_cov_prob)), 
                                        rep("petrale", nrow(petrale_sole_stats$short_cov_prob)),
                                        rep("sablefish", nrow(sablefish_stats$short_cov_prob)),
                                        rep("splitnose", nrow(splitnose_stats$short_cov_prob)), 
                                        rep("widow", nrow(widow_stats$short_cov_prob)),
                                        rep("yelloweye", nrow(yelloweye_stats$short_cov_prob)))




coverage_probs_long_wc <- rbind(aurora_stats$long_cov_prob, black_ca_stats$long_cov_prob, black_wa_stats$long_cov_prob,
                                 bocaccio_stats$long_cov_prob, cabezon_ncs_stats$long_cov_prob, cabezon_ors_stats$long_cov_prob,
                                 cabezon_scs_stats$long_cov_prob, canary_stats$long_cov_prob, chilipepper_stats$long_cov_prob,
                                 darkblotched_stats$long_cov_prob, dover_sole_stats$long_cov_prob, kelp_greenling_stats$long_cov_prob,
                                 lingcod_n_stats$long_cov_prob, lingcod_s_stats$long_cov_prob, petrale_sole_stats$long_cov_prob,
                                 sablefish_stats$long_cov_prob, splitnose_stats$long_cov_prob, widow_stats$long_cov_prob,
                                 yelloweye_stats$long_cov_prob)

coverage_probs_long_wc$stock_name <- c(rep("aurora", nrow(aurora_stats$long_cov_prob)),
                                        rep("black_ca", nrow(black_ca_stats$long_cov_prob)),
                                        rep("black_wa", nrow(black_wa_stats$long_cov_prob)),
                                        rep("bocaccio", nrow(bocaccio_stats$long_cov_prob)),
                                        rep("cabezon_ncs", nrow(cabezon_ncs_stats$long_cov_prob)),
                                        rep("cabezon_ors", nrow(cabezon_ors_stats$long_cov_prob)), 
                                        rep("cabezon_scs", nrow(cabezon_scs_stats$long_cov_prob)),
                                        rep("canary", nrow(canary_stats$long_cov_prob)),
                                        rep("chilipepper", nrow(chilipepper_stats$long_cov_prob)),
                                        rep("darkblotched", nrow(darkblotched_stats$long_cov_prob)), 
                                        rep("dover_sole", nrow(dover_sole_stats$long_cov_prob)),
                                        rep("kelp_greenling", nrow(kelp_greenling_stats$long_cov_prob)), 
                                        rep("lingcod_n", nrow(lingcod_n_stats$long_cov_prob)),
                                        rep("lingcod_s", nrow(lingcod_s_stats$long_cov_prob)), 
                                        rep("petrale", nrow(petrale_sole_stats$long_cov_prob)),
                                        rep("sablefish", nrow(sablefish_stats$long_cov_prob)),
                                        rep("splitnose", nrow(splitnose_stats$long_cov_prob)), 
                                        rep("widow", nrow(widow_stats$long_cov_prob)),
                                        rep("yelloweye", nrow(yelloweye_stats$long_cov_prob)))


# Coverage probability analysis -------------------------------------------
# create data frames with the worst method for each stock by forecast length
worst_method_alaska_short <- coverage_probs_short %>% 
  group_by(stock_name) %>% 
  mutate(bad = min(coverage_prob)) %>%
  filter(coverage_prob == bad) %>% 
  ungroup() %>% 
  select(stock_name, method, coverage_prob)
#summarize
worst_method_alaska_short %>% 
  group_by(method) %>% summarize(n = n())


worst_method_alaska_long <- coverage_probs_long %>% 
  group_by(stock_name) %>% 
  mutate(bad = min(coverage_prob)) %>%
  filter(coverage_prob == bad) %>% 
  ungroup() %>% 
  select(stock_name, method, coverage_prob)

# summarize
worst_method_alaska_long %>% 
  group_by(method) %>% summarize(n = n())

worst_method_wc_short <- coverage_probs_short_wc %>% 
  group_by(stock_name) %>% 
  mutate(bad = min(coverage_prob)) %>%
  filter(coverage_prob == bad) %>% 
  ungroup() %>% 
  select(stock_name, method, coverage_prob)

worst_method_wc_short %>% 
  group_by(method) %>% summarize(n = n())

worst_method_wc_long <- coverage_probs_long_wc %>% 
  group_by(stock_name) %>% 
  mutate(bad = min(coverage_prob)) %>%
  filter(coverage_prob == bad) %>% 
  ungroup() %>% 
  select(stock_name, method, coverage_prob)

worst_method_wc_long %>% 
  group_by(method) %>% summarize(n = n())


# Also interested in which methods are the worst when you consider the absolute value
# of the difference from the coverage probability to the mean of the target range 
# (0.8-0.95)

worst_method_alaska_short2 <- coverage_probs_short %>% 
  group_by(stock_name) %>% 
  mutate(abs_value = abs(coverage_prob - 0.879)) %>%
  filter( abs(coverage_prob - 0.879) == max(abs_value)) %>% 
  ungroup() %>% 
  select(stock_name, method, coverage_prob)

worst_method_alaska_short2 %>% 
  group_by(method) %>% summarize(n = n())

worst_method_wc_short2 <- coverage_probs_short_wc %>% 
  group_by(stock_name) %>% 
  mutate(abs_value = abs(coverage_prob - 0.879)) %>%
  filter( abs(coverage_prob - 0.879) == max(abs_value)) %>% 
  ungroup() %>% 
  select(stock_name, method, coverage_prob)

worst_method_wc_short2 %>% 
  group_by(method) %>% summarize(n = n())

worst_method_alaska_long2 <- coverage_probs_long %>% 
  group_by(stock_name) %>% 
  mutate(abs_value = abs(coverage_prob - 0.875)) %>%
  filter( abs(coverage_prob - 0.875) == max(abs_value)) %>% 
  ungroup() %>% 
  select(stock_name, method, coverage_prob)

worst_method_alaska_long2 %>% 
  group_by(method) %>% summarize(n = n())

worst_method_wc_long2 <- coverage_probs_long_wc %>% 
  group_by(stock_name) %>% 
  mutate(abs_value = abs(coverage_prob - 0.875)) %>%
  filter( abs(coverage_prob - 0.875) == max(abs_value)) %>% 
  ungroup() %>% 
  select(stock_name, method, coverage_prob)

worst_method_wc_long2 %>% 
  group_by(method) %>% summarize(n = n())


# Save coverage probability data ---------------------------------------------------------------
# need to add some columns to distinguish between short and long term forecasts

short_coverage_probs_all <- rbind(coverage_probs_short, coverage_probs_short_wc)
short_coverage_probs_all$type <- rep("short", nrow(short_coverage_probs_all))

long_coverage_probs_all <- rbind(coverage_probs_long, coverage_probs_long_wc)
long_coverage_probs_all$type <- rep("long", nrow(long_coverage_probs_all))

# combine to one data frame
all_stocks_coverage_probs <- rbind(short_coverage_probs_all, long_coverage_probs_all)

# save as csv
write_csv(all_stocks_coverage_probs, here('results/simulation_results/all_stocks_coverage_probs.csv'))



# MASE analysis -----------------------------------------------------------
## Short forecasts ---------------------------------------------------------
# first create data frames for Alaska and West Coast stocks

alaska_mase_short <- rbind(arrowtooth_stats$mase_short, atka_mackerel_bsai_stats$mase_short,
                           blackspotted_rougheye_goa_stats$mase_short, dusky_goa_stats$mase_short,
                           greenland_turbot_stats$mase_short, kamchatka_flounder_stats$mase_short,
                           northern_rock_sole_stats$mase_short, northern_rockfish_goa_stats$mase_short,
                           ns_rock_sole_goa_stats$mase_short, pacific_cod_ebs_stats$mase_short,
                           pacific_cod_goa_stats$mase_short, pollock_ebs_stats$mase_short,
                           pollock_goa_stats$mase_short, pop_bsai_stats$mase_short,
                           pop_goa_stats$mase_short, sablefish_alaska_stats$mase_short,
                           yellowfin_sole_bsai_stats$mase_short)

alaska_mase_short$stock_name <- c(rep("arrowtooth_flounder_bsai", nrow(arrowtooth_stats$mase_short)), rep("atka_mackerel_bsai", nrow(atka_mackerel_bsai_stats$mase_short)),
                                    rep("blackspotted_rougheye_goa", nrow(blackspotted_rougheye_goa_stats$mase_short)), rep("dusky_goa", nrow(dusky_goa_stats$mase_short)),
                                    rep("greenland_turbot", nrow(greenland_turbot_stats$mase_short)), rep("kamchatka_flounder", nrow(kamchatka_flounder_stats$mase_short)),
                                    rep("northern_rock_sole", nrow(northern_rock_sole_stats$mase_short)), rep("northern_rockfish_goa", nrow(northern_rockfish_goa_stats$mase_short)),
                                    rep("ns_rock_sole_goa", nrow(ns_rock_sole_goa_stats$mase_short)), rep("pacific_cod_ebs", nrow(pacific_cod_ebs_stats$mase_short)),
                                    rep("pacific_cod_goa", nrow(pacific_cod_goa_stats$mase_short)), rep("pollock_ebs", nrow(pollock_ebs_stats$mase_short)),
                                    rep("pollock_goa", nrow(pollock_goa_stats$mase_short)), rep("pop_bsai", nrow(pop_bsai_stats$mase_short)),
                                    rep("pop_goa", nrow(pop_goa_stats$mase_short)), rep("sablefish_alaska", nrow(sablefish_alaska_stats$mase_short)),
                                    rep("yellowfin_sole_bsai", nrow(yellowfin_sole_bsai_stats$mase_short)))


wc_mase_short <- rbind(aurora_stats$mase_short, black_ca_stats$mase_short, black_wa_stats$mase_short,
                                bocaccio_stats$mase_short, cabezon_ncs_stats$mase_short, cabezon_ors_stats$mase_short,
                                cabezon_scs_stats$mase_short, canary_stats$mase_short, chilipepper_stats$mase_short,
                                darkblotched_stats$mase_short, dover_sole_stats$mase_short, kelp_greenling_stats$mase_short,
                                lingcod_n_stats$mase_short, lingcod_s_stats$mase_short, petrale_sole_stats$mase_short,
                                sablefish_stats$mase_short, splitnose_stats$mase_short, widow_stats$mase_short,
                                yelloweye_stats$mase_short)

wc_mase_short$stock_name <- c(rep("aurora", nrow(aurora_stats$mase_short)), rep("black_ca", nrow(black_ca_stats$mase_short)),
                        rep("black_wa", nrow(black_wa_stats$mase_short)), rep("bocaccio", nrow(bocaccio_stats$mase_short)),
                        rep("cabezon_ncs", nrow(cabezon_ncs_stats$mase_short)), rep("cabezon_ors", nrow(cabezon_ors_stats$mase_short)), 
                        rep("cabezon_scs", nrow(cabezon_scs_stats$mase_short)), rep("canary", nrow(canary_stats$mase_short)), 
                        rep("chilipepper", nrow(chilipepper_stats$mase_short)), rep("darkblotched", nrow(darkblotched_stats$mase_short)), 
                        rep("dover_sole", nrow(dover_sole_stats$mase_short)), rep("kelp_greenling", nrow(kelp_greenling_stats$mase_short)), 
                        rep("lingcod_n", nrow(lingcod_n_stats$mase_short)), rep("lingcod_s", nrow(lingcod_s_stats$mase_short)), 
                        rep("petrale", nrow(petrale_sole_stats$mase_short)), rep("sablefish", nrow(sablefish_stats$mase_short)),
                        rep("splitnose", nrow(splitnose_stats$mase_short)), rep("widow", nrow(widow_stats$mase_short)),
                        rep("yelloweye", nrow(yelloweye_stats$mase_short)))


# will look at MASE values for first 10 years, then later 10 years
# going to look at non-summarized violin plots 
# Alaska
alaska_mase_short <- alaska_mase_short %>% 
  mutate(period = case_when(
    year <= 10 ~ 'early',
    year <= 20 ~ 'mid',
    year > 20 ~ 'late'
  ))

a <- alaska_mase_short %>%
  filter(period == 'early') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = element_blank(), y = element_blank(), subtitle = '(a) Forecast years 1-10') +
  theme_minimal() + theme(legend.position = "none")
b <- alaska_mase_short %>%
  filter(period == 'mid') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = element_blank(), y = 'Forecast method', subtitle = '(b) Forecast years 11-20') +
  theme_minimal() + theme(legend.position = "none")

c <- alaska_mase_short %>%
  filter(period == 'late') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = 'MASE', y = element_blank(), subtitle = '(c) Forecast years > 20') +
  theme_minimal() + theme(legend.position = "none")

pdf(here('results/figures/alaska_MASE_1step_violin_plots.pdf'), width = 10, height = 10)
print(a + b + c + plot_layout(nrow = 3))
dev.off()


# West Coast
wc_mase_short <- wc_mase_short %>% 
  mutate(period = case_when(
    year <= 10 ~ 'early',
    year <= 20 ~ 'mid',
    year > 20 ~ 'late'
  ))

a <- wc_mase_short %>%
  filter(period == 'early') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = element_blank(), y = element_blank(), subtitle = '(a)') +
  theme_minimal() + theme(legend.position = "none")
b <- wc_mase_short %>%
  filter(period == 'mid') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = element_blank(), y = 'Forecast method', subtitle = '(b)') +
  theme_minimal() + theme(legend.position = "none")

c <- wc_mase_short %>%
  filter(period == 'late') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = 'MASE', y = element_blank(), subtitle = '(c)') +
  theme_minimal() + theme(legend.position = "none")

pdf(here('results/figures/west_coast_MASE_1step_violin_plots.pdf'), width = 10, height = 10)
print(a + b + c + plot_layout(nrow = 3))
dev.off()


## Long forecasts ----------------------------------------------------------
alaska_mase_long <- rbind(arrowtooth_stats$mase_long, atka_mackerel_bsai_stats$mase_long,
                           blackspotted_rougheye_goa_stats$mase_long, dusky_goa_stats$mase_long,
                           greenland_turbot_stats$mase_long, kamchatka_flounder_stats$mase_long,
                           northern_rock_sole_stats$mase_long, northern_rockfish_goa_stats$mase_long,
                           ns_rock_sole_goa_stats$mase_long, pacific_cod_ebs_stats$mase_long,
                           pacific_cod_goa_stats$mase_long, pollock_ebs_stats$mase_long,
                           pollock_goa_stats$mase_long, pop_bsai_stats$mase_long,
                           pop_goa_stats$mase_long, sablefish_alaska_stats$mase_long,
                           yellowfin_sole_bsai_stats$mase_long)

alaska_mase_long$stock_name <- c(rep("arrowtooth_flounder_bsai", nrow(arrowtooth_stats$mase_long)), rep("atka_mackerel_bsai", nrow(atka_mackerel_bsai_stats$mase_long)),
                                  rep("blackspotted_rougheye_goa", nrow(blackspotted_rougheye_goa_stats$mase_long)), rep("dusky_goa", nrow(dusky_goa_stats$mase_long)),
                                  rep("greenland_turbot", nrow(greenland_turbot_stats$mase_long)), rep("kamchatka_flounder", nrow(kamchatka_flounder_stats$mase_long)),
                                  rep("northern_rock_sole", nrow(northern_rock_sole_stats$mase_long)), rep("northern_rockfish_goa", nrow(northern_rockfish_goa_stats$mase_long)),
                                  rep("ns_rock_sole_goa", nrow(ns_rock_sole_goa_stats$mase_long)), rep("pacific_cod_ebs", nrow(pacific_cod_ebs_stats$mase_long)),
                                  rep("pacific_cod_goa", nrow(pacific_cod_goa_stats$mase_long)), rep("pollock_ebs", nrow(pollock_ebs_stats$mase_long)),
                                  rep("pollock_goa", nrow(pollock_goa_stats$mase_long)), rep("pop_bsai", nrow(pop_bsai_stats$mase_long)),
                                  rep("pop_goa", nrow(pop_goa_stats$mase_long)), rep("sablefish_alaska", nrow(sablefish_alaska_stats$mase_long)),
                                  rep("yellowfin_sole_bsai", nrow(yellowfin_sole_bsai_stats$mase_long)))


wc_mase_long <- rbind(aurora_stats$mase_long, black_ca_stats$mase_long, black_wa_stats$mase_long,
                       bocaccio_stats$mase_long, cabezon_ncs_stats$mase_long, cabezon_ors_stats$mase_long,
                       cabezon_scs_stats$mase_long, canary_stats$mase_long, chilipepper_stats$mase_long,
                       darkblotched_stats$mase_long, dover_sole_stats$mase_long, kelp_greenling_stats$mase_long,
                       lingcod_n_stats$mase_long, lingcod_s_stats$mase_long, petrale_sole_stats$mase_long,
                       sablefish_stats$mase_long, splitnose_stats$mase_long, widow_stats$mase_long,
                       yelloweye_stats$mase_long)

wc_mase_long$stock_name <- c(rep("aurora", nrow(aurora_stats$mase_long)), rep("black_ca", nrow(black_ca_stats$mase_long)),
                              rep("black_wa", nrow(black_wa_stats$mase_long)), rep("bocaccio", nrow(bocaccio_stats$mase_long)),
                              rep("cabezon_ncs", nrow(cabezon_ncs_stats$mase_long)), rep("cabezon_ors", nrow(cabezon_ors_stats$mase_long)), 
                              rep("cabezon_scs", nrow(cabezon_scs_stats$mase_long)), rep("canary", nrow(canary_stats$mase_long)), 
                              rep("chilipepper", nrow(chilipepper_stats$mase_long)), rep("darkblotched", nrow(darkblotched_stats$mase_long)), 
                              rep("dover_sole", nrow(dover_sole_stats$mase_long)), rep("kelp_greenling", nrow(kelp_greenling_stats$mase_long)), 
                              rep("lingcod_n", nrow(lingcod_n_stats$mase_long)), rep("lingcod_s", nrow(lingcod_s_stats$mase_long)), 
                              rep("petrale", nrow(petrale_sole_stats$mase_long)), rep("sablefish", nrow(sablefish_stats$mase_long)),
                              rep("splitnose", nrow(splitnose_stats$mase_long)), rep("widow", nrow(widow_stats$mase_long)),
                              rep("yelloweye", nrow(yelloweye_stats$mase_long)))


# will look at MASE values for first 10 years, then later 10 years
# going to look at non-summarized violin plots 
# Alaska
alaska_mase_long <- alaska_mase_long %>% 
  mutate(period = case_when(
    year <= 10 ~ 'early',
    year <= 20 ~ 'mid',
    year > 20 ~ 'late'
  ))

a <- alaska_mase_long %>%
  filter(period == 'early') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = element_blank(), y = element_blank(), subtitle = '(a) Forecast years 1-10') +
  theme_minimal() + theme(legend.position = "none")
b <- alaska_mase_long %>%
  filter(period == 'mid') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = element_blank(), y = 'Forecast method', subtitle = '(b) Forecast years 11-20') +
  theme_minimal() + theme(legend.position = "none")

c <- alaska_mase_long %>%
  filter(period == 'late') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = 'MASE', y = element_blank(), subtitle = '(c) Forecast years > 20') +
  theme_minimal() + theme(legend.position = "none")

pdf(here('results/figures/alaska_MASE_5step_violin_plots.pdf'), width = 10, height = 10)
print(a + b + c + plot_layout(nrow = 3))
dev.off()


# West Coast
wc_mase_long <- wc_mase_long %>% 
  mutate(period = case_when(
    year <= 10 ~ 'early',
    year <= 20 ~ 'mid',
    year > 20 ~ 'late'
  ))

a <- wc_mase_long %>%
  filter(period == 'early') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = element_blank(), y = element_blank(), subtitle = '(a) Forecast years 1-10') +
  theme_minimal() + theme(legend.position = "none")
b <- wc_mase_long %>%
  filter(period == 'mid') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = element_blank(), y = 'Forecast method', subtitle = '(b) Forecast years 11-20') +
  theme_minimal() + theme(legend.position = "none")

c <- wc_mase_long %>%
  filter(period == 'late') %>% 
  ggplot(aes(x = mase, y = method, fill = method)) + geom_violin() + 
  geom_boxplot(width = 0.1, color = "black") + 
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  scale_y_discrete(limits = rev) +
  labs(x = 'MASE', y = element_blank(), subtitle = '(c) Forecast years > 20') +
  theme_minimal() + theme(legend.position = "none")

pdf(here('results/figures/west_coast_MASE_5step_violin_plots.pdf'), width = 10, height = 10)
print(a + b + c + plot_layout(nrow = 3))
dev.off()


# want boxplots of MASE for each method without dividing by period
a <- MASE_short %>% 
  group_by(stock_name, method, period) %>% 
  summarise(median_mase = median(mase)) %>% 
  ggplot() + geom_boxplot(aes(x = factor(period, levels = c('early', 'mid', 'late')), y = median_mase, fill = period)) +
  geom_hline(yintercept = 1, linetype = 'dashed') +
  scale_fill_manual(values = c("#006475", "#55CFD8", "#00A1B7")) +
  labs(x = 'Forecast period', y = "Median stock\n MASE", subtitle = '(a)') +
  theme_minimal() + theme(legend.position = 'none') + facet_wrap(~ method)
  
b <- MASE_long %>% 
  group_by(stock_name, method, period) %>% 
  summarise(median_mase = median(mase)) %>% 
  ggplot() + geom_boxplot(aes(x = factor(period, levels = c('early', 'mid', 'late')), y = median_mase, fill = period)) +
  geom_hline(yintercept = 1, linetype = 'dashed') +
  scale_fill_manual(values = c("#006475", "#55CFD8", "#00A1B7")) +
  labs(x = 'Forecast period', y = "Median stock\n MASE", subtitle = '(b)') +
  theme_minimal() + theme(legend.position = 'none') + facet_wrap(~ method)

pdf(here('results/figures/median_MASE_boxplots_period.pdf'), width = 8, height = 12)
print(a + b + plot_layout(nrow = 2))
dev.off()



# % MASE below 1 ----------------------------------------------------------
# 1 step forecasts
tots <- MASE_short %>% 
  group_by(region, stock_name, method) %>% 
  summarise(n = n()) 

freq <- MASE_short %>%
  group_by(region, stock_name, method) %>% 
  summarise(mase_less_1 = sum(mase < 1))
  
freq <- left_join(freq, tots)

freq <- freq %>% 
  mutate(freq = mase_less_1/n)


MASE_freq1 <- freq %>% 
  ggplot() + geom_boxplot(aes(x = method, y = freq, fill = method)) +
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  facet_wrap(~region, nrow = 2) +
  ylim(0, 1) + labs(x = 'Forecast method', y = 'Frequency MASE < 1', subtitle = '(a)') +
  theme_minimal() + theme(legend.position = 'none', axis.text.x = element_text(angle = 45, vjust = 0.5))



MASE_freq_a <- ggplot(freq, aes(x = region, #factor(period, levels = c('early', 'mid', 'late')), 
                                y = freq, color = region)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("#006475", "#55CFD8", "#00A1B7")) +
  facet_wrap(~ method) + labs(x = 'Region', y = 'Frequency MASE < 1', subtitle = '(a)') +
  ylim(0, 1) + theme_minimal() + theme(legend.position = 'none')

# 5 step forecasts
tots2 <- MASE_long %>% 
  group_by(region, stock_name, method) %>% 
  summarise(n = n()) 

freq2 <- MASE_long %>%
  group_by(region, stock_name, method) %>% 
  summarise(mase_less_1 = sum(mase < 1))

freq2 <- left_join(freq2, tots2)

freq2 <- freq2 %>% 
  mutate(freq = mase_less_1/n)

MASE_freq2 <- freq2 %>% 
  ggplot() + geom_boxplot(aes(x = method, y = freq, fill = method)) +
  scale_fill_manual(values = c("#006475","#00A1B7", "#55CFD8", "#586028", "#898928", "#9DA7BF")) +
  facet_wrap(~region, nrow = 2) +
  ylim(0, 1) + labs(x = 'Forecast method', y = 'Frequency MASE < 1', subtitle = '(b)') +
  theme_minimal() + theme(legend.position = 'none', axis.text.x = element_text(angle = 45, vjust = 0.5))


MASE_freq_b <- ggplot(freq2, aes(x = factor(period, levels = c('early', 'mid', 'late')), 
                                y = freq, color = period)) + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("#006475", "#55CFD8", "#00A1B7")) +
  facet_wrap(~ method) + labs(x = 'Forecast period', y = 'Frequency MASE < 1', subtitle = '(b)') +
  ylim(0,1) + theme_minimal() + theme(legend.position = 'none')

pdf(here('results/figures/frequency_MASE_below_1.pdf'), height = 8.5)
print(MASE_freq_a + MASE_freq_b + plot_layout(nrow = 2))
dev.off()

pdf(here('results/figures/frequency_MASE_below_1_region.pdf'), height = 9.5, width = 10)
print(MASE_freq1 + MASE_freq2 + plot_layout(ncol = 2))
dev.off()


# look at short/long in 1 boxplot
freq$type <- rep('short', nrow(freq))
freq2$type <- rep('long', nrow(freq2))
mase_freq <- rbind(freq, freq2)

mase_freq_all <- mase_freq %>% 
  ggplot() + geom_boxplot(aes(x = method, y = freq, fill = factor(type, levels = c('short', 'long')))) + 
  scale_fill_manual(values = c("#00A1B7", "#898928")) +
  facet_wrap(~region, nrow = 3) + ylim(0, 1) + geom_hline(yintercept = 0.5, linetype = 'dashed') +
  labs(x = 'Forecast method', y = 'Frequency\n MASE < 1', fill = 'Forecast\n length') + 
  theme_minimal()

pdf(here('results/figures/frequency_MASE_below_1_region_length.pdf'))
print(mase_freq_all)
dev.off()


## Save data frames --------------------------------------------------------
# short forecasts
alaska_mase_short <- left_join(alaska_mase_short, alaska_ts_characteristics)
wc_mase_short <- left_join(wc_mase_short, west_coast_ts_characteristics)
wc_mase_short$region <- rep("West Coast", nrow(wc_mase_short))

MASE_short <- rbind(alaska_mase_short, wc_mase_short)

write_csv(MASE_short, here('results/simulation_results/all_stocks_short_MASE.csv'))

# long forecasts
alaska_mase_long <- left_join(alaska_mase_long, alaska_ts_characteristics)
wc_mase_long <- left_join(wc_mase_long, west_coast_ts_characteristics)
wc_mase_long$region <- rep("West Coast", nrow(wc_mase_long))

MASE_long <- rbind(alaska_mase_long, wc_mase_long)

write_csv(MASE_long, here('results/simulation_results/all_stocks_long_MASE.csv'))

