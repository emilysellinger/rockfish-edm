# coverage probability
# Load in forecast performance data files ---------------------------------------------
arrowtooth_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/arrowtooth_stats.Rds"))
atka_mackerel_bsai_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/atka_mackerel_bsai_stats.Rds"))
#blackspotted_rougheye_goa_stats <- readRDS(here("results/simulation_results/alaska/performance_stats/blackspotted_rougheye_goa_stats.Rds"))
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

alaska_ts_characteristics <- read_csv(here("data/alaska_ts_characteristics.csv"))


# coverage probs
coverage_probs_short <- rbind(arrowtooth_stats$short_cov_prob, atka_mackerel_bsai_stats$short_cov_prob,
                              dusky_goa_stats$short_cov_prob,
                              greenland_turbot_stats$short_cov_prob, kamchatka_flounder_stats$short_cov_prob,
                              northern_rock_sole_stats$short_cov_prob, northern_rockfish_goa_stats$short_cov_prob,
                              ns_rock_sole_goa_stats$short_cov_prob, pacific_cod_ebs_stats$short_cov_prob,
                              pacific_cod_goa_stats$short_cov_prob, pollock_ebs_stats$short_cov_prob,
                              pollock_goa_stats$short_cov_prob, pop_bsai_stats$short_cov_prob,
                              pop_goa_stats$short_cov_prob, sablefish_alaska_stats$short_cov_prob,
                              yellowfin_sole_bsai_stats$short_cov_prob)

coverage_probs_short$stock_name <- c(rep("arrowtooth_flounder_bsai", nrow(arrowtooth_stats$short_cov_prob)), rep("atka_mackerel_bsai", nrow(atka_mackerel_bsai_stats$short_cov_prob)),
                                     rep("dusky_goa", nrow(dusky_goa_stats$short_cov_prob)),
                                     rep("greenland_turbot", nrow(greenland_turbot_stats$short_cov_prob)), rep("kamchatka_flounder", nrow(kamchatka_flounder_stats$short_cov_prob)),
                                     rep("northern_rock_sole", nrow(northern_rock_sole_stats$short_cov_prob)), rep("northern_rockfish_goa", nrow(northern_rockfish_goa_stats$short_cov_prob)),
                                     rep("ns_rock_sole_goa", nrow(ns_rock_sole_goa_stats$short_cov_prob)), rep("pacific_cod_ebs", nrow(pacific_cod_ebs_stats$short_cov_prob)),
                                     rep("pacific_cod_goa", nrow(pacific_cod_goa_stats$short_cov_prob)), rep("pollock_ebs", nrow(pollock_ebs_stats$short_cov_prob)),
                                     rep("pollock_goa", nrow(pollock_goa_stats$short_cov_prob)), rep("pop_bsai", nrow(pop_bsai_stats$short_cov_prob)),
                                     rep("pop_goa", nrow(pop_goa_stats$short_cov_prob)), rep("sablefish_alaska", nrow(sablefish_alaska_stats$short_cov_prob)),
                                     rep("yellowfin_sole_bsai", nrow(yellowfin_sole_bsai_stats$short_cov_prob)))

coverage_probs_long <- rbind(arrowtooth_stats$long_cov_prob, atka_mackerel_bsai_stats$long_cov_prob,
                             dusky_goa_stats$long_cov_prob,
                             greenland_turbot_stats$long_cov_prob, kamchatka_flounder_stats$long_cov_prob,
                             northern_rock_sole_stats$long_cov_prob, northern_rockfish_goa_stats$long_cov_prob,
                             ns_rock_sole_goa_stats$long_cov_prob, pacific_cod_ebs_stats$long_cov_prob,
                             pacific_cod_goa_stats$long_cov_prob, pollock_ebs_stats$long_cov_prob,
                             pollock_goa_stats$long_cov_prob, pop_bsai_stats$long_cov_prob,
                             pop_goa_stats$long_cov_prob, sablefish_alaska_stats$long_cov_prob,
                             yellowfin_sole_bsai_stats$long_cov_prob)

coverage_probs_long$stock_name <- c(rep("arrowtooth_flounder_bsai", nrow(arrowtooth_stats$long_cov_prob)), rep("atka_mackerel_bsai", nrow(atka_mackerel_bsai_stats$long_cov_prob)),
                                    rep("dusky_goa", nrow(dusky_goa_stats$long_cov_prob)),
                                    rep("greenland_turbot", nrow(greenland_turbot_stats$long_cov_prob)), rep("kamchatka_flounder", nrow(kamchatka_flounder_stats$long_cov_prob)),
                                    rep("northern_rock_sole", nrow(northern_rock_sole_stats$long_cov_prob)), rep("northern_rockfish_goa", nrow(northern_rockfish_goa_stats$long_cov_prob)),
                                    rep("ns_rock_sole_goa", nrow(ns_rock_sole_goa_stats$long_cov_prob)), rep("pacific_cod_ebs", nrow(pacific_cod_ebs_stats$long_cov_prob)),
                                    rep("pacific_cod_goa", nrow(pacific_cod_goa_stats$long_cov_prob)), rep("pollock_ebs", nrow(pollock_ebs_stats$long_cov_prob)),
                                    rep("pollock_goa", nrow(pollock_goa_stats$long_cov_prob)), rep("pop_bsai", nrow(pop_bsai_stats$long_cov_prob)),
                                    rep("pop_goa", nrow(pop_goa_stats$long_cov_prob)), rep("sablefish_alaska", nrow(sablefish_alaska_stats$long_cov_prob)),
                                    rep("yellowfin_sole_bsai", nrow(yellowfin_sole_bsai_stats$long_cov_prob)))


# west coast
# coverage probs
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

west_coast_ts_characteristics <- read_csv(here("data/west_coast_stock_characteristics.csv"))

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
  mutate(abs_value = abs(coverage_prob - 0.875)) %>%
  filter( abs(coverage_prob - 0.875) == max(abs_value)) %>% 
  ungroup() %>% 
  select(stock_name, method, coverage_prob)

worst_method_alaska_short2 %>% 
  group_by(method) %>% summarize(n = n())

worst_method_wc_short2 <- coverage_probs_short_wc %>% 
  group_by(stock_name) %>% 
  mutate(abs_value = abs(coverage_prob - 0.875)) %>%
  filter( abs(coverage_prob - 0.875) == max(abs_value)) %>% 
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

