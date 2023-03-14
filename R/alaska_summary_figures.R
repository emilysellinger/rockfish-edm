
# Load in forecast performance data files ---------------------------------------------
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

alaska_ts_characteristics <- read_csv(here("data/alaska_ts_characteristics.csv"))


# coverage probs
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
                                     rep("blackspotted_rougheye_bsai", nrow(blackspotted_rougheye_goa_stats$long_cov_prob)), rep("dusky_goa", nrow(dusky_goa_stats$long_cov_prob)),
                                     rep("greenland_turbot", nrow(greenland_turbot_stats$long_cov_prob)), rep("kamchatka_flounder", nrow(kamchatka_flounder_stats$long_cov_prob)),
                                     rep("northern_rock_sole", nrow(northern_rock_sole_stats$long_cov_prob)), rep("northern_rockfish_goa", nrow(northern_rockfish_goa_stats$long_cov_prob)),
                                     rep("ns_rock_sole_goa", nrow(ns_rock_sole_goa_stats$long_cov_prob)), rep("pacific_cod_ebs", nrow(pacific_cod_ebs_stats$long_cov_prob)),
                                     rep("pacific_cod_goa", nrow(pacific_cod_goa_stats$long_cov_prob)), rep("pollock_ebs", nrow(pollock_ebs_stats$long_cov_prob)),
                                     rep("pollock_goa", nrow(pollock_goa_stats$long_cov_prob)), rep("pop_bsai", nrow(pop_bsai_stats$long_cov_prob)),
                                     rep("pop_goa", nrow(pop_goa_stats$long_cov_prob)), rep("sablefish_alaska", nrow(sablefish_alaska_stats$long_cov_prob)),
                                     rep("yellowfin_sole_bsai", nrow(yellowfin_sole_bsai_stats$long_cov_prob)))

# Summary stats -----------------------------------------------------------

# plot results
pdf(here("results/figures/alaska_stocks/summary_figures/1step_coverage_prob_boxplot.pdf"))
ggplot(coverage_probs_short) + geom_boxplot(aes(x = coverage_prob, y = method)) + 
  labs(x = "Coverage probability", y = "Forecast method", title = "1 step ahead forecast coverage probabilty")
dev.off()

pdf(here("results/figures/alaska_stocks/summary_figures/5step_coverage_prob_boxplot.pdf"))
ggplot(coverage_probs_long) + geom_boxplot(aes(x = coverage_prob, y = method)) + 
  labs(x = "Coverage probability", y = "Forecast method", title = "5 step ahead forecast coverage probabilty")
dev.off()

# combine w/ stock characteristics
coverage_probs_short <- left_join(coverage_probs_short, alaska_ts_characteristics)
coverage_probs_short$regime_shift <- recode(coverage_probs_short$regime_shift, `1` = "yes", `0` = "no")
coverage_probs_short$detectable_SR <- recode(coverage_probs_short$detectable_SR, `1` = "yes", `0` = "no", `2` = "some")

coverage_probs_long <- left_join(coverage_probs_long, alaska_ts_characteristics)
coverage_probs_long$regime_shift <- recode(coverage_probs_long$regime_shift, `1` = "yes", `0` = "no")
coverage_probs_long$detectable_SR <- recode(coverage_probs_long$detectable_SR, `1` = "yes", `0` = "no", `2` = "some")


# short coverage probabilities
a <- ggplot(coverage_probs_short) + 
  geom_point(aes(x = depletion, y = coverage_prob, color = method), size = 2, alpha = 0.5, position = "jitter") +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Historical depletion", y = "Coverage probability", subtitle = "(a)", title = "1 step ahead forecast \ncoverage probabilities")

b <- ggplot(coverage_probs_short) + 
  geom_point(aes(x = autocorrR, y = coverage_prob, color = method), size = 2, alpha = 0.5, position = "jitter") +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Recruitment autocorrelation", y = "Coverage probability", subtitle = "(b)")

c <- ggplot(coverage_probs_short) + 
  geom_point(aes(x = log_sigmaR_full, y = coverage_prob, color = method), size = 2, alpha = 0.4, position = "jitter") +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Log(sd) of full \nrecruitment time series", y = "Coverage probability", subtitle = "(c)")

d <- ggplot(coverage_probs_short) + 
  geom_point(aes(x = log_sigmaR_test, y = coverage_prob, color = method), size = 2, alpha = 0.4, position = "jitter") +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Log(sd) of initial test \nrecruitment time series", y = "Coverage probability", subtitle = "(d)")

e <- ggplot(coverage_probs_short) + 
  geom_boxplot(aes(x = regime_shift, y = coverage_prob, color = method)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Presence of a \nrecruitment regime change", y = "Coverage probability", subtitle = "(e)")

f <- ggplot(coverage_probs_short) + 
  geom_boxplot(aes(x = detectable_SR, y = coverage_prob, color = method)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Detectable influence of \nspawning biomass on recruitment", y = "Coverage probability", subtitle = "(f)")

pdf(here("results/figures/alaska_stocks/summary_figures/alaska_ts_characteristics_1stp_coverage_prob.pdf"))
(a + b)/ (c + d)/ (e + f) + plot_layout(guides = 'collect')
dev.off()


# long coverage probabilities
a <- ggplot(coverage_probs_long) + 
  geom_point(aes(x = depletion, y = coverage_prob, color = method), size = 2, alpha = 0.5, position = "jitter") +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Historical depletion", y = "Coverage probability", subtitle = "(a)", title = "5 step ahead forecast \ncoverage probabilities")

b <- ggplot(coverage_probs_long) + 
  geom_point(aes(x = autocorrR, y = coverage_prob, color = method), size = 2, alpha = 0.5, position = "jitter") +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Recruitment autocorrelation", y = "Coverage probability", subtitle = "(b)")

c <- ggplot(coverage_probs_long) + 
  geom_point(aes(x = log_sigmaR_full, y = coverage_prob, color = method), size = 2, alpha = 0.4, position = "jitter") +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Log(sd) of full \nrecruitment time series", y = "Coverage probability", subtitle = "(c)")

d <- ggplot(coverage_probs_long) + 
  geom_point(aes(x = log_sigmaR_test, y = coverage_prob, color = method), size = 2, alpha = 0.4, position = "jitter") +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Log(sd) of initial test \nrecruitment time series", y = "Coverage probability", subtitle = "(d)")

e <- ggplot(coverage_probs_long) + 
  geom_boxplot(aes(x = regime_shift, y = coverage_prob, color = method)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Presence of a \nrecruitment regime change", y = "Coverage probability", subtitle = "(e)")

f <- ggplot(coverage_probs_long) + 
  geom_boxplot(aes(x = detectable_SR, y = coverage_prob, color = method)) +
  geom_hline(yintercept = 0.8, linetype = "dashed") + geom_hline(yintercept = 0.95, linetype = "dashed") +
  labs(x = "Detectable influence of \nspawning biomass on recruitment", y = "Coverage probability", subtitle = "(f)")

pdf(here("results/figures/alaska_stocks/summary_figures/alaska_ts_characteristics_5stp_coverage_prob.pdf"))
(a + b)/ (c + d)/ (e + f) + plot_layout(guides = 'collect')
dev.off()

