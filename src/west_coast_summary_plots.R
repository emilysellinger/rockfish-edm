
# coverage probs
coverage_probs_short <- rbind(aurora_stats$short_cov_prob, black_ca_stats$short_cov_prob,
                              black_wa_stats$short_cov_prob, cabezon_ncs_stats$short_cov_prob,
                              cabezon_ors_stats$short_cov_prob, cabezon_scs_stats$short_cov_prob,
                              canary1_stats$short_cov_prob, chilipepper_stats$short_cov_prob,
                              darkblotched_stats$short_cov_prob, dover_sole_stats$short_cov_prob,
                              kelp_greenling_stats$short_cov_prob, lingcod_n_stats$short_cov_prob,
                              lingcod_s_stats$short_cov_prob, sablefish_stats$short_cov_prob,
                              splitnose_stats$short_cov_prob, yelloweye1_stats$short_cov_prob)

coverage_probs_short$stock_name <- c(rep("aurora", nrow(aurora_stats$short_cov_prob)), rep("black_ca", nrow(black_ca_stats$short_cov_prob)),
                                     rep("black_wa", nrow(black_wa_stats$short_cov_prob)), rep("cabezon_ncs", nrow(cabezon_ncs_stats$short_cov_prob)),
                                     rep("cabezon_ors", nrow(cabezon_ors_stats$short_cov_prob)), rep("cabezon_scs", nrow(cabezon_scs_stats$short_cov_prob)),
                                     rep("canary", nrow(canary1_stats$short_cov_prob)), rep("chilipepper", nrow(chilipepper_stats$short_cov_prob)),
                                     rep("darkblotched", nrow(darkblotched_stats$short_cov_prob)), rep("dover_sole", nrow(dover_sole_stats$short_cov_prob)),
                                     rep("kelp_greenling", nrow(kelp_greenling_stats$short_cov_prob)), rep("lingcod_n", nrow(lingcod_n_stats$short_cov_prob)),
                                     rep("lingcod_s", nrow(lingcod_s_stats$short_cov_prob)), rep("sablefish", nrow(sablefish_stats$short_cov_prob)),
                                     rep("splitnose", nrow(splitnose_stats$short_cov_prob)), rep("yelloweye", nrow(yelloweye1_stats$short_cov_prob)))

coverage_probs_long <- rbind(aurora_stats$long_cov_prob, black_ca_stats$long_cov_prob,
                              black_wa_stats$long_cov_prob, cabezon_ncs_stats$long_cov_prob,
                              cabezon_ors_stats$long_cov_prob, cabezon_scs_stats$long_cov_prob,
                              canary1_stats$long_cov_prob, chilipepper_stats$long_cov_prob,
                              darkblotched_stats$long_cov_prob, dover_sole_stats$long_cov_prob,
                              kelp_greenling_stats$long_cov_prob, lingcod_n_stats$long_cov_prob,
                              lingcod_s_stats$long_cov_prob, sablefish_stats$long_cov_prob,
                              splitnose_stats$long_cov_prob, yelloweye1_stats$long_cov_prob)

coverage_probs_long$stock_name <- c(rep("aurora", nrow(aurora_stats$long_cov_prob)), rep("black_ca", nrow(black_ca_stats$long_cov_prob)),
                                     rep("black_wa", nrow(black_wa_stats$long_cov_prob)), rep("cabezon_ncs", nrow(cabezon_ncs_stats$long_cov_prob)),
                                     rep("cabezon_ors", nrow(cabezon_ors_stats$long_cov_prob)), rep("cabezon_scs", nrow(cabezon_scs_stats$long_cov_prob)),
                                     rep("canary", nrow(canary1_stats$long_cov_prob)), rep("chilipepper", nrow(chilipepper_stats$long_cov_prob)),
                                     rep("darkblotched", nrow(darkblotched_stats$long_cov_prob)), rep("dover_sole", nrow(dover_sole_stats$long_cov_prob)),
                                     rep("kelp_greenling", nrow(kelp_greenling_stats$long_cov_prob)), rep("lingcod_n", nrow(lingcod_n_stats$long_cov_prob)),
                                     rep("lingcod_s", nrow(lingcod_s_stats$long_cov_prob)), rep("sablefish", nrow(sablefish_stats$long_cov_prob)),
                                     rep("splitnose", nrow(splitnose_stats$long_cov_prob)), rep("yelloweye", nrow(yelloweye1_stats$long_cov_prob)))

# Summary stats -----------------------------------------------------------

# plot results
pdf(here("results/figures/west_coast_stocks/summary_figures/1step_coverage_prob_boxplot.pdf"))
ggplot(coverage_probs_short) + geom_boxplot(aes(x = coverage_prob, y = method)) + 
  labs(x = "Coverage probability", y = "Forecast method", title = "1 step ahead forecast coverage probabilty")
dev.off()

pdf(here("results/figures/west_coast_stocks/summary_figures/5step_coverage_prob_boxplot.pdf"))
ggplot(coverage_probs_long) + geom_boxplot(aes(x = coverage_prob, y = method)) + 
  labs(x = "Coverage probability", y = "Forecast method", title = "5 step ahead forecast coverage probabilty")
dev.off()

# Combine with stock characteristics
west_coast_stock_characteristics <- read_csv(here("data/west_coast_stock_characteristics.csv"))

coverage_probs_short <- left_join(coverage_probs_short, west_coast_stock_characteristics)
coverage_probs_short$regime_shift <- recode(coverage_probs_short$regime_shift, `1` = "yes", `0` = "no")
coverage_probs_short$detectable_SR <- recode(coverage_probs_short$detectable_SR, `1` = "yes", `0` = "no", `2` = "some")

coverage_probs_long <- left_join(coverage_probs_long, west_coast_stock_characteristics)
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

pdf(here("results/figures/west_coast_stocks/summary_figures/westcoast_ts_characteristics_1stp_coverage_prob.pdf"))
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

pdf(here("results/figures/west_coast_stocks/summary_figures/westcoast_ts_characteristics_5stp_coverage_prob.pdf"))
(a + b)/ (c + d)/ (e + f) + plot_layout(guides = 'collect')
dev.off()

