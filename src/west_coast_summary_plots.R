# Create plots for to summarize simulations across species
coverage_probs <- read_csv(here("results/simulation_results/west_coast/performance_stats/west_coast_short_coverage_probs.csv"))
# Summary stats -----------------------------------------------------------

# plot results
pdf(here("results/figures/west_coast_stocks/stock_cov_prob_1stp.pdf"))
print(ggplot(coverage_probs) + geom_point(aes(x = method, y = coverage_prob, color = stock)) +
  labs(x = "Forecast method", y = "Coverage probability", color = "Stock", title = "1 step forecasts"))
dev.off()


pdf(here("results/figures/west_coast_stocks/1step_coverage_prob_boxplot.pdf"))
ggplot(coverage_probs) + geom_boxplot(aes(x = coverage_prob, y = method)) + 
  labs(x = "Coverage probability", y = "Forecast method", title = "1 step ahead forecast coverage probabilty")
dev.off()

# check with autocorrelation
west_coast_stock_characteristics <- read_csv(here("data/west_coast_stock_characteristics.csv"))

colnames(coverage_probs)[1] <- "stock_name"
coverage_probs$stock_name[which(coverage_probs == "black_CA")] <- "black_ca"
coverage_probs$stock_name[which(coverage_probs == "black_WA")] <- "black_wa"

coverage_probs$stock_name[which(coverage_probs == "canary1")] <- "canary"
coverage_probs$stock_name[which(coverage_probs == "yelloweye1")] <- "yelloweye"
coverage_probs <- left_join(coverage_probs, west_coast_stock_characteristics)

a <- ggplot(coverage_probs) + 
  geom_point(aes(x = depletion, y = coverage_prob, color = method), size = 2, alpha = 0.4) +
  labs(x = "Historical depletion", y = "Coverage probability", subtitle = "(a)", title = "1 step ahead forecast coverage probabilities")

b <- ggplot(coverage_probs) + geom_point(aes(x = autocorrR, y = coverage_prob, color = method), 
                                  size = 2, alpha = 0.4) +
  labs(x = "Recruitment autocorrelation", y = "Coverage probability", subtitle = "(b)")
c <- ggplot(coverage_probs) + geom_point(aes(x = num_yrs, y = coverage_prob, color = method), 
                                  size = 2, alpha = 0.4) +
  labs(x = "Length of recruitment time series", y = "Coverage probability", subtitle = "(c)")

pdf(here("results/figures/westcoast_ts_characteristics_coverage_prob.pdf"))
grid.arrange(a, b, c, nrow = 3)
dev.off()


# 5-step forecasts --------------------------------------------------------
# Aurora rockfish
aurora_m_preds <- as.matrix(read_csv(here("results", "aurora_5stp_mean.csv")))
aurora_ar_preds <- as.matrix(read_csv(here("results", "aurora_5stp_ar.csv")))
aurora_bh_preds <- as.matrix(read_csv(here("results", "aurora_5stp_bh.csv")))
aurora_simplex_preds <- as.matrix(read_csv(here("results", "aurora_5stp_simplex.csv")))

# Black CA rockfish
blackCA_m_preds <- as.matrix(read_csv(here("results", "blackCA_5stp_mean.csv")))
blackCA_ar_preds <- as.matrix(read_csv(here("results", "blackCA_5stp_ar.csv")))
blackCA_bh_preds <- as.matrix(read_csv(here("results", "blackCA_5stp_bh.csv")))
blackCA_simplex_preds <- as.matrix(read_csv(here("results", "blackCA_5stp_simplex.csv")))

# Black WA rockfish
blackWA_m_preds <- as.matrix(read_csv(here("results", "blackWA_5stp_mean.csv")))
blackWA_ar_preds <- as.matrix(read_csv(here("results", "blackWA_5stp_ar.csv")))
blackWA_bh_preds <- as.matrix(read_csv(here("results", "blackWA_5stp_bh.csv")))
blackWA_simplex_preds <- as.matrix(read_csv(here("results", "blackWA_5stp_simplex.csv")))

# Bocaccio rockfish
bocaccio_m_preds <- as.matrix(read_csv(here("results", "bocaccio_5stp_mean.csv")))
bocaccio_ar_preds <- as.matrix(read_csv(here("results", "bocaccio_5stp_ar.csv")))
bocaccio_bh_preds <- as.matrix(read_csv(here("results", "bocaccio_5stp_bh.csv")))
bocaccio_simplex_preds <- as.matrix(read_csv(here("results", "bocaccio_5stp_simplex.csv")))

# Cabezon NCS
cabezon_ncs_m_preds <- as.matrix(read_csv(here("results", "cabezon_ncs_5stp_mean.csv")))
cabezon_ncs_ar_preds <- as.matrix(read_csv(here("results", "cabezon_ncs_5stp_ar.csv")))
cabezon_ncs_bh_preds <- as.matrix(read_csv(here("results", "cabezon_ncs_5stp_bh.csv")))
cabezon_ncs_simplex_preds <- as.matrix(read_csv(here("results", "cabezon_ncs_5stp_simplex.csv")))

# Cabezon OCS
cabezon_ocs_m_preds <- as.matrix(read_csv(here("results", "cabezon_ocs_5stp_mean.csv")))
cabezon_ocs_ar_preds <- as.matrix(read_csv(here("results", "cabezon_ocs_5stp_ar.csv")))
cabezon_ocs_bh_preds <- as.matrix(read_csv(here("results", "cabezon_ocs_5stp_bh.csv")))
cabezon_ocs_simplex_preds <- as.matrix(read_csv(here("results", "cabezon_ocs_5stp_simplex.csv")))

# Cabezon SCS
cabezon_scs_m_preds <- as.matrix(read_csv(here("results", "cabezon_scs_5stp_mean.csv")))
cabezon_scs_ar_preds <- as.matrix(read_csv(here("results", "cabezon_scs_5stp_ar.csv")))
cabezon_scs_bh_preds <- as.matrix(read_csv(here("results", "cabezon_scs_5stp_bh.csv")))
cabezon_scs_simplex_preds <- as.matrix(read_csv(here("results", "cabezon_scs_5stp_simplex.csv")))

# Canary 1 rockfish
canary1_m_preds <- as.matrix(read_csv(here("results", "canary1_5stp_mean.csv")))
canary1_ar_preds <- as.matrix(read_csv(here("results", "canary1_5stp_ar.csv")))
canary1_bh_preds <- as.matrix(read_csv(here("results", "canary1_5stp_bh.csv")))
canary1_simplex_preds <- as.matrix(read_csv(here("results", "canary1_5stp_simplex.csv")))

# Chilipepper rockfish
chilipepper_m_preds <- as.matrix(read_csv(here("results", "chilipepper_5stp_mean.csv")))
chilipepper_ar_preds <- as.matrix(read_csv(here("results", "chilipepper_5stp_ar.csv")))
chilipepper_bh_preds <- as.matrix(read_csv(here("results", "chilipepper_5stp_bh.csv")))
chilipepper_simplex_preds <- as.matrix(read_csv(here("results", "chilipepper_5stp_simplex.csv")))

# Summary stats -----------------------------------------------------------

aurora_sim_CI_prob <- tibble(stock = rep("aurora", 4),
                             method = c("mean", "AR(1)", "Beverton-Holt", "simplex"),
                             CI_prob = c(sim_CI_prob(aurora_m_preds, 0.95), sim_CI_prob(aurora_ar_preds, 0.95),
                                         sim_CI_prob(aurora_bh_preds, 0.95), sim_CI_prob(aurora_simplex_preds, 0.95)))
blackCA_sim_CI_prob <- tibble(stock = rep("blackCA", 4),
                              method = c("mean", "AR(1)", "Beverton-Holt", "simplex"),
                              CI_prob = c(sim_CI_prob(blackCA_m_preds, 0.95), sim_CI_prob(blackCA_ar_preds, 0.95),
                                          sim_CI_prob(blackCA_bh_preds, 0.95), sim_CI_prob(blackCA_simplex_preds, 0.95)))
blackWA_sim_CI_prob <- tibble(stock = rep("blackWA", 4),
                              method = c("mean", "AR(1)", "Beverton-Holt", "simplex"),
                              CI_prob = c(sim_CI_prob(blackWA_m_preds, 0.95), sim_CI_prob(blackWA_ar_preds, 0.95),
                                          sim_CI_prob(blackWA_bh_preds, 0.95), sim_CI_prob(blackWA_simplex_preds, 0.95)))
bocaccio_sim_CI_prob <- tibble(stock = rep("bocaccio", 4),
                               method = c("mean", "AR(1)", "Beverton-Holt", "simplex"),
                               CI_prob = c(sim_CI_prob(bocaccio_m_preds, 0.95), sim_CI_prob(bocaccio_ar_preds, 0.95),
                                           sim_CI_prob(bocaccio_bh_preds, 0.95), sim_CI_prob(bocaccio_simplex_preds, 0.95)))
cabezon_ncs_sim_CI_prob <- tibble(stock = rep("cabezon_ncs", 4),
                                  method = c("mean", "AR(1)", "Beverton-Holt", "simplex"),
                                  CI_prob = c(sim_CI_prob(cabezon_ncs_m_preds, 0.95), sim_CI_prob(cabezon_ncs_ar_preds, 0.95),
                                              sim_CI_prob(cabezon_ncs_bh_preds, 0.95), sim_CI_prob(cabezon_ncs_simplex_preds, 0.95)))
cabezon_ocs_sim_CI_prob <- tibble(stock = rep("cabezon_ocs", 4),
                                  method = c("mean", "AR(1)", "Beverton-Holt", "simplex"),
                                  CI_prob = c(sim_CI_prob(cabezon_ocs_m_preds, 0.95), sim_CI_prob(cabezon_ocs_ar_preds, 0.95),
                                              sim_CI_prob(cabezon_ocs_bh_preds, 0.95), sim_CI_prob(cabezon_ocs_simplex_preds, 0.95)))
cabezon_scs_sim_CI_prob <- tibble(stock = rep("cabezon_scs", 4),
                                  method = c("mean", "AR(1)", "Beverton-Holt", "simplex"),
                                  CI_prob = c(sim_CI_prob(cabezon_scs_m_preds, 0.95), sim_CI_prob(cabezon_scs_ar_preds, 0.95),
                                              sim_CI_prob(cabezon_scs_bh_preds, 0.95), sim_CI_prob(cabezon_scs_simplex_preds, 0.95)))
canary1_sim_CI_prob <- tibble(stock = rep("canary1", 4),
                              method = c("mean", "AR(1)", "Beverton-Holt", "simplex"),
                              CI_prob = c(sim_CI_prob(canary1_m_preds, 0.95), sim_CI_prob(canary1_ar_preds, 0.95),
                                          sim_CI_prob(canary1_bh_preds, 0.95), sim_CI_prob(canary1_simplex_preds, 0.95)))
chilipepper_sim_CI_prob <- tibble(stock = rep("chilipepper", 4),
                                  method = c("mean", "AR(1)", "Beverton-Holt", "simplex"),
                                  CI_prob = c(sim_CI_prob(chilipepper_m_preds, 0.95), sim_CI_prob(chilipepper_ar_preds, 0.95),
                                              sim_CI_prob(chilipepper_bh_preds, 0.95), sim_CI_prob(chilipepper_simplex_preds, 0.95)))

sim_CIs_5stp <- rbind(aurora_sim_CI_prob, blackCA_sim_CI_prob, blackWA_sim_CI_prob, bocaccio_sim_CI_prob,
                 cabezon_ncs_sim_CI_prob, cabezon_ocs_sim_CI_prob, cabezon_scs_sim_CI_prob, 
                 canary1_sim_CI_prob, chilipepper_sim_CI_prob)

# plot results
pdf(here("results/figures/stock_cov_prob_5stp.pdf"))
print(ggplot(sim_CIs_5stp) + geom_point(aes(x = method, y = CI_prob, color = stock_name)) +
  labs(x = "Forecast Method", y = "Coverage probability", color  = "Stock name", title = "5 step ahead forecasts"))
dev.off()


pdf(here("results/figures/5step_coverage_prob.pdf"))
ggplot(sim_CIs_5stp) + geom_boxplot(aes(x = CI_prob, y = method)) + 
  labs(x = "Coverage probability", y = "Forecast method", title = "5 step ahead forecast coverage probabilty")
dev.off()


colnames(sim_CIs_5stp)[1] <- "stock_name"
sim_CIs_5stp$stock_name[which(sim_CIs_5stp == "blackCA")] <- "black_ca"
sim_CIs_5stp$stock_name[which(sim_CIs_5stp == "blackWA")] <- "black_wa"
sim_CIs_5stp$stock_name[which(sim_CIs_5stp == "cabezon_ocs")] <- "cabezon_ors"
sim_CIs_5stp$stock_name[which(sim_CIs_5stp == "canary1")] <- "canary"
sim_CIs_5stp <- left_join(sim_CIs_5stp, west_coast_stock_characteristics)

a <- ggplot(sim_CIs_5stp) + 
  geom_point(aes(x = depletion, y = CI_prob, color = method), size = 2, position = position_dodge(width = 0.1)) +
  labs(x = "Historical depletion", y = "Coverage probability", subtitle = "(a)", title = "5 step ahead forecast coverage probabilities")

b <- ggplot(sim_CIs_5stp) + geom_point(aes(x = autocorrR, y = CI_prob, color = method), 
                                  size = 2, position = position_dodge(width = 0.1)) +
  labs(x = "Recruitment autocorrelation", y = "Coverage probability", subtitle = "(b)")
c <- ggplot(sim_CIs_5stp) + geom_point(aes(x = num_yrs, y = CI_prob, color = method), 
                                  size = 2, position = position_dodge(width = 0.1)) +
  labs(x = "Length of recruitment time series", y = "Coverage probability", subtitle = "(c)")

pdf(here("results/figures/westcoast_ts_characteristics_coverage_prob_5stp.pdf"))
grid.arrange(a, b, c, nrow = 3)
dev.off()



