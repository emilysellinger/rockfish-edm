# Create plots for to summarize simulations across species

# Aurora rockfish
aurora_m_preds <- as.matrix(read_csv(here("results", "aurora_1stp_mean.csv")))
aurora_ar_preds <- as.matrix(read_csv(here("results", "aurora_1stp_ar.csv")))
aurora_bh_preds <- as.matrix(read_csv(here("results", "aurora_1stp_bh.csv")))
aurora_simplex_preds <- as.matrix(read_csv(here("results", "aurora_1stp_simplex.csv")))

# Black CA rockfish
blackCA_m_preds <- as.matrix(read_csv(here("results", "blackCA_1stp_mean.csv")))
blackCA_ar_preds <- as.matrix(read_csv(here("results", "blackCA_1stp_ar.csv")))
blackCA_bh_preds <- as.matrix(read_csv(here("results", "blackCA_1stp_bh.csv")))
blackCA_simplex_preds <- as.matrix(read_csv(here("results", "blackCA_1stp_simplex.csv")))

# Black WA rockfish
blackWA_m_preds <- as.matrix(read_csv(here("results", "blackWA_1stp_mean.csv")))
blackWA_ar_preds <- as.matrix(read_csv(here("results", "blackWA_1stp_ar.csv")))
blackWA_bh_preds <- as.matrix(read_csv(here("results", "blackWA_1stp_bh.csv")))
blackWA_simplex_preds <- as.matrix(read_csv(here("results", "blackWA_1stp_simplex.csv")))

# Bocaccio rockfish
bocaccio_m_preds <- as.matrix(read_csv(here("results", "bocaccio_1stp_mean.csv")))
bocaccio_ar_preds <- as.matrix(read_csv(here("results", "bocaccio_1stp_ar.csv")))
bocaccio_bh_preds <- as.matrix(read_csv(here("results", "bocaccio_1stp_bh.csv")))
bocaccio_simplex_preds <- as.matrix(read_csv(here("results", "bocaccio_1stp_simplex.csv")))

# Cabezon NCS
cabezon_ncs_m_preds <- as.matrix(read_csv(here("results", "cabezon_ncs_1stp_mean.csv")))
cabezon_ncs_ar_preds <- as.matrix(read_csv(here("results", "cabezon_ncs_1stp_ar.csv")))
cabezon_ncs_bh_preds <- as.matrix(read_csv(here("results", "cabezon_ncs_1stp_bh.csv")))
cabezon_ncs_simplex_preds <- as.matrix(read_csv(here("results", "cabezon_ncs_1stp_simplex.csv")))

# Cabezon OCS
cabezon_ocs_m_preds <- as.matrix(read_csv(here("results", "cabezon_ocs_1stp_mean.csv")))
cabezon_ocs_ar_preds <- as.matrix(read_csv(here("results", "cabezon_ocs_1stp_ar.csv")))
cabezon_ocs_bh_preds <- as.matrix(read_csv(here("results", "cabezon_ocs_1stp_bh.csv")))
cabezon_ocs_simplex_preds <- as.matrix(read_csv(here("results", "cabezon_ocs_1stp_simplex.csv")))

# Cabezon SCS
cabezon_scs_m_preds <- as.matrix(read_csv(here("results", "cabezon_scs_1stp_mean.csv")))
cabezon_scs_ar_preds <- as.matrix(read_csv(here("results", "cabezon_scs_1stp_ar.csv")))
cabezon_scs_bh_preds <- as.matrix(read_csv(here("results", "cabezon_scs_1stp_bh.csv")))
cabezon_scs_simplex_preds <- as.matrix(read_csv(here("results", "cabezon_scs_1stp_simplex.csv")))

# Canary 1 rockfish
canary1_m_preds <- as.matrix(read_csv(here("results", "canary1_1stp_mean.csv")))
canary1_ar_preds <- as.matrix(read_csv(here("results", "canary1_1stp_ar.csv")))
canary1_bh_preds <- as.matrix(read_csv(here("results", "canary1_1stp_bh.csv")))
canary1_simplex_preds <- as.matrix(read_csv(here("results", "canary1_1stp_simplex.csv")))

# Chilipepper rockfish
chilipepper_m_preds <- as.matrix(read_csv(here("results", "chilipepper_1stp_mean.csv")))
chilipepper_ar_preds <- as.matrix(read_csv(here("results", "chilipepper_1stp_ar.csv")))
chilipepper_bh_preds <- as.matrix(read_csv(here("results", "chilipepper_1stp_bh.csv")))
chilipepper_simplex_preds <- as.matrix(read_csv(here("results", "chilipepper_1stp_simplex.csv")))

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

sim_CIs <- rbind(aurora_sim_CI_prob, blackCA_sim_CI_prob, blackWA_sim_CI_prob, bocaccio_sim_CI_prob,
                 cabezon_ncs_sim_CI_prob, cabezon_ocs_sim_CI_prob, cabezon_scs_sim_CI_prob, 
                 canary1_sim_CI_prob, chilipepper_sim_CI_prob)

# plot results
ggplot(sim_CIs) + geom_point(aes(x = method, y = CI_prob, color = stock))
ggplot(sim_CIs) + geom_boxplot(aes(x = CI_prob, y = method))
