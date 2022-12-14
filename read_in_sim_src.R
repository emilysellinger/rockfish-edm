m_preds <- as.matrix(read_csv(here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_1stp_mean.csv")))
ar_preds <-  as.matrix(read_csv(here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_1stp_ar.csv")))
bh_preds <- as.matrix(read_csv(here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_1stp_bh.csv")))
simplex_preds <- as.matrix(read_csv(here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_1stp_simplex.csv")))

cabezon_ors_sims_short <- array(c(m_preds, ar_preds, bh_preds, simplex_preds), dim = c(10,1001, 4))



m_preds_long <- as.matrix(read_csv(here("results/simulation_results/west_coast/long_forecasts", "cabezon_ocs_5stp_mean.csv")))
ar_preds_long <- as.matrix(read_csv(here("results/simulation_results/west_coast/long_forecasts", "cabezon_ocs_5stp_ar.csv")))
bh_preds_long <- as.matrix(read_csv(here("results/simulation_results/west_coast/long_forecasts", "cabezon_ocs_5stp_bh.csv")))
simplex_preds_long <- as.matrix(read_csv(here("results/simulation_results/west_coast/long_forecasts", "cabezon_ocs_5stp_simplex.csv")))

cabezon_ors_sims_long <- array(c(m_preds_long, ar_preds_long, bh_preds_long, simplex_preds_long), dim = c(10,1001, 4))
