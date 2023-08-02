# Set seed ###########################################################################
set.seed(211)
# Arrowtooth Flounder forecasts ####################################################
## Set Up ---------------------------------------------------------------------
arrowtooth_flounder_bsai <- filter_alaska_sr_data(arrowtooth_flounder_bsai)

plot(arrowtooth_flounder_bsai$year, arrowtooth_flounder_bsai$recruits, type = "l")
plot(arrowtooth_flounder_bsai$year, arrowtooth_flounder_bsai$sbiomass, type = "l")

rec_ts <- arrowtooth_flounder_bsai$recruits
spawn_ts <- arrowtooth_flounder_bsai$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
arrowtooth_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(arrowtooth_sims, here("results/simulation_results/alaska/short_forecasts/arrowtooth_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
arrowtooth_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(arrowtooth_long_sims, here("results/simulation_results/alaska/long_forecasts/arrowtooth_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/arrowtooth_forecast_figs.pdf"))
print_plots(arrowtooth_sims, arrowtooth_long_sims, arrowtooth_flounder_bsai$recruits, arrowtooth_flounder_bsai$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
arrowtooth_stats <- save_performance_stats(arrowtooth_sims, arrowtooth_long_sims, arrowtooth_flounder_bsai$recruits, arrowtooth_flounder_bsai$year, time_vec1, time_vec2)
saveRDS(arrowtooth_stats, file = here("results/simulation_results/alaska/performance_stats/arrowtooth_stats.Rds"))


# Atka Mackerel forecasts ####################################################
## Set Up ---------------------------------------------------------------------
atka_mackerel_bsai <- filter_alaska_sr_data(atka_mackerel_bsai)

plot(atka_mackerel_bsai$year, atka_mackerel_bsai$recruits, type = "l")
plot(atka_mackerel_bsai$year, atka_mackerel_bsai$sbiomass, type = "l")

rec_ts <- atka_mackerel_bsai$recruits
spawn_ts <- atka_mackerel_bsai$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
atka_mackerel_bsai_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(atka_mackerel_bsai_sims, here("results/simulation_results/alaska/short_forecasts/atka_mackerel_bsai_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
atka_mackerel_bsai_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(atka_mackerel_bsai_long_sims, here("results/simulation_results/alaska/long_forecasts/atka_mackerel_bsai_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/atka_mackerel_bsai_forecast_figs.pdf"))
print_plots(atka_mackerel_bsai_sims, atka_mackerel_bsai_long_sims, atka_mackerel_bsai$recruits, atka_mackerel_bsai$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
atka_mackerel_bsai_stats <- save_performance_stats(atka_mackerel_bsai_sims, atka_mackerel_bsai_long_sims, atka_mackerel_bsai$recruits, atka_mackerel_bsai$year, time_vec1, time_vec2)
saveRDS(atka_mackerel_bsai_stats, file = here("results/simulation_results/alaska/performance_stats/atka_mackerel_bsai_stats.Rds"))


# Blackspotted rougheye GOA forecasts ####################################################
## Set Up ---------------------------------------------------------------------
blackspotted_rougheye_goa <- filter_alaska_sr_data(blackspotted_rougheye_goa)

plot(blackspotted_rougheye_goa$year, blackspotted_rougheye_goa$recruits, type = "l")
plot(blackspotted_rougheye_goa$year, blackspotted_rougheye_goa$sbiomass, type = "l")

rec_ts <- blackspotted_rougheye_goa$recruits
spawn_ts <- blackspotted_rougheye_goa$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
blackspotted_rougheye_goa_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(blackspotted_rougheye_goa_sims, here("results/simulation_results/alaska/short_forecasts/blackspotted_rougheye_goa_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
blackspotted_rougheye_goa_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(blackspotted_rougheye_goa_sims, here("results/simulation_results/alaska/long_forecasts/blackspotted_rougheye_goa_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/blackspotted_rougheye_goa_forecast_figs.pdf"))
print_plots(blackspotted_rougheye_goa_sims, blackspotted_rougheye_goa_long_sims, blackspotted_rougheye_goa$recruits, blackspotted_rougheye_goa$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
blackspotted_rougheye_goa_stats <- save_performance_stats(blackspotted_rougheye_goa_sims, blackspotted_rougheye_goa_long_sims, blackspotted_rougheye_goa$recruits, blackspotted_rougheye_goa$year, time_vec1, time_vec2)
saveRDS(blackspotted_rougheye_goa_stats, file = here("results/simulation_results/alaska/performance_stats/blackspotted_rougheye_goa_stats.Rds"))


# Dusky rockfish GOA forecasts ####################################################
## Set Up ---------------------------------------------------------------------
dusky_goa <- filter_alaska_sr_data(dusky_goa)

plot(dusky_goa$year, dusky_goa$recruits, type = "l")
plot(dusky_goa$year, dusky_goa$sbiomass, type = "l")

rec_ts <- dusky_goa$recruits
spawn_ts <- dusky_goa$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
dusky_goa_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(dusky_goa_sims, here("results/simulation_results/alaska/short_forecasts/dusky_goa_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
dusky_goa_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(dusky_goa_long_sims, here("results/simulation_results/alaska/long_forecasts/dusky_goa_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/dusky_goa_forecast_figs.pdf"))
print_plots(dusky_goa_sims, dusky_goa_long_sims, dusky_goa$recruits, dusky_goa$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
dusky_goa_stats <- save_performance_stats(dusky_goa_sims, dusky_goa_long_sims, dusky_goa$recruits, dusky_goa$year, time_vec1, time_vec2)
saveRDS(dusky_goa_stats, file = here("results/simulation_results/alaska/performance_stats/dusky_goa_stats.Rds"))


# Greenland Turbot forecasts ####################################################
## Set Up ---------------------------------------------------------------------
greenland_turbot <- filter_alaska_sr_data(greenland_turbot)

plot(greenland_turbot$year, greenland_turbot$recruits, type = "l")
plot(greenland_turbot$year, greenland_turbot$sbiomass, type = "l")

rec_ts <- greenland_turbot$recruits
spawn_ts <- greenland_turbot$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
greenland_turbot_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(greenland_turbot_sims, here("results/simulation_results/alaska/short_forecasts/greenland_turbot_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
greenland_turbot_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(greenland_turbot_long_sims, here("results/simulation_results/alaska/long_forecasts/greenland_turbot_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/greenland_turbot_forecast_figs.pdf"))
print_plots(greenland_turbot_sims, greenland_turbot_long_sims, greenland_turbot$recruits, greenland_turbot$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
greenland_turbot_stats <- save_performance_stats(greenland_turbot_sims, greenland_turbot_long_sims, greenland_turbot$recruits, greenland_turbot$year, time_vec1, time_vec2)
saveRDS(greenland_turbot_stats, file = here("results/simulation_results/alaska/performance_stats/greenland_turbot_stats.Rds"))



# Kamchatka flounder forecasts ####################################################
## Set Up ---------------------------------------------------------------------
kamchatka_flounder <- filter_alaska_sr_data(kamchatka_flounder)

plot(kamchatka_flounder$year, kamchatka_flounder$recruits, type = "l")
plot(kamchatka_flounder$year, kamchatka_flounder$sbiomass, type = "l")

rec_ts <- kamchatka_flounder$recruits
spawn_ts <- kamchatka_flounder$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
kamchatka_flounder_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(kamchatka_flounder_sims, here("results/simulation_results/alaska/short_forecasts/kamchatka_flounder_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
kamchatka_flounder_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(kamchatka_flounder_long_sims, here("results/simulation_results/alaska/long_forecasts/kamchatka_flounder_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/kamchatka_flounder_forecast_figs.pdf"))
print_plots(kamchatka_flounder_sims, kamchatka_flounder_long_sims, kamchatka_flounder$recruits, kamchatka_flounder$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
kamchatka_flounder_stats <- save_performance_stats(kamchatka_flounder_sims, kamchatka_flounder_long_sims, kamchatka_flounder$recruits, kamchatka_flounder$year, time_vec1, time_vec2)
saveRDS(kamchatka_flounder_stats, file = here("results/simulation_results/alaska/performance_stats/kamchatka_flounder_stats.Rds"))


# Northern rock sole forecasts ####################################################
## Set Up ---------------------------------------------------------------------
northern_rock_sole <- filter_alaska_sr_data(northern_rock_sole)

plot(northern_rock_sole$year, northern_rock_sole$recruits, type = "l")
plot(northern_rock_sole$year, northern_rock_sole$sbiomass, type = "l")

rec_ts <- northern_rock_sole$recruits
spawn_ts <- northern_rock_sole$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
northern_rock_sole_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(northern_rock_sole_sims, here("results/simulation_results/alaska/short_forecasts/northern_rock_sole_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
northern_rock_sole_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(northern_rock_sole_long_sims, here("results/simulation_results/alaska/long_forecasts/northern_rock_sole_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/northern_rock_sole_forecast_figs.pdf"))
print_plots(northern_rock_sole_sims, northern_rock_sole_long_sims, northern_rock_sole$recruits, northern_rock_sole$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
northern_rock_sole_stats <- save_performance_stats(northern_rock_sole_sims, northern_rock_sole_long_sims, northern_rock_sole$recruits, northern_rock_sole$year, time_vec1, time_vec2)
saveRDS(northern_rock_sole_stats, file = here("results/simulation_results/alaska/performance_stats/northern_rock_sole_stats.Rds"))


# Northern/Southern rock soles GOA forecasts ####################################################
## Set Up ---------------------------------------------------------------------
ns_rock_sole_goa <- filter_alaska_sr_data(ns_rock_sole_goa)

plot(ns_rock_sole_goa$year, ns_rock_sole_goa$recruits, type = "l")
plot(ns_rock_sole_goa$year, ns_rock_sole_goa$sbiomass, type = "l")

rec_ts <- ns_rock_sole_goa$recruits
spawn_ts <- ns_rock_sole_goa$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
ns_rock_sole_goa_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(ns_rock_sole_goa_sims, here("results/simulation_results/alaska/short_forecasts/ns_rock_sole_goa_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
ns_rock_sole_goa_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(ns_rock_sole_goa_long_sims, here("results/simulation_results/alaska/long_forecasts/ns_rock_sole_goa_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/ns_rock_sole_goa_forecast_figs.pdf"))
print_plots(ns_rock_sole_goa_sims, ns_rock_sole_goa_long_sims, ns_rock_sole_goa$recruits, ns_rock_sole_goa$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
ns_rock_sole_goa_stats <- save_performance_stats(ns_rock_sole_goa_sims, ns_rock_sole_goa_long_sims, ns_rock_sole_goa$recruits, ns_rock_sole_goa$year, time_vec1, time_vec2)
saveRDS(ns_rock_sole_goa_stats, file = here("results/simulation_results/alaska/performance_stats/ns_rock_sole_goa_stats.Rds"))



# Northern rockfish GOA forecasts ####################################################
## Set Up ---------------------------------------------------------------------
northern_rockfish_goa <- filter_alaska_sr_data(northern_rockfish_goa)

plot(northern_rockfish_goa$year, northern_rockfish_goa$recruits, type = "l")
plot(northern_rockfish_goa$year, northern_rockfish_goa$sbiomass, type = "l")

rec_ts <- northern_rockfish_goa$recruits
spawn_ts <- northern_rockfish_goa$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
northern_rockfish_goa_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(northern_rockfish_goa_sims, here("results/simulation_results/alaska/short_forecasts/northern_rockfish_goa_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
northern_rockfish_goa_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(northern_rockfish_goa_long_sims, here("results/simulation_results/alaska/long_forecasts/northern_rockfish_goa_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/northern_rockfish_goa_forecast_figs.pdf"))
print_plots(northern_rockfish_goa_sims, northern_rockfish_goa_long_sims, northern_rockfish_goa$recruits, northern_rockfish_goa$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
northern_rockfish_goa_stats <- save_performance_stats(northern_rockfish_goa_sims, northern_rockfish_goa_long_sims, northern_rockfish_goa$recruits, northern_rockfish_goa$year, time_vec1, time_vec2)
saveRDS(northern_rockfish_goa_stats, file = here("results/simulation_results/alaska/performance_stats/northern_rockfish_goa_stats.Rds"))



# Pacific cod EBS forecasts ####################################################
## Set Up ---------------------------------------------------------------------
pacific_cod_ebs <- filter_alaska_sr_data(pacific_cod_ebs)

plot(pacific_cod_ebs$year, pacific_cod_ebs$recruits, type = "l")
plot(pacific_cod_ebs$year, pacific_cod_ebs$sbiomass, type = "l")

rec_ts <- pacific_cod_ebs$recruits
spawn_ts <- pacific_cod_ebs$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
pacific_cod_ebs_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(pacific_cod_ebs_sims, here("results/simulation_results/alaska/short_forecasts/pacific_cod_ebs_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
pacific_cod_ebs_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(pacific_cod_ebs_long_sims, here("results/simulation_results/alaska/long_forecasts/pacific_cod_ebs_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/pacific_cod_ebs_forecast_figs.pdf"))
print_plots(pacific_cod_ebs_sims, pacific_cod_ebs_long_sims, pacific_cod_ebs$recruits, pacific_cod_ebs$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
pacific_cod_ebs_stats <- save_performance_stats(pacific_cod_ebs_sims, pacific_cod_ebs_long_sims, pacific_cod_ebs$recruits, pacific_cod_ebs$year, time_vec1, time_vec2)
saveRDS(pacific_cod_ebs_stats, file = here("results/simulation_results/alaska/performance_stats/pacific_cod_ebs_stats.Rds"))



# Pacific cod GOA forecasts ####################################################
## Set Up ---------------------------------------------------------------------
pacific_cod_goa <- filter_alaska_sr_data(pacific_cod_goa)

plot(pacific_cod_goa$year, pacific_cod_goa$recruits, type = "l")
plot(pacific_cod_goa$year, pacific_cod_goa$sbiomass, type = "l")

rec_ts <- pacific_cod_goa$recruits
spawn_ts <- pacific_cod_goa$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
pacific_cod_goa_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(pacific_cod_goa_sims, here("results/simulation_results/alaska/short_forecasts/pacific_cod_goa_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
pacific_cod_goa_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(pacific_cod_goa_long_sims, here("results/simulation_results/alaska/long_forecasts/pacific_cod_goa_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/pacific_cod_goa_forecast_figs.pdf"))
print_plots(pacific_cod_goa_sims, pacific_cod_goa_long_sims, pacific_cod_goa$recruits, pacific_cod_goa$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
pacific_cod_goa_stats <- save_performance_stats(pacific_cod_goa_sims, pacific_cod_goa_long_sims, pacific_cod_goa$recruits, pacific_cod_goa$year, time_vec1, time_vec2)
saveRDS(pacific_cod_goa_stats, file = here("results/simulation_results/alaska/performance_stats/pacific_cod_goa_stats.Rds"))



# Pollock EBS forecasts ####################################################
## Set Up ---------------------------------------------------------------------
pollock_ebs <- filter_alaska_sr_data(pollock_ebs)

plot(pollock_ebs$year, pollock_ebs$recruits, type = "l")
plot(pollock_ebs$year, pollock_ebs$sbiomass, type = "l")

rec_ts <- pollock_ebs$recruits
spawn_ts <- pollock_ebs$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
pollock_ebs_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(pollock_ebs_sims, here("results/simulation_results/alaska/short_forecasts/pollock_ebs_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
pollock_ebs_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(pollock_ebs_long_sims, here("results/simulation_results/alaska/long_forecasts/pollock_ebs_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/pollock_ebs_forecast_figs.pdf"))
print_plots(pollock_ebs_sims, pollock_ebs_long_sims, pollock_ebs$recruits, pollock_ebs$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
pollock_ebs_stats <- save_performance_stats(pollock_ebs_sims, pollock_ebs_long_sims, pollock_ebs$recruits, pollock_ebs$year, time_vec1, time_vec2)
saveRDS(pollock_ebs_stats, file = here("results/simulation_results/alaska/performance_stats/pollock_ebs_stats.Rds"))


# Pollock GOA forecasts ####################################################
## Set Up ---------------------------------------------------------------------
pollock_goa <- filter_alaska_sr_data(pollock_goa)

plot(pollock_goa$year, pollock_goa$recruits, type = "l")
plot(pollock_goa$year, pollock_goa$sbiomass, type = "l")

rec_ts <- pollock_goa$recruits
spawn_ts <- pollock_goa$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
pollock_goa_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(pollock_goa_sims, here("results/simulation_results/alaska/short_forecasts/pollock_goa_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
pollock_goa_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(pollock_goa_long_sims, here("results/simulation_results/alaska/long_forecasts/pollock_goa_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/pollock_goa_forecast_figs.pdf"))
print_plots(pollock_goa_sims, pollock_goa_long_sims, pollock_goa$recruits, pollock_goa$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
pollock_goa_stats <- save_performance_stats(pollock_goa_sims, pollock_goa_long_sims, pollock_goa$recruits, pollock_goa$year, time_vec1, time_vec2)
saveRDS(pollock_goa_stats, file = here("results/simulation_results/alaska/performance_stats/pollock_goa_stats.Rds"))



# Pacific ocean perch BSAI forecasts ####################################################
## Set Up ---------------------------------------------------------------------
pop_bsai <- filter_alaska_sr_data(pop_bsai)

plot(pop_bsai$year, pop_bsai$recruits, type = "l")
plot(pop_bsai$year, pop_bsai$sbiomass, type = "l")

rec_ts <- pop_bsai$recruits
spawn_ts <- pop_bsai$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
pop_bsai_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(pop_bsai_sims, here("results/simulation_results/alaska/short_forecasts/pop_bsai_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
pop_bsai_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(pop_bsai_long_sims, here("results/simulation_results/alaska/long_forecasts/pop_bsai_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/pop_bsai_forecast_figs.pdf"))
print_plots(pop_bsai_sims, pop_bsai_long_sims, pop_bsai$recruits, pop_bsai$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
pop_bsai_stats <- save_performance_stats(pop_bsai_sims, pop_bsai_long_sims, pop_bsai$recruits, pop_bsai$year, time_vec1, time_vec2)
saveRDS(pop_bsai_stats, file = here("results/simulation_results/alaska/performance_stats/pop_bsai_stats.Rds"))



# Pacific ocean perch GOA forecasts ####################################################
## Set Up ---------------------------------------------------------------------
pop_goa <- filter_alaska_sr_data(pop_goa)

plot(pop_goa$year, pop_goa$recruits, type = "l")
plot(pop_goa$year, pop_goa$sbiomass, type = "l")

rec_ts <- pop_goa$recruits
spawn_ts <- pop_goa$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
pop_goa_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(pop_goa_sims, here("results/simulation_results/alaska/short_forecasts/pop_goa_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
pop_goa_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(pop_goa_long_sims, here("results/simulation_results/alaska/long_forecasts/pop_goa_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/pop_goa_forecast_figs.pdf"))
print_plots(pop_goa_sims, pop_goa_long_sims, pop_goa$recruits, pop_goa$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
pop_goa_stats <- save_performance_stats(pop_goa_sims, pop_goa_long_sims, pop_goa$recruits, pop_goa$year, time_vec1, time_vec2)
saveRDS(pop_goa_stats, file = here("results/simulation_results/alaska/performance_stats/pop_goa_stats.Rds"))



# Sablefish forecasts ####################################################
## Set Up ---------------------------------------------------------------------
sablefish_alaska <- filter_alaska_sr_data(sablefish_alaska)

plot(sablefish_alaska$year, sablefish_alaska$recruits, type = "l")
plot(sablefish_alaska$year, sablefish_alaska$sbiomass, type = "l")

rec_ts <- sablefish_alaska$recruits
spawn_ts <- sablefish_alaska$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
sablefish_alaska_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(sablefish_alaska_sims, here("results/simulation_results/alaska/short_forecasts/sablefish_alaska_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
sablefish_alaska_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(sablefish_alaska_long_sims, here("results/simulation_results/alaska/long_forecasts/sablefish_alaska_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/sablefish_alaska_forecast_figs.pdf"))
print_plots(sablefish_alaska_sims, sablefish_alaska_long_sims, sablefish_alaska$recruits, sablefish_alaska$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sablefish_alaska_stats <- save_performance_stats(sablefish_alaska_sims, sablefish_alaska_long_sims, sablefish_alaska$recruits, sablefish_alaska$year, time_vec1, time_vec2)
saveRDS(sablefish_alaska_stats, file = here("results/simulation_results/alaska/performance_stats/sablefish_alaska_stats.Rds"))


# Yellowfin sole BSAI forecasts ####################################################
## Set Up ---------------------------------------------------------------------
yellowfin_sole_bsai <- filter_alaska_sr_data(yellowfin_sole_bsai)

plot(yellowfin_sole_bsai$year, yellowfin_sole_bsai$recruits, type = "l")
plot(yellowfin_sole_bsai$year, yellowfin_sole_bsai$sbiomass, type = "l")

rec_ts <- yellowfin_sole_bsai$recruits
spawn_ts <- yellowfin_sole_bsai$sbiomass

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts)-4), 1)

## Short-term forecasts ----------------------------------------------------
yellowfin_sole_bsai_sims <- expanding_window(fmethods = c("mean", "AR(1)", "simplex", "PELT", "HMM"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(yellowfin_sole_bsai_sims, here("results/simulation_results/alaska/short_forecasts/yellowfin_sole_bsai_short.Rds"))

## Long-term forecasts ---------------------------------------------------------
yellowfin_sole_bsai_long_sims <- expanding_window_5yr(c("mean", "AR(1)", "simplex", "PELT", "HMM"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(yellowfin_sole_bsai_long_sims, here("results/simulation_results/alaska/long_forecasts/yellowfin_sole_bsai_long.Rds"))

## Visualize simulations
pdf(here("results/figures/alaska_stocks/stock_forecast_figures/yellowfin_sole_bsai_forecast_figs.pdf"))
print_plots(yellowfin_sole_bsai_sims, yellowfin_sole_bsai_long_sims, yellowfin_sole_bsai$recruits, yellowfin_sole_bsai$year, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
yellowfin_sole_bsai_stats <- save_performance_stats(yellowfin_sole_bsai_sims, yellowfin_sole_bsai_long_sims, yellowfin_sole_bsai$recruits, yellowfin_sole_bsai$year, time_vec1, time_vec2)
saveRDS(yellowfin_sole_bsai_stats, file = here("results/simulation_results/alaska/performance_stats/yellowfin_sole_bsai_stats.Rds"))
