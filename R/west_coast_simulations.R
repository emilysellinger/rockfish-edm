## Set seed ####################################################################
set.seed(211)
# Aurora rockfish forecasts ####################################################
## Set Up ---------------------------------------------------------------------
aurora <- filter_sr_data(aurora)

plot(aurora$Yr, aurora$Recruit_0, type = "l")
plot(aurora$Yr, aurora$SpawnBio, type = "l")

rec_ts <- aurora$Recruit_0
spawn_ts <- aurora$SpawnBio

time_vec1 <- seq(20, length(rec_ts), 1)
time_vec2 <- seq(20, (length(rec_ts) - 4), 1)

## Short-term forecasts ----------------------------------------------------
aurora_sims <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(aurora_sims, here("results/simulation_results/west_coast/short_forecasts/aurora_short.Rds"))
## Long-term forecasts ---------------------------------------------------------
long_sims <- expanding_window_5yr(c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)
saveRDS(long_sims, here("results/simulation_results/west_coast/long_forecasts/aurora_long.Rds"))

## Visualize simulations -------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/aurora_forecast_figs.pdf"))
print_plots(aurora_sims, long_sims, aurora$Recruit_0, aurora$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
aurora_stats <- save_performance_stats(aurora_sims, long_sims, aurora$Recruit_0, aurora$Yr, time_vec1, time_vec2)
saveRDS(aurora_stats, file = here("results/simulation_results/west_coast/performance_stats/aurora_stats.Rds"))

# Black Rockfish CA forecasts ##################################################
## Set Up ------------------------------------------------------------------
black_ca <- filter_sr_data(black_ca)

plot(black_ca$Yr, black_ca$Recruit_0, type = "l")
plot(black_ca$SpawnBio, black_ca$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- black_ca$Recruit_0
spawn_ts <- black_ca$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
black_ca_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(black_ca_sims_short, here("results/simulation_results/west_coast/short_forecasts/black_ca_short.Rds"))

## Long-term forecasts -----------------------------------------------------
black_ca_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(black_ca_sims_long, here("results/simulation_results/west_coast/long_forecasts/black_ca_long.Rds"))

## Visualize forecasts -----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/blackCA_forecast_figs.pdf"))
print_plots(black_ca_sims_short, black_ca_sims_long, black_ca$Recruit_0, black_ca$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
black_ca_stats <- save_performance_stats(black_ca_sims_short, black_ca_sims_long, black_ca$Recruit_0, black_ca$Yr, time_vec1, time_vec2)
saveRDS(black_ca_stats, file = here("results/simulation_results/west_coast/performance_stats/black_ca_stats.Rds"))


# Black rockfish WA forecasts ######################################
## Set Up ------------------------------------------------------------------
black_wa <- filter_sr_data(black_wa)

plot(black_wa$Yr, black_wa$Recruit_0, type = "l")
plot(black_wa$SpawnBio, black_wa$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- black_wa$Recruit_0
spawn_ts <- black_wa$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
black_wa_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(black_wa_sims_short, here("results/simulation_results/west_coast/short_forecasts/black_wa_short.Rds"))

## Long-term forecasts -----------------------------------------------------
black_wa_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(black_wa_sims_long, here("results/simulation_results/west_coast/long_forecasts/black_wa_long.Rds"))

## Visualize forecasts -------------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/blackWA_forecast_figs.pdf"))
print_plots(black_wa_sims_short, black_wa_sims_long, black_wa$Recruit_0, black_wa$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
black_wa_stats <- save_performance_stats(black_ca_sims_short, black_ca_sims_long, black_ca$Recruit_0, black_ca$Yr, time_vec1, time_vec2)
saveRDS(black_wa_stats, file = here("results/simulation_results/west_coast/performance_stats/black_wa_stats.Rds"))


# Bocaccio rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
bocaccio <- filter_sr_data(bocaccio)

plot(bocaccio$Yr, bocaccio$Recruit_0, type = "l")
plot(bocaccio$SpawnBio, bocaccio$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- bocaccio$Recruit_0
spawn_ts <- bocaccio$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
bocaccio_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(bocaccio_sims_short, here("results/simulation_results/west_coast/short_forecasts/bocaccio_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
bocaccio_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(bocaccio_sims_long, here("results/simulation_results/west_coast/long_forecasts/bocaccio_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/bocaccio_forecast_figs.pdf"))
print_plots(bocaccio_sims_short, bocaccio_sims_long, bocaccio$Recruit_0, bocaccio$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
bocaccio_stats <- save_performance_stats(bocaccio_sims_short, bocaccio_sims_long, bocaccio$Recruit_0, bocaccio$Yr, time_vec1, time_vec2)
saveRDS(bocaccio_stats, file = here("results/simulation_results/west_coast/performance_stats/bocaccio_stats.Rds"))



# Cabezon NCS forecasts ###################################
## Set Up ------------------------------------------------------------------
cabezon_ncs <- filter_sr_data(cabezon_ncs)

plot(cabezon_ncs$Yr, cabezon_ncs$Recruit_0, type = "l")
plot(cabezon_ncs$SpawnBio, cabezon_ncs$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- cabezon_ncs$Recruit_0
spawn_ts <- cabezon_ncs$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
cabezon_ncs_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(cabezon_ncs_sims_short, here("results/simulation_results/west_coast/short_forecasts/cabezon_ncs_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
cabezon_ncs_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(cabezon_ncs_sims_long, here("results/simulation_results/west_coast/long_forecasts/cabezon_ncs_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/cabezon_ncs_forecast_figs.pdf"))
print_plots(cabezon_ncs_sims_short, cabezon_ncs_sims_long, cabezon_ncs$Recruit_0, cabezon_ncs$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
cabezon_ncs_stats <- save_performance_stats(cabezon_ncs_sims_short, cabezon_ncs_sims_long, cabezon_ncs$Recruit_0, cabezon_ncs$Yr, time_vec1, time_vec2)
saveRDS(cabezon_ncs_stats, file = here("results/simulation_results/west_coast/performance_stats/cabezon_ncs_stats.Rds"))



# Cabezon ORS rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
cabezon_ors <- filter_sr_data(cabezon_ors)

plot(cabezon_ors$Yr, cabezon_ors$Recruit_0, type = "l")
plot(cabezon_ors$SpawnBio, cabezon_ors$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- cabezon_ors$Recruit_0
spawn_ts <- cabezon_ors$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
cabezon_ors_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(cabezon_ors_sims_short, here("results/simulation_results/west_coast/short_forecasts/cabezon_ors_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
cabezon_ors_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(cabezon_ors_sims_long, here("results/simulation_results/west_coast/long_forecasts/cabezon_ors_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/cabezon_ors_forecast_figs.pdf"))
print_plots(cabezon_ors_sims_short, cabezon_ors_sims_long, cabezon_ors$Recruit_0, cabezon_ors$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
cabezon_ors_stats <- save_performance_stats(cabezon_ors_sims_short, cabezon_ors_sims_long, cabezon_ors$Recruit_0, cabezon_ors$Yr, time_vec1, time_vec2)
saveRDS(cabezon_ors_stats, file = here("results/simulation_results/west_coast/performance_stats/cabezon_ors_stats.Rds"))


# Cabezon SCS rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
cabezon_scs <- filter_sr_data(cabezon_scs)

plot(cabezon_scs$Yr, cabezon_scs$Recruit_0, type = "l")
plot(cabezon_scs$SpawnBio, cabezon_scs$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- cabezon_scs$Recruit_0
spawn_ts <- cabezon_scs$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
cabezon_scs_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(cabezon_scs_sims_short, here("results/simulation_results/west_coast/short_forecasts/cabezon_scs_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
cabezon_scs_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(cabezon_scs_sims_long, here("results/simulation_results/west_coast/long_forecasts/cabezon_scs_short.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/cabezon_scs_forecast_figs.pdf"))
print_plots(cabezon_scs_sims_short, cabezon_scs_sims_long, cabezon_scs$Recruit_0, cabezon_scs$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
cabezon_scs_stats <- save_performance_stats(cabezon_scs_sims_short, cabezon_scs_sims_long, cabezon_scs$Recruit_0, cabezon_scs$Yr, time_vec1, time_vec2)
saveRDS(cabezon_scs_stats, file = here("results/simulation_results/west_coast/performance_stats/cabezon_scs_stats.Rds"))



# Canary rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
canary <- filter_sr_data(canary)
canary1 <- canary %>% filter(Area == 1)

plot(canary1$Yr, canary1$Recruit_0, type = "l")
plot(canary1$SpawnBio, canary1$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- canary1$Recruit_0
spawn_ts <- canary1$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
canary1_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(canary1_sims_short, here("results/simulation_results/west_coast/short_forecasts/canary_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
canary1_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(canary1_sims_long, here("results/simulation_results/west_coast/long_forecasts/canary_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/canary1_forecast_figs.pdf"))
print_plots(canary1_sims_short, canary1_sims_long, canary1$Recruit_0, canary1$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
canary1_stats <- save_performance_stats(canary1_sims_short, canary1_sims_long, canary1$Recruit_0, canary1$Yr, time_vec1, time_vec2)
saveRDS(canary1_stats, file = here("results/simulation_results/west_coast/performance_stats/canary1_stats.Rds"))



# Chilipepper rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
chilipepper <- filter_sr_data(chilipepper)

plot(chilipepper$Yr, chilipepper$Recruit_0, type = "l")
plot(chilipepper$SpawnBio, chilipepper$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- chilipepper$Recruit_0
spawn_ts <- chilipepper$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
chilipepper_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(chilipepper_sims_short, here("results/simulation_results/west_coast/short_forecasts/chilipepper_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
chilipepper_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(chilipepper_sims_long, here("results/simulation_results/west_coast/long_forecasts/chilipepper_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/chilipepper_forecast_figs.pdf"))
print_plots(chilipepper_sims_short, chilipepper_sims_long, chilipepper$Recruit_0, chilipepper$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
chilipepper_stats <- save_performance_stats(chilipepper_sims_short, chilipepper_sims_long, chilipepper$Recruit_0, chilipepper$Yr, time_vec1, time_vec2)
saveRDS(chilipepper_stats, file = here("results/simulation_results/west_coast/performance_stats/chilipepper_stats.Rds"))



# Darkblotched rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
darkblotched <- filter_sr_data(darkblotched)

plot(darkblotched$Yr, darkblotched$Recruit_0, type = "l")
plot(darkblotched$SpawnBio, darkblotched$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- darkblotched$Recruit_0
spawn_ts <- darkblotched$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
darkblotched_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(darkblotched_sims_short, here("results/simulation_results/west_coast/short_forecasts/darkblotched_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
darkblotched_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(darkblotched_sims_short, here("results/simulation_results/west_coast/long_forecasts/darkblotched_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/darkblotched_forecast_figs.pdf"))
print_plots(darkblotched_sims_short, darkblotched_sims_long, darkblotched$Recruit_0, darkblotched$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
darkblotched_stats <- save_performance_stats(darkblotched_sims_short, darkblotched_sims_long, darkblotched$Recruit_0, darkblotched$Yr, time_vec1, time_vec2)
saveRDS(darkblotched_stats, file = here("results/simulation_results/west_coast/performance_stats/darkblotched_stats.Rds"))


# Dover Sole forecasts ###################################
## Set Up ------------------------------------------------------------------
dover_sole <- filter_sr_data(dover_sole)

plot(dover_sole$Yr, dover_sole$Recruit_0, type = "l")
plot(dover_sole$SpawnBio, dover_sole$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- dover_sole$Recruit_0
spawn_ts <- dover_sole$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
dover_sole_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(dover_sole_sims_short, here("results/simulation_results/west_coast/short_forecasts/dover_sole_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
dover_sole_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(dover_sole_sims_long, here("results/simulation_results/west_coast/long_forecasts/dover_sole_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/dover_sole_forecast_figs.pdf"))
print_plots(dover_sole_sims_short, dover_sole_sims_long, dover_sole$Recruit_0, dover_sole$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
dover_sole_stats <- save_performance_stats(dover_sole_sims_short, dover_sole_sims_long, dover_sole$Recruit_0, dover_sole$Yr, time_vec1, time_vec2)
saveRDS(dover_sole_stats, file = here("results/simulation_results/west_coast/performance_stats/dover_sole_stats.Rds"))



# Kelp Greenling forecasts ###################################
## Set Up ------------------------------------------------------------------
kelp_greenling <- filter_sr_data(kelp_greenling)

plot(kelp_greenling$Yr, kelp_greenling$Recruit_0, type = "l")
plot(kelp_greenling$SpawnBio, kelp_greenling$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- kelp_greenling$Recruit_0
spawn_ts <- kelp_greenling$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
kelp_greenling_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(kelp_greenling_sims_short, here("results/simulation_results/west_coast/short_forecasts/kelp_greenling_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
kelp_greenling_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(kelp_greenling_sims_short, here("results/simulation_results/west_coast/long_forecasts/kelp_greenling_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/kelp_greenling_forecast_figs.pdf"))
print_plots(kelp_greenling_sims_short, kelp_greenling_sims_long, kelp_greenling$Recruit_0, kelp_greenling$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
kelp_greenling_stats <- save_performance_stats(kelp_greenling_sims_short, kelp_greenling_sims_long, kelp_greenling$Recruit_0, kelp_greenling$Yr, time_vec1, time_vec2)
saveRDS(kelp_greenling_stats, file = here("results/simulation_results/west_coast/performance_stats/kelp_greenling_stats.Rds"))


# Lingcod North forecasts ###################################
## Set Up ------------------------------------------------------------------
lingcod_n <- filter_sr_data(lingcod_n)

plot(lingcod_n$Yr, lingcod_n$Recruit_0, type = "l")
plot(lingcod_n$SpawnBio, lingcod_n$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- lingcod_n$Recruit_0
spawn_ts <- lingcod_n$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
lingcod_n_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(lingcod_n_sims_short, here("results/simulation_results/west_coast/short_forecasts/lingcod_n_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
lingcod_n_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(lingcod_n_sims_short, here("results/simulation_results/west_coast/long_forecasts/lingcod_n_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/lingcod_n_forecast_figs.pdf"))
print_plots(lingcod_n_sims_short, lingcod_n_sims_long, lingcod_n$Recruit_0, lingcod_n$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
lingcod_n_stats <- save_performance_stats(lingcod_n_sims_short, lingcod_n_sims_long, lingcod_n$Recruit_0, lingcod_n$Yr, time_vec1, time_vec2)
saveRDS(lingcod_n_stats, file = here("results/simulation_results/west_coast/performance_stats/lingcod_n_stats.Rds"))


# Lingcod South forecasts ###################################
## Set Up ------------------------------------------------------------------
lingcod_s <- filter_sr_data(lingcod_s)

plot(lingcod_s$Yr, lingcod_s$Recruit_0, type = "l")
plot(lingcod_s$SpawnBio, lingcod_s$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- lingcod_s$Recruit_0
spawn_ts <- lingcod_s$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
lingcod_s_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(lingcod_s_sims_short, here("results/simulation_results/west_coast/short_forecasts/lingcod_s_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
lingcod_s_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex","PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(lingcod_s_sims_short, here("results/simulation_results/west_coast/long_forecasts/lingcod_s_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/lingcod_s_forecast_figs.pdf"))
print_plots(lingcod_s_sims_short, lingcod_s_sims_long, lingcod_s$Recruit_0, lingcod_s$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
lingcod_s_stats <- save_performance_stats(lingcod_s_sims_short, lingcod_s_sims_long, lingcod_s$Recruit_0, lingcod_s$Yr, time_vec1, time_vec2)
saveRDS(lingcod_s_stats, file = here("results/simulation_results/west_coast/performance_stats/lingcod_s_stats.Rds"))



# Petrale Sole sole forecasts ###################################
## Set Up ---------------------------------------------------
petrale_sole <- filter_sr_data(petrale_sole)

plot(petrale_sole$Yr, petrale_sole$Recruit_0, type = "l")
plot(petrale_sole$SpawnBio, petrale_sole$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- petrale_sole$Recruit_0
spawn_ts <- petrale_sole$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
petrale_sole_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(petrale_sole_sims_short, here("results/simulation_results/west_coast/short_forecasts/petrale_sole_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
petrale_sole_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(petrale_sole_sims_long, here("results/simulation_results/west_coast/long_forecasts/petrale_sole_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/petrale_sole_forecast_figs.pdf"))
print_plots(petrale_sole_sims_short, petrale_sole_sims_long, petrale_sole$Recruit_0, petrale_sole$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
petrale_sole_stats <- save_performance_stats(petrale_sole_sims_short, petrale_sole_sims_long, petrale_sole$Recruit_0, petrale_sole$Yr, time_vec1, time_vec2)
saveRDS(petrale_sole_stats, file = here("results/simulation_results/west_coast/performance_stats/petrale_sole_stats.Rds"))



# Sablefish forecasts ###################################
## Set Up -------------------------------------------------------
sablefish <- filter_sr_data(sablefish)

plot(sablefish$Yr, sablefish$Recruit_0, type = "l")
plot(sablefish$SpawnBio, sablefish$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- sablefish$Recruit_0
spawn_ts <- sablefish$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
sablefish_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(sablefish_sims_short, here("results/simulation_results/west_coast/short_forecasts/sablefish_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
sablefish_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(sablefish_sims_short, here("results/simulation_results/west_coast/long_forecasts/sablefish_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/sablefish_forecast_figs.pdf"))
print_plots(sablefish_sims_short, sablefish_sims_long, sablefish$Recruit_0, sablefish$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sablefish_stats <- save_performance_stats(sablefish_sims_short, sablefish_sims_long, sablefish$Recruit_0, sablefish$Yr, time_vec1, time_vec2)
saveRDS(sablefish_stats, file = here("results/simulation_results/west_coast/performance_stats/sablefish_stats.Rds"))



# Splitnose rockfish forecasts ###################################
## Set Up ---------------------------------------------------------
splitnose <- filter_sr_data(splitnose)

plot(splitnose$Yr, splitnose$Recruit_0, type = "l")
plot(splitnose$SpawnBio, splitnose$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- splitnose$Recruit_0
spawn_ts <- splitnose$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
splitnose_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(splitnose_sims_short, here("results/simulation_results/west_coast/short_forecasts/splitnose_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
splitnose_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(splitnose_sims_long, here("results/simulation_results/west_coast/long_forecasts/splitnose_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/splitnose_forecast_figs.pdf"))
print_plots(splitnose_sims_short, splitnose_sims_long, splitnose$Recruit_0, splitnose$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
splitnose_stats <- save_performance_stats(splitnose_sims_short, splitnose_sims_long, splitnose$Recruit_0, splitnose$Yr, time_vec1, time_vec2)
saveRDS(splitnose_stats, file = here("results/simulation_results/west_coast/performance_stats/splitnose_stats.Rds"))


# Yelloweye forecasts ###################################
## Set Up -------------------------------------------------------------
yelloweye <- filter_sr_data(yelloweye)
yelloweye1 <- yelloweye %>% filter(Area==1)

plot(yelloweye1$Yr, yelloweye1$Recruit_0, type = "l")
plot(yelloweye1$SpawnBio, yelloweye1$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- yelloweye1$Recruit_0
spawn_ts <- yelloweye1$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
yelloweye1_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(yelloweye1_sims_short, here("results/simulation_results/west_coast/short_forecasts/yelloweye_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
yelloweye1_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "HMM", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(yelloweye1_sims_long, here("results/simulation_results/west_coast/long_forecasts/yelloweye_long.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/yelloweye1_forecast_figs.pdf"))
print_plots(yelloweye1_sims_short, yelloweye1_sims_long, yelloweye1$Recruit_0, yelloweye1$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
yelloweye1_stats <- save_performance_stats(yelloweye1_sims_short, yelloweye1_sims_long, yelloweye1$Recruit_0, yelloweye1$Yr, time_vec1, time_vec2)
saveRDS(yelloweye1_stats, file = here("results/simulation_results/west_coast/performance_stats/yelloweye1_stats.Rds"))



# Widow rockfish forecasts ###################################
## Set Up -------------------------------------------------------
widow <- filter_sr_data(widow)

plot(widow$Yr, widow$Recruit_0, type = "l")
plot(widow$SpawnBio, widow$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- widow$Recruit_0
spawn_ts <- widow$SpawnBio
# create time vectors
time_vec1 <- seq(20, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(20, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
widow_sims_short <- expanding_window(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(widow_sims_short, here("results/simulation_results/west_coast/short_forecasts/widow_short.Rds"))

## Long-term forecasts ------------------------------------------------------------
widow_sims_long <- expanding_window_5yr(fmethods = c("mean", "AR(1)", "Beverton-Holt", "simplex", "PELT"), 1000, time_vec1, rec_ts, spawn_ts)
saveRDS(widow_sims_short, here("results/simulation_results/west_coast/long_forecasts/widow_short.Rds"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/widow_forecast_figs.pdf"))
print_plots(widow_sims_short, widow_sims_long, widow$Recruit_0, widow$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
widow_stats <- save_performance_stats(widow_sims_short, widow_sims_long, widow$Recruit_0, widow$Yr, time_vec1, time_vec2)
saveRDS(widow_stats, file = here("results/simulation_results/west_coast/performance_stats/widow_stats.Rds"))

