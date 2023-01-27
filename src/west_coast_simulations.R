# Aurora rockfish forecasts ####################################################
## Set Up ---------------------------------------------------------------------
aurora <- filter_sr_data(aurora)

plot(aurora$Yr, aurora$Recruit_0, type = "l")
plot(aurora$Yr, aurora$SpawnBio, type = "l")

rec_ts <- aurora$Recruit_0
spawn_ts <- aurora$SpawnBio

time_vec1 <- seq(30, 50, 1)
time_vec2 <- seq(30, (50-4), 1)

## Short-term forecasts ----------------------------------------------------
set.seed(112)
aurora_sims <- expanding_window(fmethods = c("m", "ar", "bh", "simplex", "hmm"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- aurora_sims[,,1]
ar_preds <- aurora_sims[,,2]
bh_preds <- aurora_sims[,,3]
simplex_preds <- aurora_sims[,,4]
hmm_preds <- aurora_sims[,,5]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/aurora_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/aurora_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/aurora_1stp_bh.csv"))
write_csv(as.data.frame(hmm_preds), file = here("results/simulation_results/west_coast/short_forecasts/aurora_1stp_hmm.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/aurora_1stp_simplex.csv"))

## Long-term forecasts ---------------------------------------------------------
long_sims <- expanding_window_5yr(c("m", "ar", "bh", "simplex", "hmm"), 1000, time_vec = time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- long_sims[,,1]
ar_preds_long <- long_sims[,,2]
bh_preds_long <- long_sims[,,3]
simplex_preds_long <- long_sims[,,4]
hmm_preds_long <- long_sims[,,5]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/aurora_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/aurora_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/aurora_5stp_bh.csv"))
write_csv(as.data.frame(hmm_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/aurora_5stp_hmm.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/aurora_5stp_simplex.csv"))

## Visualize simulations
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
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
black_ca_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex", "hmm"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- black_ca_sims_short[,,1]
ar_preds <- black_ca_sims_short[,,2]
bh_preds <- black_ca_sims_short[,,3]
simplex_preds <- black_ca_sims_short[,,4]
hmm_preds <- black_ca_sims_short[,,5]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackCA_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackCA_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackCA_1stp_bh.csv"))
write_csv(as.data.frame(hmm_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackCA_1stp_hmm.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackCA_1stp_simplex.csv"))


## Long-term forecasts -----------------------------------------------------
black_ca_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex", "hmm"), 1000, time_vec1, rec_ts, spawn_ts)


# extract forecasts
m_preds_long <- black_ca_sims_long[,,1]
ar_preds_long <- black_ca_sims_long[,,2]
bh_preds_long <- black_ca_sims_long[,,3]
simplex_preds_long <- black_ca_sims_long[,,4]
hmm_preds_long <- black_ca_sims_long[,,5]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/blackCA_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/blackCA_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/blackCA_5stp_bh.csv"))
write_csv(as.data.frame(hmm_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/blackCA_5stp_hmm.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/blackCA_5stp_simplex.csv"))

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
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
black_wa_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex", "hmm"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- black_wa_sims_short[,,1]
ar_preds <- black_wa_sims_short[,,2]
bh_preds <- black_wa_sims_short[,,3]
simplex_preds <- black_wa_sims_short[,,4]
hmm_preds <- black_wa_sims_short[,,5]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackWA_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackWA_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackWA_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackWA_1stp_simplex.csv"))
write_csv(as.data.frame(hmm_preds), file = here("results/simulation_results/west_coast/short_forecasts/blackWA_1stp_hmm.csv"))
## Long-term forecasts -----------------------------------------------------
black_wa_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex", "hmm"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- black_wa_sims_long[,,1]
ar_preds_long <- black_wa_sims_long[,,2]
bh_preds_long <- black_wa_sims_long[,,3]
simplex_preds_long <- black_wa_sims_long[,,4]
hmm_preds_long <- black_wa_sims_long[,,5]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/blackWA_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/blackWA_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/blackWA_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/long_forecasts/blackWA_5stp_simplex.csv"))

## Visualize forecasts -------------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/blackWA_forecast_figs.pdf"))
print_plots(black_wa_sims_short, black_wa_sims_long, black_wa$Recruit_0, black_wa$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/blackWA_stats.txt"))
print(save_performance_stats(black_wa_sims_short, black_wa_sims_long, black_wa$Recruit_0, black_wa$Yr, time_vec1, time_vec2))
sink()



# Bocaccio rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
bocaccio <- filter_sr_data(bocaccio)

plot(bocaccio$Yr, bocaccio$Recruit_0, type = "l")
plot(bocaccio$SpawnBio, bocaccio$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- bocaccio$Recruit_0
spawn_ts <- bocaccio$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
bocaccio_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- bocaccio_sims_short[,,1]
ar_preds <- bocaccio_sims_short[,,2]
bh_preds <- bocaccio_sims_short[,,3]
simplex_preds <- bocaccio_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/bocaccio_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/bocaccio_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/bocaccio_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/bocaccio_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
bocaccio_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- bocaccio_sims_long[,,1]
ar_preds_long <- bocaccio_sims_long[,,2]
bh_preds_long <- bocaccio_sims_long[,,3]
simplex_preds_long <- bocaccio_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/bocaccio_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/bocaccio_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/bocaccio_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/bocaccio_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/bocaccio_forecast_figs.pdf"))
print_plots(bocaccio_sims_short, bocaccio_sims_long, bocaccio$Recruit_0, bocaccio$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/bocaccio_stats.txt"))
print(save_performance_stats(bocaccio_sims_short, bocaccio_sims_long, bocaccio$Recruit_0, bocaccio$Yr, time_vec1, time_vec2))
sink()



# Cabezon NCS forecasts ###################################
## Set Up ------------------------------------------------------------------
cabezon_ncs <- filter_sr_data(cabezon_ncs)

plot(cabezon_ncs$Yr, cabezon_ncs$Recruit_0, type = "l")
plot(cabezon_ncs$SpawnBio, cabezon_ncs$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- cabezon_ncs$Recruit_0
spawn_ts <- cabezon_ncs$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
cabezon_ncs_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- cabezon_ncs_sims_short[,,1]
ar_preds <- cabezon_ncs_sims_short[,,2]
bh_preds <- cabezon_ncs_sims_short[,,3]
simplex_preds <- cabezon_ncs_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ncs_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ncs_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ncs_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ncs_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
cabezon_ncs_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- cabezon_ncs_sims_long[,,1]
ar_preds_long <- cabezon_ncs_sims_long[,,2]
bh_preds_long <- cabezon_ncs_sims_long[,,3]
simplex_preds_long <- cabezon_ncs_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ncs_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ncs_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ncs_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ncs_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/cabezon_ncs_forecast_figs.pdf"))
print_plots(cabezon_ncs_sims_short, cabezon_ncs_sims_long, cabezon_ncs$Recruit_0, cabezon_ncs$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/cabezon_ncs_stats.txt"))
print(save_performance_stats(cabezon_ncs_sims_short, cabezon_ncs_sims_long, cabezon_ncs$Recruit_0, cabezon_ncs$Yr, time_vec1, time_vec2))
sink()



# Cabezon ORS rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
cabezon_ors <- filter_sr_data(cabezon_ors)

plot(cabezon_ors$Yr, cabezon_ors$Recruit_0, type = "l")
plot(cabezon_ors$SpawnBio, cabezon_ors$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- cabezon_ors$Recruit_0
spawn_ts <- cabezon_ors$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
cabezon_ocs_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- cabezon_ocs_sims_short[,,1]
ar_preds <- cabezon_ocs_sims_short[,,2]
bh_preds <- cabezon_ocs_sims_short[,,3]
simplex_preds <- cabezon_ocs_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
cabezon_ocs_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- cabezon_ocs_sims_long[,,1]
ar_preds_long <- cabezon_ocs_sims_long[,,2]
bh_preds_long <- cabezon_ocs_sims_long[,,3]
simplex_preds_long <- cabezon_ocs_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_ocs_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/cabezon_ors_forecast_figs.pdf"))
print_plots(cabezon_ors_sims_short, cabezon_ors_sims_long, cabezon_ors$Recruit_0, cabezon_ors$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/cabezon_ors_stats.txt"))
print(save_performance_stats(cabezon_ors_sims_short, cabezon_ors_sims_long, cabezon_ors$Recruit_0, cabezon_ors$Yr, time_vec1, time_vec2))
sink()


# Cabezon SCS rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
cabezon_scs <- filter_sr_data(cabezon_scs)

plot(cabezon_scs$Yr, cabezon_scs$Recruit_0, type = "l")
plot(cabezon_scs$SpawnBio, cabezon_scs$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- cabezon_scs$Recruit_0
spawn_ts <- cabezon_scs$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
cabezon_scs_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- cabezon_scs_sims_short[,,1]
ar_preds <- cabezon_scs_sims_short[,,2]
bh_preds <- cabezon_scs_sims_short[,,3]
simplex_preds <- cabezon_scs_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_scs_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_scs_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_scs_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_scs_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
cabezon_scs_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- cabezon_scs_sims_long[,,1]
ar_preds_long <- cabezon_scs_sims_long[,,2]
bh_preds_long <- cabezon_scs_sims_long[,,3]
simplex_preds_long <- cabezon_scs_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_scs_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_scs_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_scs_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/cabezon_scs_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/cabezon_scs_forecast_figs.pdf"))
print_plots(cabezon_scs_sims_short, cabezon_scs_sims_long, cabezon_scs$Recruit_0, cabezon_scs$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/cabezon_scs_stats.txt"))
print(save_performance_stats(cabezon_scs_sims_short, cabezon_scs_sims_long, cabezon_scs$Recruit_0, cabezon_scs$Yr, time_vec1, time_vec2))
sink()



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
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
canary1_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- canary1_sims_short[,,1]
ar_preds <- canary1_sims_short[,,2]
bh_preds <- canary1_sims_short[,,3]
simplex_preds <- canary1_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/canary1_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/canary1_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/canary1_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/canary1_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
canary1_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- canary1_sims_long[,,1]
ar_preds_long <- canary1_sims_long[,,2]
bh_preds_long <- canary1_sims_long[,,3]
simplex_preds_long <- canary1_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/canary1_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/canary1_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/canary1_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/canary1_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/canary1_forecast_figs.pdf"))
print_plots(canary1_sims_short, canary1_sims_long, canary1$Recruit_0, canary1$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/canary1_stats.txt"))
print(save_performance_stats(canary1_sims_short, canary1_sims_long, canary1$Recruit_0, canary1$Yr, time_vec1, time_vec2))
sink()



# Chilipepper rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
chilipepper <- filter_sr_data(chilipepper)

plot(chilipepper$Yr, chilipepper$Recruit_0, type = "l")
plot(chilipepper$SpawnBio, chilipepper$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- chilipepper$Recruit_0
spawn_ts <- chilipepper$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
chilipepper_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- chilipepper_sims_short[,,1]
ar_preds <- chilipepper_sims_short[,,2]
bh_preds <- chilipepper_sims_short[,,3]
simplex_preds <- chilipepper_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/chilipepper_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/chilipepper_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/chilipepper_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/chilipepper_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
chilipepper_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- chilipepper_sims_long[,,1]
ar_preds_long <- chilipepper_sims_long[,,2]
bh_preds_long <- chilipepper_sims_long[,,3]
simplex_preds_long <- chilipepper_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/chilipepper_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/chilipepper_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/chilipepper_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/chilipepper_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/chilipepper_forecast_figs.pdf"))
print_plots(chilipepper_sims_short, chilipepper_sims_long, chilipepper$Recruit_0, chilipepper$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/chilipepper_stats.txt"))
print(save_performance_stats(chilipepper_sims_short, chilipepper_sims_long, chilipepper$Recruit_0, chilipepper$Yr, time_vec1, time_vec2))
sink()



# Darkblotched rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
darkblotched <- filter_sr_data(darkblotched)

plot(darkblotched$Yr, darkblotched$Recruit_0, type = "l")
plot(darkblotched$SpawnBio, darkblotched$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- darkblotched$Recruit_0
spawn_ts <- darkblotched$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
darkblotched_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- darkblotched_sims_short[,,1]
ar_preds <- darkblotched_sims_short[,,2]
bh_preds <- darkblotched_sims_short[,,3]
simplex_preds <- darkblotched_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/darkblotched_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/darkblotched_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/darkblotched_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/darkblotched_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
darkblotched_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- darkblotched_sims_long[,,1]
ar_preds_long <- darkblotched_sims_long[,,2]
bh_preds_long <- darkblotched_sims_long[,,3]
simplex_preds_long <- darkblotched_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/darkblotched_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/darkblotched_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/darkblotched_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/darkblotched_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/darkblotched_forecast_figs.pdf"))
print_plots(darkblotched_sims_short, darkblotched_sims_long, darkblotched$Recruit_0, darkblotched$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/darkblotched_stats.txt"))
print(save_performance_stats(darkblotched_sims_short, darkblotched_sims_long, darkblotched$Recruit_0, darkblotched$Yr, time_vec1, time_vec2))
sink()


# Dover Sole forecasts ###################################
## Set Up ------------------------------------------------------------------
dover_sole <- filter_sr_data(dover_sole)

plot(dover_sole$Yr, dover_sole$Recruit_0, type = "l")
plot(dover_sole$SpawnBio, dover_sole$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- dover_sole$Recruit_0
spawn_ts <- dover_sole$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
dover_sole_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- dover_sole_sims_short[,,1]
ar_preds <- dover_sole_sims_short[,,2]
bh_preds <- dover_sole_sims_short[,,3]
simplex_preds <- dover_sole_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/dover_sole_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/dover_sole_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/dover_sole_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/dover_sole_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
dover_sole_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- dover_sole_sims_long[,,1]
ar_preds_long <- dover_sole_sims_long[,,2]
bh_preds_long <- dover_sole_sims_long[,,3]
simplex_preds_long <- dover_sole_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/dover_sole_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/dover_sole_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/dover_sole_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/dover_sole_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/dover_sole_forecast_figs.pdf"))
print_plots(dover_sole_sims_short, dover_sole_sims_long, dover_sole$Recruit_0, dover_sole$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/dover_sole_stats.txt"))
print(save_performance_stats(dover_sole_sims_short, dover_sole_sims_long, dover_sole$Recruit_0, dover_sole$Yr, time_vec1, time_vec2))
sink()


# Kelp Greenling forecasts ###################################
## Set Up ------------------------------------------------------------------
kelp_greenling <- filter_sr_data(kelp_greenling)

plot(kelp_greenling$Yr, kelp_greenling$Recruit_0, type = "l")
plot(kelp_greenling$SpawnBio, kelp_greenling$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- kelp_greenling$Recruit_0
spawn_ts <- kelp_greenling$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
kelp_greenling_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- kelp_greenling_sims_short[,,1]
ar_preds <- kelp_greenling_sims_short[,,2]
bh_preds <- kelp_greenling_sims_short[,,3]
simplex_preds <- kelp_greenling_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/kelp_greenling_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/kelp_greenling_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/kelp_greenling_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/kelp_greenling_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
kelp_greenling_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- kelp_greenling_sims_long[,,1]
ar_preds_long <- kelp_greenling_sims_long[,,2]
bh_preds_long <- kelp_greenling_sims_long[,,3]
simplex_preds_long <- kelp_greenling_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/kelp_greenling_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/kelp_greenling_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/kelp_greenling_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/kelp_greenling_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/kelp_greenling_forecast_figs.pdf"))
print_plots(kelp_greenling_sims_short, kelp_greenling_sims_long, kelp_greenling$Recruit_0, kelp_greenling$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/kelp_greenling_stats.txt"))
print(save_performance_stats(kelp_greenling_sims_short, kelp_greenling_sims_long, kelp_greenling$Recruit_0, kelp_greenling$Yr, time_vec1, time_vec2))
sink()



# Lingcod North forecasts ###################################
## Set Up ------------------------------------------------------------------
lingcod_n <- filter_sr_data(lingcod_n)

plot(lingcod_n$Yr, lingcod_n$Recruit_0, type = "l")
plot(lingcod_n$SpawnBio, lingcod_n$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- lingcod_n$Recruit_0
spawn_ts <- lingcod_n$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
lingcod_n_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- lingcod_n_sims_short[,,1]
ar_preds <- lingcod_n_sims_short[,,2]
bh_preds <- lingcod_n_sims_short[,,3]
simplex_preds <- lingcod_n_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_n_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_n_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_n_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_n_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
lingcod_n_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- lingcod_n_sims_long[,,1]
ar_preds_long <- lingcod_n_sims_long[,,2]
bh_preds_long <- lingcod_n_sims_long[,,3]
simplex_preds_long <- lingcod_n_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_n_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_n_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_n_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_n_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/lingcod_n_forecast_figs.pdf"))
print_plots(lingcod_n_sims_short, lingcod_n_sims_long, lingcod_n$Recruit_0, lingcod_n$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/lingcod_n_stats.txt"))
print(save_performance_stats(lingcod_n_sims_short, lingcod_n_sims_long, lingcod_n$Recruit_0, lingcod_n$Yr, time_vec1, time_vec2))
sink()



# Lingcod South forecasts ###################################
## Set Up ------------------------------------------------------------------
lingcod_s <- filter_sr_data(lingcod_s)

plot(lingcod_s$Yr, lingcod_s$Recruit_0, type = "l")
plot(lingcod_s$SpawnBio, lingcod_s$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- lingcod_s$Recruit_0
spawn_ts <- lingcod_s$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
lingcod_s_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- lingcod_s_sims_short[,,1]
ar_preds <- lingcod_s_sims_short[,,2]
bh_preds <- lingcod_s_sims_short[,,3]
simplex_preds <- lingcod_s_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_s_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_s_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_s_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_s_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
lingcod_s_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- lingcod_s_sims_long[,,1]
ar_preds_long <- lingcod_s_sims_long[,,2]
bh_preds_long <- lingcod_s_sims_long[,,3]
simplex_preds_long <- lingcod_s_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_s_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_s_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_s_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/lingcod_s_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/lingcod_s_forecast_figs.pdf"))
print_plots(lingcod_s_sims_short, lingcod_s_sims_long, lingcod_s$Recruit_0, lingcod_s$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/lingcod_s_stats.txt"))
print(save_performance_stats(lingcod_s_sims_short, lingcod_s_sims_long, lingcod_s$Recruit_0, lingcod_s$Yr, time_vec1, time_vec2))
sink()


# Petrale sole forecasts ###################################
## Set Up ------------------------------------------------------------------
petrale_sole <- filter_sr_data(petrale_sole)

plot(petrale_sole$Yr, petrale_sole$Recruit_0, type = "l")
plot(petrale_sole$SpawnBio, petrale_sole$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- petrale_sole$Recruit_0
spawn_ts <- petrale_sole$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
petrale_sole_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- petrale_sole_sims_short[,,1]
ar_preds <- petrale_sole_sims_short[,,2]
bh_preds <- petrale_sole_sims_short[,,3]
simplex_preds <- petrale_sole_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/petrale_sole_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/petrale_sole_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/petrale_sole_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/petrale_sole_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
petrale_sole_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- petrale_sole_sims_long[,,1]
ar_preds_long <- petrale_sole_sims_long[,,2]
bh_preds_long <- petrale_sole_sims_long[,,3]
simplex_preds_long <- petrale_sole_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/petrale_sole_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/petrale_sole_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/petrale_sole_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/petrale_sole_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/petrale_sole_forecast_figs.pdf"))
print_plots(petrale_sole_sims_short, petrale_sole_sims_long, petrale_sole$Recruit_0, petrale_sole$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/petrale_sole_stats.txt"))
print(save_performance_stats(petrale_sole_sims_short, petrale_sole_sims_long, petrale_sole$Recruit_0, petrale_sole$Yr, time_vec1, time_vec2))
sink()


# Sablefish forecasts ###################################
## Set Up ------------------------------------------------------------------
sablefish <- filter_sr_data(sablefish)

plot(sablefish$Yr, sablefish$Recruit_0, type = "l")
plot(sablefish$SpawnBio, sablefish$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- sablefish$Recruit_0
spawn_ts <- sablefish$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
sablefish_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- sablefish_sims_short[,,1]
ar_preds <- sablefish_sims_short[,,2]
bh_preds <- sablefish_sims_short[,,3]
simplex_preds <- sablefish_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/sablefish_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/sablefish_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/sablefish_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/sablefish_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
sablefish_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- sablefish_sims_long[,,1]
ar_preds_long <- sablefish_sims_long[,,2]
bh_preds_long <- sablefish_sims_long[,,3]
simplex_preds_long <- sablefish_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/sablefish_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/sablefish_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/sablefish_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/sablefish_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/sablefish_forecast_figs.pdf"))
print_plots(sablefish_sims_short, sablefish_sims_long, sablefish$Recruit_0, sablefish$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/sablefish_stats.txt"))
print(save_performance_stats(sablefish_sims_short, sablefish_sims_long, sablefish$Recruit_0, sablefish$Yr, time_vec1, time_vec2))
sink()


# Splitnose rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
splitnose <- filter_sr_data(splitnose)

plot(splitnose$Yr, splitnose$Recruit_0, type = "l")
plot(splitnose$SpawnBio, splitnose$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- splitnose$Recruit_0
spawn_ts <- splitnose$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
splitnose_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- splitnose_sims_short[,,1]
ar_preds <- splitnose_sims_short[,,2]
bh_preds <- splitnose_sims_short[,,3]
simplex_preds <- splitnose_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/splitnose_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/splitnose_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/splitnose_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/splitnose_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
splitnose_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- splitnose_sims_long[,,1]
ar_preds_long <- splitnose_sims_long[,,2]
bh_preds_long <- splitnose_sims_long[,,3]
simplex_preds_long <- splitnose_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/splitnose_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/splitnose_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/splitnose_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/splitnose_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/splitnose_forecast_figs.pdf"))
print_plots(splitnose_sims_short, splitnose_sims_long, splitnose$Recruit_0, splitnose$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/splitnose_stats.txt"))
print(save_performance_stats(splitnose_sims_short, splitnose_sims_long, splitnose$Recruit_0, splitnose$Yr, time_vec1, time_vec2))
sink()


# Yelloweye forecasts ###################################
## Set Up ------------------------------------------------------------------
yelloweye <- filter_sr_data(yelloweye)
yelloweye1 <- yelloweye %>% filter(Area == 1)

plot(yelloweye1$Yr, yelloweye1$Recruit_0, type = "l")
plot(yelloweye1$SpawnBio, yelloweye1$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- yelloweye1$Recruit_0
spawn_ts <- yelloweye1$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
yelloweye1_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- yelloweye1_sims_short[,,1]
ar_preds <- yelloweye1_sims_short[,,2]
bh_preds <- yelloweye1_sims_short[,,3]
simplex_preds <- yelloweye1_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/yelloweye1_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/yelloweye1_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/yelloweye1_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/yelloweye1_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
yelloweye1_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- yelloweye1_sims_long[,,1]
ar_preds_long <- yelloweye1_sims_long[,,2]
bh_preds_long <- yelloweye1_sims_long[,,3]
simplex_preds_long <- yelloweye1_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/yelloweye1_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/yelloweye1_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/yelloweye1_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/yelloweye1_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/yelloweye1_forecast_figs.pdf"))
print_plots(yelloweye1_sims_short, yelloweye1_sims_long, yelloweye1$Recruit_0, yelloweye1$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/yelloweye1_stats.txt"))
print(save_performance_stats(yelloweye1_sims_short, yelloweye1_sims_long, yelloweye1$Recruit_0, yelloweye1$Yr, time_vec1, time_vec2))
sink()



# Widow rockfish forecasts ###################################
## Set Up ------------------------------------------------------------------
widow <- filter_sr_data(widow)

plot(widow$Yr, widow$Recruit_0, type = "l")
plot(widow$SpawnBio, widow$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- widow$Recruit_0
spawn_ts <- widow$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


## Short-term forecasts ----------------------------------------------------
widow_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- widow_sims_short[,,1]
ar_preds <- widow_sims_short[,,2]
bh_preds <- widow_sims_short[,,3]
simplex_preds <- widow_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/simulation_results/west_coast/short_forecasts/widow_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/simulation_results/west_coast/short_forecasts/widow_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/simulation_results/west_coast/short_forecasts/widow_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/simulation_results/west_coast/short_forecasts/widow_1stp_simplex.csv"))

## Long-term forecasts ------------------------------------------------------------
widow_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- widow_sims_long[,,1]
ar_preds_long <- widow_sims_long[,,2]
bh_preds_long <- widow_sims_long[,,3]
simplex_preds_long <- widow_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/widow_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/widow_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/widow_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/simulation_results/west_coast/short_forecasts/widow_5stp_simplex.csv"))

## Visualize forecasts ----------------------------------------------------------
pdf(here("results/figures/west_coast_stocks/stock_forecast_figures/widow_forecast_figs.pdf"))
print_plots(widow_sims_short, widow_sims_long, widow$Recruit_0, widow$Yr, time_vec1, time_vec2)
dev.off()

## Save performance stats ------------------------------------------------------
sink(here("results/simulation_results/west_coast/performance_stats/widow_stats.txt"))
print(save_performance_stats(widow_sims_short, widow_sims_long, widow$Recruit_0, widow$Yr, time_vec1, time_vec2))
sink()
