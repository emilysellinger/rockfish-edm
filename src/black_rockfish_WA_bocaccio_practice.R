############# Black rockfish WA forecasts ######################################
# Set Up ------------------------------------------------------------------
black_wa <- filter_sr_data(black_wa)

plot(black_wa$Yr, black_wa$Recruit_0, type = "l")
plot(black_wa$SpawnBio, black_wa$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- black_wa$Recruit_0
spawn_ts <- black_wa$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


# Short-term forecasts ----------------------------------------------------
set.seed(1112)
black_wa_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)
#black_wa_sims_short2 <- expanding_window(fmethods = c("hmm"), 1000, time_vec1, rec_ts, spawn_ts)
# can't fit this

# extract forecasts
m_preds <- black_wa_sims_short[,,1]
ar_preds <- black_wa_sims_short[,,2]
bh_preds <- black_wa_sims_short[,,3]
simplex_preds <- black_wa_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/blackWA_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/blackWA_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/blackWA_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/blackWA_1stp_simplex.csv"))

# Visualize short-term forecasts ------------------------------------------
## Caluculate coverage proability -----------------------------------------
m_preds_ci <- sim_CI_prob(m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bh_preds, 0.95)
simplex_preds_ci <- sim_CI_prob(simplex_preds, 0.95)

### Plot coverage probability ---------------------------------------------
bayes_prob_df <- tibble(method = c("mean", "AR(1)", "beverton-holt", "simplex projection"),
                        coverage_prob = c(m_preds_ci, ar_preds_ci, bh_preds_ci, simplex_preds_ci))

ggplot(bayes_prob_df) + geom_point(aes(x = method, y = coverage_prob), size = 3) + 
  ylim(0, 1) + ylab("Coverage probability") + xlab("Recruitment forecast method")

## Calculate MRAE ----------------------------------------------------------------------
m_preds_mare <- sim_mare(m_preds, rec_ts, time_vec1)
ar_preds_mare <- sim_mare(ar_preds, rec_ts, time_vec1)
bh_preds_mare <- sim_mare(bh_preds, rec_ts, time_vec1)
simplex_preds_mare <- sim_mare(simplex_preds, rec_ts, time_vec1)

### Plot MRAE --------------------------------------------------------------------------
mare_df <- tibble(year = c(rep(seq(1, nrow(m_preds)), 4)),
                  method = c(rep("mean", nrow(m_preds)), rep("AR(1)", nrow(m_preds)), rep("beverton-holt", nrow(m_preds)), 
                            rep("simplex", nrow(m_preds))),
                  mrae = c(m_preds_mare, ar_preds_mare, bh_preds_mare, simplex_preds_mare))

ggplot(mare_df) + geom_line(aes(x = year, y = mrae, color = method)) + 
  ylab("Mean relative absolute error") + xlab("Years added to training set")


## Calculate simulation quantiles --------------------------------------------------
m_quants <- apply(m_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
ar_quants <- apply(ar_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
bh_quants <- apply(bh_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
simplex_quants <- apply(simplex_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))


### Plot simulation quantiles ------------------------------------------------------
m_df <- tibble(year = black_wa$Yr,
               obs = black_wa$Recruit_0,
               med_pred = c(rep(NA, 29), m_quants[2,]),
               low_ci = c(rep(NA, 29), m_quants[1,]),
               up_ci = c(rep(NA, 29), m_quants[3,]))

mean_plot <- ggplot(data = m_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(a) Mean")

ar_df <- tibble(year = black_wa$Yr,
                obs = black_wa$Recruit_0,
                med_pred = c(rep(NA, 29), ar_quants[2,]),
                low_ci = c(rep(NA, 29), ar_quants[1,]),
                up_ci = c(rep(NA, 29), ar_quants[3,]))

ar_plot <- ggplot(data = ar_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(b) AR(1)")

bh_df <- tibble(year = black_wa$Yr,
                obs = black_wa$Recruit_0,
                med_pred = c(rep(NA, 29), bh_quants[2,]),
                low_ci = c(rep(NA, 29), bh_quants[1,]),
                up_ci = c(rep(NA, 29), bh_quants[3,]))

bh_plot <- ggplot(data = bh_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(c) Beverton-Holt")


simplex_df <- tibble(year = black_wa$Yr,
                     obs = black_wa$Recruit_0,
                     med_pred = c(rep(NA, 29), simplex_quants[2,]),
                     low_ci = c(rep(NA, 29), simplex_quants[1,]),
                     up_ci = c(rep(NA, 29), simplex_quants[3,]))

simplex_plot <- ggplot(data = simplex_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(e) Simplex projection")

grid.arrange(mean_plot, ar_plot, bh_plot, simplex_plot, nrow = 2, ncol = 2)


# Long-term forecasts -----------------------------------------------------
black_wa_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- black_wa_sims_long[,,1]
ar_preds_long <- black_wa_sims_long[,,2]
bh_preds_long <- black_wa_sims_long[,,3]
simplex_preds_long <- black_wa_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/blackWA_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/blackWA_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/blackWA_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/blackWA_5stp_simplex.csv"))

# Bocaccio rockfish forecasts ###################################
# Set Up ------------------------------------------------------------------
bocaccio <- filter_sr_data(bocaccio)

plot(bocaccio$Yr, bocaccio$Recruit_0, type = "l")
plot(bocaccio$SpawnBio, bocaccio$Recruit_0)

# create recruitment/spawning biomass vectors
rec_ts <- bocaccio$Recruit_0
spawn_ts <- bocaccio$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts


# Short-term forecasts ----------------------------------------------------
bocaccio_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)
bocaccio_sims_short2 <- expanding_window(fmethods = c("hmm"), 1000, time_vec1, rec_ts, spawn_ts)
# can't fit this

# extract forecasts
m_preds <- bocaccio_sims_short[,,1]
ar_preds <- bocaccio_sims_short[,,2]
bh_preds <- bocaccio_sims_short[,,3]
simplex_preds <- bocaccio_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/bocaccio_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/bocaccio_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/bocaccio_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/bocaccio_1stp_simplex.csv"))

# Visualize short-term forecasts ------------------------------------------
## Caluculate coverage proability -----------------------------------------
m_preds_ci <- sim_CI_prob(m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bh_preds, 0.95)
simplex_preds_ci <- sim_CI_prob(simplex_preds, 0.95)

### Plot coverage probability ---------------------------------------------
bayes_prob_df <- tibble(method = c("mean", "AR(1)", "beverton-holt", "simplex projection"),
                        coverage_prob = c(m_preds_ci, ar_preds_ci, bh_preds_ci, simplex_preds_ci))

ggplot(bayes_prob_df) + geom_point(aes(x = method, y = coverage_prob), size = 3) + 
  ylim(0, 1) + ylab("Coverage probability") + xlab("Recruitment forecast method")

## Calculate MRAE ----------------------------------------------------------------------
m_preds_mare <- sim_mare(m_preds, rec_ts, time_vec1)
ar_preds_mare <- sim_mare(ar_preds, rec_ts, time_vec1)
bh_preds_mare <- sim_mare(bh_preds, rec_ts, time_vec1)
simplex_preds_mare <- sim_mare(simplex_preds, rec_ts, time_vec1)

### Plot MRAE --------------------------------------------------------------------------
mare_df <- tibble(year = c(rep(seq(1, nrow(m_preds)), 4)),
                  method = c(rep("mean", nrow(m_preds)), rep("AR(1)", nrow(m_preds)), rep("beverton-holt", nrow(m_preds)), 
                             rep("simplex", nrow(m_preds))),
                  mrae = c(m_preds_mare, ar_preds_mare, bh_preds_mare, simplex_preds_mare))

ggplot(mare_df) + geom_line(aes(x = year, y = mrae, color = method)) + 
  ylab("Mean relative absolute error") + xlab("Years added to training set")


## Calculate simulation quantiles --------------------------------------------------
m_quants <- apply(m_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
ar_quants <- apply(ar_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
bh_quants <- apply(bh_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
#hmm_quants <- apply(hmm_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
simplex_quants <- apply(simplex_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))


### Plot simulation quantiles ------------------------------------------------------
m_df <- tibble(year = bocaccio$Yr,
               obs = bocaccio$Recruit_0,
               med_pred = c(rep(NA, 29), m_quants[2,]),
               low_ci = c(rep(NA, 29), m_quants[1,]),
               up_ci = c(rep(NA, 29), m_quants[3,]))

mean_plot <- ggplot(data = m_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(a) Mean")

ar_df <- tibble(year = bocaccio$Yr,
                obs = bocaccio$Recruit_0,
                med_pred = c(rep(NA, 29), ar_quants[2,]),
                low_ci = c(rep(NA, 29), ar_quants[1,]),
                up_ci = c(rep(NA, 29), ar_quants[3,]))

ar_plot <- ggplot(data = ar_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(b) AR(1)")

bh_df <- tibble(year = bocaccio$Yr,
                obs = bocaccio$Recruit_0,
                med_pred = c(rep(NA, 29), bh_quants[2,]),
                low_ci = c(rep(NA, 29), bh_quants[1,]),
                up_ci = c(rep(NA, 29), bh_quants[3,]))

bh_plot <- ggplot(data = bh_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(c) Beverton-Holt")

# hmm_df <- tibble(year = aurora$Yr,
#                  obs = aurora$Recruit_0,
#                  med_pred = c(rep(NA, 29), hmm_quants[2,]),
#                  low_ci = c(rep(NA, 29), hmm_quants[1,]),
#                  up_ci = c(rep(NA, 29), hmm_quants[3,]))
# 
# hmm_plot <- ggplot(data = hmm_df) + geom_line(aes(x = year, y = obs)) +
#   geom_line(aes(x = year, y = med_pred), color = "blue",) +
#   geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
#   xlab("Year") + ylab("Recruitment") + labs(subtitle = "(d) HMM sampling")

simplex_df <- tibble(year = bocaccio$Yr,
                     obs = bocaccio$Recruit_0,
                     med_pred = c(rep(NA, 29), simplex_quants[2,]),
                     low_ci = c(rep(NA, 29), simplex_quants[1,]),
                     up_ci = c(rep(NA, 29), simplex_quants[3,]))

simplex_plot <- ggplot(data = simplex_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(e) Simplex projection")

grid.arrange(mean_plot, ar_plot, bh_plot, simplex_plot, nrow = 2, ncol = 2)


# Long-term forecasts -----------------------------------------------------
bocaccio_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- bocaccio_sims_long[,,1]
ar_preds_long <- bocaccio_sims_long[,,2]
bh_preds_long <- bocaccio_sims_long[,,3]
simplex_preds_long <- bocaccio_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/bocaccio_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/bocaccio_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/bocaccio_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/bocaccio_5stp_simplex.csv"))

# Cabezon NCS forecasts ##########################################################################
cabezon_ncs <- filter_sr_data(cabezon_ncs)

plot(cabezon_ncs$Yr, cabezon_ncs$Recruit_0, type = "l")
plot(cabezon_ncs$SpawnBio, cabezon_ncs$Recruit_0)

rec_ts <- cabezon_ncs$Recruit_0
spawn_ts <- cabezon_ncs$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts

# Short-term forecasts ----------------------------------------------------
cabezon_ncs_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- cabezon_ncs_sims_short[,,1]
ar_preds <- cabezon_ncs_sims_short[,,2]
bh_preds <- cabezon_ncs_sims_short[,,3]
simplex_preds <- cabezon_ncs_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/cabezon_ncs_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/cabezon_ncs_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/cabezon_ncs_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/cabezon_ncs_1stp_simplex.csv"))

# Visualize short-term forecasts ------------------------------------------
## Caluculate coverage proability -----------------------------------------
m_preds_ci <- sim_CI_prob(m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bh_preds, 0.95)
simplex_preds_ci <- sim_CI_prob(simplex_preds, 0.95)

# Long-term forecasts ----------------------------------------------------
cabezon_ncs_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- cabezon_ncs_sims_long[,,1]
ar_preds_long <- cabezon_ncs_sims_long[,,2]
bh_preds_long <- cabezon_ncs_sims_long[,,3]
simplex_preds_long <- cabezon_ncs_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/cabezon_ncs_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/cabezon_ncs_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/cabezon_ncs_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/cabezon_ncs_5stp_simplex.csv"))



# Cabezon OCS forecasts ##########################################################################
cabezon_ocs <- filter_sr_data(cabezon_ocs)

plot(cabezon_ocs$Yr, cabezon_ocs$Recruit_0, type = "l")
plot(cabezon_ocs$SpawnBio, cabezon_ocs$Recruit_0)

rec_ts <- cabezon_ocs$Recruit_0
spawn_ts <- cabezon_ocs$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts

# Short-term forecasts ----------------------------------------------------
cabezon_ocs_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- cabezon_ocs_sims_short[,,1]
ar_preds <- cabezon_ocs_sims_short[,,2]
bh_preds <- cabezon_ocs_sims_short[,,3]
simplex_preds <- cabezon_ocs_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/cabezon_ocs_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/cabezon_ocs_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/cabezon_ocs_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/cabezon_ocs_1stp_simplex.csv"))

# Visualize short-term forecasts ------------------------------------------
## Caluculate coverage proability -----------------------------------------
m_preds_ci <- sim_CI_prob(m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bh_preds, 0.95)
simplex_preds_ci <- sim_CI_prob(simplex_preds, 0.95)

# Long-term forecasts ----------------------------------------------------
cabezon_ocs_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- cabezon_ocs_sims_long[,,1]
ar_preds_long <- cabezon_ocs_sims_long[,,2]
bh_preds_long <- cabezon_ocs_sims_long[,,3]
simplex_preds_long <- cabezon_ocs_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/cabezon_ocs_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/cabezon_ocs_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/cabezon_ocs_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/cabezon_ocs_5stp_simplex.csv"))



# Cabezon SCS forecasts ##########################################################################
cabezon_scs <- filter_sr_data(cabezon_scs)

plot(cabezon_scs$Yr, cabezon_scs$Recruit_0, type = "l")
plot(cabezon_scs$SpawnBio, cabezon_scs$Recruit_0)

rec_ts <- cabezon_scs$Recruit_0
spawn_ts <- cabezon_scs$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts

# Short-term forecasts ----------------------------------------------------
cabezon_scs_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- cabezon_scs_sims_short[,,1]
ar_preds <- cabezon_scs_sims_short[,,2]
bh_preds <- cabezon_scs_sims_short[,,3]
simplex_preds <- cabezon_scs_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/cabezon_scs_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/cabezon_scs_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/cabezon_scs_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/cabezon_scs_1stp_simplex.csv"))

# Visualize short-term forecasts ------------------------------------------
## Caluculate coverage proability -----------------------------------------
m_preds_ci <- sim_CI_prob(m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bh_preds, 0.95)
simplex_preds_ci <- sim_CI_prob(simplex_preds, 0.95)

# Long-term forecasts ----------------------------------------------------
cabezon_scs_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- cabezon_scs_sims_long[,,1]
ar_preds_long <- cabezon_scs_sims_long[,,2]
bh_preds_long <- cabezon_scs_sims_long[,,3]
simplex_preds_long <- cabezon_scs_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/cabezon_scs_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/cabezon_scs_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/cabezon_scs_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/cabezon_scs_5stp_simplex.csv"))


# Canary rockfish forecasts ##########################################################################
canary <- filter_sr_data(canary)
canary1 <- canary %>% filter(Area == 1)
canary2 <- canary %>% filter(Area == 2)
canary3 <- canary %>% filter(Area == 3)

# going to do these forecasts for Area 1 first
plot(canary1$Yr, canary1$Recruit_0, type = "l")
plot(canary1$SpawnBio, canary1$Recruit_0)

rec_ts <- canary1$Recruit_0
spawn_ts <- canary1$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts

# Short-term forecasts ----------------------------------------------------
canary1_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- canary1_sims_short[,,1]
ar_preds <- canary1_sims_short[,,2]
bh_preds <- canary1_sims_short[,,3]
simplex_preds <- canary1_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/canary1_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/canary1_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/canary1_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/canary1_1stp_simplex.csv"))

# Visualize short-term forecasts ------------------------------------------
## Caluculate coverage proability -----------------------------------------
m_preds_ci <- sim_CI_prob(m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bh_preds, 0.95)
simplex_preds_ci <- sim_CI_prob(simplex_preds, 0.95)

# Long-term forecasts ----------------------------------------------------
canary1_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- canary1_sims_long[,,1]
ar_preds_long <- canary1_sims_long[,,2]
bh_preds_long <- canary1_sims_long[,,3]
simplex_preds_long <- canary1_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/canary1_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/canary1_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/canary1_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/canary1_5stp_simplex.csv"))

# Chilipepper rockfish forecasts ##########################################################################
chilipepper <- filter_sr_data(chilipepper)

# going to do these forecasts for Area 1 first
plot(chilipepper$Yr, chilipepper$Recruit_0, type = "l")
plot(chilipepper$SpawnBio, chilipepper$Recruit_0)

rec_ts <- chilipepper$Recruit_0
spawn_ts <- chilipepper$SpawnBio
# create time vectors
time_vec1 <- seq(30, length(rec_ts), 1) # 1-step ahead forecasts
time_vec2 <- seq(30, (length(rec_ts) - 4), 1) # 5 step forecasts

# Short-term forecasts ----------------------------------------------------
chilipepper_sims_short <- expanding_window(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, rec_ts, spawn_ts)

# extract forecasts
m_preds <- chilipepper_sims_short[,,1]
ar_preds <- chilipepper_sims_short[,,2]
bh_preds <- chilipepper_sims_short[,,3]
simplex_preds <- chilipepper_sims_short[,,4]

# save to csv
write_csv(as.data.frame(m_preds), file = here("results/chilipepper_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/chilipepper_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/chilipepper_1stp_bh.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/chilipepper_1stp_simplex.csv"))

# Visualize short-term forecasts ------------------------------------------
## Caluculate coverage proability -----------------------------------------
m_preds_ci <- sim_CI_prob(m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bh_preds, 0.95)
simplex_preds_ci <- sim_CI_prob(simplex_preds, 0.95)

# Long-term forecasts ----------------------------------------------------
chilipepper_sims_long <- expanding_window_5yr(fmethods = c("m", "ar", "bh", "simplex"), 1000, time_vec1, time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- chilipepper_sims_long[,,1]
ar_preds_long <- chilipepper_sims_long[,,2]
bh_preds_long <- chilipepper_sims_long[,,3]
simplex_preds_long <- chilipepper_sims_long[,,4]

# save to csv
write_csv(as.data.frame(m_preds_long), file = here("results/chilipepper_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/chilipepper_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/chilipepper_5stp_bh.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/chilipepper_5stp_simplex.csv"))