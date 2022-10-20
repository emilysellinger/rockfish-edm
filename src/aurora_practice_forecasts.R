
# Aurora 1-step forecasts -------------------------------------------------
aurora <- filter_sr_data(aurora)

plot(aurora$Yr, aurora$Recruit_0, type = "l")
plot(aurora$Yr, aurora$SpawnBio, type = "l")

rec_ts <- aurora$Recruit_0
spawn_ts <- aurora$SpawnBio

time_vec <- seq(30, 50, 1)

# 1 step ahead sims
set.seed(112)
aurora_sims <- expanding_window(fmethods = c("m", "ar", "bh", "hmm", "simplex"), 1000, time_vec, rec_ts, spawn_ts)

# extract forecasts
m_preds <- aurora_sims[,,1]
ar_preds <- aurora_sims[,,2]
bh_preds <- aurora_sims[,,3]
hmm_preds <- aurora_sims[,,4]
simplex_preds <- aurora_sims[,,5]

# Caluculate performance metrics
m_preds_ci <- sim_CI_prob(m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bh_preds, 0.95)
hmm_preds_ci <- sim_CI_prob(hmm_preds, 0.95)
simplex_preds_ci <- sim_CI_prob(simplex_preds, 0.95)

# plot bayesian coverage probability
bayes_prob_df <- tibble(method = c("mean", "AR(1)", "beverton-holt", "HMM sampling", "simplex projection"),
                        coverage_prob = c(m_preds_ci, ar_preds_ci, bh_preds_ci, hmm_preds_ci, simplex_preds_ci))

ggplot(bayes_prob_df) + geom_point(aes(x = method, y = coverage_prob), size = 3) + 
  ylim(0, 1) + ylab("Coverage probability") + xlab("Recruitment forecast method")


m_preds_mare <- sim_mare(m_preds, rec_ts, time_vec)
ar_preds_mare <- sim_mare(ar_preds, rec_ts, time_vec)
bh_preds_mare <- sim_mare(bh_preds, rec_ts, time_vec)
hmm_preds_mare <- sim_mare(hmm_preds, rec_ts, time_vec)
simplex_preds_mare <- sim_mare(simplex_preds, rec_ts, time_vec)

mare_df <- tibble(year = c(rep(seq(1, nrow(m_preds)), 5)),
                  method = c(rep("mean", nrow(m_preds)), rep("AR(1)", nrow(m_preds)), rep("beverton-holt", nrow(m_preds)), 
                             rep("HMM sampling", nrow(m_preds)), rep("simplex", nrow(m_preds))),
                  mrae = c(m_preds_mare, ar_preds_mare, bh_preds_mare, hmm_preds_mare, simplex_preds_mare))

ggplot(mare_df) + geom_line(aes(x = year, y = mrae, color = method)) + 
  ylab("Mean relative absolute error") + xlab("Years added to training set")


# will plot simulation quantiles

m_quants <- apply(m_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
ar_quants <- apply(ar_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
bh_quants <- apply(bh_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
hmm_quants <- apply(hmm_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
simplex_quants <- apply(simplex_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))


m_df <- tibble(year = aurora$Yr,
               obs = aurora$Recruit_0,
               med_pred = c(rep(NA, 29), m_quants[2,]),
               low_ci = c(rep(NA, 29), m_quants[1,]),
               up_ci = c(rep(NA, 29), m_quants[3,]))

mean_plot <- ggplot(data = m_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(a) Mean")

ar_df <- tibble(year = aurora$Yr,
               obs = aurora$Recruit_0,
               med_pred = c(rep(NA, 29), ar_quants[2,]),
               low_ci = c(rep(NA, 29), ar_quants[1,]),
               up_ci = c(rep(NA, 29), ar_quants[3,]))

ar_plot <- ggplot(data = ar_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(b) AR(1)")

bh_df <- tibble(year = aurora$Yr,
               obs = aurora$Recruit_0,
               med_pred = c(rep(NA, 29), bh_quants[2,]),
               low_ci = c(rep(NA, 29), bh_quants[1,]),
               up_ci = c(rep(NA, 29), bh_quants[3,]))

bh_plot <- ggplot(data = bh_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(c) Beverton-Holt")

hmm_df <- tibble(year = aurora$Yr,
               obs = aurora$Recruit_0,
               med_pred = c(rep(NA, 29), hmm_quants[2,]),
               low_ci = c(rep(NA, 29), hmm_quants[1,]),
               up_ci = c(rep(NA, 29), hmm_quants[3,]))

hmm_plot <- ggplot(data = hmm_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(d) HMM sampling")

simplex_df <- tibble(year = aurora$Yr,
               obs = aurora$Recruit_0,
               med_pred = c(rep(NA, 29), simplex_quants[2,]),
               low_ci = c(rep(NA, 29), simplex_quants[1,]),
               up_ci = c(rep(NA, 29), simplex_quants[3,]))

simplex_plot <- ggplot(data = simplex_df) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(e) Simplex projection")

grid.arrange(mean_plot, ar_plot, bh_plot, hmm_plot, simplex_plot, nrow = 3, ncol = 2)


# Save 1 step data --------------------------------------------------------
m_preds1 <- as.data.frame(m_preds)
write_csv(m_preds1, file = here("results/aurora_1stp_mean.csv"))
write_csv(as.data.frame(ar_preds), file = here("results/aurora_1stp_ar.csv"))
write_csv(as.data.frame(bh_preds), file = here("results/aurora_1stp_bh.csv"))
write_csv(as.data.frame(hmm_preds), file = here("results/aurora_1stp_hmm.csv"))
write_csv(as.data.frame(simplex_preds), file = here("results/aurora_1stp_simplex.csv"))


# Long-term forecast practice ---------------------------------------------
time_vec2 <- seq(30, (50-4), 1)

long_sims <- expanding_window_5yr(c("m", "ar", "bh", "hmm", "simplex"), 1000, time_vec = time_vec, time_vec2 = time_vec2, rec_ts, spawn_ts)

# extract forecasts
m_preds_long <- long_sims[,,1]
ar_preds_long <- long_sims[,,2]
bh_preds_long <- long_sims[,,3]
hmm_preds_long <- long_sims[,,4]
simplex_preds_long <- long_sims[,,5]

# Caluculate performance metrics
m_preds_ci2 <- sim_CI_prob(m_preds_long, 0.95)
ar_preds_ci2 <- sim_CI_prob(ar_preds_long, 0.95)
bh_preds_ci2 <- sim_CI_prob(bh_preds_long, 0.95)
hmm_preds_ci2 <- sim_CI_prob(hmm_preds_long, 0.95)
simplex_preds_ci2 <- sim_CI_prob(simplex_preds_long, 0.95)

# plot bayesian coverage probability
bayes_prob_df2 <- tibble(method = c("mean", "AR(1)", "beverton-holt", "HMM sampling", "simplex projection"),
                        coverage_prob = c(m_preds_ci2, ar_preds_ci2, bh_preds_ci2, hmm_preds_ci2, simplex_preds_ci2))

ggplot(bayes_prob_df2) + geom_point(aes(x = method, y = coverage_prob), size = 3) + 
  ylim(0, 1) + ylab("Coverage probability") + xlab("Recruitment forecast method")

# will plot simulation quantiles

m_quants2 <- apply(m_preds_long[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
ar_quants2 <- apply(ar_preds_long[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
bh_quants2 <- apply(bh_preds_long[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
hmm_quants2 <- apply(hmm_preds_long[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
simplex_quants2 <- apply(simplex_preds_long[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))


m_df2 <- tibble(year = aurora$Yr,
               obs = aurora$Recruit_0,
               med_pred = c(rep(NA, 29), m_quants2[2,]),
               low_ci = c(rep(NA, 29), m_quants2[1,]),
               up_ci = c(rep(NA, 29), m_quants2[3,]))

mean_plot2 <- ggplot(data = m_df2) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(a) Mean")

ar_df2 <- tibble(year = aurora$Yr,
                obs = aurora$Recruit_0,
                med_pred = c(rep(NA, 29), ar_quants2[2,]),
                low_ci = c(rep(NA, 29), ar_quants2[1,]),
                up_ci = c(rep(NA, 29), ar_quants2[3,]))

ar_plot2 <- ggplot(data = ar_df2) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(b) AR(1)")

bh_df2 <- tibble(year = aurora$Yr,
                obs = aurora$Recruit_0,
                med_pred = c(rep(NA, 29), bh_quants2[2,]),
                low_ci = c(rep(NA, 29), bh_quants2[1,]),
                up_ci = c(rep(NA, 29), bh_quants2[3,]))

bh_plot2 <- ggplot(data = bh_df2) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(c) Beverton-Holt")

hmm_df2 <- tibble(year = aurora$Yr,
                 obs = aurora$Recruit_0,
                 med_pred = c(rep(NA, 29), hmm_quants2[2,]),
                 low_ci = c(rep(NA, 29), hmm_quants2[1,]),
                 up_ci = c(rep(NA, 29), hmm_quants2[3,]))

hmm_plot2 <- ggplot(data = hmm_df2) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(d) HMM sampling")

simplex_df2 <- tibble(year = aurora$Yr,
                     obs = aurora$Recruit_0,
                     med_pred = c(rep(NA, 29), simplex_quants2[2,]),
                     low_ci = c(rep(NA, 29), simplex_quants2[1,]),
                     up_ci = c(rep(NA, 29), simplex_quants2[3,]))

simplex_plot2 <- ggplot(data = simplex_df2) + geom_line(aes(x = year, y = obs)) +
  geom_line(aes(x = year, y = med_pred), color = "blue",) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(e) Simplex projection")

grid.arrange(mean_plot, ar_plot, bh_plot, hmm_plot, simplex_plot, nrow = 3, ncol = 2)

# plot 5 yr trend data
# going to calculate the actual 5 year rolling average data for the aurora time series
aurora_rec_trend <- rollmean(rec_ts[30:50], 5)


m_trend <- sim_5yr_trend(m_preds_long, time_vec2)
ar_trend <- sim_5yr_trend(ar_preds_long, time_vec2)
bh_trend <- sim_5yr_trend(bh_preds_long, time_vec2)
hmm_trend <- sim_5yr_trend(hmm_preds_long, time_vec2)
simplex_trend <- sim_5yr_trend(simplex_preds_long, time_vec2)

m_preds_trend <- tibble(real = aurora_rec_trend,
                        med = m_trend[2,],
                        low_ci = m_trend[1,],
                        up_ci = m_trend[3,])

mean_trend_plot <- ggplot(m_preds_trend) + geom_line(aes(x = seq(1, nrow(m_preds_trend)), y = real))+
  geom_line(aes(x = seq(1, nrow(m_preds_trend)), y = med), color = "blue") + 
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = seq(1, nrow(m_preds_trend))), fill = "blue", alpha = 0.1) +
  labs(y = "Five-year rolling average", subtitle = "(a) Mean", x = "Years added to training set")


ar_preds_trend <- tibble(real = aurora_rec_trend,
                         med = ar_trend[2,],
                         low_ci = ar_trend[1,],
                         up_ci = ar_trend[3,])

ar_trend_plot <- ggplot(ar_preds_trend) + geom_line(aes(x = seq(1, nrow(ar_preds_trend)), y = real)) +
  geom_line(aes(x = seq(1, nrow(ar_preds_trend)), y = med), color = "blue") + 
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = seq(1, nrow(ar_preds_trend))), fill = "blue", alpha = 0.1) +
  labs(y = "Five-year rolling average", subtitle = "(b) AR(1)", x = "Years added to training set")

bh_preds_trend <- tibble(real = aurora_rec_trend,
                         med = bh_trend[2,],
                        low_ci = bh_trend[1,],
                        up_ci = bh_trend[3,])

bh_trend_plot <- ggplot(bh_preds_trend) + geom_line(aes(x = seq(1, nrow(bh_preds_trend)), y = real)) +
  geom_line(aes(x = seq(1, nrow(bh_preds_trend)), y = med), color = "blue") + 
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = seq(1, nrow(bh_preds_trend))), fill = "blue", alpha = 0.1) +
  labs(y = "Five-year rolling average", subtitle = "(c) Beverton-Holt", x = "Years added to training set")

hmm_preds_trend <- tibble(real = aurora_rec_trend,
                         med = hmm_trend[2,],
                         low_ci = hmm_trend[1,],
                         up_ci = hmm_trend[3,])

hmm_trend_plot <- ggplot(hmm_preds_trend) + geom_line(aes(x = seq(1, nrow(hmm_preds_trend)), y = real)) +
  geom_line(aes(x = seq(1, nrow(hmm_preds_trend)), y = med), color = "blue") + 
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = seq(1, nrow(hmm_preds_trend))), fill = "blue", alpha = 0.1) +
  labs(y = "Five-year rolling average", subtitle = "(d) HMM sampling", x = "Years added to training set")

simplex_preds_trend <- tibble(real = aurora_rec_trend,
                         med = simplex_trend[2,],
                         low_ci = simplex_trend[1,],
                         up_ci = simplex_trend[3,])

simplex_trend_plot <- ggplot(simplex_preds_trend) + geom_line(aes(x = seq(1, nrow(simplex_preds_trend)), y = real)) +
  geom_line(aes(x = seq(1, nrow(simplex_preds_trend)), y = med), color = "blue") + 
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = seq(1, nrow(simplex_preds_trend))), fill = "blue", alpha = 0.1, linetype = "dashed") +
  labs(y = "Five-year rolling average", subtitle = "(e) Simplex projection", x = "Years added to training set")



grid.arrange(mean_trend_plot, ar_trend_plot, bh_trend_plot, hmm_trend_plot, simplex_trend_plot, nrow = 3, ncol = 2)


# Save sim data -----------------------------------------------------------
write_csv(as.data.frame(m_preds_long), file = here("results/aurora_5stp_mean.csv"))
write_csv(as.data.frame(ar_preds_long), file = here("results/aurora_5stp_ar.csv"))
write_csv(as.data.frame(bh_preds_long), file = here("results/aurora_5stp_bh.csv"))
write_csv(as.data.frame(hmm_preds_long), file = here("results/aurora_5stp_hmm.csv"))
write_csv(as.data.frame(simplex_preds_long), file = here("results/aurora_5stp_simplex.csv"))


# Load Simulation Data ----------------------------------------------------
m_preds <- as.matrix(read_csv(here("results", "aurora_1stp_mean.csv")))
ar_preds <- as.matrix(read_csv(here("results", "aurora_1stp_ar.csv")))
bh_preds <- as.matrix(read_csv(here("results", "aurora_1stp_bh.csv")))
hmm_preds <- as.matrix(read_csv(here("results", "aurora_1stp_hmm.csv")))
simplex_preds <- as.matrix(read_csv(here("results", "aurora_1stp_simplex.csv")))

m_preds_long <- as.matrix(read_csv(here("results", "aurora_5stp_mean.csv")))
ar_preds_long <- as.matrix(read_csv(here("results", "aurora_5stp_ar.csv")))
bh_preds_long <- as.matrix(read_csv(here("results", "aurora_5stp_bh.csv")))
hmm_preds_long <- as.matrix(read_csv(here("results", "aurora_5stp_hmm.csv")))
simplex_preds_long <- as.matrix(read_csv(here("results", "aurora_5stp_simplex.csv")))
