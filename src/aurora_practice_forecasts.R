# aurora forecast tests
aurora <- filter_sr_data(aurora)

plot(aurora$Yr, aurora$Recruit_0, type = "l")
plot(aurora$Yr, aurora$SpawnBio, type = "l")

rec_ts <- aurora$Recruit_0
spawn_ts <- aurora$SpawnBio

time_vec <- seq(30, 50, 1)
aurora_sims <- expanding_window(fmethods = c("m", "ar", "bh", "hmm", "simplex"), 100, time_vec, rec_ts, spawn_ts)

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


# Long-term forecast practice ---------------------------------------------

a <- lrec_BH(time_vec[1], rec_ts, spawn_ts)
time_vec2 <- seq(30, (50-4), 1)



long_sims <- expanding_window_5yr(c("bh"), 10, time_vec = time_vec, time_vec2 = time_vec2, rec_ts, spawn_ts)
bh_long_sims <- long_sims[,,1]

bh_preds_ci <- sim_CI_prob(bh_long_sims, 0.95)

bh_5yr_trend <- matrix(NA, nrow = length(time_vec2), ncol = (dim(bh_long_sims)[2] - 1))

for(i in 1:length(time_vec2)){
  for(j in 1:ncol(bh_5yr_trend)){
    df <- tibble(yr = seq(1,5),
                 preds = bh_long_sims[i:(i+4), j+1])
    
    rec_lm <- lm(preds ~ yr, data = df)
    
    bh_5yr_trend[i,j] <- unname(rec_lm$coefficients[2])
  }
}
