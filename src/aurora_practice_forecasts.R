# aurora forecast tests
aurora <- filter_sr_data(aurora)

plot(aurora$Yr, aurora$Recruit_0, type = "l")
plot(aurora$Yr, aurora$SpawnBio, type = "l")

rec_ts <- aurora$Recruit_0
spawn_ts <- aurora$SpawnBio

time_vec <- seq(40, 50, 1)
aurora_sims <- expanding_window(fmethods = c("m", "ar", "bh", "hmm"), 1000, time_vec, rec_ts, spawn_ts)

# extract forecasts
m_preds <- aurora_sims[,,1]
ar_preds <- aurora_sims[,,2]
bh_preds <- aurora_sims[,,3]
hmm_preds <- aurora_sims[,,4]

# Caluculate performance metrics
m_preds_ci <- sim_CI_prob(m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bh_preds, 0.95)
hmm_preds_ci <- sim_CI_prob(hmm_preds, 0.95)

m_preds_mare <- sim_mare(m_preds, rec_ts, time_vec)
ar_preds_mare <- sim_mare(ar_preds, rec_ts, time_vec)
bh_preds_mare <- sim_mare(bh_preds, rec_ts, time_vec)
hmm_preds_mare <- sim_mare(hmm_preds, rec_ts, time_vec)
