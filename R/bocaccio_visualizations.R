# Bocaccio rockfish
bocaccio_m_preds <- as.matrix(read_csv(here("results", "bocaccio_1stp_mean.csv")))
bocaccio_ar_preds <- as.matrix(read_csv(here("results", "bocaccio_1stp_ar.csv")))
bocaccio_bh_preds <- as.matrix(read_csv(here("results", "bocaccio_1stp_bh.csv")))
bocaccio_simplex_preds <- as.matrix(read_csv(here("results", "bocaccio_1stp_simplex.csv")))

# Bocaccio rockfish
bocaccio_m_preds_long <- as.matrix(read_csv(here("results", "bocaccio_5stp_mean.csv")))
bocaccio_ar_preds_long <- as.matrix(read_csv(here("results", "bocaccio_5stp_ar.csv")))
bocaccio_bh_preds_long <- as.matrix(read_csv(here("results", "bocaccio_5stp_bh.csv")))
bocaccio_simplex_preds_long <- as.matrix(read_csv(here("results", "bocaccio_5stp_simplex.csv")))

# Short-term forecasts ----------------------------------------------------
## Caluculate coverage proability -----------------------------------------
m_preds_ci <- sim_CI_prob(bocaccio_m_preds, 0.95)
ar_preds_ci <- sim_CI_prob(bocaccio_ar_preds, 0.95)
bh_preds_ci <- sim_CI_prob(bocaccio_bh_preds, 0.95)
#hmm_preds_ci <- sim_CI_prob(bocaccio_hmm_preds, 0.95)
simplex_preds_ci <- sim_CI_prob(bocaccio_simplex_preds, 0.95)

### Plot coverage probability ---------------------------------------------
bayes_prob_df <- tibble(method = c("mean", "AR(1)", "beverton-holt", "simplex projection"),
                        coverage_prob = c(m_preds_ci, ar_preds_ci, bh_preds_ci, simplex_preds_ci))
caption <- str_wrap(c("Probability the observed bocaccio recruitment fell within the 95% confidence intervals for the forecasted years."), 150)

pdf(here("results/figures/bocaccio_1stp_cov_prob.pdf"))
print(ggplot(bayes_prob_df) + geom_point(aes(x = method, y = coverage_prob), size = 3) + 
  ylim(0, 1) + ylab("Coverage probability") + xlab("Recruitment forecast method") +
  labs(caption = caption, title = "1 step forecast coverage probability") + 
  theme(plot.caption = element_text(hjust = 0)))
dev.off()

## Calculate MRAE ----------------------------------------------------------------------
m_preds_mare <- sim_mare(bocaccio_m_preds, rec_ts, time_vec1)
ar_preds_mare <- sim_mare(bocaccio_ar_preds, rec_ts, time_vec1)
bh_preds_mare <- sim_mare(bocaccio_bh_preds, rec_ts, time_vec1)
simplex_preds_mare <- sim_mare(bocaccio_simplex_preds, rec_ts, time_vec1)

### Plot MRAE --------------------------------------------------------------------------
mare_df <- tibble(year = c(rep(seq(1, nrow(bocaccio_m_preds)), 4)),
                  method = c(rep("mean", nrow(bocaccio_m_preds)), rep("AR(1)", nrow(bocaccio_m_preds)), rep("beverton-holt", nrow(bocaccio_m_preds)), 
                             rep("simplex", nrow(bocaccio_m_preds))),
                  mrae = c(m_preds_mare, ar_preds_mare, bh_preds_mare, simplex_preds_mare))
cation <- str_wrap(c("Mean relative absolute error for each year added to the training set by forecast method."), 100)

pdf(here("results/figures/bocaccio_1stp_MRAE.pdf"))
print(ggplot(mare_df) + geom_line(aes(x = year, y = mrae, color = method)) + 
  ylab("Mean relative absolute error") + xlab("Years added to training set") +
  labs(caption = caption, title = "1 step forecast MRAE"))
dev.off()


## Calculate simulation quantiles --------------------------------------------------
m_quants <- apply(bocaccio_m_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
ar_quants <- apply(bocaccio_ar_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
bh_quants <- apply(bocaccio_bh_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
simplex_quants <- apply(bocaccio_simplex_preds[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))


### Plot simulation quantiles ------------------------------------------------------
m_df <- tibble(year = bocaccio$Yr,
               obs = bocaccio$Recruit_0,
               med_pred = c(rep(NA, 29), m_quants[2,]),
               low_ci = c(rep(NA, 29), m_quants[1,]),
               up_ci = c(rep(NA, 29), m_quants[3,]))

mean_plot <- ggplot(data = m_df) + geom_line(aes(x = year, y = obs)) +
  geom_point(aes(x = year, y = med_pred), color = "blue", size = 2) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(a) Mean")

ar_df <- tibble(year = bocaccio$Yr,
                obs = bocaccio$Recruit_0,
                med_pred = c(rep(NA, 29), ar_quants[2,]),
                low_ci = c(rep(NA, 29), ar_quants[1,]),
                up_ci = c(rep(NA, 29), ar_quants[3,]))

ar_plot <- ggplot(data = ar_df) + geom_line(aes(x = year, y = obs)) +
  geom_point(aes(x = year, y = med_pred), color = "blue",size = 2) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(b) AR(1)")

bh_df <- tibble(year = bocaccio$Yr,
                obs = bocaccio$Recruit_0,
                med_pred = c(rep(NA, 29), bh_quants[2,]),
                low_ci = c(rep(NA, 29), bh_quants[1,]),
                up_ci = c(rep(NA, 29), bh_quants[3,]))

bh_plot <- ggplot(data = bh_df) + geom_line(aes(x = year, y = obs)) +
  geom_point(aes(x = year, y = med_pred), color = "blue", size = 2) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(c) Beverton-Holt")

simplex_df <- tibble(year = bocaccio$Yr,
                     obs = bocaccio$Recruit_0,
                     med_pred = c(rep(NA, 29), simplex_quants[2,]),
                     low_ci = c(rep(NA, 29), simplex_quants[1,]),
                     up_ci = c(rep(NA, 29), simplex_quants[3,]))

simplex_plot <- ggplot(data = simplex_df) + geom_line(aes(x = year, y = obs)) +
  geom_point(aes(x = year, y = med_pred), color = "blue", size = 2) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(d) Simplex projection")

pdf(here("results/figures/bocaccio_1stp_forecasts.pdf"))
grid.arrange(mean_plot, ar_plot, bh_plot, simplex_plot, bottom = textGrob("1-step expanding window forecasts for bocaccio. Black line represents the observed recruitment. Blue \npoints are the median recruitment value for 1000 simulations. Shaded blue region represents the 95% confidence intervals \nfor the simulations.", 
                                                                          x = 0, just = "left", gp = gpar(fontsize = 10)), nrow = 2, ncol = 2)
dev.off()

# Long-term forecasts ----------------------------------------------------
## Caluculate coverage proability -----------------------------------------
m_preds_ci <- sim_CI_prob(bocaccio_m_preds_long, 0.95)
ar_preds_ci <- sim_CI_prob(bocaccio_ar_preds_long, 0.95)
bh_preds_ci <- sim_CI_prob(bocaccio_bh_preds_long, 0.95)
simplex_preds_ci <- sim_CI_prob(bocaccio_simplex_preds_long, 0.95)

### Plot coverage probability ---------------------------------------------
bayes_prob_df <- tibble(method = c("mean", "AR(1)", "beverton-holt", "simplex projection"),
                        coverage_prob = c(m_preds_ci, ar_preds_ci, bh_preds_ci, simplex_preds_ci))
caption <- str_wrap(c("Probability the observed bocaccio recruitment fell within the 95% confidence intervals for the forecasted years."), 150)

pdf(here("results/figures/bocaccio_5stp_cov_prob.pdf"))
print(ggplot(bayes_prob_df) + geom_point(aes(x = method, y = coverage_prob), size = 3) + 
        ylim(0, 1) + ylab("Coverage probability") + xlab("Recruitment forecast method") +
        labs(caption = caption, title = "1 step forecast coverage probability") + 
        theme(plot.caption = element_text(hjust = 0)))
dev.off()

## Calculate simulation quantiles --------------------------------------------------
m_quants <- apply(bocaccio_m_preds_long[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
ar_quants <- apply(bocaccio_ar_preds_long[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
bh_quants <- apply(bocaccio_bh_preds_long[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
simplex_quants <- apply(bocaccio_simplex_preds_long[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))


### Plot simulation quantiles ------------------------------------------------------
m_df <- tibble(year = bocaccio$Yr,
               obs = bocaccio$Recruit_0,
               med_pred = c(rep(NA, 29), m_quants[2,]),
               low_ci = c(rep(NA, 29), m_quants[1,]),
               up_ci = c(rep(NA, 29), m_quants[3,]))

mean_plot <- ggplot(data = m_df) + geom_line(aes(x = year, y = obs)) +
  geom_point(aes(x = year, y = med_pred), color = "blue", size = 2) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(a) Mean")

ar_df <- tibble(year = bocaccio$Yr,
                obs = bocaccio$Recruit_0,
                med_pred = c(rep(NA, 29), ar_quants[2,]),
                low_ci = c(rep(NA, 29), ar_quants[1,]),
                up_ci = c(rep(NA, 29), ar_quants[3,]))

ar_plot <- ggplot(data = ar_df) + geom_line(aes(x = year, y = obs)) +
  geom_point(aes(x = year, y = med_pred), color = "blue",size = 2) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(b) AR(1)")

bh_df <- tibble(year = bocaccio$Yr,
                obs = bocaccio$Recruit_0,
                med_pred = c(rep(NA, 29), bh_quants[2,]),
                low_ci = c(rep(NA, 29), bh_quants[1,]),
                up_ci = c(rep(NA, 29), bh_quants[3,]))

bh_plot <- ggplot(data = bh_df) + geom_line(aes(x = year, y = obs)) +
  geom_point(aes(x = year, y = med_pred), color = "blue", size = 2) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(c) Beverton-Holt")

simplex_df <- tibble(year = bocaccio$Yr,
                     obs = bocaccio$Recruit_0,
                     med_pred = c(rep(NA, 29), simplex_quants[2,]),
                     low_ci = c(rep(NA, 29), simplex_quants[1,]),
                     up_ci = c(rep(NA, 29), simplex_quants[3,]))

simplex_plot <- ggplot(data = simplex_df) + geom_line(aes(x = year, y = obs)) +
  geom_point(aes(x = year, y = med_pred), color = "blue", size = 2) +
  geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
  xlab("Year") + ylab("Recruitment") + labs(subtitle = "(d) Simplex projection")

pdf(here("results/figures/bocaccio_5stp_forecasts.pdf"))
grid.arrange(mean_plot, ar_plot, bh_plot, simplex_plot, bottom = textGrob("5-step expanding window forecasts for bocaccio. Black line represents the observed recruitment. Blue \npoints are the median recruitment value for 1000 simulations. Shaded blue region represents the 95% confidence \nintervals for the simulations.", 
                                                                          x = 0, just = "left", gp = gpar(fontsize = 10)), nrow = 2, ncol = 2)
dev.off()
