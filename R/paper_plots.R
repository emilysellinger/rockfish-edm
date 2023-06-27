# Load pacakages ----------------------------------------------------------
library(NatParksPalettes)
library(gridExtra)

# Chapter 2 Figures -------------------------------------------------------
# expanding window figure

ex_df <- tibble(year = seq(1, 20),
                value = runif(20, min = 5, max = 25))
point_pred <- tibble(year = c(11, 12, 13, 14),
                     preds = c(15, 8, 6, 14),
                     fac = as.factor(c(1,1,2,2)))

a <- ggplot() + geom_line(data = ex_df, aes(x = year, y = value)) + 
  labs(x = "Year", y = "Value") + 
  geom_point(data = point_pred, aes(x = year, y = preds, color = fac), size = 3,
             color = c("#00A1B7", "#00A1B7", "#898928", "#898928")) +
  geom_rect(aes(xmin = 1, xmax = 10, ymin = 5, ymax = 25), fill = "#00A1B7", alpha = 0.5) +
  geom_rect(aes(xmin = 1, xmax = 12, ymin = 5, ymax = 25), fill = "#898928", alpha = 0.3) +
  theme(legend.position = "none") +
  theme_minimal()

pdf(here("results/expanding_window_explainer.pdf"))
print(a)
dev.off()


# used these plots for example in paper
a <- sim_quants_plots(pop_goa_sims, pop_goa$year, pop_goa$recruits, "short")
b <- sim_quants_plots(pop_goa_long_sims, pop_goa$year, pop_goa$recruits, "long")


# create plots summarizing results 

coverage_probs_short <- rbind(coverage_probs_short, coverage_probs_short_wc)
coverage_probs_short$method <- recode(coverage_probs_short$method, "AR(1)" = "AR(1)", "mean" = "mean",
                             "Beverton-Holt" = "Beverton-Holt", "HMM" = "HMM", "PELT" = "PELT sample",
                             "simplex" = "simplex projection")
short_totals <- coverage_probs_short %>% 
  group_by(method) %>% 
  summarise(n = n()) 

coverage_probs_long <- rbind(coverage_probs_long, coverage_probs_long_wc)
coverage_probs_long$method <- recode(coverage_probs_long$method, "AR(1)" = "AR(1)", "mean" = "mean",
                                      "Beverton-Holt" = "Beverton-Holt", "HMM" = "HMM", "PELT" = "PELT sample",
                                      "simplex" = "simplex projection")
long_totals <- coverage_probs_long %>% 
  group_by(method) %>% 
  summarise(n = n()) 


hits_target <- coverage_probs_short %>% 
  filter(coverage_prob >= 0.8 & coverage_prob <= 0.95)


a <- hits_target %>% group_by(method) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/as.vector(short_totals$n))

hits_target2 <- coverage_probs_long %>% 
  filter(coverage_prob >= 0.8 & coverage_prob <= 0.95)


b <- hits_target2 %>% group_by(method) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/as.vector(long_totals$n))

hits_target_df <- rbind(a, b)
hits_target_df$type <- c(rep("short", 6), rep("long", 6))

ggplot(data = hits_target_df, aes(x = method, y = freq, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Forecast method", y = "Frequency in target\n coverage probability range",
       fill = "Forecast length") +
  scale_x_discrete(labels = c("AR(1)", "Beverton-Holt", "HMM\n sampling", "mean", "PELT\n sampling",
                              "simplex\n projection")) +
  ylim(0,1) + scale_fill_manual(values = c("#00A1B7", "#586028")) +
  theme_minimal()


## look at success rates relative to different stock characteristics
df <- rbind(hits_target, hits_target2)
df$type <- c(rep("short", 105), rep("long", 84))

df <- left_join(df, all_stocks)

coverage_probs_all <- rbind(coverage_probs_short, coverage_probs_long)
coverage_probs_all$type <- c(rep("short", 204), rep("long", 197))
coverage_probs_all <- left_join(coverage_probs_all, all_stocks)

a <- coverage_probs_all %>%
  filter(type == "short") %>% 
  ggplot() + geom_point(aes(x = autocorrR, y = coverage_prob, color = method)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2),
              aes(x = autocorrR, y = coverage_prob, color = method)) +
  scale_color_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Recruitment autocorrelation\n at lag = 1", y = "Coverage probability", subtitle = "(a) Short-term forecasts") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  xlim(-0.5, 1) +
  facet_wrap(~ method) +
  theme_minimal() +
  theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45))

b <- coverage_probs_all %>%
  filter(type == "long") %>% 
  ggplot() + geom_point(aes(x = autocorrR, y = coverage_prob, color = method)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), 
              aes(x = autocorrR, y = coverage_prob, color = method)) +
  scale_color_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Recruitment autocorrelation\n at lag = 1", y = "Coverage probability", subtitle = "(b) Long-term forecasts") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  xlim(-0.5, 1) +
  facet_wrap(~ method) +
  theme_minimal() +
  theme(legend.position = "none")

pdf(file = here("coverage_prob_vs_autocorrelation.pdf"))
print(a)
print(b)
dev.off()

c <- coverage_probs_all %>%
  filter(type == "short") %>% 
  ggplot() + geom_point(aes(x = depletion, y = coverage_prob, color = method)) +
  geom_smooth(method = lm, aes(x = depletion, y = coverage_prob, color = method)) +
  scale_color_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Depletion", y = "Coverage probability", subtitle = "(a) Short-term forecasts") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  xlim(0, 1) +
  facet_wrap(~ method) +
  theme_minimal() +
  theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45))

d <- coverage_probs_all %>%
  filter(type == "long") %>% 
  ggplot() + geom_point(aes(x = depletion, y = coverage_prob, color = method)) +
  geom_smooth(method = lm, aes(x = depletion, y = coverage_prob, color = method)) +
  scale_color_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Depletion", y = "Coverage probability", subtitle = "(b) Long-term forecasts") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  xlim(0, 1) +
  facet_wrap(~ method) +
  theme_minimal() +
  theme(legend.position = "none")

pdf(file = here("coverage_prob_vs_depletion.pdf"))
print(c)
print(d)
dev.off()

e <- coverage_probs_all %>%
  filter(type == "short") %>% 
  ggplot() + geom_point(aes(x = log_sigmaR_full, y = coverage_prob, color = method)) +
  geom_smooth(method = lm, aes(x = log_sigmaR_full, y = coverage_prob, color = method)) +
  scale_color_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Log recruitment\n standard deviation", y = "Coverage probability", subtitle = "(a) Short-term forecasts") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  xlim(0, 1.25) +
  facet_wrap(~ method) +
  theme_minimal() +
  theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45))

f <- coverage_probs_all %>%
  filter(type == "long") %>% 
  ggplot() + geom_point(aes(x = log_sigmaR_full, y = coverage_prob, color = method)) +
  geom_smooth(method = lm, aes(x = log_sigmaR_full, y = coverage_prob, color = method)) +
  scale_color_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Log recruitment\n standard deviation", y = "Coverage probability", subtitle = "(b) Long-term forecasts") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  xlim(0, 1.25) +
  facet_wrap(~ method) +
  theme_minimal() +
  theme(legend.position = "none")

pdf(file = here("coverage_prob_vs_sigmaR.pdf"))
print(e)
print(f)
dev.off()