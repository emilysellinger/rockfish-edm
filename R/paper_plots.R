# Load pacakages ----------------------------------------------------------
library(NatParksPalettes)
library(patchwork)
library(here)
library(tidyverse)


# Load data ---------------------------------------------------------------
# West Coast
west_coast_ts_characteristics <- read_csv(here('data/west_coast_stock_characteristics.csv'))
west_coast_ts_characteristics$stock_name[which(west_coast_ts_characteristics$stock_name == "petrale_sole")] <- "petrale"
west_coast_ts_characteristics$region <- rep("west coast", nrow(west_coast_ts_characteristics))

# Alaska
alaska_ts_characteristics <- read_csv(here('data/alaska_ts_characteristics.csv'))
# need to remove the BSAI and GOA stock of blackspotted and rougheye rockfish
# BSAI stock does not have enough years in time series
alaska_ts_characteristics <- alaska_ts_characteristics[-3,]


# create one data frame for all stocks
all_stocks <- rbind(alaska_ts_characteristics, west_coast_ts_characteristics)

# Coverage probabilities for stocks
coverage_probs_all <- read_csv(here('results/simulation_results/all_stocks_coverage_probs.csv'))

coverage_probs_all <- left_join(coverage_probs_all, all_stocks)

# Chapter 2 Figures -------------------------------------------------------
## expanding window figure -----------------------------

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


## POP GOA example ----------------------------------------- 
#used these plots for example in paper
pop_goa <- coverage_probs_all %>% 
  filter(stock_name == 'pop_goa') %>% 
  mutate(method = recode(method, "AR(1)" = "AR(1)", "Mean" = "mean",
                         "Beverton-Holt" = "Beverton-Holt", "HMM" = "HMM sampling", "PELT" = "PELT sampling",
                         "simplex" = "Simplex projection"))

pop_goa_a <- pop_goa %>% 
  filter(type == 'short') %>% 
  ggplot() + geom_point(aes(x = method, y = coverage_prob), size = 3, color = "#00A1B7") +
  geom_hline(yintercept = 0.95, linetype = 'dashed') +
  geom_hline(yintercept = 0.8, linetype = 'dashed') +
  ylim(c(0, 1)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  labs(x = 'Forecast method', y = 'Coverage probability', subtitle = '(a)')

pop_goa_b <- pop_goa %>% 
  filter(type == 'long') %>% 
  ggplot() + geom_point(aes(x = method, y = coverage_prob), size = 3, color = "#00A1B7") +
  geom_hline(yintercept = 0.95, linetype = 'dashed') +
  geom_hline(yintercept = 0.8, linetype = 'dashed') +
  ylim(c(0, 1)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = 'Forecast method', y = 'Coverage probability', subtitle = '(b)')

pdf(here('results/figures/pop_goa_coverage_probabilities.pdf'))
print(pop_goa_a + pop_goa_b + plot_layout(ncol = 1))
dev.off()



## Frequency with which methods fall inside ideal range ---------------------------- 

coverage_probs_short <- coverage_probs_all %>% 
  filter(type == 'short')
  
coverage_probs_short$method <- recode(coverage_probs_short$method, "AR(1)" = "AR(1)", "mean" = "mean",
                             "Beverton-Holt" = "Beverton-Holt", "HMM" = "HMM", "PELT" = "PELT sample",
                             "simplex" = "simplex projection")
short_totals <- coverage_probs_short %>% 
  group_by(method) %>% 
  summarise(n = n()) 

coverage_probs_long <- coverage_probs_all %>% 
  filter(type == 'long')

coverage_probs_long$method <- recode(coverage_probs_long$method, "AR(1)" = "AR(1)", "mean" = "mean",
                                      "Beverton-Holt" = "Beverton-Holt", "HMM" = "HMM", "PELT" = "PELT sample",
                                      "simplex" = "simplex projection")
long_totals <- coverage_probs_long %>% 
  group_by(method) %>% 
  summarise(n = n()) 


hits_target_short <- coverage_probs_short %>% 
  filter(coverage_prob >= 0.8 & coverage_prob <= 0.95)


a <- hits_target_short %>% group_by(method) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/as.vector(short_totals$n))

hits_target_long <- coverage_probs_long %>% 
  filter(coverage_prob >= 0.8 & coverage_prob <= 0.95)


b <- hits_target_long %>% group_by(method) %>% 
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
######################################


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

pdf(file = here("results/figures/coverage_prob_vs_autocorrelation.pdf"))
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

pdf(file = here("results/figures/coverage_prob_vs_depletion.pdf"))
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

pdf(file = here("results/figures/coverage_prob_vs_sigmaR.pdf"))
print(e)
print(f)
dev.off()

g <- coverage_probs_all %>% 
  filter(type == "short") %>% 
  ggplot() + geom_boxplot(aes(x = factor(detectable_SR), y = coverage_prob, fill = method)) +
  scale_fill_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Detectable SR\n relationship", y = "Coverage probability", subtitle = "(a) Short-term forecasts", fill = "Method") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  theme_minimal()
h <- coverage_probs_all %>% 
  filter(type == "long") %>% 
  ggplot() + geom_boxplot(aes(x = factor(detectable_SR), y = coverage_prob, fill = method)) +
  scale_fill_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Detectable SR\n relationship", y = "Coverage probability", subtitle = "(b) Long-term forecasts", fill = "Method") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  theme_minimal()

pdf(file = here("results/figures/coverage_prob_vs_detectableSR.pdf"))
print(g)
print(h)
dev.off()

i <- coverage_probs_all %>% 
  filter(type == "short") %>% 
  ggplot() + geom_boxplot(aes(x = factor(regime_shift), y = coverage_prob, fill = method)) +
  scale_fill_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Detectable\n regime shift", y = "Coverage probability", subtitle = "(a) Short-term forecasts", fill = "Method") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  theme_minimal()
j <- coverage_probs_all %>% 
  filter(type == "long") %>% 
  ggplot() + geom_boxplot(aes(x = factor(regime_shift), y = coverage_prob, fill = method)) +
  scale_fill_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Detectable\n regime shift", y = "Coverage probability", subtitle = "(b) Long-term forecasts", fill = "Method") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  theme_minimal()

pdf(file = here("results/figures/coverage_prob_vs_regime_shift.pdf"))
print(i)
print(j)
dev.off()


k <- coverage_probs_all %>%
  filter(type == "short") %>% 
  ggplot() + geom_point(aes(x = num_yrs, y = coverage_prob, color = method)) +
  geom_smooth(method = lm, aes(x = num_yrs, y = coverage_prob, color = method)) +
  scale_color_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Length of recruitment\n time series", y = "Coverage probability", subtitle = "(a) Short-term forecasts") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  facet_wrap(~ method) +
  theme_minimal() +
  theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45))

l <- coverage_probs_all %>%
  filter(type == "long") %>% 
  ggplot() + geom_point(aes(x = num_yrs, y = coverage_prob, color = method)) +
  geom_smooth(method = lm, aes(x = num_yrs, y = coverage_prob, color = method)) +
  scale_color_manual(values = c("#006475","#00A1B7","#55CFD8","#586028","#898928","#616571")) +
  labs(x = "Length of recruitment\n time series", y = "Coverage probability", subtitle = "(b) Long-term forecasts") +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  facet_wrap(~ method) +
  theme_minimal() +
  theme(legend.position = "none")

pdf(file = here("results/figures/coverage_prob_vs_num_yrs.pdf"))
print(k)
print(l)
dev.off()