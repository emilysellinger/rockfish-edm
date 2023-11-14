library(patchwork)
library(tidyverse)
library(here)
# Comparison of management regions ----------------------------------------
# read in data if needed
west_coast_ts_characteristics <- read_csv(here("data/west_coast_stock_characteristics.csv"))
alaska_ts_characteristics <- read_csv(here("data/alaska_ts_characteristics.csv"))

# add column to west coast
west_coast_ts_characteristics$region <- rep("West Coast", nrow(west_coast_ts_characteristics))

# combine dataframes
all_stocks <- rbind(alaska_ts_characteristics, west_coast_ts_characteristics)
# need to remove the BSAI stock of blackspotted and rougheye rockfish
# stock does not have enough years in time series
all_stocks <- all_stocks[-3,]

# Visualize data ----------------------------------------------------------
# depletion boxplot
a <- ggplot(all_stocks, aes(x = region, y = num_yrs)) + 
  geom_violin(fill = "#00A1B7", alpha = 0.5) +
  geom_point(position = position_jitter(seed = 1, width = 0.15)) +
  labs(x = element_blank(), y = "Time series length", subtitle = "(a)") + theme_minimal()

b <- ggplot(all_stocks, aes(x = region, y = depletion)) + 
  geom_violin(fill = "#00A1B7", alpha = 0.5) + 
  geom_point(position = position_jitter(seed = 1, width = 0.15)) +
  labs(x = element_blank(), y = "Depletion", subtitle = "(b)") + 
  theme_minimal()

c <- ggplot(all_stocks, aes(x = region, y = autocorrR)) + 
  geom_violin(fill = "#00A1B7", alpha = 0.5) +
  geom_point(position = position_jitter(seed = 1, width = 0.15)) +
  labs(x = "Management region", y = "Recruitment autocorrelation\n Lag 1", subtitle = "(c)") + 
  theme_minimal()

d <- ggplot(all_stocks, aes(x = region, y = log_sigmaR_full)) + 
  geom_violin(fill = "#00A1B7", alpha = 0.5) + 
  geom_point(position = position_jitter(seed = 1, width = 0.15)) +
  labs(x = "Management region", y = expression(log(sigma[r])), subtitle = "(d)") + 
  theme_minimal()

e <- ggplot(all_stocks) + 
  geom_bar(aes(x = factor(regime_shift), fill = region), position = "dodge",) + 
  scale_fill_manual(values = c("#00A1B7", "#586028", "#616571")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  labs(x = "Detectable regime shift", y = "Number of stocks", fill = 'Region', subtitle = "(e)") + 
  theme_minimal()

f <- ggplot(all_stocks) + 
  geom_bar(aes(x = factor(detectable_SR), fill = region), position = "dodge",) + 
  scale_fill_manual(values = c("#00A1B7", "#586028", "#616571")) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes", "2" = "Some\n evidence")) +
  labs(x = "Detectable S-R relationship", y = "Number of stocks", fill = 'Region', subtitle = "(f)") + 
  theme_minimal()

pdf(here("results/ts_characteristic_comp.pdf"), width = 10)
print(a + b + c + d + e + f + plot_layout(nrow = 3))
dev.off()
