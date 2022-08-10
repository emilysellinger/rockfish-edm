library(gridExtra)
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
a <- ggplot(all_stocks) + geom_boxplot(aes(x = region, y = depletion), fill = "#00A1B7") + 
  labs(x = element_blank(), y = "Depletion", subtitle = "(a)") + theme_minimal()

b <- ggplot(all_stocks) + geom_boxplot(aes(x = region, y = autocorrS), fill = "#00A1B7") + 
  labs(x = element_blank(), y = "Spawning biomass autocorrelation\n Lag 1", subtitle = "(b)") + 
  theme_minimal()

c <- ggplot(all_stocks) + geom_boxplot(aes(x = region, y = autocorrR), fill = "#00A1B7") + 
  labs(x = "Management region", y = "Recruitment autocorrelation\n Lag 1", subtitle = "(c)") + 
  theme_minimal()

d <- ggplot(all_stocks) + geom_boxplot(aes(x = region, y = num_yrs), fill = "#00A1B7") + 
  labs(x = "Management region", y = "Time series length", subtitle = "(d)") + 
  theme_minimal()

pdf(here("results/ts_characteristic_comp.pdf"))
print(grid.arrange(a, b, c, d, nrow = 2,ncol = 2))
dev.off()