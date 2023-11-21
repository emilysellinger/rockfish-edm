# some minor investigation into why some of the results
# want to see what is up with stocks with SR
hasSR_mase_short <- all_stocks_short_MASE %>% 
  filter(detectable_SR == 1) %>% 
  filter(method == "Beverton-Holt")

# hasSR_mase_short_n <- hasSR_mase_short %>% 
#   group_by(period) %>% 
#   summarise(n_obs = n())

hasSR_mase_long <- all_stocks_long_MASE %>% 
  filter(detectable_SR == 1) %>% 
  filter(method == "Beverton-Holt")

# stocks with some evidence of a sr relationship
maybeSR_mase_short <- all_stocks_short_MASE %>% 
  filter(detectable_SR == 2) %>% 
  filter(method == "Beverton-Holt")

maybeSR_mase_long <- all_stocks_long_MASE %>% 
  filter(detectable_SR == 2) %>% 
  filter(method == "Beverton-Holt")


a <- hasSR_mase_short %>% 
  ggplot() + geom_boxplot(aes(x = factor(period, levels = c('early', 'mid', 'late')), y = mase)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_bw() + labs(x = "Forecast period", y = "MASE", subtitle = 'SR detectable, \n1 year forecasts')

b <- hasSR_mase_long %>% 
  ggplot() + geom_boxplot(aes(x = factor(period, levels = c('early', 'mid', 'late')), y = mase)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_bw() + labs(x = "Forecast period", y = "MASE", subtitle = 'SR detectable, \n5 year forecasts')

c <- maybeSR_mase_short %>% 
  ggplot() + geom_boxplot(aes(x = factor(period, levels = c('early', 'mid', 'late')), y = mase)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_bw() + labs(x = "Forecast period", y = "MASE", subtitle = 'Some evidence of SR, \n1 year forecasts')

d <- maybeSR_mase_long %>% 
  ggplot() + geom_boxplot(aes(x = factor(period, levels = c('early', 'mid', 'late')), y = mase)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_bw() + labs(x = "Forecast period", y = "MASE", subtitle = 'Some evidence of SR,\n5 year forecasts')

pdf(here('results/figures/comparison_of_MASE_by_period_BH.pdf'))
print(a + b + c + d + plot_layout(nrow = 2))
dev.off()


# want to look at stocks with a regime shift
has_regime_shift <- all_stocks %>% 
  filter(regime_shift == 1)
has_regime_shift$stock_name

has_regime_shift_cp <- coverage_probs_all %>% 
  filter(regime_shift == 1)


# calculate change points for full time series for each of the stocks with a detected regime shift
# alaska
arrowtooth_flounder_bsai <- read_csv(here("data/alaska_stocks/trimmed_time_series", "arrowtooth_flounder_bsai_trimmed.csv"))
dusky_goa <- read_csv(here("data/alaska_stocks/trimmed_time_series", "dusky_goa_trimmed.csv"))
greenland_turbot <- read_csv(here("data/alaska_stocks/trimmed_time_series", "greenland_turbot_trimmed.csv"))
northern_rock_sole <- read_csv(here("data/alaska_stocks/trimmed_time_series", "northern_rock_sole_trimmed.csv"))
northern_rockfish_goa <- read_csv(here("data/alaska_stocks/trimmed_time_series", "northern_rockfish_goa_trimmed.csv"))
pollock_goa <- read_csv(here("data/alaska_stocks/trimmed_time_series", "pollock_goa_trimmed.csv"))
pop_goa <- read_csv(here("data/alaska_stocks/trimmed_time_series", "pop_goa_trimmed.csv"))

# west coast
bocaccio <- read_csv(here('data/west_coast_stocks/trimmed_time_series/bocaccio_trimmed.csv'))
canary <- read_csv(here('data/west_coast_stocks/trimmed_time_series/canary_trimmed.csv'))
canary <- canary %>% 
  filter(Area == 1)

chilipepper <- read_csv(here('data/west_coast_stocks/trimmed_time_series/chilipepper_trimmed.csv'))
sablefish <- read_csv(here('data/west_coast_stocks/trimmed_time_series/sablefish_trimmed.csv'))
splitnose <- read_csv(here('data/west_coast_stocks/trimmed_time_series/splitnose_trimmed.csv'))


# fit change point algo
# Alaska
fitPelt_arrow <- cpt.mean(log(arrowtooth_flounder_bsai$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                    pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
arrow_locs <- fitPelt_arrow@cpts

fitPelt_dusky <- cpt.mean(log(dusky_goa$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                          pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
dusky_locs <- fitPelt_dusky@cpts

fitPelt_green <- cpt.mean(log(greenland_turbot$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                          pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
green_locs <- fitPelt_green@cpts

fitPelt_nrocksole <- cpt.mean(log(northern_rock_sole$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                          pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
nrocksole_locs <- fitPelt_nrocksole@cpts


fitPelt_nrockfish <- cpt.mean(log(northern_rockfish_goa$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                              pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
nrockfish_locs <- fitPelt_nrockfish@cpts


fitPelt_pollock <- cpt.mean(log(pollock_goa$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                              pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
pollock_locs <- fitPelt_pollock@cpts

fitPelt_pop <- cpt.mean(log(pop_goa$recruits),method="PELT",test.stat="Normal",penalty="Manual",
                              pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
pop_locs <- fitPelt_pop@cpts

# West Coast
fitPelt_bocc <- cpt.mean(log(bocaccio$Recruit_0),method="PELT",test.stat="Normal",penalty="Manual",
                              pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
bocc_locs <- fitPelt_bocc@cpts

fitPelt_canary <- cpt.mean(log(canary$Recruit_0),method="PELT",test.stat="Normal",penalty="Manual",
                         pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
canary_locs <- fitPelt_canary@cpts

fitPelt_chili <- cpt.mean(log(chilipepper$Recruit_0),method="PELT",test.stat="Normal",penalty="Manual",
                         pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
chili_locs <- fitPelt_chili@cpts

fitPelt_sable <- cpt.mean(log(sablefish$Recruit_0),method="PELT",test.stat="Normal",penalty="Manual",
                         pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
sable_locs <- fitPelt_sable@cpts

fitPelt_split <- cpt.mean(log(splitnose$Recruit_0),method="PELT",test.stat="Normal",penalty="Manual",
                         pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
split_locs <- fitPelt_split@cpts



# create data frame with change point years for all stocks
chpt_df <- tibble(stock_name = c(rep("arrowtooth_flounder", length(arrow_locs)), rep("bocaccio", length(bocc_locs)),
                                 rep("canary", length(canary_locs)), rep("chilipepper", length(chili_locs)),
                                 rep("dusky_goa", length(dusky_locs)), rep("greenland_turbot", length(green_locs)),
                                 rep("northern_rock_sole", length(nrocksole_locs)), rep("northern_rockfish", length(nrockfish_locs)),
                                 rep("pollock_goa", length(pollock_locs)), rep("pop_goa", length(pop_locs)),
                                 rep("sablefish", length(sable_locs)), rep("splitnose", length(split_locs))),
                  change_points = c(arrow_locs, bocc_locs, canary_locs, chili_locs, 
                                    dusky_locs, green_locs, nrocksole_locs, nrockfish_locs,
                                    pollock_locs, pop_locs, sable_locs, split_locs))

# arrowtooth flounder years: 10, 44
# dusky goa years: 9, 36, 42
# greenland turbot years: 11, 26, 32, 41
# northern rock sole years: 6, 33, 40, 46
# northern rockfish goa years : 27, 44
# pollock goa years: 14, 38, 44
# POP goa years: 8, 44
# bocaccio years: 20, 50, 56
# canary years: 43, 49, 55
# chilipepper years: 10, 43, 49
# sablefish years: 31, 53, 61
# splitnose years: 23, 34

# filter which stocks have a regime shift in initial training data
early_cpt <- chpt_df %>% 
  filter(change_points < 20)
early_cpt_stocks <- unique(early_cpt$stock_name) 

# Initial training set goes to 20 years
a <- has_regime_shift_cp %>% 
  filter(method == "PELT" | method == "HMM") %>% 
  filter((stock_name %in% early_cpt_stocks)) %>% 
  ggplot() + geom_boxplot(aes(x = method, y = coverage_prob, fill = factor(type, levels = c('short', 'long')))) + 
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  scale_fill_manual(values = c("#586028", "#00A1B7"), labels = c('1 year', '5 years')) +
  labs(y = "Coverage probability", x = "Method", fill = "Forecast\nlength", subtitle = "(a)") +
  ylim(0,1) +
  theme_minimal()


b <- has_regime_shift_cp %>% 
  filter(method == "PELT" | method == "HMM") %>% 
  filter(!(stock_name %in% early_cpt_stocks)) %>% 
  ggplot() + geom_boxplot(aes(x = method, y = coverage_prob, fill = factor(type, levels = c('short', 'long')))) + 
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2) +
  scale_fill_manual(values = c("#586028", "#00A1B7"), labels = c('1 year', '5 years')) +
  labs(y = element_blank(), x = "Method", fill = "Forecast\nlength", subtitle = "(b)") +
  ylim(0,1) +
  theme_minimal()
 

pdf(here('results/figures/regime_shift_comparison.pdf'), width = 11)
print(a + b + plot_layout(guides = 'collect'))
dev.off()


# want to look into which stocks have negative autocorrelation
# Mark is skeptical of those stocks

negative_autocorr <- all_stocks %>% 
  filter(autocorrR < 0)
