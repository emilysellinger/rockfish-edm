# some minor investigation into why some of the results
# want to see what is up with stocks with SR
hasSR <- all_stocks %>% 
  filter(detectable_SR == 1)

hasSR <- left_join(hasSR, coverage_probs, multiple = "all")

might_haveSR <- all_stocks %>% 
  filter(detectable_SR == 2)

might_haveSR <- left_join(might_haveSR, coverage_probs_all, multiple = "all")

View(hasSR %>% 
  filter(method == "Beverton-Holt")) 


# want to look at stocks with a regime shift
has_regime_shift <- all_stocks %>% 
  filter(regime_shift == 1)
has_regime_shift$stock_name

has_regime_shift <- left_join(has_regime_shift, coverage_probs_all, multiple = "all")
# calculate change points for full time series
canary <- canary %>% 
  filter(Area == 1)
canary <- filter_sr_data(canary)

fitPelt <- cpt.mean(log(canary$Recruit_0),method="PELT",test.stat="Normal",penalty="Manual",
                    pen.value = "2*(diffparam+1)*(n/(n-diffparam-2))", minseglen=6)
fitPelt@cpts

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
# lingcod_s years: 18, 51
# sablefish years: 31, 53, 61
# splitnose years: 23, 34

# Initial training set goes to 20 years
has_regime_shift %>% 
  filter(method == "PELT sample" | method == "HMM") %>% 
  filter((stock_name %in% c("arrowtooth_flounder", "dusky_goa", "greenland_turbot",
                             "northern_rock_sole", "pollock_goa", "pop_goa", "lingcod_s"))) %>% 
  ggplot() + geom_boxplot(aes(x = method, y = coverage_prob, fill = type)) + 
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2)


has_regime_shift %>% 
  filter(method == "PELT sample" | method == "HMM") %>% 
  filter(!(stock_name %in% c("arrowtooth_flounder", "dusky_goa", "greenland_turbot",
                             "northern_rock_sole", "pollock_goa", "pop_goa", "lingcod_s"))) %>% 
  ggplot() + geom_boxplot(aes(x = method, y = coverage_prob, fill = type)) + 
  geom_hline(yintercept = 0.8, linetype = 2) + geom_hline(yintercept = 0.95, linetype = 2)
 

# want to look into which stocks have negative autocorrelation
# Mark is skeptical of those stocks

negative_autocorr <- all_stocks %>% 
  filter(autocorrR < 0)
