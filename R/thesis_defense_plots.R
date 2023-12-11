pop_goa$factor <- c(rep(1, 23), rep(2,11), rep(3, 10))


ggplot(pop_goa, aes(x = year, y = recruits/1000)) + 
  geom_point(aes(color = factor(factor)), size = 2) +
  geom_line(linetype = 'dashed') +
  scale_color_manual(values = c("#003459", "#0A94A6", "#BD7327")) +
  labs(x = 'Year', y = "Recruitment ('000s)") +
  theme_minimal() + theme(legend.position = "none")



# Likelihood function
BHminusLL <- function(loga, logb, logsigmaR){
  # extract parameters
  a <- exp(loga); b <- exp(logb); sigmaR <- exp(logsigmaR)
  
  # make predictions
  pred <- log(a*(pop_goa$sbiomass/1000)/(1 + b*(pop_goa$sbiomass/1000)))
  
  #calculate negative log like
  NegLogL <- (-1)*sum(dnorm(log(pop_goa$recruits/1000), pred, sigmaR, log = TRUE))
  return(NegLogL)
}


starts <- list(loga = log(2), logb = log(0.2), logsigmaR = 5)
mle_out <- mle(BHminusLL, start = starts)

# extract parameters
a <- exp(coef(mle_out)[1])
b <- exp(coef(mle_out)[2])

max_sb <- max((pop_goa$sbiomass/1000), na.rm = TRUE)
sb_values <- seq(1, max_sb, length.out = 200)
rec_pred_bh <- (a*sb_values)/(1 + (b*sb_values))

# create tibble with preditions
preds <- tibble(sb = sb_values,
                bh_preds = rec_pred_bh)
ggplot(pop_goa, aes(x = sbiomass/1000, y = recruits/1000)) + geom_point(size = 1.5, color = "#003459") +
  labs(x = "Spawning biomass ('000s)", y = "Recruitment ('000s)") + 
  geom_line(data = preds, aes(x = sb, y = bh_preds), linetype = "dashed") +
  theme_minimal()

##################################################
hits_target_region %>% 
  filter(type == 'long') %>% 
  ggplot(aes(x = factor(method, levels = c("mean", "AR(1)", "Beverton-Holt", "simplex projection", "HMM", "PELT sample")), y = freq, fill = region)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Forecast method", y = "Frequency in target range",
       fill = "Region") +
  scale_x_discrete(labels = c("Mean", "AR(1)", "Beverton-\nHolt", "Simplex\n projection", "HMM\n sampling",  "PELT\n sampling")) +
  ylim(0,1) + scale_fill_manual(values = c("#003459", "#0A94A6", "#9AB87A"), labels = c("BSAI", "GOA", "US West coast")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45))


mase_freq %>% 
  filter(type == 'short') %>% 
  ggplot() + geom_boxplot(aes(x = factor(method, levels = c("mean", "AR(1)", "Beverton-Holt", "simplex projection", "HMM", "PELT")), y = freq, fill = region), alpha = 0.8) + 
  scale_fill_manual(values = c("#003459", "#0A94A6", "#9AB87A"), labels = c("BSAI", "GOA", "US West coast")) +
  scale_color_manual(values =  c("black", "black", "black", guide = "none")) +
  ylim(0, 1) + geom_hline(yintercept = 0.5, linetype = 'dashed') +
  labs(x = 'Forecast method', y = 'Frequency\n MASE < 1', fill = 'Region') + 
  scale_x_discrete(labels = c("Mean", "AR(1)", "Beverton-\nHolt", "Simplex\n projection", "HMM\n sampling",  "PELT\n sampling")) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45))  



a <- rnorm(20, 15, 1)
b <- rnorm(15, 3, 1)

chpt_df <- tibble(year = seq(1, 35,1),
                  rec = c(a, b),
                  regime = c(rep(1, 20), rep(2, 15)))

ggplot(chpt_df, aes(x = year, y = rec)) + geom_point(aes(color = factor(regime)),size = 3) +
  geom_line(linetype = 'dashed') +
  scale_color_manual(values = c("#003459", "#0A94A6"), guide = 'none') +
   labs(x = 'Year', y = 'Recruitment') + theme_minimal()
