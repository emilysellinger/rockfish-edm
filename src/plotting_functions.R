coverage_prob_plot <- function(preds){
  fmethod <- dim(preds)[3]
  methods <- c("mean", "AR(1)", "Beverton-Holt", "simplex projection", "HMM")
  
  bayes_prob_df <- tibble(method = methods[1:fmethod],
                          coverage_prob = rep(NA, fmethod))
  for(i in 1:fmethod){
    
    bayes_prob_df[i,2] <- sim_CI_prob(preds[,,i], 0.95)
  }
  
  cplot <- ggplot(bayes_prob_df) + 
    geom_point(aes(x = method, y = coverage_prob), size = 3) + 
    ylim(0, 1) + ylab("Coverage probability") + 
    xlab("Recruitment forecast method")
  
  return(cplot)
}

mrae_plot <- function(preds, obs, ts_vec){
  fmethod <- dim(preds)[3]
  ts <- dim(preds)[1]
  methods <- c("mean", "AR(1)", "Beverton-Holt", "simplex projection", "HMM")
  
  mrae_df <- tibble(year = c(rep(seq(1, ts), fmethod)),
                    method = c(rep(NA, ts*fmethod)),
                    mrae = c(rep(NA, ts*fmethod)))
  for(i in 1:fmethod){
    mrae_df[((ts*i-(ts-1)):(ts*i)), 2] <- c(rep(methods[i], ts))
    mrae_df[((ts*i-(ts-1)):(ts*i)), 3] <- sim_mare(preds[,,i], obs, ts_vec)
  }
  
  plot_mrae <- ggplot(mrae_df) + geom_line(aes(x = year, y = mrae, color = method)) + 
    ylab("Mean relative absolute error") + xlab("Years added to training set")
  
  return(plot_mrae)
}


sim_quants_plots <- function(preds, yrs, obs, type){
  plist <- list()
  fmethod <- dim(preds)[3]
  methods <- c("Mean", "AR(1)", "Beverton-Holt", "Simplex projection", "HMM")
  plot_num <- c("(a)", "(b)", "(c)", "(d)", "(e)")
  
  for(i in 1:fmethod){
    predsi <- preds[,,i]
    
    quants <- apply(predsi[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
    
    quant_df <- tibble(year = yrs,
                   obs = obs,
                   med_pred = c(rep(NA, 29), quants[2,]),
                   low_ci = c(rep(NA, 29), quants[1,]),
                   up_ci = c(rep(NA, 29), quants[3,]))
    
    plist[[i]] <- ggplot(data = quant_df) + geom_line(aes(x = year, y = obs)) +
      geom_line(aes(x = year, y = med_pred), color = "blue",) +
      geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = year), fill = "blue", alpha = 0.1, linetype = "dashed") + 
      xlab("Year") + ylab("Recruitment") + labs(subtitle = paste(plot_num[i], methods[i]))
  }
  
  if(type == "short"){
    grid.arrange(grobs = plist, ncol = 2, top = textGrob("1-step Forecasts")) 
  }else{
    grid.arrange(grobs = plist, ncol = 2, top = textGrob("5-step Forecasts"))
  }

}

yr_trend_plot <- function(preds, obs, ts_vec){
  plist <- list()
  fmethod <- dim(preds)[3]
  methods <- c("Mean", "AR(1)", "Beverton-Holt", "Simplex projection", "HMM")
  plot_num <- c("(a)", "(b)", "(c)", "(d)", "(e)")
  
  real_trend <- rollmean(obs[30:length(obs)], 5)
  
  for(i in 1:fmethod){
    sim_trend <- sim_5yr_trend(preds[,,i], ts_vec)
    
    trend_df <- tibble(real = real_trend,
                            med = sim_trend[2,],
                            low_ci = sim_trend[1,],
                            up_ci = sim_trend[3,])
    
    plist[[i]] <- ggplot(trend_df) + geom_line(aes(x = seq(1, nrow(trend_df)), y = real))+
      geom_line(aes(x = seq(1, nrow(trend_df)), y = med), color = "blue") + 
      geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = seq(1, nrow(trend_df))), fill = "blue", alpha = 0.1) +
      labs(y = "Five-year rolling average", subtitle = paste(plot_num[i], methods[i]), x = "Years added to training set")
    
  }
  
  grid.arrange(grobs = plist, ncol = 2)
}


aurora_preds <- array(c(m_preds, ar_preds, bh_preds, simplex_preds, hmm_preds),
                      dim = c(21,1001,5))
aurora_preds_long <- array(c(m_preds_long, ar_preds_long, bh_preds_long, 
                           simplex_preds_long, hmm_preds_long), dim = c(21,1001,5))

print_plots <- function(preds1, preds2, obs, yrs, t1, t2){
  print(sim_quants_plots(preds1, yrs, obs, type = "short"))
  print(coverage_prob_plot(preds1))
  print(mrae_plot(preds1, obs, t1))
  print(sim_quants_plots(preds2, yrs, obs, type = "long"))
  print(coverage_prob_plot(preds2))
  print(yr_trend_plot(preds2, obs, t2))
}

pdf(here("results/figures/aurora_forecast_figs.pdf"))
print_plots(aurora_preds, aurora_preds_long, aurora$Recruit_0, aurora$Yr,
            time_vec, time_vec2)
dev.off()
