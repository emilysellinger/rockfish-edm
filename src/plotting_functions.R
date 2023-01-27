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
    ylim(0, 1) + geom_hline(yintercept = 0.80, linetype = "dashed") +
    geom_hline(yintercept = 0.95, linetype = "dashed") +
    ylab("Coverage probability") + 
    xlab("Recruitment forecast method")
  
  return(list(cplot, as.data.frame(bayes_prob_df)))
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
  
  return(list(plot_mrae, as.data.frame(mrae_df)))
}


sim_quants_plots <- function(preds, yrs, obs, type){
  plist <- list()
  fmethod <- dim(preds)[3]
  methods <- c("Mean", "AR(1)", "Beverton-Holt", "Simplex projection", "HMM")
  plot_num <- c("(a)", "(b)", "(c)", "(d)", "(e)")
  total_yrs <- length(yrs)
  
  for(i in 1:fmethod){
    predsi <- preds[,,i]
    
    quants <- apply(predsi[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
    
    quant_df <- tibble(year = yrs,
                   obs = obs,
                   med_pred = c(rep(NA, (total_yrs - dim(quants)[2])), quants[2,]),
                   low_ci = c(rep(NA, (total_yrs - dim(quants)[2])), quants[1,]),
                   up_ci = c(rep(NA, (total_yrs - dim(quants)[2])), quants[3,]))
    
    plist[[i]] <- ggplot(data = quant_df) + geom_line(aes(x = year, y = obs), alpha = 0.5) +
      geom_point(aes(x = year, y = obs)) +
      geom_point(aes(x = year, y = med_pred), color = "blue") +
      geom_line(aes(x = year, y = med_pred), color = "blue", alpha = 0.5) +
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
      geom_line(aes(x = seq(1, nrow(trend_df)), y = med), color = "blue", alpha = 0.5) + 
      geom_point(aes(x = seq(1, nrow(trend_df)), y = med), color = "blue") +
      geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = seq(1, nrow(trend_df))), fill = "blue", alpha = 0.1) +
      labs(y = "Five-year rolling average", subtitle = paste(plot_num[i], methods[i]), x = "Years added to training set")
    
  }
  
  grid.arrange(grobs = plist, ncol = 2)
}



print_plots <- function(preds1, preds2, obs, yrs, t1, t2){
  print(sim_quants_plots(preds1, yrs, obs, type = "short"))
  print(coverage_prob_plot(preds1)[[1]])
  print(mrae_plot(preds1, obs, t1)[[1]])
  print(sim_quants_plots(preds2, yrs, obs, type = "long"))
  print(coverage_prob_plot(preds2)[[1]])
  print(yr_trend_plot(preds2, obs, t2))
}


sim_quants_df <- function(preds){
  fmethod <- dim(preds)[3]
  yrs <- dim(preds)[1]
  methods <- c("Mean", "AR(1)", "Beverton-Holt", "Simplex projection", "HMM")
  
  sim_quants_df <- tibble(method = rep(NA, fmethod*yrs),
                          low_ci = rep(NA, fmethod*yrs),
                          median = rep(NA, fmethod*yrs),
                          up_ci = rep(NA, fmethod*yrs))
  
  for(i in 1:fmethod){
    predsi <- preds[,,i]
    quants <- apply(predsi[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
    
    sim_quants_df[((yrs*i-(yrs-1)):(yrs*i)), 1] <- rep(methods[i], yrs)
    sim_quants_df[((yrs*i-(yrs-1)):(yrs*i)), 2] <- quants[1,]
    sim_quants_df[((yrs*i-(yrs-1)):(yrs*i)), 3] <- quants[2,]
    sim_quants_df[((yrs*i-(yrs-1)):(yrs*i)), 4] <- quants[3,]
    
  }
  
  return(as.data.frame(sim_quants_df))
}

yr_trend_df <- function(preds, ts_vec){
  fmethod <- dim(preds)[3]
  yrs <- length(ts_vec)
  methods <- c("Mean", "AR(1)", "Beverton-Holt", "Simplex projection", "HMM")
  
  sim_trend_df <- tibble(method = rep(NA, fmethod*yrs),
                          low_ci = rep(NA, fmethod*yrs),
                          median = rep(NA, fmethod*yrs),
                          up_ci = rep(NA, fmethod*yrs))
  
  for(i in 1:fmethod){
    predsi <- preds[,,i]
    sim_trend <- sim_5yr_trend(preds[,,i], ts_vec)
    
    sim_trend_df[((yrs*i-(yrs-1)):(yrs*i)), 1] <- rep(methods[i], yrs)
    sim_trend_df[((yrs*i-(yrs-1)):(yrs*i)), 2] <- sim_trend[1,]
    sim_trend_df[((yrs*i-(yrs-1)):(yrs*i)), 3] <- sim_trend[2,]
    sim_trend_df[((yrs*i-(yrs-1)):(yrs*i)), 4] <- sim_trend[3,]
    
  }
  
  return(as.data.frame(sim_trend_df))
}


save_performance_stats <- function(preds1, preds2, obs, yrs, t1, t2){
  performance_list <- list(short_forecast_quants = sim_quants_df(preds1),
                           short_cov_prob = coverage_prob_plot(preds1)[[2]],
                           mrae = mrae_plot(preds1, obs, t1)[[2]],
                           long_forecast_quants = sim_quants_df(preds2),
                           long_cov_prob = coverage_prob_plot(preds2)[[2]],
                           yr_trend = yr_trend_df(preds2, t2))
  
  return(performance_list)
}


