coverage_prob_plot <- function(preds){
  fmethod <- length(names(preds))
  
  bayes_prob_df <- tibble(method = names(preds),
                          coverage_prob = rep(NA, fmethod))
  for(i in 1:fmethod){
    
    bayes_prob_df[i,2] <- sim_CI_prob(preds[[i]], 0.95)
  }
  
  cplot <- ggplot(bayes_prob_df) + 
    geom_point(aes(x = method, y = coverage_prob), size = 3) + 
    ylim(0, 1) + geom_hline(yintercept = 0.80, linetype = "dashed") +
    geom_hline(yintercept = 0.957, linetype = "dashed") +
    ylab("Coverage probability") + 
    xlab("Recruitment forecast method")
  
  return(list(cplot, as.data.frame(bayes_prob_df)))
}

mase_plot <- function(preds, obs, ts_vec, type){
  fmethod <- length(names(preds))
  ts <- dim(preds[[1]])[1]
  methods <- names(preds)
  
  mase_df <- tibble(year = c(rep(seq(1, ts), fmethod)),
                    method = c(rep(NA, ts*fmethod)),
                    mase = c(rep(NA, ts*fmethod)))
  if(type == 'short'){
    for(i in 1:fmethod){
      mase_df[((ts*i-(ts-1)):(ts*i)), 2] <- c(rep(methods[i], ts))
      mase_df[((ts*i-(ts-1)):(ts*i)), 3] <- sim_mase_short(preds[[i]], obs, ts_vec)
    }
  }else{
    for(i in 1:fmethod){
      mase_df[((ts*i-(ts-1)):(ts*i)), 2] <- c(rep(methods[i], ts))
      mase_df[((ts*i-(ts-1)):(ts*i)), 3] <- sim_mase_long(preds[[i]], obs, ts_vec)
    }
  }
  
  
  plot_mase <- ggplot(mase_df) + geom_line(aes(x = year, y = mase, color = method), alpha = 0.6) + 
    geom_point(aes(x = year, y = mase, color = method)) +
    ylab("Mean absolute scaled error") + xlab("Years added to training set")
  
  return(list(plot_mase, as.data.frame(mase_df)))
}


sim_quants_plots <- function(preds, yrs, obs, type){
  plist <- list()
  fmethod <- length(names(preds))
  methods <- names(preds)
  plot_num <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")
  total_yrs <- length(yrs)
  
  # maxes <- rep(NA, length(preds))
  # mins <- rep(NA, length(preds))
  # for(i in 1:length(preds)){
  #   quants <- apply(preds[[i]][,-1], 1, quantile, probs = c(0.025, 0.975))
  #   maxes[i] <- max(quants[2,])
  #   mins[i] <- min(quants[1,])
  # }
  # plotmax <- max(maxes)
  # plotmin <- min(mins)
  
  for(i in 1:fmethod){
    predsi <- preds[[i]]
    
    quants <- apply(predsi[,-1], 1, quantile, probs = c(0.022, 0.1, 0.5, 0.9, 0.979))
    
    quant_df <- tibble(year = yrs,
                   obs = obs/1000,
                   med_pred = c(rep(NA, (total_yrs - dim(quants)[2])), (quants[3,]/1000)),
                   low_ci1 = c(rep(NA, (total_yrs - dim(quants)[2])), (quants[1,]/1000)),
                   up_ci1 = c(rep(NA, (total_yrs - dim(quants)[2])), (quants[5,]/1000)),
                   low_ci2 = c(rep(NA, (total_yrs - dim(quants)[2])), (quants[2,]/1000)),
                   up_ci2 = c(rep(NA, (total_yrs - dim(quants)[2])), (quants[4,]/1000)))
    
    plist[[i]] <- ggplot(data = quant_df) + geom_line(aes(x = year, y = obs), alpha = 0.5) +
      geom_point(aes(x = year, y = obs)) +
      geom_point(aes(x = year, y = med_pred), color = "#00A1B7") +
      geom_line(aes(x = year, y = med_pred), color = "#00A1B7", alpha = 0.5) +
      geom_ribbon(aes(ymin = low_ci1, ymax = up_ci1, x = year), fill = "#55CFD8", alpha = 0.5, linetype = "dashed") + 
      geom_ribbon(aes(ymin = low_ci2, ymax = up_ci2, x = year), fill = "#898928", alpha = 0.3, linetype = "dashed") + 
      xlab("Year") + ylab("Recruitment ('000s)") + labs(subtitle = paste(plot_num[i], methods[i])) + #ylim(plotmin, plotmax) +
      theme_minimal()
  }
  
  if(type == "short"){
    grid.arrange(grobs = plist, ncol = 2, top = textGrob("1-step Forecasts")) 
  }else{
    grid.arrange(grobs = plist, ncol = 2, top = textGrob("5-step Forecasts"))
  }

}

# yr_trend_plot <- function(preds, obs, ts_vec){
#   plist <- list()
#   fmethod <- length(names(preds))
#   methods <- names(preds)
#   plot_num <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")
#   
#   real_trend <- rollmean(obs[20:length(obs)], 5)
#   
#   for(i in 1:fmethod){
#     sim_trend <- sim_5yr_trend(preds[[i]], ts_vec)
#     
#     trend_df <- tibble(real = real_trend,
#                             med = sim_trend[2,],
#                             low_ci = sim_trend[1,],
#                             up_ci = sim_trend[3,])
#     
#     plist[[i]] <- ggplot(trend_df) + geom_line(aes(x = seq(1, nrow(trend_df)), y = real))+
#       geom_line(aes(x = seq(1, nrow(trend_df)), y = med), color = "blue", alpha = 0.5) + 
#       geom_point(aes(x = seq(1, nrow(trend_df)), y = med), color = "blue") +
#       geom_ribbon(aes(ymin = low_ci, ymax = up_ci, x = seq(1, nrow(trend_df))), fill = "blue", alpha = 0.1) +
#       labs(y = "Five-year rolling average", subtitle = paste(plot_num[i], methods[i]), x = "Years added to training set")
#     
#   }
#   
#   grid.arrange(grobs = plist, ncol = 2)
# }



print_plots <- function(preds1, preds2, obs, yrs, t1, t2){
  print(sim_quants_plots(preds1, yrs, obs, type = "short"))
  print(coverage_prob_plot(preds1)[[1]])
  print(mase_plot(preds1, obs, t1, type = 'short')[[1]])
  print(sim_quants_plots(preds2, yrs, obs, type = "long"))
  print(coverage_prob_plot(preds2)[[1]])
  print(mase_plot(preds1, obs, t1, type = 'long')[[1]])
}


sim_quants_df <- function(preds){
  fmethod <- length(names(preds))
  yrs <- dim(preds[[1]])[1]
  methods <- names(preds)
  
  sim_quants_df <- tibble(method = rep(NA, fmethod*yrs),
                          low_ci = rep(NA, fmethod*yrs),
                          median = rep(NA, fmethod*yrs),
                          up_ci = rep(NA, fmethod*yrs))
  
  for(i in 1:fmethod){
    predsi <- preds[[i]]
    quants <- apply(predsi[,-1], 1, quantile, probs = c(0.025, 0.5, 0.975))
    
    sim_quants_df[((yrs*i-(yrs-1)):(yrs*i)), 1] <- rep(methods[i], yrs)
    sim_quants_df[((yrs*i-(yrs-1)):(yrs*i)), 2] <- quants[1,]
    sim_quants_df[((yrs*i-(yrs-1)):(yrs*i)), 3] <- quants[2,]
    sim_quants_df[((yrs*i-(yrs-1)):(yrs*i)), 4] <- quants[3,]
    
  }
  
  return(as.data.frame(sim_quants_df))
}

# yr_trend_df <- function(preds, ts_vec){
#   fmethod <- length(names(preds))
#   yrs <- length(ts_vec)
#   methods <- names(preds)
#   
#   sim_trend_df <- tibble(method = rep(NA, fmethod*yrs),
#                           low_ci = rep(NA, fmethod*yrs),
#                           median = rep(NA, fmethod*yrs),
#                           up_ci = rep(NA, fmethod*yrs))
#   
#   for(i in 1:fmethod){
#     predsi <- preds[[i]]
#     sim_trend <- sim_5yr_trend(predsi, ts_vec)
#     
#     sim_trend_df[((yrs*i-(yrs-1)):(yrs*i)), 1] <- rep(methods[i], yrs)
#     sim_trend_df[((yrs*i-(yrs-1)):(yrs*i)), 2] <- sim_trend[1,]
#     sim_trend_df[((yrs*i-(yrs-1)):(yrs*i)), 3] <- sim_trend[2,]
#     sim_trend_df[((yrs*i-(yrs-1)):(yrs*i)), 4] <- sim_trend[3,]
#     
#   }
#   
#   return(as.data.frame(sim_trend_df))
# }


save_performance_stats <- function(preds1, preds2, obs, yrs, t1, t2){
  performance_list <- list(short_forecast_quants = sim_quants_df(preds1),
                           short_cov_prob = coverage_prob_plot(preds1)[[2]],
                           mase_short = mase_plot(preds1, obs, t1, type = 'short')[[2]],
                           long_forecast_quants = sim_quants_df(preds2),
                           long_cov_prob = coverage_prob_plot(preds2)[[2]],
                           mase_long = mase_plot(preds1, obs, t1, type = 'long')[[2]])
  
  return(performance_list)
}


