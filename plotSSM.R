plotSSM <- function(mcmc_sample, time_vec, obs_vec = NULL,
                    state_name, graph_title, y_label,
                    date_labels = "%Y-%m"){

  result_df <- data.frame(t(apply(
    X = mcmc_sample[[state_name]],
    MARGIN = 2, quantile, probs = c(0.025, 0.5, 0.975)
  )))
  
  colnames(result_df) <- c("lwr", "fit", "upr")
  
  result_df$time <- time_vec
  
  if(!is.null(obs_vec)){
    result_df$obs <- obs_vec
  }
  
  p <- ggplot(data = result_df, aes(x = time)) + 
    labs(title = graph_title) +
    ylab(y_label) +
    geom_line(aes(y = fit), size = 1.2) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + 
    scale_x_datetime(date_labels = date_labels)
  
  if(!is.null(obs_vec)){
    p <- p + geom_point(alpha = 0.6, size = 0.9, 
                        data = result_df, aes(x = time, y = obs))
  }
  
  return(p)
}