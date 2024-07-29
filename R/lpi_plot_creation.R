# This script creates the LPI plot using the jags output

# method to calculate LPI from jags output and show resulting plot
LPI <- function(posterior_output, title, counts){
  
  
  # empty data frame to be populated with
  perc_change_matrix = matrix(nrow = nrow(posterior_output), ncol = ncol(posterior_output))
  for (col in 1:ncol(posterior_output)){
    
    for (index in 1:nrow(posterior_output)){
      if(col==1){
        perc_change_matrix[index,col] = posterior_output[index,col]
      }
      else{
        perc_change_matrix[index,col] = ((posterior_output[index,col]-
                                            posterior_output[index,col-1])/
                                           posterior_output[index,col-1])*100
      }
    }
  }
  
  # apply mean to each column of perc_change matrix
  perc_change_matrix = as.data.frame(perc_change_matrix)
  means = apply(perc_change_matrix, 2, mean)
  
  # find 2.5% quantile of each column
  q25 = apply(perc_change_matrix, 2, quantile, probs=c(0.025))
  
  # find 97.5% quantile of each column
  q975 = apply(perc_change_matrix, 2, quantile, probs=c(0.975))
  
  # column bind all 3
  perc_change_df = as.data.frame(cbind(mu=means,
                                       upper=q975,
                                       lower=q25,
                                       years=unique(counts$Year)))
  
  # display the ribbon plot
  lpi_plot <- ggplot(perc_change_df, aes(x = years)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
    geom_line(aes(y = mu, color = "red"), lwd = 1) +
    geom_hline(yintercept=0, linetype = 'dashed') +
    scale_x_continuous(breaks = seq(min(counts$Year), max(counts$Year), by = 1)) +
    labs(title = title, y = "Average yearly percentage change", x = "") +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = 'none')
  
  return(list("lpi_df" = perc_change_df,
              "lpi_plot" = lpi_plot))
}

posterior_output_s = as.data.frame(analysis$sims.list$mu_t)[5000:15000,]
lpiPlot = LPI(posterior_output_s, "Summer LPI", b_counts)

posterior_output_w = as.data.frame(analysis$sims.list$mu_wt)[5000:15000,]
lpiPlot_w = LPI(posterior_output_w, "Winter LPI", b_counts)

lpiPlot$lpi_plot
lpiPlot_w$lpi_plot
