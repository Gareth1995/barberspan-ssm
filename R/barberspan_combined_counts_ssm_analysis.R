# This script applies jags analysis to the combined bird counts in barberspan then plots the modelled counts
library(tidyr)
library(dplyr)
library(ggplot2)
library(jagsUI)


# processing data:
# function to ensure the dataset has one year-season pair per count
# contains correct data types
# create log counts column
process_data <- function(bird_df){
  
  # convert to correct data types
  bird_df$counts = as.numeric(bird_df$counts)
  bird_df$Year = as.numeric(bird_df$Year)
  
  # select all rows where season != O
  bird_df = bird_df %>% filter(Season != "O")
  
  # group by year and sum counts
  bird_df = bird_df %>%
    group_by(Year, Season) %>%
    summarise(counts = sum(counts, na.rm = TRUE)) %>%
    ungroup()
  
  # fill in missing year values
  bird_df = bird_df %>% tidyr::complete(Year = min(Year):max(Year), nesting(Season))
  
  # convert any NA values to 0
  bird_df[bird_df==0] <- NA
  
  # convert to log counts for jags
  bird_df <- bird_df %>%
    mutate(logCounts = log(as.numeric(bird_df$counts) + 1))
  
  return (bird_df)
}

# function that applies jags analysis to given bird count data
jags_analysis <- function(bird_df){
  # bird_df = b_counts
  bird_df <- process_data(bird_df)
  
  # get summer and winter count lengths
  summer <- bird_df[which(bird_df$Season == 'S'),]
  winter <- bird_df[which(bird_df$Season == 'W'),]
  
  # data list for jags analysis
  data_jags <- list(summer = summer$logCounts,
                    winter = winter$logCounts,
                    N = nrow(bird_df)/2)
  
  # variables to be tracked
  params <- c('mu_t', 'mu_wt', 'lambda',
              'beta', 'winter', 'summer',
              'q', 'p', 'w', 'eps', 'zeta',
              'tau.alpha', 'tau.e', 'sigma_alpha',
              'sigma_e', 'tau.eps', 'tau.w2', 'sigma_epsilon',
              'sigma_w', 'sigma_zeta', 'tau.zeta')
  
  # running the model
  jag.mod <- jags(data = data_jags,
                  parameters.to.save = params,
                  model.file = 'model/cwac_ssm.jags',
                  n.chains = 3,
                  n.iter = 10000,
                  n.burnin = 5000,
                  n.thin = 1,
                  modules = c('glm','lecuyer', 'dic'),
                  factories = NULL,
                  parallel = T,
                  n.cores = 3,
                  DIC = TRUE,
                  verbose = TRUE)
  
  jag.mod$summary
  return(jag.mod)
}

# function to plot the estimated counts and error bands resulting from the jags analysis
ts_jag_plot <- function(jag.model, bird_df, title){
  
  # get data in correct format
  bird_df <- process_data(bird_df)
  
  # remove first 5000 iterations and calculate the mean, upper and lower bounds
  summer_jag_df <- as.data.frame(jag.model$sims.list$mu_t)[5000:15000,] # remove first 5000 iterations
  sEstimated <- apply(summer_jag_df, MARGIN = 2, mean)
  sLower <- apply(summer_jag_df, MARGIN = 2, quantile, probs = c(0.025))
  sUpper <- apply(summer_jag_df, MARGIN = 2, quantile, probs = c(0.975))
  
  # separating the summer and winter counts
  summer <- bird_df[which(bird_df$Season == 'S'),]
  winter <- bird_df[which(bird_df$Season == 'W'),]
  
  # storing data in data frames
  summerdf <- data.frame(Year = summer$Year,
                         s_estimated = sEstimated,
                         s_counts = summer$logCounts,
                         lower = sLower,
                         upper = sUpper)
  
  
  winter_jag_df <- as.data.frame(jag.model$sims.list$mu_wt)[5000:15000,] # remove first 5000 iterations
  wEstimated <- apply(winter_jag_df, MARGIN = 2, mean)
  wLower <- apply(winter_jag_df, MARGIN = 2, quantile, probs = c(0.025))
  wUpper <- apply(winter_jag_df, MARGIN = 2, quantile, probs = c(0.975))
  
  winterdf <- data.frame(Year = winter$Year,
                         w_estimated = wEstimated,
                         w_counts = winter$logCounts,
                         lower = wLower,
                         upper = wUpper)
  
  
  summerdf <- arrange(summerdf, Year)
  winterdf <- arrange(winterdf, Year)
  
  # plotting the observed and estimated population sizes produced by the state process
  # summer
  summer_plot <- ggplot(summerdf, aes(x = Year, group = 1)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
    geom_line(aes(y = s_estimated, color = "grey1"), lwd = 1, lty = 2) +
    geom_point(aes(y = s_counts, color = "red")) +
    scale_color_identity(guide = "legend",
                         name = "",
                         labels = c("State process", "Log Counts")) +
    labs(title = title,
         subtitle = "Summer",
         y = "log counts",
         x = "") +
    scale_x_discrete("",
                     labels = as.character(bird_df$Year),
                     breaks = as.numeric(bird_df$Year)) +
    
    theme(axis.text.x = element_text(angle = 90),
          axis.text.x.bottom = element_blank(),
          axis.ticks.x = element_blank(),
          text = element_text(size = 10),
          axis.text = element_text(size = 10))
  
  
  # winter
  winter_plot <- ggplot(winterdf, aes(x = Year, group = 1)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
    geom_line(aes(y = w_estimated, color = "gray1"), lwd = 1, lty = 2) +
    geom_point(aes(y = w_counts, color = "blue")) +
    scale_color_identity(guide = "legend",
                         name = "",
                         labels = c("Log Counts", "State Process")) +
    labs(subtitle = "Winter",
         y = "log counts",
         x = "Years") +
    
    scale_x_continuous("Years",
                       labels = as.character(bird_df$Year),
                       breaks = as.numeric(bird_df$Year)) +
    
    theme(axis.text.x = element_text(angle = 90),
          text = element_text(size = 10),
          axis.text = element_text(size = 10))
  
  return(list("summer" = summer_plot,
              "winter" = winter_plot))
}

# load barberspan combined bird counts
load('data/Barberspan_counts.RData')

# sum up each bird count row for each year
b_counts = rowSums(barberspan_counts[3:49], na.rm = T) # sum across rows
b_counts = as.data.frame(cbind(Year = barberspan_counts$Year,
                               Season = barberspan_counts$Season,
                               counts = b_counts)) 

analysis = jags_analysis(b_counts)
model_plot = ts_jag_plot(analysis, b_counts, "Combined Barberspan bird counts")

par(mfrow=c(1,2))
model_plot$summer
model_plot$winter
