# shannon and simpson plot creations

# calculate jags output for each species in Barberspan counts
jags_per_bird = list()

# get dataframe into correct format for jags analysis
for (i in 1:(length(barberspan_counts)-2)){
  bird_name = colnames(barberspan_counts)[2+i]
  abird = as.data.frame(cbind("Year" = barberspan_counts$Year,
                              "Season" = barberspan_counts$Season,
                              "counts" = barberspan_counts[,2+i]))
  
  
  # perform jags analysis on each bird in the dataframe
  jags_per_bird[[i]] <- jags_analysis(as.data.frame(abird))
}

# create function that calculates shannon or simpson index on posterior output
shan_simp <- function(jag_list, season, index){
  
  # create empty dataframe to store exponentiated shannon values
  shan_simp_df <- data.frame('lower' = 0, 'med' = 0, 'upper' = 0)
  
  for(i in 1:26){
    
    # get all year i mu_t values for each species
    jags_species_ayear <- lapply(jags_per_bird, function(x){
      if(season=='summer'){
        return (x$sims.list$mu_t[5000:15000,i]) # get all year i's summer counts for each species
      }else{
        return (x$sims.list$mu_wt[5000:15000,i]) # get all year i's winter counts for each species
      }
    })
    
    # run the diversity() function on the year i df
    shan_simp_df <- rbind(shan_simp_df,
                          quantile(diversity(exp(as.data.frame(jags_species_ayear)),
                                             index = index),
                                   probs = c(0.025,0.5,0.975)))
  }
  
  if (index == 'shannon'){
    shan_simp_df <- exp(shan_simp_df[2:nrow(shan_simp_df),])
  }else{
    shan_simp_df <- shan_simp_df[2:nrow(shan_simp_df),]
  }
  
  return(shan_simp_df)
}


summer_shan = shan_simp(jags_per_bird, 'summer', 'shannon')
winter_shan = shan_simp(jags_per_bird, 'winter', 'shannon')

# shannon index plot for summer
summer_shan = cbind(summer_shan, 'years' = unique(b_counts$Year))
summer_shan_plot = ggplot(summer_shan, aes(x = years)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
  geom_line(aes(y = med, color = 'red')) +
  scale_x_continuous(breaks = seq(min(summer_shan$years),
                                  max(summer_shan$years),
                                  by = 1)) +
  labs(title = 'Exponentiated shannon index for summer',
       y = 'Exponentiated Shannon Index',
       x = 'Years') +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')

# shannon index plot for winter
winter_shan = cbind(winter_shan, 'years' = unique(b_counts$Year))
winter_shan_plot = ggplot(winter_shan, aes(x = years)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
  geom_line(aes(y = med, color = 'red')) +
  scale_x_continuous(breaks = seq(min(winter_shan$years),
                                  max(winter_shan$years),
                                  by = 1)) +
  labs(title = 'Exponentiated shannon index for winter',
       y = 'Exponentiated Shannon Index',
       x = 'Years') +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')

summer_shan_plot
winter_shan_plot

summer_simp = shan_simp(jags_per_bird, 'summer', 'simpson')
winter_simp = shan_simp(jags_per_bird, 'winter', 'simpson')

# simpson index plot for summer
summer_simp = cbind(summer_simp, 'years' = unique(b_counts$Year))
summer_simp_plot <- ggplot(summer_simp, aes(x = years)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
  geom_line(aes(y = med, color = 'red')) +
  scale_x_continuous(breaks = seq(min(summer_simp$years),
                                  max(summer_simp$years),
                                  by = 1)) +
  labs(title = 'Simpson index for summer',
       y = 'Simpson Index',
       x = 'Years') +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')

# simpson index plot for winter
winter_simp = cbind(winter_simp, 'years' = unique(b_counts$Year))
winter_simp_plot <- ggplot(winter_simp, aes(x = years)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
  geom_line(aes(y = med, color = 'red')) +
  scale_x_continuous(breaks = seq(min(winter_simp$years),
                                  max(winter_simp$years),
                                  by = 1)) +
  labs(title = 'Simpson index for winter',
       y = 'Simpson Index',
       x = 'Years') +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')

summer_simp_plot
winter_simp_plot
  
  