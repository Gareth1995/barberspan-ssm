# This script contains the code to generate the combined bird counts visual
library(tidyr)
library(dplyr)
library(ggplot2)

# add up all bird counts for each year in Barberspan
load('data/Barberspan_counts.RData')

# sum up each bird count row for each year
b_counts = rowSums(barberspan_counts[3:49], na.rm = T) # sum across rows
b_counts = as.data.frame(cbind(Year = barberspan_counts$Year,
                               Season = barberspan_counts$Season,
                               counts = b_counts)) 
b_counts$counts = as.numeric(b_counts$counts)
b_counts$Year = as.numeric(b_counts$Year)

# select all rows where season != O
b_counts = b_counts %>% filter(Season != "O")

# group by year and sum counts
b_counts = b_counts %>%
  group_by(Year, Season) %>%
  summarise(counts = sum(counts)) %>%
  ungroup()

# fill in missing year values
b_counts = b_counts %>% tidyr::complete(Year = min(Year):max(Year), nesting(Season))

# plot summer fluctuation plot
ggplot(b_counts, aes(x=Year)) +
  geom_line(aes(y=counts)) +
  geom_point(aes(y=counts)) +
  facet_grid(rows = vars(Season)) +
  labs(title = "Barberspan Counts",
       y = "Combined Counts") +
  
  scale_x_continuous("Years",
                     labels = as.character(b_counts$Year),
                     breaks = as.numeric(b_counts$Year)) +
  theme(axis.text.x = element_text(angle = 90))
