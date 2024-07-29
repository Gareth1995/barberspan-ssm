# This script creates the top 10 most abundant bird species in barberspan plot
library(ggplot2)

# add up all bird counts for each year in Barberspan
load('data/Barberspan_counts.RData')

# get proportions of top 10 most abundant waterbirds
cols = colSums(barberspan_counts[3:49], na.rm = T) # sum across rows
total = sum(cols)
inds = order(cols, decreasing=TRUE)[1:10]
top_10 = cols[inds]
top_10_prop = top_10/total
top_10_df = as.data.frame(top_10_prop)
top_10_df$names = row.names(top_10_df)

# plot bar graph
abundance_plot <- ggplot(as.data.frame(top_10_df), aes(x=names, y=top_10_prop)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, size = 10),
        plot.title = element_text(size = 14),
        axis.title.x.top = element_text(size = 16),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  labs(title = "The top 10 most abundant waterbirds in Barberspan and their proportions\n to the overall abundance at Barberspan",
       y = "proportion",
       x = "waterbird names")
abundance_plot

# Save the plot as a PNG file
# ggsave("img/top_10_abundant_waterbirds.png", plot = abundance_plot)