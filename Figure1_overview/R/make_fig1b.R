# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the data
data_file <- '../data/1b.tsv'
df <- read.csv(data_file, sep = '\t', header = TRUE)

# Check the data
print(df)

# Create the plot
p <- ggplot(df, aes(x = reorder(test, -n), y = n, fill = test)) +
  geom_bar(stat = "identity", width = 0.7) +
  theme_minimal(base_size = 15) +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Test", y = "Number of Cases", title = "Distribution of Cases by Test") +
  geom_text(aes(label = n), hjust = 0.5, vjust = 1.5, size = 4, color = "black") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5))

# Print the plot
print(p)