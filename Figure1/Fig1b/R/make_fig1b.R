
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(argparse)



# Parse command line arguments
parser <- ArgumentParser()
parser$add_argument("--input", help="A space-separated file test-n", required=TRUE)
parser$add_argument("--output", help="Output plot as pdf", required=TRUE)
args <- parser$parse_args()

df <- read.csv(args$input, sep = '\t', header = TRUE)

# Check the data
print(df)

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

p <- ggplot(df, aes(y = reorder(test, n), x = n, fill = test)) +
  geom_bar(stat = "identity", width = 0.7) +
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c25) +
  labs(x = "# Tests required", y="") +
  geom_text(aes(label = n), hjust = 1.5, vjust = 0.4, size = 4, color = "black") +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 1, size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5))

ggsave(args$output, p, width = 6, height = 4)


ggsave(args$output, p, width = 6, height = 4)
