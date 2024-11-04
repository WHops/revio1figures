library(ggplot2)
library(dplyr)
library(argparse)

#Parse command line arguments
parser <- ArgumentParser()
parser$add_argument("--in_tsv", help="A tsv file with the following columns: PatientID, VariantID, Class, Method, ReasonLong, Reason. Header required but will be overwritten.", required=TRUE)
parser$add_argument("--out_pdf", help="Output plot as pdf", required=TRUE)
args <- parser$parse_args()



#df = read.table('../../inclusion_table_fixed.tsv', sep='\t', header=T)
df = read.table(args$in_tsv, sep='\t', header=F)

# Load the data
colnames(df) <- c('PatientID', 'VariantID', 'Class', 'Method', 'ReasonLong', 'Reason')

# Group by PatientID and collect all unique reasons per patient
# Group by PatientID and collect all unique reasons per patient
patient_reasons <- df %>%
  group_by(PatientID) %>%
  summarise(Reasons = paste(unique(Reason), collapse = ", ")) %>%
  ungroup()

# Count occurrences of each unique reason combination and unique VariantIDs
reason_summary <- patient_reasons %>%
  count(Reasons) %>%
  rename(Count = n) %>%
  left_join(
    df %>%
      group_by(Reasons = Reason) %>%
      summarise(UniqueVariants = n_distinct(VariantID)),
    by = "Reasons"
  ) %>%
  arrange(desc(Count))

# Reorder the Reasons factor based on Count
reason_summary <- reason_summary %>%
  mutate(Reasons = factor(Reasons, levels = Reasons[order(-Count)]))

# Define a custom color palette
custom_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", 
                   "#FFD92F", "#E5C494", "#B3B3B3", "#1B9E77", "#D95F02")

# Create the donut chart
p <- ggplot(reason_summary, aes(x = 1.5, y = Count, fill = Reasons)) +
  geom_bar(stat = "identity", width = 1) +
  coord_radial(theta = "y", start = 0, end = pi) +
  #xlim(0.5, 2.5) +
  labs(title = "Reasons per Patient") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_fill_manual(values = custom_colors) + 
geom_text(aes(label = paste0(Count, " (", UniqueVariants, " Vars)")),
          position = position_stack(vjust = 0.5), size = 5, color = "black")

# save p to out_pdf as a pdf
ggsave(args$out_pdf, p, width = 6, height = 6)

# Print the path to the output pdf in a verbose way
cat("Output pdf saved to:", args$out_pdf, "\n")










