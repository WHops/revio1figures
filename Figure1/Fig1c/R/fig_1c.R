library(ggplot2)
library(dplyr)
library(argparse)

#Parse command line arguments
parser <- ArgumentParser()
parser$add_argument("--in_tsv", help="A tsv file with the following columns: PatientID, VariantID, Class, Method, ReasonLong, Reason. Header required but will be overwritten.", required=TRUE)
parser$add_argument("--out_pdf", help="Output plot as pdf", required=TRUE)
args <- parser$parse_args()

df = read.table(args$in_tsv, sep='\t', header=F)


#df = read.table('../../data/derived_from_s1/table_inclusion_methods_simplified.tsv', sep='\t', header=T)

colnames(df) = c('PatientID', 'VariantID', 'Class', 'Method', 'ReasonLong', 'Reason')

# Group by Reason and Method and count occurrences
df_summary <- df %>%
  group_by(Reason, Method) %>%
  summarise(Count = n()) %>%
  ungroup()

# Reorder the Reason factor based on the total count and reverse the order
df_summary <- df_summary %>%
  group_by(Reason) %>%
  mutate(TotalCount = sum(Count)) %>%
  ungroup() %>%
  arrange(desc(TotalCount)) %>%
  mutate(Reason = factor(Reason, levels = rev(unique(Reason))))

# Define the order for Method
df_summary <- df_summary %>%
  mutate(Method = factor(Method, levels = rev(c(
    "Auto-called:yes, Visual detection:yes",
    "Auto-called:N/A, Visual detection:yes",
    "Auto-called:no, Visual detection:yes",
    "Auto-called:N/A, Visual detection:no",
    "Auto-called:no, Visual detection:no"
  ))))

# Create the stacked bar plot with flipped coordinates and custom colors
p = ggplot(df_summary, aes(x = Reason, y = Count, fill = Method)) +
  geom_bar(stat = "identity", width = 0.8) +  # Adjust width here
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4, color = "white") +
  scale_fill_manual(values = c(

    "Auto-called:yes, Visual detection:yes" = "#009E73",
    "Auto-called:N/A, Visual detection:yes" = '#36cfa5',
    "Auto-called:no, Visual detection:yes" = '#bab300',
    "Auto-called:no, Visual detection:no" = '#D55E00',
    "Auto-called:N/A, Visual detection:no" ="#e67300"
  )) +
  labs(x = "Reason for Inclusion",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  coord_flip()

ggsave(args$out_pdf, p, width = 30, height = 12, unit='cm', device='pdf')
