library(ggplot2)
library(dplyr)
library(ggbeeswarm)
library(ggsci)


downsample_res_file = '../data/merged_variant_report.tsv' #'\\\\wsl$\\Ubuntu\\home\\hoeps\\turbo_mount_3\\data\\research\\projects\\wolfram\\projects\\downsampling\\merged_variant_report.tsv'

# Needed for all
samples_blacklisted = c('P13-G4','P50-G5','P50-H7')
n_data_per_sample = 31

# Needed for tool-based part
levels_labels <- c("snv" = "SNV/indel", "str" = "STR", "pbsv" = "SV", "hificnv" = "CNV",  
                   "para" = "para_SNV", "para_json" = "para_CNV", "mt" = "Mito_SNV")

levels_labels <- c("snv" = "SNV (DeepVariant)", "str" = "STR (Target)", "pbsv" = "SV (pbsv)", "hificnv" = "CNV (HiFiCNV)",  
                   "para" = "SNV (Paraphase)", "para_json" = "CNV (Paraphase)", "mt" = "SNV_Mito (DeepVariant)")

# Needed for variant-specific part
n_vars_called_orig = 120
n_vars_notool = 4
n_vars_lowq = 3
n_vars_missed = 8
n_vars_callable = n_vars_called_orig + n_vars_missed
n_vars = n_vars_called_orig + n_vars_notool + n_vars_missed


full_df = read.table(downsample_res_file, header=TRUE, sep='\t')
full_df[full_df$found == 'CN-correct','found'] = 'False'
full_df = full_df[!full_df$sample %in% samples_blacklisted,]

###############################
# Tool-based data preparation #
###############################

tool_df = full_df %>% group_by(source) %>% mutate(n_var = n())

# Calculate the percentage of 'True' for each 'X', 'perm', and 'source'
percentage_data <- tool_df %>%
  group_by(X, perm, source) %>%
  summarise(percent_true = sum(found == "True") / sum(found %in% c("True", "False")) * 100) %>%
  ungroup()

# Calculate the medians for each group
median_data <- percentage_data %>%
  group_by(X, source) %>%
  summarise(median_true = median(percent_true)) %>%
  ungroup()

# Get the number of elements per source
source_counts <- tool_df %>%
  group_by(source) %>%
  summarise(count = n()/n_data_per_sample)  # Use n_distinct to count unique combinations

# Ensure the source column is a factor with the desired order and rename
percentage_data$source <- factor(percentage_data$source, 
                                 levels = names(levels_labels), 
                                 labels = levels_labels)
median_data$source <- factor(median_data$source, 
                             levels = names(levels_labels), 
                             labels = levels_labels)
# Create a named vector for facet labels with counts
facet_labels <- setNames(
  paste(levels_labels, "\nn=", source_counts$count[match(names(levels_labels), source_counts$source)], "", sep = ""), 
  levels_labels
)


###############################
# Tool-based plots            #
###############################

jco_colors <- pal_jco()(9)[c(1, 2, 3, 4, 7, 8, 9)]

p1 = ggplot(percentage_data, aes(x = factor(X), y = percent_true, fill = source)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_line(data = median_data, aes(x = factor(X), y = median_true, group = source, color = source), 
            position = position_dodge(width = 0.75)) +
  labs(x = "", y = "% Recovered calls (of 30X-based callset)") +
  theme_minimal() +
  ylim(c(40, 100)) +
  facet_grid(. ~ source, scales = "free_x", space = "free_x", labeller = labeller(source = facet_labels), switch = "x") +
  theme(strip.placement = "outside", strip.background = element_blank(), strip.text.x = element_text(size = 10),
        legend.position = "none") +
  scale_fill_manual(values = jco_colors) +
  scale_color_manual(values = jco_colors)
#   facet_wrap(~ source, scales = "free_x", labeller = labeller(source = facet_labels), strip.position = "bottom") 

# Save p1

###############################
# Variant-based data preparation #
###############################

#Step 1: Summarize the data to check if there is at least one 'found == True' for each combination of variantID, X, and perm
summary_data_var <- full_df %>%
  group_by(variantID, X, perm) %>%
  summarise(found_true = any(found == "True")) %>%
  ungroup()

# Step 2: Calculate the percentage of 'True' per variantID for each X
percentage_data_var <- summary_data_var %>%
  group_by(variantID,X, perm) %>%
  summarise(percent_true = mean(found_true) * 100) %>%
  ungroup()

percentage_data_var$found_once = percentage_data_var$percent_true > 0

xyz = percentage_data_var %>% group_by(X,perm) %>% mutate(n_var_recovered = sum(found_once)) %>% slice(1)


xyz$pct_vars_recovered = (xyz$n_var_recovered / (n_vars)) * 100
# Step 3: Create a boxplot stratified by X values

###############################
# Variant-based plotting      #
###############################

p2 = ggplot(xyz, aes(x = factor(X), y = pct_vars_recovered)) +
  geom_boxplot(fill='grey') +
  labs(x = "X", y = "Percentage of True") +
  theme_bw() +
  ylim(c(70, 100)) +
  stat_summary(fun=median, geom="line", aes(group=1), color="grey", linewidth=1) +
  geom_hline(aes(yintercept = (n_vars_callable/n_vars) * 100), color='red') +
  annotate("text", x = length(unique(xyz$X)), y = (n_vars_callable/n_vars) * 100 + 1,
           label = "Variants for which a caller is implemented", hjust = 0.8, fontface = "bold", color='red') +
  labs(x = 'Subsampling depth',
       y = '% Biological variants identified by ≥1 caller (n=132)',
       title = paste0(n_vars, ' disease-associated variants: variants recovered by at least 1 caller'))


# p3 = ggplot(xyz, aes(x = factor(X), y = n_var_recovered)) +
#   geom_boxplot() +
#   labs(x = "X", y = "Percentage of True") +
#   theme_bw() +
#   ylim(c(0, n_vars)) +
#   geom_hline(aes(yintercept = n_vars_callable), color='red') +
#   annotate("text", x = length(unique(xyz$X)), y = n_vars_callable + 1.5,
#            label = "Variants for which a caller is implemented", hjust = 0.8, fontface = "bold") +
#   labs(x = 'Downsampled depth',
#        y = 'Disease-associated variants discovered',
#        title = paste0(n_vars, ' disease-associated variants: variants recovered by at least 1 caller'))

## Save all of that stuff
ggsave("../res/downsample_tool_based.pdf", p1, width = 10, height = 5, units = "in", dpi = 300)
ggsave("../res/downsample_variant_based.pdf", p2, width = 6, height = 6, units = "in", dpi = 300)
#ggsave("../res/downsample_variant_based_count.pdf", p3, width = 12, height = 6, units = "in", dpi = 300)