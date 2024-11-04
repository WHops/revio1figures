# Whops, Oct 31

# Figure 3

library(ggplot2)
library(dplyr)
library(ggbeeswarm)
library(ggsci)

load_and_annotate_table <- function(file_link, samples_blacklisted, levels_labels){
  
  full_df = read.table(downsample_res_file, header=TRUE, sep='\t')
  full_df = full_df[!full_df$sample %in% samples_blacklisted,]
  full_df[full_df$found == 'CN-correct','found'] = 'False'
  
  # Sorry mito, you are leaving us
  full_df[full_df$source == 'mt', 'SD.overlap'] = 0
  full_df[full_df$source == 'mt', 'source'] = 'snv'
  
  snv_df = full_df[full_df$source == 'snv',]
  full_df[(full_df$source == 'snv' & full_df$SD.overlap == 1), 'source'] = 'snv-SD'
  full_df[(full_df$source == 'snv' & full_df$SD.overlap == 0), 'source'] = 'snv-noSD'
  full_df = rbind(full_df, snv_df)
  
  #full_df = full_df[full_df$source != 'snv-SD',]
  full_df = full_df[full_df$source != 'snv',]
  
  return(full_df)
}

calculate_percentage_data <- function(tool_df){
  # Calculate the percentage of 'True' for each 'X', 'perm', and 'source'
  percentage_data <- tool_df %>%
    group_by(X, perm, source) %>%
    summarise(percent_true = sum(found == "True") / sum(found %in% c("True", "False")) * 100) %>%
    ungroup()
  # Ensure the source column is a factor with the desired order and rename
  percentage_data$source <- factor(percentage_data$source, 
                                   levels = names(levels_labels), 
                                   labels = levels_labels)
  
  return(percentage_data)
}

calculate_source_counts <- function(tool_df){
  # Get the number of elements per source
  source_counts <- tool_df %>%
    group_by(source) %>%
    summarise(count = n()/n_data_per_sample)  
  source_counts = rbind(source_counts, c('all', sum(source_counts[source_counts$source != 'snv',]$count)))
  return(source_counts)
}

create_df_to_plot_sources_percentages <- function(tool_df, percentage_data){
  # Step 1: Aggregate percent_true across all sources using tool_df
  all_sources_data <- tool_df %>%
    filter(source != 'snv') %>%
    group_by(X, perm) %>%
    summarise(percent_true = sum(found == "True") / sum(found %in% c("True", "False")) * 100) %>%
    mutate(source = "All Sources")
  
  
  # Step 4: Combine percentage_data with all_sources_data, keeping 'All Sources' first
  combined_data <- bind_rows(all_sources_data, percentage_data)
  
  return(combined_data)
}

sort_combined_data_by_group_median <- function(combined_data){
  median_values <- combined_data %>%
    filter(X == "10X") %>%
    group_by(source) %>%
    summarise(median_true = median(-percent_true))
  
  median_values[median_values$source == 'All Sources', 'median_true'] = -100#-1000
  
  median_values <- median_values %>% arrange(median_true)
  
  # Reorder the source factor levels based on the calculated medians
  combined_data_sorted <- combined_data %>%
    mutate(source = factor(source, levels = median_values$source))
  
  return(combined_data_sorted)
}

get_median_df_for_line_connecting_boxplots <- function(combined_data, sorted_levels){
  # Calculate the medians for each group
  median_data <- combined_data %>%
    group_by(X, source) %>%
    summarise(median_true = median(percent_true)) %>%
    ungroup()
  
  median_data$source <- factor(median_data$source, 
                               levels = (levels_labels), 
                               labels = levels_labels)
  return(median_data)
}

downsample_res_file = 'data/merged_variant_report_with_SD.tsv'

n_permutations = 10
n_depths = 3
n_data_per_sample = (n_permutations * n_depths) + 1 # +1: non-downsampled

samples_blacklisted = c('P13-G4','P50-G5','P50-H7')

sorted_levels = 
  c('All Sources',
    'SV (pbsv)',
    "SNV (DeepVariant)",
    "SNV-noSD (DeepVariant)",
    'SNV-SD (DeepVariant)',
    'CNV (HiFiCNV)',
    'SNV (Paraphase)',
    'STR (TRGT)',
    'CNV (Paraphase)'
  )

levels_labels <- rev(c("snv" = "SNV (DeepVariant)", 
                   'snv-noSD' = "SNV-noSD (DeepVariant)", 
                   'snv-SD' = "SNV-SD (DeepVariant)", 
                   "str" = "STR (TRGT)", 
                   "pbsv" = "SV (pbsv)", 
                   "hificnv" = "CNV (HiFiCNV)",
                   "para" = "SNV (Paraphase)", 
                   "para_json" = "CNV (Paraphase)", 
                   "all" = "All Sources"))

# Outplot parameters

outplot_colors <- c("#808080", "#E69F00", "#56B4E9", "#009E73",
                    "#aba229", "#0072B2", "#D55E00", "#AA9945",
                    "#CC79A7", '#CA3012')


# Data pre-processing #
full_df = load_and_annotate_table(downsample_res_file, samples_blacklisted, levels_labels)
tool_df = full_df %>% group_by(source) %>% mutate(n_var = n())
percentage_data = calculate_percentage_data(tool_df)
source_counts = calculate_source_counts(tool_df)
combined_data_unsort = create_df_to_plot_sources_percentages(tool_df, percentage_data)
combined_data = sort_combined_data_by_group_median(combined_data_unsort)
combined_data <- combined_data %>% mutate(source = factor(source, levels = sorted_levels))
median_data = get_median_df_for_line_connecting_boxplots(combined_data, sorted_levels)
median_data$X <- factor(median_data$X, levels = rev(levels(factor(median_data$X))))
combined_data$X <- factor(combined_data$X, levels = rev(levels(factor(combined_data$X))))

# Plot # 
facet_labels <- setNames(
  paste(levels_labels, "\nn=", 
        source_counts$count[match(names(levels_labels), source_counts$source)]
        , "", sep = ""), 
  levels_labels
)

p1 <- ggplot(combined_data, aes(x = factor(X), y = percent_true, fill = source)) +
      geom_line(data = median_data, aes(x = factor(X), y = median_true, group = source),
                position = position_dodge(width = 0.75)) +
      geom_boxplot(position = position_dodge(width = 0.75)) +
      labs(x = "", y = "% Recovered calls (of 30X-based callset)") +
      facet_grid(. ~ source, scales = "free_x", space = "free_x", 
                 labeller = labeller(source = facet_labels), switch = "x") +
      theme(strip.placement = "outside", strip.text.x = element_text(size = 10),
            legend.position = "none") +
      scale_fill_manual(values = outplot_colors) +
      scale_color_manual(values = outplot_colors) +
      #ylim(c(40, 100)) +
      scale_y_continuous(limits= c(40,100), breaks = seq(40, 100, by = 10)) 


p1










# Numbers #

# Filter data for SNV-SD and SNV-noSD

snv_all_data <- combined_data %>% filter(source == 'SNV (DeepVariant)' )
snv_sd_data <- combined_data %>% filter(source == "SNV-SD (DeepVariant)")
snv_nosd_data <- combined_data %>% filter(source == "SNV-noSD (DeepVariant)")


# Percent down
full_df %>% group_by(X, perm) %>%  filter(source != 'snv') %>% 
  summarise(n_true =  sum(found == "True"), percent_true = sum(found == "True") / sum(found %in% c("True", "False")) * 100) %>%
  group_by(X) %>% summarize(median_true = median(percent_true), median_n_true=median(n_true))

# Percent down
full_df %>% group_by(X, perm, source) %>%  
  summarise(n_true =  sum(found == "True"), percent_true = sum(found == "True") / sum(found %in% c("True", "False")) * 100) %>%
  group_by(X, source) %>% summarize(median_true = median(percent_true), median_n_true=median(n_true)) %>% filter(X=='15X') %>%
  mutate(median_reduction = 100-median_true) %>% arrange(desc(median_reduction))

xxx = full_df %>% group_by(X, perm, source) %>%  
  summarise(n_true =  sum(found == "True"), percent_true = sum(found == "True") / sum(found %in% c("True", "False")) * 100) %>%
  group_by(X, source) %>% summarize(median_true = round(median(percent_true),2))







