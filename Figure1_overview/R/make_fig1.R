library(ggplot2)
library(dplyr)

# Data Processing
df <- data.frame(
  Category = c("Homologous (pseudo) genes", "Repeat locus", "Pseudoautosomal Regions X+Y", 
               "Homopolymer repeat", "Complex structural rearrangement", "Imprinting/Methylation", "Other"),
  Value = c(30, 26, 4, 2, 21, 8, 9),
  Auto_Resolved = c(30 * 0.98, 26 * 0.80, 4 * 1.00, 2 * 1.00, 21 * 0.85, 8 * 1.00, 9 * 0.89),
  Visual_Resolved = c(30 * 0.00, 26 * 0.15, 4 * 0.00, 2 * 0.00, 21 * 0.10, 8 * 0.00, 9 * 0.00)
)

# Round the resolved counts to integers and calculate percentages
df <- df %>%
  mutate(Auto_Resolved = round(Auto_Resolved),
         Visual_Resolved = round(Visual_Resolved),
         Resolved = Auto_Resolved + Visual_Resolved,
         Unresolved = Value - Resolved,
         Auto_Resolved_Percent = Auto_Resolved / Value * 100,
         Visual_Resolved_Percent = Visual_Resolved / Value * 100,
         Unresolved_Percent = Unresolved / Value * 100,
         Category = factor(Category, levels = rev(c("Homologous (pseudo) genes", "Repeat locus", 
                                                    "Pseudoautosomal Regions X+Y", "Homopolymer repeat", 
                                                    "Complex structural rearrangement", "Imprinting/Methylation", 
                                                    "Other"))))

# Visualization: Number of Cases
p1 <- ggplot(df, aes(x = Category)) +
  geom_bar(aes(y = Auto_Resolved + Visual_Resolved + Unresolved, fill = "Unresolved"), stat = "identity", width = 0.7) +
  geom_bar(aes(y = Auto_Resolved + Visual_Resolved, fill = "Visually Resolved"), stat = "identity", width = 0.7) +
  geom_bar(aes(y = Auto_Resolved, fill = "Auto Resolved"), stat = "identity", width = 0.7) +
  #coord_flip() +
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c("Auto Resolved" = "#a6cee3", "Visually Resolved" = "#b2df8a", "Unresolved" = "#fb9a99")) +
  labs(x = "Category", y = "Number of Cases", title = "Distribution of Cases by Category") +
  geom_text(data = df %>% filter(Auto_Resolved > 0), aes(y = Auto_Resolved / 2, label = Auto_Resolved), size = 4, color = "black") +
  geom_text(data = df %>% filter(Visual_Resolved > 0), aes(y = Auto_Resolved + Visual_Resolved / 2, label = Visual_Resolved), size = 4, color = "black") +
  geom_text(data = df %>% filter(Unresolved > 0), aes(y = Auto_Resolved + Visual_Resolved + Unresolved / 2, label = Unresolved), size = 4, color = "black") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"))
p1
# Visualization: Percentage of Cases
p2 <- ggplot(df, aes(x = Category)) +
  geom_bar(aes(y = Auto_Resolved_Percent + Visual_Resolved_Percent + Unresolved_Percent, fill = "Unresolved"), stat = "identity", width = 0.7) +
  geom_bar(aes(y = Auto_Resolved_Percent + Visual_Resolved_Percent, fill = "Visually Resolved"), stat = "identity", width = 0.7) +
  geom_bar(aes(y = Auto_Resolved_Percent, fill = "Auto Resolved"), stat = "identity", width = 0.7) +
  coord_flip() +
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c("Auto Resolved" = "#b2df8a", "Visually Resolved" = "#a6cee3", "Unresolved" = "#fb9a99")) +
  labs(x = "Category", y = "Percentage of Cases", title = "Distribution of Cases by Category") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  geom_text(data = df %>% filter(Auto_Resolved_Percent > 0), aes(y = Auto_Resolved_Percent / 2, label = paste0(round(Auto_Resolved_Percent), "%")), size = 4, color = "black") +
  geom_text(data = df %>% filter(Visual_Resolved_Percent > 0), aes(y = Auto_Resolved_Percent + Visual_Resolved_Percent / 2, label = paste0(round(Visual_Resolved_Percent), "%")), size = 4, color = "black") +
  geom_text(data = df %>% filter(Unresolved_Percent > 0), aes(y = Auto_Resolved_Percent + Visual_Resolved_Percent + Unresolved_Percent / 2, label = paste0(round(Unresolved_Percent), "%")), size = 4, color = "black") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"))

# Print the plots
print(p1)
print(p2)

p3 <- ggplot(df, aes(x = Category)) +
  geom_bar(aes(y = Auto_Resolved + Visual_Resolved + Unresolved, fill = "Unresolved"), stat = "identity", width = 0.7) +
  geom_bar(aes(y = Auto_Resolved + Visual_Resolved, fill = "Visually Resolved"), stat = "identity", width = 0.7) +
  geom_bar(aes(y = Auto_Resolved, fill = "Auto Resolved"), stat = "identity", width = 0.7) +
  #coord_flip() +
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c("Auto Resolved" = "#a6cee3", "Visually Resolved" = "#b2df8a", "Unresolved" = "#fb9a99")) +
  labs(x = "Category", y = "Number of Cases", title = "Distribution of Cases by Category") +
  geom_text(data = df %>% filter(Auto_Resolved > 0), aes(y = Auto_Resolved / 2, label = Auto_Resolved), size = 4, color = "black") +
  geom_text(data = df %>% filter(Visual_Resolved > 0), aes(y = Auto_Resolved + Visual_Resolved / 2, label = Visual_Resolved), size = 4, color = "black") +
  geom_text(data = df %>% filter(Unresolved > 0), aes(y = Auto_Resolved + Visual_Resolved + Unresolved / 2, label = Unresolved), size = 4, color = "black") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"))
p