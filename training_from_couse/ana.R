library(ggplot2)
data <- read.delim("//c-files/HEAP-EPI/Diplome_gi_Imanalieyva_Amina/nanopore_methylation/modkit/2700060_DMR_stats.csv")
data$percent_m <- as.numeric(data$percent_m)

average_methylation <- c(65, 35)

ggplot(data, aes(x = name, y = percent_m)) + 
  geom_bar(stat = "identity", fill = "gray", alpha = 0.6, width = 0.6) +
  geom_hline(yintercept = average_methylation, color = "blue", linetype = "dashed", size = 0.5) +
  theme_minimal() +
  labs(title = "DMR methylation for  ID=27000120",
       x = "regions",
       y = "%") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  scale_fill_brewer(palette = "Set2") +
  
  theme(
    plot.title = element_text(size = 17),  
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15, color = 'darkgray')    
  )


ggsave(filename = "Percentage of DMR methylation for  ID=27000120.png", plot = p, width = 8, height = 6, dpi = 500)



