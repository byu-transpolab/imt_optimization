library(dplyr)
library(ggplot2)
library(RColorBrewer)

make_all_links_plot <- function(delay_table) {
  
  # Summarize total delay for each scenario
  delay_summary <- delay_table %>%
    group_by(scenario) %>%
    summarise(total_delay = sum(total, na.rm = TRUE))
  
  # Adjust total delay values for specific scenarios by dividing by 20
  delay_summary <- delay_summary %>%
    mutate(total_delay = ifelse(scenario %in% c("Incidents", "Current", "Increased"), 
                                total_delay/20, total_delay))
  
  ordered_scenarios <- c("Baseline", "Incidents", "Current", "Increased")
  
  # Set the levels of the factor to your desired order for proper arrangement of the bars
  delay_summary$scenario <- factor(delay_summary$scenario, levels = ordered_scenarios)
  
  color_palette <- brewer.pal(n = length(ordered_scenarios), name = "Set2")
  
  # Create the bar plot
  all_links_plot <- ggplot(delay_summary, aes(x= scenario, 
                                              y = total_delay, 
                                              fill= scenario)) +
    geom_bar(stat="identity") +  # Use identity stat for pre-computed values
    scale_fill_manual(values = color_palette) +  # Manually set colors based on the palette
    labs(title = "Total Delay by Scenario",
         x = "Scenario",
         y = "Total Delay [hours]") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "none"  # No legend required since colors match scenario names
    )
}
