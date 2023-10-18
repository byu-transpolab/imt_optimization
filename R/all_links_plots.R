install.packages("ggpattern")

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpattern)

write_delay_summary_table <- function(delay_table){
  delay_per_seed <- delay_table %>%
    group_by(scenario, seed, incidents) %>%
    summarise(total_delay = sum(total, na.rm = TRUE))
  
  # Filter rows where incidents are greater than 14
  increased_incidents <- delay_per_seed %>%
    filter(incidents > 14) %>%
    group_by(scenario) %>%
    summarise(total_delay = sum(total_delay, na.rm = TRUE) / n()) %>%
    mutate(incident_frequency = "Increased")
  
  # Filter rows where incidents are 14 or less
  current_incidents <- delay_per_seed %>%
    filter(incidents <= 14) %>%
    group_by(scenario) %>%
    summarise(total_delay = sum(total_delay, na.rm = TRUE) / n()) %>%
    mutate(incident_frequency = "Current") %>%
    mutate(incident_frequency = if_else(scenario == "Baseline", "Baseline", incident_frequency))
  
  # Combine the tables
  delay_summary <- bind_rows(increased_incidents, current_incidents)
}

make_all_links_plot <- function(delay_summary) {
  
  ordered_scenarios <- c("Baseline", "Incidents", "Current", "Increased")
  
  # Set the levels of the factor for the scenario column
  delay_summary$scenario <- factor(delay_summary$scenario, levels = ordered_scenarios)
  
  # Define colors and patterns based on your description
  color_palette <- brewer.pal(n = length(ordered_scenarios), name = "Set2")
  pattern_palette <- c("transparent", "transparent", "transparent", "gray")
  
  # Create the bar plot
  all_links_plot <- ggplot(delay_summary, 
                           aes(x= scenario, 
                               y = total_delay, 
                               fill= scenario, 
                               pattern = incident_frequency)) +
    geom_bar_pattern(stat="identity",
                     position = position_dodge(width = 0.9),
                     color = "black",  # Border color
                     pattern_density = 0.1) + # Density for the diagonal stripes
    scale_fill_manual(values = color_palette) +
    scale_pattern_manual(values = c(Baseline = "none", Current = "none", Increased = "stripe")) +
    labs(title = "Total Delay by Scenario",
         x = "Scenario",
         y = "Total Delay [hours]") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "top right",
      legend.title = element_text(face = "bold")
    )
}
