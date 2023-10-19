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
    mutate(incident_frequency = "Increased")
  
  # Filter rows where incidents are 14 or less
  current_incidents <- delay_per_seed %>%
    filter(incidents <= 14) %>%
    group_by(scenario) %>%
    mutate(incident_frequency = "Current") %>%
    mutate(incident_frequency = if_else(scenario == "Baseline", "-", incident_frequency))
  
  # Combine the tables
  delay_summary <- bind_rows(increased_incidents, current_incidents)
  
  return (delay_summary)
}

write_all_links_comparison_table <- function(delay_summary){
  # Group by scenario and incident frequency
  delay_summary_table <- delay_summary %>%
    group_by(scenario, incident_frequency) %>%
    summarise(total_delay = sum(total_delay, na.rm = TRUE) / n()) %>%
  # Rename columns
    rename(
      "Scenario" = "scenario",
      "Incident Frequency" = "incident_frequency",
      "Total VHD" = "total_delay"
    )
  
  # Create the "Change In VHD" column
  baseline_vhd <- delay_summary_table$`Total VHD`[delay_summary_table$Scenario == "Baseline"]
  
  # Calculate Absolute difference
  # delay_summary_table$`Change In VHD (Absolute)` <- delay_summary_table$`Total VHD` - baseline_vhd
  
  # Calculate Percent difference
  delay_summary_table$`Change In VHD (Percent)` <- ((delay_summary_table$`Total VHD` - baseline_vhd) / baseline_vhd) * 100
  
  # Reorder columns for better readability
  all_links_comparison_table <- delay_summary_table %>% 
    select(Scenario, `Incident Frequency`, `Total VHD`, `Change In VHD (Percent)`)
  
  # Reorder the rows based on custom ordering of Scenario and Incident Frequency
  all_links_comparison_table <- all_links_comparison_table %>%
    arrange(
      factor(Scenario, levels = c("Baseline", "Incidents", "Current", "Increased")),
      factor(`Incident Frequency`, levels = c("Current", "Increased", "Baseline"))
    )
  
  return (all_links_comparison_table)
}

make_all_links_plot <- function(delay_summary) {
  
  ordered_scenarios <- c("Baseline", "Incidents", "Current", "Increased")
  
  # Set the levels of the factor for the scenario column
  delay_summary$scenario <- factor(delay_summary$scenario, levels = ordered_scenarios)
  
  # Define colors based on your description
  color_palette <- brewer.pal(n = length(ordered_scenarios) - 1, name = "Set2") # -1 because we're excluding Baseline from the plot
  
  # Get the baseline value
  baseline_value <- mean(delay_summary$total_delay[delay_summary$scenario == "Baseline"])
  
  # Exclude Baseline from the delay_summary for plotting
  plot_data <- delay_summary[delay_summary$scenario != "Baseline",]
  
  # Create the violin plot
  all_links_plot <- ggplot(plot_data, 
                           aes(x = scenario, 
                               y = total_delay, 
                               fill = scenario)) +
    geom_violin(trim = FALSE, scale = "width") + # trim=FALSE displays the entire violin
    geom_hline(aes(yintercept = baseline_value), linetype = "dashed", color = "black", size = 1.2) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = color_palette) +
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
    ) +
    annotate("text", x = 1, y = baseline_value - 1000, label = "Baseline", color = "black", size = 4.5, hjust = 1) # Adjust y position (baseline_value - 20) to move text lower
  
  return(all_links_plot)
}


