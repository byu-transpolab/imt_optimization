install.packages("ggpattern")

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpattern)


  write_motorway_summary_table <- function(delay_table){
    motorway_delay_table <- delay_table %>%
      filter(`Link Type` == "motorway") %>%
      group_by(scenario, seed, incidents) %>%
      summarise(total_delay = sum(total, na.rm = TRUE))
    
    # Filter rows where incidents are greater than 14
    increased_incidents <- motorway_delay_table %>%
      filter(incidents > 14) %>%
      group_by(scenario) %>%
      mutate(incident_frequency = "Increased")
    
    # Filter rows where incidents are 14 or less
    current_incidents <- motorway_delay_table %>%
      filter(incidents <= 14) %>%
      group_by(scenario) %>%
      mutate(incident_frequency = "Current") %>%
      mutate(incident_frequency = if_else(scenario == "Baseline", "", incident_frequency))
    
    # Combine the tables
    # The delay on the baseline scenario might be somewhat misleading because it is still running to finish its 450th iteration.
    motorway_delay_summary <- bind_rows(increased_incidents, current_incidents)
    
    # Create the group column
    motorway_delay_summary$group <- paste(motorway_delay_summary$scenario, motorway_delay_summary$incident_frequency, sep = "_")
    
    return (motorway_delay_summary)
  }
  
  write_motorway_comparison_table <- function(motorway_delay_summary){
    # Group by scenario and incident frequency
    motorway_summary_table <- motorway_delay_summary %>%
      group_by(scenario, incident_frequency) %>%
      summarise(total_delay = sum(total_delay, na.rm = TRUE) / n()) %>%
      # Rename columns
      rename(
        "Scenario" = "scenario",
        "Incident Frequency" = "incident_frequency",
        "Total VHD" = "total_delay"
      )
    
    # Create the "Change In VHD" column
    baseline_vhd <- motorway_summary_table$`Total VHD`[motorway_summary_table$Scenario == "Baseline"]
    
    # Calculate Absolute difference
    # motorway_summary_table$`Change In VHD (Absolute)` <- motorway_summary_table$`Total VHD` - baseline_vhd
    
    # Calculate Percent difference
    motorway_summary_table$`Change In VHD (Percent)` <- ((motorway_summary_table$`Total VHD` - baseline_vhd) / baseline_vhd) * 100
    
    # Reorder columns for better readability
    motorway_summary_table <- motorway_summary_table %>% 
      select(Scenario, `Incident Frequency`, `Total VHD`, `Change In VHD (Percent)`)
    
    # Reorder the rows based on custom ordering of Scenario and Incident Frequency
    motorway_summary_table <- motorway_summary_table %>%
      arrange(
        factor(Scenario, levels = c("Baseline", "Incidents", "Current", "Increased")),
        factor(`Incident Frequency`, levels = c("Current", "Increased", "Baseline"))
      )
    
    return (motorway_summary_table)
  }

  make_motorway_links_plot <- function(motorway_delay_summary) {
    
    ordered_groups <- c("Baseline_", "Incidents_Current", "Incidents_Increased", "Current_Current", "Current_Increased", "Increased_Current", "Increased_Increased")
    
    # Set the levels of the factor for the group column
    motorway_delay_summary$group <- factor(motorway_delay_summary$group, levels = ordered_groups)
    
    # Define colors based on your description
    color_palette <- brewer.pal(n = length(ordered_groups) - 1, name = "Set2")
    
    # Get the baseline value
    baseline_value <- mean(motorway_delay_summary$total_delay[motorway_delay_summary$group == "Baseline_"])
    
    # Exclude Baseline_Baseline from the motorway_delay_summary for plotting
    plot_data <- motorway_delay_summary[motorway_delay_summary$group != "Baseline_",]
    
    # Create the violin plot
    motorway_links_plot <- ggplot(plot_data, 
                                  aes(x = group, 
                                      y = total_delay, 
                                      fill = group)) +
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
      annotate("text", x = 1, y = baseline_value - 500, label = "Baseline", color = "black", size = 4.5, hjust = 1) # Adjust y position (baseline_value - 20) to move text lower
    
    return(motorway_links_plot)
  }
