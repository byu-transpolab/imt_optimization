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
    
    # Mutate the scenario first
    motorway_delay_table <- motorway_delay_table %>%
      mutate(scenario = case_when(
        scenario == "Current" ~ "20 IMT",
        scenario == "Increased" ~ "30 IMT",
        TRUE ~ as.character(scenario)
      ))
    
    # Filter rows where incidents are greater than 14
    increased_incidents <- motorway_delay_table %>%
      filter(incidents > 14) %>%
      group_by(scenario) %>%
      mutate(incident_frequency = "Increased")
    
    # Filter rows where incidents are 14 or less
    current_incidents <- motorway_delay_table %>%
      filter(incidents <= 14) %>%
      group_by(scenario) %>%
      mutate(incident_frequency = "Current")

    # Combine the tables
    motorway_delay_summary <- bind_rows(increased_incidents, current_incidents)
    
    # Create the group column
    motorway_delay_summary$group <- ifelse(motorway_delay_summary$scenario == "Baseline",
                                           "Baseline",
                                           paste(motorway_delay_summary$scenario, 
                                                 motorway_delay_summary$incident_frequency, sep = " "))
    
    
    return (motorway_delay_summary)
  }
  
  write_motorway_comparison_table <- function(motorway_delay_summary){
    
    # Group by scenario and incident frequency
    motorway_summary_table <- motorway_delay_summary %>%
      mutate(incident_frequency = if_else
             (scenario == "Baseline", "-", incident_frequency)) %>%
      group_by(scenario, incident_frequency) %>%
      summarise(average_delay = sum(total_delay, na.rm = TRUE) / n()) %>%
      # Rename columns
      rename(
        "Group" = "scenario",
        "Incident Frequency" = "incident_frequency",
        "Average VHD" = "average_delay"
      ) %>%
      # Round the Average VHD to the nearest whole number
      mutate(`Average VHD` = round(`Average VHD`))
    
    # Create the "VHD Change (%)" column
    baseline_vhd <- motorway_summary_table$`Average VHD`[motorway_summary_table$Group == "Baseline"]
    
    # Calculate Percent difference
    motorway_summary_table$`Change (%)` <- ((motorway_summary_table$`Average VHD` - baseline_vhd) / baseline_vhd) * 100
    
    # Round the 'Change (%)' to the first decimal point
    motorway_summary_table <- motorway_summary_table %>%
      mutate(`Change (%)` = round(`Change (%)`, 1))
    
    # Reorder columns for better readability
    motorway_summary_table <- motorway_summary_table %>% 
      select(Group, `Incident Frequency`, `Average VHD`, `Change (%)`)
    
    # Reorder the rows based on custom ordering of Group and Incident Frequency
    motorway_summary_table <- motorway_summary_table %>%
      arrange(
        factor(Group, levels = c("Baseline", "Incidents", "Current", "Increased")),
      )
    
    return (motorway_summary_table)
  }

  make_motorway_links_plot <- function(motorway_delay_summary) {
    
    # Filter out 'Baseline' and reorder the factor levels for 'group'
    plot_data <- motorway_delay_summary %>%
      filter(group != "Baseline") %>%
      mutate(group = factor(group, levels = c("Incidents Current", "Incidents Increased",
                                              "20 IMT Current", "20 IMT Increased", 
                                              "30 IMT Current", "30 IMT Increased")))
    
    # Compute the mean of the 'total_delay' for the 'Baseline' group
    baseline_value <- motorway_delay_summary %>%
      filter(group == "Baseline") %>%
      summarize(mean(total_delay)) %>%
      pull()
    
    # Create the violin plot
    motorway_links_plot <- ggplot(plot_data, aes(x = group, y = total_delay, fill = group)) +
      geom_violin(trim = FALSE, scale = "width") +
      # Add a horizontal reference line for the baseline value
      geom_hline(aes(yintercept = baseline_value), linetype = "dashed", color = "black", linewidth = 1.2) +
      # Overlay points that show the mean of each group
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", position = position_dodge(width = 0.75)) +
      # Set colors for the fill of each violin plot
      scale_fill_manual(values = brewer.pal(n = 6, name = "Set2"), guide = "none") +
      
      # Modify the y-axis breaks and labels
      scale_y_continuous(
        breaks = function(b) {
          breaks <- pretty(b)
          breaks[which.min(abs(breaks - baseline_value))] <- baseline_value
          if (20000 %in% breaks == FALSE) {
            breaks <- c(breaks, 20000) # adding 20000 to the breaks if not already present
            breaks <- sort(breaks)    # sorting to ensure proper order
          }
          breaks
        },
        labels = function(b) {
          ifelse(b == baseline_value, "Baseline", format(round(b), big.mark = "", scientific = FALSE))
        }
      ) +
      
      
      # Set the axis titles
      labs(x = "Group", y = "Delay [hours]") +
      
      # Set theme adjustments
      theme_minimal() +
      theme(
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
      )
    
    return(motorway_links_plot)
  }
  