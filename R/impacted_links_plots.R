install.packages("ggpattern")

library(dplyr)
library(ggplot2)
library(RColorBrewer)
    
    write_sorted_impacted_links <- function(impacted_links_table) {
      
      # Add the new column 'Delay During Incident Total [hours]' 
      impacted_links_table <- impacted_links_table %>%
        mutate(`Delay During Incident Total [hours]` = `Delay During Incident [hours]` + 
                 `Post-Incident Delay (0-30 mins) [hours]` + 
                 `Post-Incident Delay (30-60 mins) [hours]`)
      
      # Sort the table by Type
      sorted_table <- impacted_links_table[order(impacted_links_table$Type), ]
      
      # Remove duplicates for impacted_links and assign the Group "Impacted"
      impacted_links <- sorted_table[!duplicated(sorted_table[, c("Scenario", "Seed", "Link Id")]), ] %>%
        mutate(Group = "Impacted")
      
      # Update the Group column based on Type for other rows
      impacted_links_table <- impacted_links_table %>%
        mutate(Group = case_when(
          Type == "Incident Link" ~ "Incident Links",
          Type == "Feeder 1" ~ "Feeder 1",
          Type == "Feeder 2" ~ "Feeder 2",
        ))
      
      # Combine the tables
      incident_links_group_table <- bind_rows(impacted_links, impacted_links_table)
      
      return(incident_links_group_table)
    }
    
    
    summarize_impacted_link_table <- function(incident_links_group_table){
      impacted_delay_table <- incident_links_group_table %>%
        filter(Group == "Impacted") %>%
        group_by(Scenario, Seed, Incidents) %>%
        summarise(total_delay_during_incident = sum(`Delay During Incident Total [hours]`, na.rm = TRUE))
    
      # Filter rows where incidents are greater than 14
      increased_incidents <- impacted_delay_table %>%
        filter(Incidents > 14) %>%
        group_by(Scenario) %>%
        summarise(total_delay = sum(total_delay_during_incident, na.rm = TRUE)) %>%
        mutate(incident_frequency = "Increased")
      
      # Filter rows where incidents are 14 or less
      current_incidents <- impacted_delay_table %>%
        filter(Incidents <= 14) %>%
        group_by(Scenario) %>%
        summarise(total_delay = sum(total_delay_during_incident, na.rm = TRUE)) %>%
        mutate(incident_frequency = "Current")
      
      # Combine the tables
      impacted_links_summary <- bind_rows(increased_incidents, current_incidents)
      
      return(impacted_links_summary)
    }
  
    make_impacted_links_plot <- function(impacted_links_summary) {

      ordered_scenarios <- c("Incidents", "Current", "Increased")

      # Set the levels of the factor for the scenario column
      impacted_links_summary$Scenario <- factor(impacted_links_summary$Scenario, levels = ordered_scenarios)

      # Define colors and patterns based on your description
      color_palette <- brewer.pal(n = length(ordered_scenarios), name = "Set2")
      pattern_palette <- c("transparent", "transparent", "transparent", "gray")

      # Create the bar plot
      impacted_links_plot <- ggplot(impacted_links_summary,
                               aes(x= Scenario,
                                   y = total_delay,
                                   fill= Scenario,
                                   pattern = incident_frequency)) +
        geom_bar_pattern(stat="identity",
                         position = position_dodge(width = 0.9),
                         color = "black",  # Border color
                         pattern_density = 0.1) + # Density for the diagonal stripes
        scale_fill_manual(values = color_palette) +
        scale_pattern_manual(values = c(Current = "none", Increased = "stripe")) +
        labs(title = "Impacted Link Total Delay by Scenario",
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