library(dplyr)
library(ggplot2)
library(RColorBrewer)

split_impacted_links <- function(impacted_links_table) {
  
  # Add the new column 'Delay During Incident Total [hours]' 
  impacted_links_table <- impacted_links_table %>%
    mutate(`Delay During Incident Total [hours]` = `Delay During Incident [hours]` + 
             `Post-Incident Delay (0-30 mins) [hours]` + 
             `Post-Incident Delay (30-60 mins) [hours]`)
  
  # For impacted_links
  # First, sort the table by Type so that "Incident Link" is prioritized over "Feeder 1" and "Feeder 1" over "Feeder 2"
  impacted_links_table <- impacted_links_table[order(impacted_links_table$Type), ]
  
  # Remove duplicates based on Scenario, Seed and Link Id
  impacted_links <- impacted_links_table[!duplicated(impacted_links_table[, c("Scenario", "Seed", "Link Id")]), ]
  
  # Incident Links
  Incident_Links <- impacted_links_table[impacted_links_table$Type == "Incident Link", ]
  
  # Feeder 1 Links
  Feeder_1_Links <- impacted_links_table[impacted_links_table$Type == "Feeder 1", ]
  
  # Feeder 2 Links
  Feeder_2_Links <- impacted_links_table[impacted_links_table$Type == "Feeder 2", ]
  
  return(list(impacted_links = impacted_links, 
              Incident_Links = Incident_Links, 
              Feeder_1_Links = Feeder_1_Links, 
              Feeder_2_Links = Feeder_2_Links))
}


# Call the function
result <- split_impacted_links(impacted_links_table)

plot_delay_during_incident <- function(data) {
  ordered_scenarios <- c("Baseline", "Incidents", "Current", "Increased")
  color_palette <- brewer.pal(n = length(ordered_scenarios), name = "Set2")
  
  ggplot(data, aes(x = factor(Scenario, levels = ordered_scenarios), y = `Delay During Incident [hours]`, fill = Scenario)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = color_palette) +
    labs(title = "Total Delay on Impacted Links",
         x = "Scenario",
         y = "Total Delay [hours]") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "none"
    )
}

plot_delay_during_incident(impacted_links)


plot_post_incident_delay <- function(data) {
  ordered_scenarios <- c("Baseline", "Incidents", "Current", "Increased")
  color_palette <- brewer.pal(n = length(ordered_scenarios), name = "Accent")
  ggplot(data, aes(x = factor(Scenario, levels = ordered_scenarios), y = Total_Post_Incident_Delay, fill = Scenario)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(values = color_palette) +
    labs(title = "Total Post-Incident Delay by Scenario",
         x = "Scenario",
         y = "Total Delay [hours]") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "none"
    )
}

# Usage:

path <- "C:/Users/djarvis3/Box/MATSim_Analysis/Link_Analysis/Delay/Impact Links"
all_files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
Impact_Links <- map_dfr(all_files, read_and_add_cols)

# Excluding specific seeds
filtered_data <- Impact_Links %>% 
  filter(!(Seed %in% c(141, 946, 340)))

VHD_summary_incident <- filtered_data %>%
  filter(Type %in% c("Incident", "Feeder 1", "Feeder 2")) %>%
  group_by(Scenario) %>%
  summarize(Delay_During_Incident = sum(`Delay During Incident [hours]`, na.rm = TRUE))

plot_delay_during_incident(VHD_summary_incident)

VHD_summary_post_incident <- filtered_data %>%
  filter(Type %in% c("Incident", "Feeder 1", "Feeder 2")) %>%
  group_by(Scenario) %>%
  summarize(Total_Post_Incident_Delay = sum(`Post-Incident Delay (0-30 mins) [hours]`, na.rm = TRUE) + 
              sum(`Post-Incident Delay (30-60 mins) [hours]`, na.rm = TRUE)) %>%
  arrange(Scenario)

plot_post_incident_delay(VHD_summary_post_incident)