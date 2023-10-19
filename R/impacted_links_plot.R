install.packages("ggridges")
library(ggridges)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

make_impacted_links_plot <- function(delay_per_seed) {
  
  # Check and handle non-numeric values in Incidents and total_incident_delay
  if(any(!grepl("^[0-9]+(\\.[0-9]+)?$", as.character(delay_per_seed$Incidents)))) {
    warning("Non-numeric values found in 'Incidents' and will be set to NA!")
    delay_per_seed$Incidents[!grepl("^[0-9]+(\\.[0-9]+)?$", as.character(delay_per_seed$Incidents))] <- NA
  }
  if(any(!grepl("^[0-9]+(\\.[0-9]+)?$", as.character(delay_per_seed$total_incident_delay)))) {
    warning("Non-numeric values found in 'total_incident_delay' and will be set to NA!")
    delay_per_seed$total_incident_delay[!grepl("^[0-9]+(\\.[0-9]+)?$", as.character(delay_per_seed$total_incident_delay))] <- NA
  }
  
  # Convert Incidents and total_incident_delay to numeric
  delay_per_seed$Incidents <- as.numeric(as.character(delay_per_seed$Incidents))
  delay_per_seed$total_incident_delay <- as.numeric(as.character(delay_per_seed$total_incident_delay))
  
  # Calculate the average incident delay
  delay_per_seed <- delay_per_seed %>%
    mutate(average_incident_delay = total_incident_delay / Incidents)
  
  # Calculate the order of `Scenario ID` based on the highest delay in the data
  scenario_order <- delay_per_seed %>%
    group_by(`Scenario ID`, `Incidents`) %>%
    summarise(max_delay = max(average_incident_delay, na.rm = TRUE)) %>%
    arrange(-max_delay) %>%  # Reverse the sorting order
    pull(`Scenario ID`)
  
  # Generate the scatter plot with the specified adjustments
  impacted_links_plot <- ggplot(delay_per_seed, aes(y = factor(`Scenario ID`, levels = scenario_order), 
                                                    x = pmin(average_incident_delay, 100),  # Cap values at 100 for plotting
                                                    color = Scenario)) +
    geom_point(size = 3) +  # Increase point size
    labs(title = "Delay by Seed", 
         x = "Average Incident Delay [hours]",  # Adjust x-axis label to reflect the average delay
         y = "Scenario ID") +
    scale_color_brewer(palette = "Set2", 
                       breaks = c("Baseline", "Incidents", "Current", "Increased")) +  # Order of scenarios
    scale_x_continuous(limits = c(NA, 100), 
                       breaks = seq(0, 100, 20),
                       labels = c(as.character(seq(0, 80, 20)), "100+")) +  # X-axis limits, increments, and labeling
    theme_minimal() +
    theme(legend.position = "top") +
    guides(color = guide_legend(order = 1))
  
  return(impacted_links_plot)
}
