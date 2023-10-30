install.packages("ggridges")
library(ggridges)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

make_impacted_links_plot <- function(delay_per_seed) {
  
  # Mutate the scenario first
  delay_per_seed <- delay_per_seed %>%
    mutate(Scenario = case_when(
      Scenario == "Current" ~ "20 IMT",
      Scenario == "Increased" ~ "30 IMT",
      TRUE ~ as.character(Scenario)
    ))
  
  # Convert Incidents and total_incident_delay to numeric
  delay_per_seed$Incidents <- as.numeric(as.character(delay_per_seed$Incidents))
  delay_per_seed$total_incident_delay <- as.numeric(as.character(delay_per_seed$total_incident_delay))
  
  # Calculate the average incident delay
  delay_per_seed <- delay_per_seed %>%
    mutate(average_incident_delay = total_incident_delay / Incidents)
  
  # Calculate the order of `Scenario ID` based on the highest delay in the data
  scenario_order <- delay_per_seed %>%
    group_by(`Scenario ID`, `Incidents`) %>%
    summarise(max_delay = max(total_incident_delay, na.rm = TRUE)) %>%
    arrange(-max_delay) %>%  # Reverse the sorting order
    pull(`Scenario ID`)
  
  # Rename scenario to group
  delay_per_seed <- delay_per_seed %>%
    rename(
      "Group" = "Scenario",
      "Seed ID" = "Scenario ID"
    )
  
  # Generate the scatter plot with the specified adjustments
  impacted_links_plot <- ggplot(delay_per_seed, aes(y = factor(`Seed ID`, levels = scenario_order), 
                                                    x = pmin(total_incident_delay, 400),  # Cap values at 400 for plotting
                                                    color = Group)) +
    geom_point(size = 2) +
    labs(x = "Delay on Impacted Links [hours]",
         y = "Seed ID") +
    scale_color_brewer(palette = "Set2", 
                       breaks = c("Baseline", "Incidents", "20 IMT", "30 IMT")) +  # Order of scenarios
    scale_x_continuous(limits = c(NA, 400), 
                       breaks = seq(0, 400, 100),
                       labels = c(as.character(seq(0, 300, 100)), "400+")) +  # X-axis limits, increments, and labeling
    theme_minimal()
  
  return(impacted_links_plot)
}
