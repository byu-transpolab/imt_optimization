install.packages("ggridges")
library(ggridges)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

  make_impacted_links_plot <- function(delay_per_seed) {
  
    # Calculate the order of `Scenario ID` based on the highest delay in the data
    scenario_order <- delay_per_seed %>%
      group_by(`Scenario ID`) %>%
      summarise(max_delay = max(total_incident_delay)) %>%
      arrange(-max_delay) %>%  # Reverse the sorting order
      pull(`Scenario ID`)
    
    # Generate the scatter plot with the specified adjustments
    impacted_links_plot <- ggplot(delay_per_seed, aes(y = factor(`Scenario ID`, levels = scenario_order), 
                               x = pmin(total_incident_delay, 500),  # Cap values at 500 for plotting
                               color = Scenario)) +
      geom_point(size = 3) +  # Increase point size
      labs(title = "Delay by Seed", 
           x = "Total Incident Delay [hours]", 
           y = "Scenario ID") +
      scale_color_brewer(palette = "Set2", 
                         breaks = c("Baseline", "Incidents", "Current", "Increased")) +  # Order of scenarios
      scale_x_continuous(limits = c(NA, 500), 
                         breaks = seq(0, 500, 100),
                         labels = c(as.character(seq(0, 400, 100)), "500+")) +  # X-axis limits, increments, and labeling
      theme_minimal() +
      theme(legend.position = "top") +
      guides(color = guide_legend(order = 1))
 
      return(impacted_links_plot)
  }