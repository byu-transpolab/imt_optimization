

write_delay_summary_table <- function(delay_table){
  delay_per_seed <- delay_table %>%
    group_by(scenario, seed, incidents) %>%
    summarise(total_delay = sum(total, na.rm = TRUE))
  
  # Mutate the scenario first
  delay_per_seed <- delay_per_seed %>%
    mutate(scenario = case_when(
      scenario == "Incidents" ~ "No IMT",
      scenario == "Current" ~ "20 IMT",
      scenario == "Increased" ~ "30 IMT",
      TRUE ~ as.character(scenario)
    ))
  
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
    summarise(average_delay = sum(total_delay, na.rm = TRUE) / n(), .groups = 'drop') %>%
    # Rename columns
    rename(
      "Group" = "scenario",
      "Incident Frequency" = "incident_frequency",
      "Average VHD" = "average_delay"
    ) %>%
    # Round the Average VHD to the nearest whole number
    mutate(`Average VHD` = round(`Average VHD`))
  
  # Create the "VHD Change (%)" column
  baseline_vhd <- delay_summary_table$`Average VHD`[delay_summary_table$Group == "Baseline"]
  
  # Calculate Percent difference
  delay_summary_table <- delay_summary_table %>%
    mutate(`Change (%)` = ((`Average VHD` - baseline_vhd) / baseline_vhd) * 100)
  
  # Round the 'Change In VHD (Percent)' to the first decimal point
  delay_summary_table <- delay_summary_table %>%
    mutate(`Change (%)` = round(`Change (%)`, 1))
  
  # Reorder columns for better readability
  all_links_comparison_table <- delay_summary_table %>% 
    select(Group, `Incident Frequency`, `Average VHD`, `Change (%)`)
  
  # Reorder the rows based on custom ordering of Scenario and Incident Frequency
  all_links_comparison_table <- all_links_comparison_table %>%
    arrange(
      factor(Group, levels = c("Baseline", "No IMT","20 IMT", "30 IMT"))
    )
  
  return (all_links_comparison_table)
}


# This function creates a violin plot for delay summaries, excluding the baseline from the plot but showing it as a reference line.
make_all_links_plot <- function(delay_summary) {
  
  # Combine 'scenario' and 'incident_frequency' into a new 'group' factor
  plot_data <- delay_summary %>%
    filter(scenario != "Baseline") %>%
    mutate(group = interaction(scenario, incident_frequency, sep = " "),
           group = factor(group, levels = c("No IMT Current", "No IMT Increased",
                                            "20 IMT Current", "20 IMT Increased", 
                                            "30 IMT Current", "30 IMT Increased")))
  
  # Compute the mean of the 'total_delay' for the 'Baseline' scenario
  baseline_value <- delay_summary %>%
    filter(scenario == "Baseline") %>%
    summarize(mean(total_delay)) %>%
    pull()
  
  # Create the violin plot
  all_links_plot <- ggplot(plot_data, aes(x = group, y = total_delay, fill = group)) +
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
        breaks
    }, 
    labels = function(b) {
      ifelse(b == baseline_value, "Baseline", format(round(b), big.mark = "", scientific = FALSE))
    }
  ) +
    
    # Set the axis titles
    labs(x = "Group", y = "Delay [hours]") +
    theme_minimal() +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
    )
      
  # Since we are no longer faceting, remove the facet_wrap line
  return(all_links_plot)
}
