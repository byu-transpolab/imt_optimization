install.packages("ggridges")
library(ggridges)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

make_impacted_links_plot <- function(delay_per_seed) {
  
  # Mutate the scenario first
  delay_per_seed <- delay_per_seed %>%
    mutate(Scenario = case_when(
      Scenario == "Incidents" ~ "No IMTs",
      Scenario == "Current" ~ "20 IMTs",
      Scenario == "Increased" ~ "30 IMTs",
      TRUE ~ as.character(Scenario)
    ))
  
  delay_per_seed <- delay_per_seed %>%
    mutate(Seed = case_when(
      Seed == "723" ~ "1",
      Seed == "518" ~ "2",
      Seed == "847" ~ "3",
      Seed == "907" ~ "4",
      Seed == "790" ~ "5",
      Seed == "418" ~ "6",
      Seed == "340" ~ "7",
      Seed == "886" ~ "8",
      Seed == "398" ~ "9",
      Seed == "141" ~ "10",
      Seed == "637" ~ "11",
      Seed == "499" ~ "12",
      Seed == "167" ~ "13",
      Seed == "472" ~ "14",
      Seed == "951" ~ "15",
      Seed == "584" ~ "16",
      Seed == "915" ~ "17",
      Seed == "227" ~ "18",
      Seed == "879" ~ "19",
      Seed == "946" ~ "20",
      TRUE ~ as.character(Seed)
    ))
  
  # Convert Incidents and total_incident_delay to numeric
  delay_per_seed$Incidents <- as.numeric(as.character(delay_per_seed$Incidents))
  delay_per_seed$total_incident_delay <- as.numeric(as.character(delay_per_seed$total_incident_delay))
  
  # Calculate the average incident delay
  delay_per_seed <- delay_per_seed %>%
    mutate(average_incident_delay = total_incident_delay / Incidents)
  
  # Create a unique label for each group
  delay_per_seed <- delay_per_seed %>%
    mutate(group_label = paste(Incidents, Seed, sep = "_"))
  
  # Filter for 'No IMT' scenario before calculating max_delay
  group_order <- delay_per_seed %>%
    filter(Scenario == "No IMTs") %>%
    group_by(Seed) %>%
    summarise(max_delay = max(total_incident_delay, na.rm = TRUE)) %>%
    arrange(-max_delay) %>% 
    pull(Seed)
  
  # Rename scenario to group
  delay_per_seed <- delay_per_seed %>%
    rename(
      "Group" = "Scenario",
      "Seed ID" = "Scenario ID"
    )
  
  # Categorize scenarios based on the number of incidents
  delay_per_seed <- delay_per_seed %>%
    mutate(incident_category = ifelse(Incidents < 14, "Current Incident Frequency", "Increased Incident Frequency"))
  
  # Define custom color shades
  my_colors <- c(
    "Baseline" = "#B0E0E6",  # Powder blue (lightest)
    "30 IMTs" = "#1E90FF",    # Dodger blue (medium)
    "20 IMTs" = "#4169E1",    # Royal blue (distinct from No IMT)
    "No IMTs" = "#000080"     # Navy (darkest)
  )
  
  # Adjust the factor levels for Group to control the plot order
  # Note that 'Baseline' is the first level because we will arrange in descending order
  delay_per_seed$Group <- factor(delay_per_seed$Group, levels = c("Baseline", "No IMTs", "30 IMTs", "20 IMTs"))
  
  # Reorder the data frame based on the Group factor in descending order
  delay_per_seed <- delay_per_seed %>%
    arrange(desc(Group))
  
  # Create the plot
  impacted_links_plot <- ggplot(delay_per_seed, aes(y = factor(Seed, levels = group_order),
                                                    x = pmin(total_incident_delay, 400), 
                                                    color = Group)) +
    geom_point(size = 2.5) + 
    labs(x = "Delay on Incident Links [hours]",
         y = "Scenario ID") +
    # Use custom colors and set legend order
    scale_color_manual(values = my_colors, 
                       breaks = c("Baseline", "30 IMTs", "20 IMTs", "No IMTs")) +        scale_x_continuous(limits = c(NA, 400),
                                                                                                            breaks = seq(0, 400, 100),
                                                                                                            labels = c(as.character(seq(0, 300, 100)), "400+")) +
    facet_wrap(~ incident_category, ncol = 1, scales = "free_y") +  # Facet by incident category
    theme_minimal() +
    theme(panel.spacing = unit(2, "lines")) # Adjust the space between facets
  
  
  return(impacted_links_plot)
}