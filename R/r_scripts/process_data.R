library(tidyverse)
library(stringr)

# Set folder path
folder_path <- "C:\\Users\\djarvis3\\Box\\MATSim_Analysis\\Link_Analysis\\Delay\\All Links"

# Function to extract scenario details from the filename
extract_scenario_details <- function(filename) {
  scenario_id <- str_extract(basename(filename), "\\d+-\\d+-\\d+")
  
  tibble(
    `Scenario Id` = scenario_id,
    `Scenario Type` = factor(  # Convert to factor for specific order
      case_when(
        str_sub(scenario_id, 1, 1) == "0" ~ "baseline",
        str_sub(scenario_id, 1, 1) == "1" ~ "incidents",
        str_sub(scenario_id, 1, 1) == "2" ~ "current",
        str_sub(scenario_id, 1, 1) == "3" ~ "increased",
        TRUE ~ NA_character_
      ),
      levels = c("baseline", "incidents", "current", "increased")  # Set order
    ),
    `Incident Num` = as.numeric(str_extract(scenario_id, "\\d+")),
    `Seed` = as.numeric(str_extract(scenario_id, "-(\\d+)$"))
  )
}

# Read and process each file
link_delays <- list.files(path = folder_path, pattern = "*.csv", recursive = TRUE, full.names = TRUE) %>% 
  map_dfr(function(file) {
    # Read and process file data
    read_csv(file, show_col_types = FALSE) %>%
      select(`Link Id`, `Total Delay` = `Total Delay [hours]`) %>%
      bind_cols(extract_scenario_details(file))
  }) %>%
  left_join(network_table$links %>%
              mutate(id = as.double(id)) %>%
              select(id, type),
            by = c("Link Id" = "id")) %>%
  mutate(
    `Link Type` = ifelse(is.na(`type`), "Unknown", `type`), # Handle missing Link Type
    `Incident Frequency` = case_when(
      `Incident Num` <= 14 ~ "Current Incidents",
      `Incident Num` > 14 ~ "Increased Incidents",
      TRUE ~ NA_character_
    )
  )

# Summarize the total delay by Link Type and Scenario Type
summarized_data <- link_delays %>%
  group_by(`Link Type`, `Scenario Type`) %>%
  summarise(`Total Delay` = sum(`Total Delay`, na.rm = TRUE) / n_distinct(`Seed`)) %>% 
  ungroup()

# ... [Rest of the script for link_plot]

# Filter the summarized data to include only 'motorway' type
motorway_data <- summarized_data %>%
  filter(`Link Type` %in% c('motorway'))

# Beautified Bar plot using ggplot for the filtered data, with interaction fill
motorway_plot <- ggplot(motorway_data, aes(x = `Scenario Type`, y = `Total Delay`, fill = `Scenario Type`)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + 
  labs(title = "Total Delay by Link Type for Motorways",
       y = "Total Delay",
       x = "Scenario Type",
       fill = "Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "top",
        legend.text = element_text(size = 12))

# Violin plot for motorway data
motorway_violin_plot <- ggplot(motorway_data, aes(x=`Link Type`, y=`Total Delay`, fill = `Scenario Type`)) +
  geom_violin(scale="width") +
  geom_line(aes(x=`Link Type`, y=`Total Delay`), motorway_data %>% filter(`Scenario Type` == "baseline"))

#############################

# Group the data by Link Type, Scenario Type, and Seed
group_data <- link_delays %>%
  group_by(`Link Type`, `Scenario Type`, `Seed`) %>%
  summarise(`Total Delay` = sum(`Total Delay`, na.rm = TRUE) / n_distinct(`Seed`))

# Bar plot using ggplot broken down by Seed and Scenario Type
link_plot <- ggplot(group_data, aes(x = `Link Type`, y = `Total Delay`, fill = `Scenario Type`)) +
  geom_point(stat = "identity", position = "dodge") +
  facet_wrap(~ `Seed`, scales = "free", ncol = 3) +
  labs(title = "Total Delay by Link Type, Scenario Type, and Seed",
       y = "Total Delay",
       x = "Link Type",
       fill = "Scenario & Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter the summarized data to include only 'motorway' type
motorway_data <- group_data %>%
  filter(`Link Type` %in% c('motorway'))

# Bar plot using ggplot for the filtered data, with interaction fill and broken down by Seed and Scenario Type
motorway_plot <- ggplot(motorway_data, aes(x = `Link Type`, y = `Total Delay`, color = `Scenario Type`)) +
  geom_point(stat = "identity", position = "dodge") +
  facet_wrap(~ `Seed`, scales = "free", ncol = 3) +
  labs(title = "Total Delay by Link Type for Motorways",
       y = "Total Delay",
       x = "Link Type",
       fill = "Scenario & Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Don't love this plot but is a violin-ish plot broken down by Seed and Scenario Type
motorway_violin_plot <- ggplot(motorway_data, aes(x=`Link Type`, y=`Total Delay`, fill = `Scenario Type`)) +
  geom_violin(scale="width") +
  geom_line(aes(x=`Link Type`, y=`Total Delay`), motorway_data %>% filter(`Scenario Type` == "baseline")) +
  facet_wrap(~ `Seed` + `Scenario Type`, scales = "free", ncol = 3)


