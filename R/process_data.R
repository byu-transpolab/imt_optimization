# Initialize an empty tibble
link_delays <- tibble()

# Set folder path
folder_path <- "link_delay"

# Loop through each CSV file in the folder
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
for (file in file_list) {
  
  # Read CSV file
  temp_data <- read_csv(file)
  
  # Extract the scenario name from the file name
  scenario_id <- sub("^(.*?)(\\.delays_perLink_.*$)", "\\1", basename(file))
  
  # Within the loop, when mutating temp_data:
  temp_data <- temp_data %>%
    mutate(
      `Scenario Id` = scenario_id,
      `Scenario Type` = case_when(
        str_sub(`Scenario Id`, 1, 1) == "0" ~ "baseline",
        str_sub(`Scenario Id`, 1, 1) == "1" ~ "incidents",
        str_sub(`Scenario Id`, 1, 1) == "2" ~ "current",
        str_sub(`Scenario Id`, 1, 1) == "3" ~ "improved",
        TRUE ~ NA_character_
      ),
# Extracting second and third values from filename
    `Incident Num` = as.numeric(unlist(strsplit(scenario_id, "-"))[2]),
    `Seed` = as.numeric(unlist(strsplit(scenario_id, "-"))[3]),
    `Incident Frequency` = case_when(
      `Incident Num` <= 14 ~ "Current Incidents",
      `Incident Num` > 14 ~ "Increased Incidents",
      TRUE ~ NA_character_
    ),
    `Total Delay` = rowSums(.[, -(1:5)], na.rm = TRUE) # Adjusting column indexing due to added columns
  )
  
  # Append to the tibble
  link_delays <- bind_rows(link_delays, temp_data)
}

# Join 'Link Id' from link_delays with 'id' from network_table$links
link_delays <- link_delays %>%
  left_join(
    network_table$links %>%
      mutate(id = as.double(id)) %>%
      select(id, type),
    by = c("Link Id" = "id")
  ) %>%
  rename(`Link Type` = type) %>%
  select(`Scenario Id`, `Seed`, `Incident Frequency`, `Scenario Type`, `Link Type`, `Link Id`, `Total Delay`, everything())

# Show the resulting tibble
print(link_delays)

# Summarize the total delay by Link Type and Scenario Type
summarized_data <- link_delays %>%
  group_by(`Link Type`, `Scenario Type`) %>%
  summarise(`Total Delay` = sum(`Total Delay`, na.rm = TRUE) / n_distinct(`Seed`)) %>% 
  mutate(`Total Delay` = `Total Delay` / 3600) %>% # Converting to hours
  ungroup()

# Bar plot using ggplot
link_plot <- ggplot(summarized_data, aes(x = `Link Type`, y = `Total Delay`, fill = `Scenario Type`)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Total Delay by Link Type, Scenario Type and Incident Frequency",
       y = "Total Delay [hours]",
       x = "Link Type",
       fill = "Scenario & Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter the summarized data to include only 'motorway' and 'motorway-link' types
motorway_data <- summarized_data %>%
  filter(`Link Type` %in% c('motorway', 'motorway-link'))

# Bar plot using ggplot for the filtered data
motorway_plot <- ggplot(motorway_data, aes(x = `Link Type`, y = `Total Delay`, fill = `Scenario Type`)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Total Delay by Link Type and Scenario Type for Motorways",
       y = "Total Delay [hours]",
       x = "Link Type",
       fill = "Scenario Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


