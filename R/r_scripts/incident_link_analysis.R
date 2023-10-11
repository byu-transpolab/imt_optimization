install.packages("tidyverse")
library(tidyverse)

read_and_add_cols <- function(file_path) {
  
  # Extract seed from directory name using regex
  seed <- as.numeric(str_extract(dirname(file_path), "(?<=Seed )[0-9]+"))
  
  scenario_code <- as.numeric(str_extract(basename(file_path), "^[0-9]"))
  
  scenario <- case_when(
    scenario_code == 0 ~ 'Baseline',
    scenario_code == 1 ~ 'Incidents',
    scenario_code == 2 ~ 'Current',
    scenario_code == 3 ~ 'Increased',
    TRUE ~ NA_character_
  )
  
  # Extract the ID from the filename without the .delay.csv extension
  file_name <- basename(file_path)
  id_code <- str_extract(file_name, ".*(?=\\.delay)")
  
  # If the ID code is "0-0-000", change it to "base_###" format
  if (id_code == "0-0-000") {
    id_code <- paste0("base_", seed)
  }
  
  data <- read_csv(file_path)
  
  data %>%
    mutate(Seed = seed, Scenario = scenario, ID = id_code) %>%
    select("Scenario", "Seed", "Delay During Incident [hours]", "Link Id", "Type", everything())
}

path <- "C:/Users/djarvis3/Box/MATSim_Analysis/Link_Analysis/Delay/Impact Links"

all_files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

Impact_Links <- map_dfr(all_files, read_and_add_cols)

# Install and load the RColorBrewer package
install.packages("RColorBrewer")
library(RColorBrewer)

# Excluding specific seeds
filtered_data <- Impact_Links %>% 
  filter(!(Seed %in% c(141, 946, 340)))

# Summary for Delay During Incident 
VHD_summary_incident <- filtered_data %>%
  filter(Type %in% c("Incident", "Feeder 1", "Feeder 2")) %>%
  group_by(Scenario) %>%
  summarize(Delay_During_Incident = sum(`Delay During Incident [hours]`, na.rm = TRUE))

# Define a more muted color palette and ordering for scenarios
ordered_scenarios <- c("Baseline", "Incidents", "Current", "Increased")
color_palette <- brewer.pal(n = length(ordered_scenarios), name = "Set2")

# Plot for Delay During Incident with adjusted colors and scenario order
ggplot(VHD_summary_incident, aes(x = factor(Scenario, levels = ordered_scenarios), y = Delay_During_Incident, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = color_palette) + # Use the more muted color palette
  labs(title = "Total Delay During Incident by Scenario",
       x = "Scenario",
       y = "Total Delay [hours]") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none" # Hide legend since colors correspond to x-axis labels
  )


# Summary for Post-Incident Delays
VHD_summary_post_incident <- filtered_data %>%
  filter(Type %in% c("Incident", "Feeder 1", "Feeder 2")) %>%
  group_by(Scenario) %>%
  summarize(Total_Post_Incident_Delay = sum(`Post-Incident Delay (0-30 mins) [hours]`, na.rm = TRUE) + 
              sum(`Post-Incident Delay (30-60 mins) [hours]`, na.rm = TRUE)) %>%
  arrange(Scenario)

# Define a more muted color palette and ordering for scenarios
ordered_scenarios <- c("Baseline", "Incidents", "Current", "Increased")
color_palette <- brewer.pal(n = length(ordered_scenarios), name = "Accent")

# Plot for Total Post-Incident Delay with adjusted colors and scenario order
ggplot(VHD_summary_post_incident, aes(x = factor(Scenario, levels = ordered_scenarios), y = Total_Post_Incident_Delay, fill = Scenario)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = color_palette) + # Use the more muted color palette
  labs(title = "Total Post-Incident Delay by Scenario",
       x = "Scenario",
       y = "Total Delay [hours]") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none" # Hide legend since colors correspond to x-axis labels
  )
