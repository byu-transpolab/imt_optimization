library(tidyverse)
library(RColorBrewer)
library(ggpattern)

write_truck_time_data <- function(truck_csv) {
  truck_time <- read.csv(truck_csv)
  
  truck_time <- truck_time %>%
    select(Seed, Scenario, Average_Time_seconds, Incidents) %>%
    distinct()
  
  truck_time$Average_Time_minutes <- truck_time$Average_Time_seconds / 60
  
  ordered_seeds <- truck_time %>%
    filter(Scenario == "Increased") %>%
    arrange(Average_Time_minutes) %>%
    pull(Seed)
  
  truck_time$Seed <- factor(truck_time$Seed, levels = ordered_seeds)
  
  return(truck_time)
}

make_truck_time_plot <- function(truck_time) {
  time_scenario <- ggplot(truck_time, aes(
    x = Seed, 
    y = Average_Time_minutes, 
    color = as.factor(Scenario),
    fill = as.factor(Scenario),
    size = Incidents
  )) +
    geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.7, shape = 21, color = "black") +  
    labs(
      title = "Average Travel Time Per Scenario per Dispatched Truck",
      x = "Scenario",
      y = "Time per Truck (mins.)",
      color = "Number of Vehicles",
      size = "Incidents",
      fill = "Number of Vehicles"
    ) +
    scale_color_brewer(palette = "Set1", 
                       labels = c("20 Vehicles", "30 Vehicles"), 
                       guide = guide_legend(override.aes = list(size = 6))) +
    scale_fill_brewer(palette = "Set1", 
                      labels = c("20 Vehicles", "30 Vehicles"), 
                      guide = guide_legend(override.aes = list(size = 6))) +
    scale_size_continuous(breaks = c(5, 10, 15, 20), 
                          range = c(2.5, 10)) + 
    theme_light(base_size = 14) + 
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      legend.key = element_blank(),  
      panel.grid.major = element_line(color = "gray80"),  
      panel.grid.minor = element_blank()  
    )
  
  return(time_scenario)
}



####################### Distance Plot #####################

write_truck_distance_data <- function(truck_csv) {
  truck_distance <- read.csv(truck_csv)
  
  # Reduce to unique combinations of Seed, Scenario, Average Time, and Incidents
  truck_distance <- truck_distance %>%
    select(Seed, Scenario, Average_Distance_meters, Incidents) %>%
    distinct()
  
  # Convert the average distance to miles
  truck_distance$Average_Distance_mile <- truck_distance$Average_Distance_meters / 1609.344
  
  # Create an ordered vector of Seeds based on Scenario 3 Average Travel Distances
  ordered_seeds <- truck_distance %>%
    filter(Scenario == "Increased") %>%
    arrange(Average_Distance_mile) %>%
    pull(Seed)
  
  # Reorder Seed factor levels in df_unique based on ordered_seeds
  truck_distance$Seed <- factor(truck_distance$Seed, levels = ordered_seeds)
  
  return(truck_distance)
}


make_truck_distance_plot <- function(truck_distance) {
  distance_scenario<- ggplot(truck_distance, aes(
    x = Seed,
    y = Average_Distance_mile,
    color = as.factor(Scenario),
    fill = as.factor(Scenario),
    size = Incidents)) +
    geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.7, shape = 21, color = "black") +
    labs(
      title = "Average Distance Travel Per Scenario per Dispatched Truck",
      x = "Scenario",
      y = "Distance per Truck (miles)",
      color = "Number of Vehicles",
      size = "Incidents",
      fill = "Number of Vehicles"
    ) +
    scale_color_brewer(palette = "Set2",
                       labels = c("20 Vehicles", "30 Vehicles"),
                       guide = guide_legend(override.aes = list(size = 6))) +
    scale_fill_brewer(palette = "Set2",
                      labels = c("20 Vehicles", "30 Vehicles"),
                      guide = guide_legend(override.aes = list(size = 6))) +
    scale_size_continuous(breaks = c(5, 10, 15, 20),
                          range = c(2.5, 10)) +
    theme_light(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      legend.key = element_blank(),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank()
    )
  return(distance_scenario)
}