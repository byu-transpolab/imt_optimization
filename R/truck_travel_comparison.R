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
  
  # Mutate the scenario labeling
  truck_time <- truck_time %>%
    mutate(Scenario = case_when(
      Scenario == "Current" ~ "20 IMT",
      Scenario == "Increased" ~ "30 IMT",
      TRUE ~ as.character(Scenario)
    ))
  
  return(truck_time)
}

make_truck_time_plot <- function(truck_time) {
  truck_time_plot <- ggplot(truck_time, aes(x = Seed, y = Average_Time_minutes, size = Incidents)) +
    geom_point(aes(color = as.factor(Scenario), fill = as.factor(Scenario)), 
               position = position_jitter(width = 0.1, height = 0), 
               alpha = 0.7, 
               shape = 21, 
               color = "black") +
    labs(x = "Seed",
         y = "Time per Truck (mins.)",
         color = "Group",
         size = "Incidents",
         fill = "Group")
  return(truck_time_plot)
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
  
  # Mutate the scenario labeling
  truck_distance <- truck_distance %>%
    mutate(Scenario = case_when(
      Scenario == "Current" ~ "20 IMT",
      Scenario == "Increased" ~ "30 IMT",
      TRUE ~ as.character(Scenario)
    ))
  
  return(truck_distance)
}


make_truck_distance_plot <- function(truck_distance) {
  truck_distance_plot <- ggplot(truck_distance, aes(x = Seed, y = Average_Distance_mile, size = Incidents)) +
    geom_point(aes(color = as.factor(Scenario), fill = as.factor(Scenario)), 
               position = position_jitter(width = 0.1, height = 0), 
               alpha = 0.7, 
               shape = 21, 
               color = "black") +
    labs(x = "Seed",
         y = "Distance per Truck (miles)",
         color = "Group",
         size = "Incidents",
         fill = "Group")
  return(truck_distance_plot)
}