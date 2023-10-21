# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggpattern)

make_truck_violin_plot <- function(truck_arrival_data) {
  
  # Read in your data
  truck_arrival <- read.csv(truck_arrival_data, check.names = FALSE)
  
  # Pre-process the data
  truck_arrival <- truck_arrival %>%
    mutate(across(c(`1st Truck Arrival [HH:MM:SS]`, `2nd Truck Arrival [HH:MM:SS]`, `3rd Truck Arrival [HH:MM:SS]`, `4th Truck Arrival [HH:MM:SS]`), \(x) as.POSIXct(x, format="%H:%M:%S"))) %>%
    mutate(across(c(`Start Time [HH:MM:SS]`), \(x) as.POSIXct(x, format="%H:%M:%S"))) %>%
    rowwise() %>%
    mutate(
      FirstTruckTime = as.numeric(`1st Truck Arrival [HH:MM:SS]` - `Start Time [HH:MM:SS]`, units = "mins"),
      SecondTruckTime = as.numeric(`2nd Truck Arrival [HH:MM:SS]` - `Start Time [HH:MM:SS]`, units = "mins"),
      ThirdTruckTime = as.numeric(`3rd Truck Arrival [HH:MM:SS]` - `Start Time [HH:MM:SS]`, units = "mins"),
      FourthTruckTime = as.numeric(`4th Truck Arrival [HH:MM:SS]` - `Start Time [HH:MM:SS]`, units = "mins")
    ) %>%
    select(Seed, Incident, Scenario, FirstTruckTime, SecondTruckTime, ThirdTruckTime, FourthTruckTime)
  
  # Filter data for scenario 2 and 3 separately
  data_scenario2 <- truck_arrival %>% filter(Scenario == 2)
  data_scenario3 <- truck_arrival %>% filter(Scenario == 3)
  
  # Join the two datasets by Incident and Seed to calculate the differences
  joined_data <- left_join(data_scenario2, data_scenario3, by = c("Seed", "Incident"), suffix = c("_scenario2", "_scenario3"))
  
  # Calculate the differences
  joined_data <- joined_data %>%
    mutate(FirstTruckDifference = FirstTruckTime_scenario2 - FirstTruckTime_scenario3,
           SecondTruckDifference = SecondTruckTime_scenario2 - SecondTruckTime_scenario3,
           ThirdTruckDifference = ThirdTruckTime_scenario2 - ThirdTruckTime_scenario3,
           FourthTruckDifference = FourthTruckTime_scenario2 - FourthTruckTime_scenario3)
  
  # Filter out the "Fourth Truck" if it only has three values
  if (sum(!is.na(joined_data$FourthTruckDifference)) <= 3) {
    joined_data$FourthTruckDifference <- NULL
  }
  
  # Transform data into long format for plotting
  long_difference_data <- joined_data %>%
    select(matches("TruckDifference")) %>%
    pivot_longer(cols = everything(), names_to = "TruckNumber", values_to = "TimeDifference") %>%
    drop_na()
  
  # Filter out only relevant Truck Differences and set the thresholds
  long_difference_data <- long_difference_data %>% 
    filter(TruckNumber %in% c("FirstTruckDifference", "SecondTruckDifference", "ThirdTruckDifference"),
           TimeDifference >= -60 & TimeDifference <= 60) 
  
  color_palette <- brewer.pal(3, "Set2")
  
  # Modify X-Axis labels for readability
  readable_labels <- c(
    "FirstTruckDifference" = "First Truck Difference",
    "SecondTruckDifference" = "Second Truck Difference",
    "ThirdTruckDifference" = "Third Truck Difference"
  )
  
  # Create the violin plot without individual data points
  truck_arrival_violin_plot <- ggplot(long_difference_data, aes(x = TruckNumber, y = TimeDifference, fill = TruckNumber)) +
    # Set color palette
    geom_violin(trim = TRUE, outlier.shape = NA) +  # Avoid plotting outliers in violin plots
    geom_point(stat = "summary", fun = "mean", size = 3, shape = 23, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    scale_fill_manual(values = color_palette) +
    scale_x_discrete(labels = readable_labels) +  # Apply the modified X-Axis labels
    labs(title = "Difference in IMT Arrival Times (20 IMTs minus 30 IMTs)",
         x = "Truck Number",
         y = "Difference in Arrival Time (minutes)",
         fill = "Truck Number") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.title = element_text(face = "bold")
    ) +
    annotate("text", x = "FirstTruckDifference", y = 0, label = "No Difference", vjust = 2, color = "black")
  
  return(truck_arrival_violin_plot)
}