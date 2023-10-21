    # Load required libraries
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    
    
    make_truck_arrival_plot <- function(truck_arrival_data) {
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
      
      # Calculate max absolute differences and first truck arrivals count before converting to long format
      # Calculate max absolute differences and first truck arrivals count before converting to long format
      seed_order_data <- joined_data %>%
        group_by(Seed) %>%
        summarise(
          MaxAbsDifference = max(abs(c(FirstTruckDifference, SecondTruckDifference, ThirdTruckDifference, FourthTruckDifference)), na.rm = TRUE),
          FirstTruckArrivals = sum(!is.na(FirstTruckDifference))
        ) %>%
        mutate(SeedLabel = paste(FirstTruckArrivals, Seed, sep = "_")) %>%
        arrange(MaxAbsDifference)
      
      long_difference_data <- joined_data %>%
        select(Seed, Incident, FirstTruckDifference, SecondTruckDifference, ThirdTruckDifference, FourthTruckDifference) %>%
        pivot_longer(cols = ends_with("TruckDifference"), names_to = "TruckNumber", values_to = "TimeDifference")
      
      # Merge the ordered seed data back into the main data frame
      long_difference_data <- left_join(long_difference_data, select(seed_order_data, Seed, SeedLabel), by = "Seed")
      
      long_difference_data$TruckNumber <- factor(long_difference_data$TruckNumber, levels = c("FirstTruckDifference", "SecondTruckDifference", "ThirdTruckDifference", "FourthTruckDifference"))
      
      # Create the scatter plot with added transparency and facet by Truck Number
      truck_arrival_plot <- ggplot(long_difference_data, aes(x = SeedLabel, y = TimeDifference, color = TruckNumber)) +
        geom_point(size = 2, position = position_jitter(width = 0.3, height = 0), alpha = 0.6) +
        labs(title = "Difference in IMT Arrival Times (20 IMTs minus 30 IMTs)",
             y = "Difference in Arrival Time (minutes)",
             x = "First Truck Arrivals_Seed",
             color = "Truck Number") +
        scale_x_discrete(limits = seed_order_data$SeedLabel) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + # Rotate x labels
        facet_wrap(~TruckNumber, scales = "free_y", labeller = labeller(TruckNumber = c(FirstTruckDifference = "First Truck Differences", SecondTruckDifference = "Second Truck Differences", ThirdTruckDifference = "Third Truck Differences", FourthTruckDifference = "Fourth Truck Differences"))) 
      
      return(truck_arrival_plot)
      
    }
