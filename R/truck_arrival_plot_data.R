write_truck_arrival_plot_data <- function(truck_arrival_csv) {
  # Read and pre-process the data
  truck_arrival <- read.csv(truck_arrival_csv, check.names = FALSE) %>%
    mutate(across(ends_with("[HH:MM:SS]"), ~as.POSIXct(.x, format = "%H:%M:%S"))) %>%
    rowwise() %>%
    mutate(across(starts_with("1st Truck Arrival"), ~as.numeric(. - `Start Time [HH:MM:SS]`, units = "mins"), .names = "FirstTruckTime"),
           across(starts_with("2nd Truck Arrival"), ~as.numeric(. - `Start Time [HH:MM:SS]`, units = "mins"), .names = "SecondTruckTime"),
           across(starts_with("3rd Truck Arrival"), ~as.numeric(. - `Start Time [HH:MM:SS]`, units = "mins"), .names = "ThirdTruckTime")) %>%
    select(Seed, id, Incident, Scenario, FirstTruckTime, SecondTruckTime, ThirdTruckTime)
  
  # Filter data for scenarios 2 and 3
  data_scenario2 <- truck_arrival %>% filter(Scenario == 2)
  data_scenario3 <- truck_arrival %>% filter(Scenario == 3)
  
  # Calculate the differences
  truck_difference <- left_join(data_scenario2, data_scenario3, by=c("Seed", "Incident", "id"), suffix=c("_scenario2", "_scenario3")) %>%
    mutate(FirstTruckDifference = FirstTruckTime_scenario2 - FirstTruckTime_scenario3,
           SecondTruckDifference = SecondTruckTime_scenario2 - SecondTruckTime_scenario3,
           ThirdTruckDifference = ThirdTruckTime_scenario2 - ThirdTruckTime_scenario3) %>%
    select(Seed, id, Incident, FirstTruckDifference, SecondTruckDifference, ThirdTruckDifference)
  
  # Pivot the data into a longer format
  long_difference_data <- truck_difference %>%
    pivot_longer(cols=ends_with("TruckDifference"), names_to="Truck", values_to="Difference")
  
  summary_data <- long_difference_data %>%
    group_by(Seed) %>%
    filter(!is.na(Difference)) %>%
    mutate(MaxAbsDifference = max(abs(Difference), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(MaxAbsDifference) %>%
    select(id, MaxAbsDifference) %>%
    distinct()
  
  plot_data <- long_difference_data
  
  return(list(plot_data = plot_data, summary_data = summary_data))
}