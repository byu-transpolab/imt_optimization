make_truck_arrival_plot <- function(arrival_plot_data, arrival_summary_data) {
  
  arrival_plot_data <- arrival_plot_data %>%
    filter(!is.na(Difference))
  
  # Add spaces to the truck names
  arrival_plot_data <- arrival_plot_data %>%
    mutate(Truck = recode_factor(Truck, 
                                 `FirstTruckDifference` = "First Truck", 
                                 `SecondTruckDifference` = "Second Truck", 
                                 `ThirdTruckDifference` = "Third Truck"))
  
  arrival_plot <- ggplot(arrival_plot_data, aes(x = id, y = Difference, color = Truck)) +
    geom_point(size = 2, position = position_jitter(width = 0.6, height = 0.2), alpha = 0.6) +
    labs(y = "Difference in Arrival Time (minutes)",
         x = "Seed ID",
         color = "Truck") +
    scale_x_discrete(limits = arrival_summary_data$id) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal()
  
  return(arrival_plot)
}