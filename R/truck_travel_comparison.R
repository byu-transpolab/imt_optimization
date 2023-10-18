library(tidyverse)

read_truck_travel_data <- function(truck_csv) {
  # Read in the data
  df <- read.csv(truck_csv)
  
  # Reduce to unique combinations of Seed, Scenario, Average Time, and Incidents
  df_unique <- df %>%
    select(Seed, Scenario, `Average.Time..seconds.`, Incidents) %>%
    distinct()
  
  # Convert the average time to minutes
  df_unique$`Average.Time..minutes.` <- df_unique$`Average.Time..seconds.` / 60
  
  # Create an ordered vector of Seeds based on Scenario 3 Average Travel Time
  ordered_seeds <- df_unique %>%
    filter(Scenario == "Increased") %>%
    arrange(`Average.Time..minutes.`) %>%
    pull(Seed)
  
  # Reorder Seed factor levels in df_unique based on ordered_seeds
  df_unique$Seed <- factor(df_unique$Seed, levels = ordered_seeds)
  
  df_unique
}

make_truck_plots <- function(truck_data) {
  time_scenario <- ggplot(truck_data, aes(x = Seed, 
                                       y = `Average.Time..minutes.`, 
                                       color = as.factor(Scenario),
                                       fill = as.factor(Scenario),
                                       size = Incidents)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.7, shape = 21, color = "black") +  
  labs(
    title = "Average Travel Time Per Scenario",
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
}

# # Plotting
# time_scenario <- ggplot(df_unique, aes(x = Seed, 
#                                        y = `Average.Time..minutes.`, 
#                                        color = as.factor(Scenario),
#                                        fill = as.factor(Scenario),
#                                        size = Incidents)) +
#   geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.7, shape = 21, color = "black") +  
#   labs(
#     title = "Average Travel Time Per Scenario",
#     x = "Scenario",
#     y = "Time per Truck (mins.)",
#     color = "Number of Vehicles",
#     size = "Incidents",
#     fill = "Number of Vehicles"
#   ) +
#   scale_color_brewer(palette = "Set1", 
#                      labels = c("20 Vehicles", "30 Vehicles"), 
#                      guide = guide_legend(override.aes = list(size = 6))) +
#   scale_fill_brewer(palette = "Set1", 
#                     labels = c("20 Vehicles", "30 Vehicles"), 
#                     guide = guide_legend(override.aes = list(size = 6))) +
#   scale_size_continuous(breaks = c(5, 10, 15, 20), 
#                         range = c(2.5, 10)) + 
#   theme_light(base_size = 14) + 
#   theme(
#     plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
#     axis.title.x = element_text(size = 16, face = "bold"),
#     axis.title.y = element_text(size = 16, face = "bold"),
#     axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
#     axis.text.y = element_text(size = 14),
#     legend.title = element_text(size = 16, face = "bold"),
#     legend.text = element_text(size = 14),
#     legend.key = element_blank(),  
#     panel.grid.major = element_line(color = "gray80"),  
#     panel.grid.minor = element_blank()  
#   )
# 
# # Save the plot with the specified dimensions
# png("time_scenario.png", width = 2100, height = 1400, units = "px", res = 300)
# print(time_scenario)
# dev.off()
# 
# ####################### Distance Plot #####################
# 
# # Reduce to unique combinations of Seed, Scenario, Average Time, and Incidents
# df_unique <- df %>%
#   select(Seed, Scenario, `Average.Distance..meters.`, Incidents) %>%
#   distinct()
# 
# # Convert the average distance to miles
# df_unique$`Average.Distance..miles.` <- df_unique$`Average.Distance..meters.` / 1609.344
# 
# # Create an ordered vector of Seeds based on Scenario 3 Average Travel Distances
# ordered_seeds <- df_unique %>%
#   filter(Scenario == 3) %>%
#   arrange(`Average.Distance..miles.`) %>%
#   pull(Seed)
# 
# # Reorder Seed factor levels in df_unique based on ordered_seeds
# df_unique$Seed <- factor(df_unique$Seed, levels = ordered_seeds)
# 
# # Plotting with the Pastel2 color palette
# distnace_scenario <- ggplot(df_unique, aes(x = Seed, 
#                                        y = `Average.Distance..miles.`, 
#                                        color = as.factor(Scenario),
#                                        fill = as.factor(Scenario),
#                                        size = Incidents)) +
#   geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.7, shape = 21, color = "black") +  
#   labs(
#     title = "Average Distance Travel Per Scenario",
#     x = "Scenario",
#     y = "Distance per Truck (miles)",
#     color = "Number of Vehicles",
#     size = "Incidents",
#     fill = "Number of Vehicles"
#   ) +
#   scale_color_brewer(palette = "Pastel3", 
#                      labels = c("20 Vehicles", "30 Vehicles"), 
#                      guide = guide_legend(override.aes = list(size = 6))) +
#   scale_fill_brewer(palette = "Pastel3", 
#                     labels = c("20 Vehicles", "30 Vehicles"), 
#                     guide = guide_legend(override.aes = list(size = 6))) +
#   scale_size_continuous(breaks = c(5, 10, 15, 20), 
#                         range = c(2.5, 10)) + 
#   theme_light(base_size = 14) + 
#   theme(
#     plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  
#     axis.title.x = element_text(size = 16, face = "bold"),
#     axis.title.y = element_text(size = 16, face = "bold"),
#     axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
#     axis.text.y = element_text(size = 14),
#     legend.title = element_text(size = 16, face = "bold"),
#     legend.text = element_text(size = 14),
#     legend.key = element_blank(),  
#     panel.grid.major = element_line(color = "gray80"),  
#     panel.grid.minor = element_blank()  
#   )
# 
# # View the plot
# print(distance_scenario)
