write_truck_arrival_table <- function(truck_arrival_csv) {
  library(dplyr)
  
  data <- read.csv(truck_arrival_csv, check.names = FALSE)
  time_cols <- c("Start Time [HH:MM:SS]", 
                 "1st Truck Arrival [HH:MM:SS]", 
                 "2nd Truck Arrival [HH:MM:SS]", 
                 "3rd Truck Arrival [HH:MM:SS]", 
                 "4th Truck Arrival [HH:MM:SS]")
  
  data[time_cols] <- lapply(data[time_cols], function(x) as.POSIXct(x, format="%H:%M:%S"))
  
  data$diff1 <- as.numeric(difftime(data$`1st Truck Arrival [HH:MM:SS]`, data$`Start Time [HH:MM:SS]`, units="mins"))
  data$diff2 <- as.numeric(difftime(data$`2nd Truck Arrival [HH:MM:SS]`, data$`Start Time [HH:MM:SS]`, units="mins"))
  data$diff3 <- as.numeric(difftime(data$`3rd Truck Arrival [HH:MM:SS]`, data$`Start Time [HH:MM:SS]`, units="mins"))
  data$diff4 <- as.numeric(difftime(data$`4th Truck Arrival [HH:MM:SS]`, data$`Start Time [HH:MM:SS]`, units="mins"))
  
  data$`Total Truck Travel [hours]` <- rowSums(data[c("diff1", "diff2", "diff3", "diff4")], na.rm = TRUE) / 60
  
  output <- data %>%
    group_by(Scenario) %>%
    summarise(
      `All IMT [mins]` = sprintf("%.1f", mean(c(diff1, diff2, diff3, diff4), na.rm=TRUE)),
      `1st [mins]` = sprintf("%.1f", mean(diff1, na.rm=TRUE)),
      `2nd [mins]` = sprintf("%.1f", mean(diff2, na.rm=TRUE)),
      `3rd [mins]` = sprintf("%.1f", mean(diff3, na.rm=TRUE))
      # `4th [mins]` = sprintf("%.1f", mean(diff4, na.rm=TRUE)),
      # `Total [hours]` = round(sum(`Total Truck Travel [hours]`, na.rm=TRUE))
    )
  
  # Correct rounding for number of incidents
  num_incidents_1st_truck <- round((sum(!is.na(data$diff1)))/2)
  num_incidents_2nd_truck <- round((sum(!is.na(data$diff2)))/2)
  num_incidents_3rd_truck <- round((sum(!is.na(data$diff3)))/2)
  # num_incidents_4th_truck <- round((sum(!is.na(data$diff4)))/2)
  
  total_hours <- sum(data$`Total Truck Travel [hours]`, na.rm=TRUE)
  
  incident_row <- data.frame(
    Scenario = "Number of Incidents",
    `All IMT [mins]` = num_incidents_1st_truck,
    `1st [mins]` = num_incidents_1st_truck,
    `2nd [mins]` = num_incidents_2nd_truck,
    `3rd [mins]` = num_incidents_3rd_truck
    # `4th [mins]` = num_incidents_4th_truck,
    # `Total [hours]` = num_incidents_1st_truck
  )
  
  names(incident_row) <- names(output)
  
  truck_arrival_table <- rbind(output, incident_row) %>%
    rename("Group" = "Scenario")
  
  return(truck_arrival_table)
}
