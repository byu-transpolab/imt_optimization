  write_truck_arrival_table <- function(truck_arrival_data) {
    library(dplyr)
    
      # Read the CSV file without changing the column names
      data <- read.csv(truck_arrival_data, check.names = FALSE)
      
      # Use the original column names
      time_cols <- c("Start Time [HH:MM:SS]", 
                     "1st Truck Arrival [HH:MM:SS]", 
                     "2nd Truck Arrival [HH:MM:SS]", 
                     "3rd Truck Arrival [HH:MM:SS]", 
                     "4th Truck Arrival [HH:MM:SS]")
      
      # Convert the columns to POSIXct
      data[time_cols] <- lapply(data[time_cols], function(x) as.POSIXct(x, format="%H:%M:%S"))
      
      # Calculate time differences in minutes
      data$diff1 <- as.numeric(difftime(data$`1st Truck Arrival [HH:MM:SS]`, data$`Start Time [HH:MM:SS]`, units="mins"))
      data$diff2 <- as.numeric(difftime(data$`2nd Truck Arrival [HH:MM:SS]`, data$`Start Time [HH:MM:SS]`, units="mins"))
      data$diff3 <- as.numeric(difftime(data$`3rd Truck Arrival [HH:MM:SS]`, data$`Start Time [HH:MM:SS]`, units="mins"))
      data$diff4 <- as.numeric(difftime(data$`4th Truck Arrival [HH:MM:SS]`, data$`Start Time [HH:MM:SS]`, units="mins"))
      
      # Calculate total travel time
      data$`Total Truck Travel [hours]` <- rowSums(data[c("diff1", "diff2", "diff3", "diff4")], na.rm = TRUE) / 60
      
      # Average calculation
      output <- data %>%
        group_by(Scenario) %>%
        summarise(
          `Average Truck Arrival Time [mins]` = mean(c(diff1, diff2, diff3, diff4), na.rm=TRUE),
          `1st Truck Average [mins]` = mean(diff1, na.rm=TRUE),
          `2nd Truck Average [mins]` = mean(diff2, na.rm=TRUE),
          `3rd Truck Average [mins]` = mean(diff3, na.rm=TRUE),
          `4th Truck Average [mins]` = mean(diff4, na.rm=TRUE),
          `Total Truck Travel [hours]` = sum(`Total Truck Travel [hours]`, na.rm=TRUE)
        )
      
      # Count the number of incidents (non-NA values) for each truck
      num_incidents_1st_truck <- sum(!is.na(data$diff1)) / length(unique(data$Scenario))
      num_incidents_2nd_truck <- sum(!is.na(data$diff2)) / length(unique(data$Scenario))
      num_incidents_3rd_truck <- sum(!is.na(data$diff3)) / length(unique(data$Scenario))
      num_incidents_4th_truck <- sum(!is.na(data$diff4)) / length(unique(data$Scenario))
      
      # Adjusted `Total Truck Travel [hours]` calculation
      total_hours <- sum(data$`Total Truck Travel [hours]`, na.rm=TRUE)
      
      incident_row <- data.frame(
        Scenario = "Number of Incidents",
        `Average Truck Arrival Time [mins]` = num_incidents_1st_truck,
        `1st Truck Average [mins]` = num_incidents_1st_truck,
        `2nd Truck Average [mins]` = num_incidents_2nd_truck,
        `3rd Truck Average [mins]` = num_incidents_3rd_truck,
        `4th Truck Average [mins]` = num_incidents_4th_truck,
        `Total Truck Travel [hours]` = num_incidents_1st_truck
      )
      
      # Set column names for incident_row
      names(incident_row) <- names(output)
      
      # Now rbind should work without errors
      truck_arrival_table <- rbind(output, incident_row)
      
      return(truck_arrival_table)
  }
