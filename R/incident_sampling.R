library(readxl)
library(dplyr)
library(ggplot2)

make_incident_sampling_data <- function(all_incident_data) {

    # Function to create a dataframe with category
    create_df <- function(all_incident_data, category_name) {
      data.frame(Date = NA, Total_Count = all_incident_data, category = category_name)
    }
    
    grouped <- read_excel(all_incident_data, sheet = 'CAD_TS_Combined') %>%
      distinct(`Call ID Number`, .keep_all = TRUE) %>%
      mutate(Timestamp = as.POSIXct(`Call Received Time`, format = "%Y-%m-%d %H:%M:%S"),
             Date = as.Date(Timestamp)) %>%
      group_by(Date, `Call Type`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = `Call Type`, values_from = Count, values_fill = 0) %>%
      mutate(Total_Count = rowSums(select(., -Date), na.rm = TRUE), category = "Incident Distribution") %>%
      select(Date, Total_Count, category)
    
    # Manually specify the incident counts and create dataframes
    current_sample_df <- create_df(c(4, 5, 7, 7, 10, 11, 11, 11, 12, 12), 
                                   "Current Frequency")
    
    inc_freq_df <- create_df(c(18, 18, 18, 19, 19, 19, 20, 20, 21, 21), "Increased Frequency")
    
    # Combine the datasets
    incident_sampling_data <- bind_rows(grouped, current_sample_df, inc_freq_df)
    
    return(incident_sampling_data)
}
    
    # The seed value Brynn used in her python script was ridiculously long and it won't work in R.  
    # This script picks the same incident distribution based on that seed value, but is not random.
    # large_seed <- 9137734339474138810


# Function for the capacity restore data to be used in graph
create_imt_restore_data <- function() {

  imt_capacity_data <- data.frame(
    time = c(0, 0, 60, 60, 15, 15, 60, 60, 30, 30, 60, 60),
    capacity = c(100, 20, 20, 100, 20, 40, 40, 100, 40, 55, 55, 100),
    group = factor(c("No IMT Response", "No IMT Response", "No IMT Response", "No IMT Response",
                     "First IMT Response", "First IMT Response", "First IMT Response", "First IMT Response",
                     "Second IMT Response", "Second IMT Response", "Second IMT Response", "Second IMT Response"), 
                   levels = c("No IMT Response", "First IMT Response", "Second IMT Response"))
  )
  
  return(imt_capacity_data)
}