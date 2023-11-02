install.packages("readr")

library(tibble)
library(dplyr)
library(readr)
library(tidyr)
library(hms)

write_impacted_links_table <- function(folder_path) {
  
  # List all csv files within the folder_path
  impacted_links_files <- list.files(path = folder_path, recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
  
  # Helper function to extract the values for new columns from the file path
  extract_values <- function(filepath) {
    # Extract seed
    seed <- gsub("^.*Seed ([0-9]+).*", "\\1", filepath)
    
    # Extract scenario and incidents
    filename <- basename(filepath)
    scenario_code <- as.integer(strsplit(filename, "-")[[1]][1])
    incidents <- strsplit(filename, "-")[[1]][2]
    
    scenario <- case_when(
      scenario_code == 0 ~ "Baseline",
      scenario_code == 1 ~ "Incidents",
      scenario_code == 2 ~ "Current",
      scenario_code == 3 ~ "Increased"
    )
    
    return(list(Seed = seed, Scenario = scenario, Incidents = incidents))
  }
  
  # Read and bind all the files together into a tibble
  impacted_links_table <- bind_rows(
    lapply(
      impacted_links_files,
      function(file) {
        data <- read_csv(file) %>%
          mutate(across(everything(), ~replace_na(., 0))) # This line replaces NA values with 0
        
        info <- extract_values(file)
        
        data %>%
          mutate(Seed = info$Seed,
                 Scenario = info$Scenario,
                 Incidents = info$Incidents)
      }
    )
  )
  
  
  impacted_links_table <- impacted_links_table %>%
    rename(
      `Feeder 1` = `Feeder 1_x`,
      `Feeder 2` = `Feeder 2_x`
    )
  
  reorder_columns <- function(data) {
    # Your predefined column order
    column_order <- c("Scenario", "Seed", "Incidents", "Link Id", "Type", 
                      "Total Delay [hours]", "Incident Link",  "Feeder 1", 
                      "Feeder 2", "Incident Start", "Incident End", 
                      "Duration [HH:MM:SS]", "Duration [mins]", 
                      "Delay During Incident [hours]", 
                      "Post-Incident Delay (0-30 mins) [hours]", 
                      "Post-Incident Delay (30-60 mins) [hours]")
    
    # Subset data to only include columns in column_order
    data <- data[, column_order]
    
    return(data)
  }
  
  impacted_links_table <- impacted_links_table %>%
    reorder_columns()
  
  return(impacted_links_table)
}