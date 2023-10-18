generate_link_delays_table <- function(folder_path, network_table) {
  library(tidyverse)
  
  # Check if folder exists
  if (!dir.exists(folder_path)) {
    stop(paste("Directory", folder_path, "does not exist."))
  }
  
  # Get list of CSV files in the folder, including subdirectories
  file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)
  
  # If no files are detected, stop the script
  if (length(file_list) == 0) {
    stop("No CSV files found in the specified directory or its subdirectories.")
  }
  
  # Use purrr's map_dfr to read and combine CSVs into a single tibble
  link_delays <- file_list %>%
    purrr::map_dfr(~{
      data <- read_csv(.x)
      
      # Extract scenario, incidents, and seed values from filename
      scenario <- str_extract(.x, "(?<=/)[0-9]+(?=-(?:[0-9]+-)[0-9]{3}\\.delay\\.csv$)") %>% as.numeric()
      incidents <- str_extract(.x, "(?<=-)[0-9]+(?=-[0-9]{3}\\.delay\\.csv$)") %>% as.numeric()
      seed <- str_extract(.x, "(?<=-)[0-9]{3}(?=\\.delay\\.csv$)") %>% as.numeric()
      
      # Add columns for scenario, incidents, and seed
      mutate(data, scenario = scenario, incidents = incidents, seed = seed)
    })
  
  link_delays <- link_delays %>%
    select(`Link Id`, scenario, incidents, seed, everything())
  
  # Additional processing steps
  link_delays <- link_delays %>%
    mutate(
      scenario = case_when(
        scenario == 0 ~ "Baseline",
        scenario == 1 ~ "Incidents",
        scenario == 2 ~ "Current",
        scenario == 3 ~ "Increased",
        TRUE          ~ as.character(scenario)  # Keeping the numeric value as a character in case it's not one of the listed scenarios
      ),
      total = (rowSums(select(., -c(scenario, incidents, seed, `Link Id`)), na.rm = TRUE)/3600)
    ) %>%
    left_join(
      network_table$links %>%
        mutate(id = as.double(id)) %>%
        select(id, type),
      by = c("Link Id" = "id")
    ) %>%
    select(
      scenario, `Link Id`, incidents, seed, total, `Link Type` = type, everything()
    )
  
  return(link_delays)
}


