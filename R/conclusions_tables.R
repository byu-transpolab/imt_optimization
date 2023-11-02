# Load necessary libraries
library(dplyr)
library(readr)

# Define the function
read_vhd_summary <- function(vhd_summary_csv) {
  # Read the data from the CSV file and convert it to a tibble
  VHD_conclusions_table <- read_csv(vhd_summary_csv) %>%
    as_tibble() %>%
    # Remove rows where all values are NA
    drop_na() %>%
    # Conditionally round the 'Average VHD' values
    mutate(`Average VHD` = ifelse(Level == "Impacted", round(`Average VHD`, 1), round(`Average VHD`))) %>%
    # Remove % sign and convert to numeric for accurate rounding
    mutate(`Percent Change` = as.numeric(gsub("%", "", `Percent Change`))) %>%
    # Round the 'Percent Change' values to one decimal place
    mutate(`Percent Change` = round(`Percent Change`, 1)) %>%
    # Re-add % sign to 'Percent Change'
    mutate(`Percent Change` = paste0(`Percent Change`, "%"))
  
  # Return the modified table
  return(VHD_conclusions_table)
}