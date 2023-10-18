# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse", "mlogit", "modelsummary"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts stored in R/ with your custom functions:
# for (file in list.files("R", full.names = TRUE)) source(file)

source("R/load_network.R")

source("R/generate_link_delays_table.R")

source("R/all_links_plots.R")
source("R/motorway_links_plots.R")

source("R/impacted_links_table.R")
source("R/impacted_links_plots.R")

source("R/truck_travel_comparison.R")

# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  
  # We add the Network and turn it into a tibble with these targets
  tar_target(
    name = network_xml,
    command = "data/network/highways_network.xml.gz",
    format = "file"
  ),
  
  tar_target(
    name = network_table,
    command = read_network(network_xml)
  ),
  
  # We then add in the link delay data with the following targets
  # The link delays data is produced by multiplying the values in two CSV files 
  # together, the "data/average_delay" files and the "data/link_volumes" files
  
  # The "average_delay" files come from a MATSim output for the average link
  # delay. We are using the delay from the final (100th) iteration in the output.
  # These output files are in the Box/MATSim_Outputs folder.
  
  # The "link_volumes" files are produced by running the "RunVolumeEventHandler" class
  # This class uses the network_xml object and the Events from the scenario outputs  "Box\MATSim_Analysis\Volume_Analysis\Events"
  
  # Both the "average_delay" and "link_volume" folders are within the data folder
  # To multiply they together we use the "DelayCalculation.ipynb" python script within the data folder.
  
  # All that being said, here is the target for the link delays and their table
  tar_target(
    name = delay_table,
    command = generate_link_delays_table("data/link_delays", network_table)
  ),
  
  # Make the all_links summary table
  tar_target(
    name = delay_summary_table,
    command = write_delay_summary_table(delay_table),
  ),
  
  # Make the all_links plot
  tar_target(
    name = all_links_plot,
    command = make_all_links_plot(delay_summary_table)
  ),
  
  # Make motorway_links summary table
  tar_target(
    name = motorway_link_table,
    command = write_motorway_delay_table(delay_table)
  ),
  
  # Make motorway_plot
  tar_target(
    name = motorway_links_plot,
    command = make_motorway_links_plot(motorway_link_table)
  ),
  
  # Combine the impacted links files into a table
  tar_target(
    name = impacted_links_table,
    command = write_impacted_links_table("data/incident_analysis/delay/incident_link_delays")
  ),
  
  # Group the impacted links by their type
  tar_target(
    name = incident_links_group_table,
    command = write_sorted_impacted_links(impacted_links_table)
  ),
  
  # Summarize the impacted link data here
  tar_target(
    name = impacted_links_summary,
    command = summarize_impacted_link_table(incident_links_group_table)
  ),
  
  # Make the impacted links plot
  tar_target(
    name = impacted_links_plot,
    command = make_impacted_links_plot(impacted_links_summary)
  ),
  
  # We make the truck travel from the Truck
  tar_target(
    name = truck_csv,
    command = "data/truck/truck_travel.csv",
    format = "file"
  ),
  
  tar_target(
    name = truck_data,
    command = read_truck_travel_data(truck_csv)
  ),
  
  tar_target(
    name = truck_plots,
    command = make_truck_plots(truck_data)
  )
)
