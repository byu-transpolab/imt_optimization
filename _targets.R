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


# incident sampling script based on the python histograms script
source("R/incident_sampling.R")

# load network script. don't mess with it
source("R/load_network.R")

# script to generate link delay table
source("R/generate_link_delays_table.R")

# link tables and plots
source("R/all_links_plots.R")
source("R/motorway_links_plots.R")

# impacted links tables and plots
source("R/impacted_links_table.R")
source("R/impacted_links_summary.R")
source("R/impacted_links_plot.R")

# truck travel r-script
source("R/truck_travel_comparison.R")

# truck arrival r-scripts
source("R/truck_arrival_comparison.R")
source("R/truck_arrival_plot.R")
source("R/truck_arrival_violin.R")


list(
  
  # Load the combined incident excel data into a target file
  tar_target(
    name = all_incident_data,
    command = "data/incident_data/Combinder_CAD_TS_2022_All_Modified_RCT.xlsm",
    format = "file"  
    ),
  
  
  # source("R/incident_sampling.R")
  # Make incident sampling plot
  tar_target(
    name = incident_sampling_data,
    command = make_incident_sampling_data(all_incident_data)
  ),
  
  # Make data for capacity restoration plot
  tar_target(
    name = imt_capacity_data,
    command = create_imt_restore_data()
  ),
  
  
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
  
  # Make delay table from delay files and network table
  tar_target(
    name = delay_table,
    command = generate_link_delays_table("data/link_delays", network_table)
  ),
  
  
  # Make the all_links summary table
  # source("R/all_links_plots.R")
  tar_target(
    name = delay_summary,
    command = write_delay_summary_table(delay_table),
  ),
  
  # Make the all_links comparison table for the report
  # From R/Script: "R/generate_link_delays_table.R"
  tar_target(
    name = all_links_comparison_table,
    command = write_all_links_comparison_table(delay_summary),
  ),
  
  # Make the all_links plot
  tar_target(
    name = all_links_plot,
    command = make_all_links_plot(delay_summary)
  ),
  
  # Make motorway_links summary table
  # From R/Script: "R/motorway_links_plots.R"
  tar_target(
    name = motorway_delay_summary,
    command = write_motorway_summary_table(delay_table)
  ),
  
  # Make the motorway comparison table for the report
  tar_target(
    name = motorway_summary_table,
    command = write_motorway_comparison_table(motorway_delay_summary)
  ),
  
  # Make motorway_plot
  tar_target(
    name = motorway_links_plot,
    command = make_motorway_links_plot(motorway_delay_summary)
  ),
  
  
  # Combine the impacted links files into a table
  # From R/Script: "R/impacted_links_table.R"
  tar_target(
    name = impacted_links_table,
    command = write_impacted_links_table("data/incident_analysis/delay/incident_link_delays")
  ),
  
  # Group the impacted links by their type
  # From R/Script: "R/impacted_links_summary.R"
  tar_target(
    name = impacted_links,
    command = write_sorted_impacted_links(impacted_links_table)
  ),
  
  # Summarize the impacted link data here
  tar_target(
    name = impacted_links_combine,
    command = summarize_impacted_link_data(impacted_links)
  ),
  
  # Write table for report
  tar_target(
    name = impacted_links_summary,
    command = write_impacted_link_summary_table(impacted_links_combine)
  ),
  
  tar_target(
    name = delay_per_seed,
    command = write_sorted_impacted_links(impacted_links)
  ),
  
  # # Make the impacted links scatter plot
  # tar_target(
  #   name = impacted_links_plot,
  #   command = make_impacted_links_plot(delay_per_seed)
  # ),
  
  # We make the truck travel from the truck_travel csv file
  tar_target(
    name = truck_csv,
    command = "data/truck_data/truck_travel.csv",
    format = "file"
  ),
  
  # Process truck time data from CSV file
  tar_target(
    name = truck_time,
    command = write_truck_time_data(truck_csv)
  ),
  
  # Make truck_time_plot
  tar_target(
    name = truck_time_plot,
    command = make_truck_time_plot(truck_time)
  ),
  
  # Make truck_distance_table
  tar_target(
    name = truck_distance,
    command = write_truck_distance_data(truck_csv)
  ),

  # Make truck_time_plot
  tar_target(
    name = truck_distance_plot,
    command = make_truck_distance_plot(truck_distance)
  ),
  
  # We make the truck_arrival_data from its corresponding CSV
  tar_target(
    name = truck_arrival_data,
    command = "data/truck_data/arrival_times.csv",
    format = "file"
  ),
  
  # Make the truck_arrival_table
  tar_target(
    name= truck_arrival_table,
    command = write_truck_arrival_table(truck_arrival_data)
  ),
  
  tar_target(
    name = truck_arrival_plot,
    command = make_truck_arrival_plot(truck_arrival_data)
  ),
  
  tar_target(
    name = truck_arrival_violin_plot,
    command = make_truck_violin_plot(truck_arrival_data)
  )
)
