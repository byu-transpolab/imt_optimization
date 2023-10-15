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
source("R/data_and_models.R")
source("R/truck_travel_comparison.R")
source("R/load_network.R")

# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    name = car_mlogit,
    command = make_data()
  ),
  tar_target(
    name = models,
    command = estimate_models(car_mlogit)
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
  
  # We add in the 
  
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
