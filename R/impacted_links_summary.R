install.packages("ggpattern")

library(dplyr)
library(ggplot2)
library(RColorBrewer)
    
    write_sorted_impacted_links <- function(impacted_links_table) {
      
      # Add the new column 'Total Incident Delay [hours]'
      impacted_links_table <- impacted_links_table %>%
        mutate(`Total Incident Delay [hours]` = `Delay During Incident [hours]` + 
                 `Post-Incident Delay (0-30 mins) [hours]` + 
                 `Post-Incident Delay (30-60 mins) [hours]`)
      
      # Custom sorting order
      impacted_links_table$Type <- factor(impacted_links_table$Type, 
                                          levels = c("Incident Link", "Feeder 1", "Feeder 2"))
      
      # Sort the table by Type
      sorted_table <- impacted_links_table[order(impacted_links_table$Type), ]
      
      # Remove duplicates for impacted_links and assign the Group "Impacted"
      impacted_links <- sorted_table[!duplicated(sorted_table[, c("Scenario", "Seed", "Link Id")]), ]
      
      return(impacted_links)
    }
    
    write_delay_per_seed_table <- function(impacted_links) {
      
      delay_per_seed <- impacted_links %>%
        group_by(Scenario, Seed, Incidents) %>%
        summarise(total_incident_delay = sum(`Total Incident Delay [hours]`, na.rm = TRUE)) %>%
        mutate(`Scenario ID` = paste0(Incidents, "_", Seed))
      
      return(delay_per_seed)
    }
    
    summarize_impacted_link_table <- function(impacted_links){
      
      # Mutate the scenario first
      impacted_links <- impacted_links %>%
        mutate(Scenario = case_when(
          Scenario == "Current" ~ "20 IMT",
          Scenario == "Increased" ~ "30 IMT",
          TRUE ~ as.character(Scenario)
        ))
      
      # Check and handle non-numeric values in Incidents
      if(any(!grepl("^[0-9]+$", as.character(impacted_links$Incidents)))) {
        warning("Non-numeric values found in 'Incidents' and will be set to NA!")
        impacted_links$Incidents[!grepl("^[0-9]+$", as.character(impacted_links$Incidents))] <- NA
      }
      
      # Convert Incidents to numeric
      impacted_links$Incidents <- as.numeric(as.character(impacted_links$Incidents))
      
      impacted_delay_table <- impacted_links %>%
        group_by(Scenario, Seed, Incidents) %>%
        summarise(total_delay_during_incident = sum(`Total Incident Delay [hours]`, na.rm = TRUE))
      
      # Filter rows where incidents are greater than 14
      increased_incidents <- impacted_delay_table %>%
        filter(Incidents > 14) %>%
        group_by(Scenario) %>%
        summarise(total_delay = sum(total_delay_during_incident, na.rm = TRUE),
                  total_incidents = sum(Incidents)) %>%
        mutate(incident_frequency = "Increased",
               `Average Delay Per Incident [hours]` = total_delay / total_incidents)
      
      # Filter rows where incidents are 14 or less
      current_incidents <- impacted_delay_table %>%
        filter(Incidents <= 14) %>%
        group_by(Scenario) %>%
        summarise(total_delay = sum(total_delay_during_incident, na.rm = TRUE),
                  total_incidents = sum(Incidents)) %>%
        mutate(incident_frequency = "Current",
               `Average Delay Per Incident [hours]` = total_delay / total_incidents)
      
      # Combine the tables
      impacted_links_summary <- bind_rows(increased_incidents, current_incidents)
      
      impacted_links_summary <- impacted_links_summary %>%
        select(Scenario, total_delay, total_incidents, incident_frequency, `Average Delay Per Incident [hours]`)
      
      impacted_summary_table <- impacted_links_summary %>%
        rename(
          "Group" = "Scenario",
          "Incident Frequency" = "incident_frequency",
          "# of Incidents" = "total_incidents",
          "Total VHD" = "total_delay",
          "Avg. Delay Per Inc. [hrs.]" = "Average Delay Per Incident [hours]"
        )
      
      impacted_summary_table <- impacted_summary_table %>%
        arrange(
          factor(`Incident Frequency`, levels = c("Current", "Increased")),
          factor(Group, levels = c("Baseline", "Incidents", "20 IMT", "30 IMT")),
        )
      
      return(impacted_summary_table)
    }