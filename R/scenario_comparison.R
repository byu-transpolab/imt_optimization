library(tidyverse)
library(readxl)

# Read in data
data <- read_excel("C:\\Users\\djarvis3\\Box\\MATSim_Analysis\\Scenario Comparison.xlsx","Congestion_Data")

# Drop rows where 'Total delay [hours]' is NA
data_clean <- data %>%
  drop_na(`Total delay [hours]`) %>% 
  mutate(
    `IMT Grouping` = factor(
      `IMT Grouping`, levels = c("No IMTs", "IMT Current", "IMT Improved")
      )
    ) %>%
  filter(ReRun =="No")

# summarise all of the last 10 iterations in to a mean value
averaged_data <- data_clean %>% 
  summarise(
    mean_delay = mean(`Total delay [hours]`),
    Type = Type[1],
    group = `IMT Grouping`[1],
    seed = `Seed`[1],
    .by = `Scenario ID`
  )

  

# Create the plot
# This is a violin plot of all scenarios and all iterations, with
# current and increased incident frequencies grouped together
ggplot(averaged_data, aes(x=`group`, y=`mean_delay`))+
  geom_point() +
  facet_wrap(~Type)


ggplot(data_clean, aes(x=`IMT Grouping`, y=`Total delay [hours]`))+
  geom_violin() #+
  # facet_wrap(~Type)

# This is 
ggplot(averaged_data,
       aes(x= group, y=mean_delay, group=seed))+
  geom_line() + 
  facet_wrap(~Type)
