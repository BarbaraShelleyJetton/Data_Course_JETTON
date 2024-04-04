library(tidyverse)
library(janitor)

dat <- read_csv("1900_2021_DISASTERS.csv")

dat <- dat %>% 
  rename("Total Damages" = "Total Damages ('000 US$)")

keep <- c("Year", "Disaster_subgroup", "Disaster_Type", "Disaster_Subtype", "Disaster_Subsubtype", "Country", "Latitude", "Longitude", "Start_Year",
          "Start_Month", "Start_Day", "End_Year", "End_Month", "End_Day", "Total_Deaths", "Total_Damages") %>% 
  str_to_lower()

clean <- dat %>%
  clean_names() %>% 
  select(all_of(keep)) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>% 
  filter(!is.na(latitude) & !is.na(longitude))

write_csv(clean, "clean_data.csv")
