library(tidyverse)
library(gganimate)
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")
names(dat) <- dat %>% 
  names() %>% 
  str_replace_all(" ", "_")

# cleaning data ####
tidy <- dat %>% 
  pivot_longer(starts_with("HR_"),
               names_to = "Time",
               values_to = "Absorbance") %>% 
  mutate(Environment_type = case_when(
    str_detect(Sample_ID, "Clear_Creek") ~ "Water",
    str_detect(Sample_ID, "Soil_1") ~ "Soil",
    str_detect(Sample_ID, "Soil_2") ~ "Soil",
    str_detect(Sample_ID, "Waste_Water") ~ "Water"
  )) %>% 
  mutate(Time = as.numeric(str_replace(Time, "Hr_", "")))

# creating first graphs ####
tidy %>% 
  filter(Dilution == 0.1) %>% 
  ggplot(mapping  = aes(x = Time, 
                   y = Absorbance,
                   color = Environment_type)) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  facet_wrap(~Substrate) +
  labs(x = "Time", y = "Absorbance", color = "Type", title = "Just dilution 0.1")

# creating animated graph ####
animate <- tidy %>%
  filter(Substrate == "Itaconic Acid") %>% 
  group_by(Time, Sample_ID, Dilution) %>% 
  summarise(Mean_absorbance = mean(Absorbance))
plot <- ggplot(animate, mapping = aes(x =  Time,
                              y = Mean_absorbance,
                              color = Sample_ID)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Time", y = "Mean Absorbance", color = "Sample_ID") +
  facet_wrap(~ Dilution) +
  transition_reveal(Time)
animate(plot)
anim_save("animated_plot.gif")
