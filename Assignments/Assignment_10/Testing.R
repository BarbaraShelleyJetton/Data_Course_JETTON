library(tidyverse)
library(rnaturalearth)
library(sf)
library(gganimate)
library(gifski)
library(cowplot)
#reading in disaster data and world data
dat <- read_csv("clean_data.csv")
world <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf")

#fixing boundary issues
boundries <- st_bbox(world)
dat <- dat %>% 
  filter(latitude >= boundries[2] & latitude <= boundries[4] &
           longitude >= boundries[1] & longitude <= boundries[3])
dat <- dat %>%
  mutate(country = gsub("\\s*\\(.*?\\)\\s*", "", country))

#looking at how world plots
ggplot() +
  geom_sf(data = world) +
  theme_light()

#looking at how disaster data plots
ggplot()+
  geom_point(data = fix_dat, aes(x = longitude,
                             y = latitude,
                             color = disaster_type),
             pch = 19,
             size = 0.5)

#smashing those together
p <- ggplot() +
  geom_sf(data = world) +
  theme_light() +
  geom_point(data = dat, aes(x = longitude,
                             y = latitude,
                             color = disaster_type,
                             size = total_deaths), alpha = 0.7) +
  scale_size_continuous(range = c(1, 10))
p

#animate by year
p_animated <- p +
  transition_time(time = year) +
  labs(title = 'Year: {frame_time}')
animate(p_animated, nframes = 500, fps = 10, width = 800, height = 600)
#wtf is going on with year

#fixing so I can animate by start date instead, make more smooth?
fix_dat$start_date <- as.Date(paste(fix_dat$start_year, fix_dat$start_month, fix_dat$start_day, sep = "-"))
fix_dat <- fix_dat[!is.na(fix_dat$start_date), ]

p_animated <- p +
  transition_time(time = start_date) +
  labs(title = 'Date: {frame_time}') +
  exit_fade()
animate(p_animated, nframes = 500, fps = 10, width = 800, height = 600)

#work on slowing down/smoothing want fade out

###################################################################################################################################
#making some plots?

disasters_by_country <- dat %>%
  group_by(country) %>%
  summarize(total_disasters = n())

world_disasters <- left_join(world, disasters_by_country, by = c("name" = "country"))

plot_country_distribution <- ggplot() +
  geom_sf(data = world_disasters, aes(fill = total_disasters)) +
  scale_fill_continuous(name = "Total Disasters", na.value = "white") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Total Disasters by Country")

plot_country_distribution

plot_total_deaths <- ggplot(dat, aes(x = year, y = total_deaths)) +
  geom_line(color = "indianred1", size = 1.25) +
  labs(title = "Total Deaths Over the Years",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal()

plot_total_deaths

plot_total_damages <- ggplot(dat, aes(x = year, y = total_damages)) +
  geom_line(color = "royalblue1" ,size = 1.25) +
  labs(title = "Total Damages Over the Years",
       x = "Year",
       y = "Total Damages (Millions)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6))

plot_total_damages

plot_disaster_type <- ggplot(dat, aes(x = disaster_type, fill = disaster_type)) +
  geom_bar() +
  labs(title = "Distribution of Disaster Types",
       x = "Disaster Type",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

plot_disaster_type

filtered_dat <- dat %>% 
  filter(!is.na(disaster_subtype))

plot_disaster_subtype <- ggplot(filtered_dat, aes(x = disaster_subtype, fill = disaster_subtype)) +
  geom_bar() +
  labs(title = "Distribution of Disaster Subtypes",
       x = "Disaster Subtype",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

plot_disaster_subtype

plot_disaster_subgroup <- ggplot(dat, aes(x = disaster_subgroup, fill = disaster_subgroup)) +
  geom_bar() +
  labs(title = "Distribution of Disaster Subgroups",
       x = "Disaster Subgroup",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

plot_disaster_subgroup

plot_grid <- plot_grid(plot_country_distribution, plot_total_deaths, plot_total_damages, 
                       plot_disaster_subgroup, plot_disaster_type, plot_disaster_subtype, ncol = 2)
plot_grid
