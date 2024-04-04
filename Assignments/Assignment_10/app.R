library(shiny)
library(tidyverse)
library(rnaturalearth)
library(sf)

dat <- read_csv("clean_data.csv")
world <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf")
boundries <- st_bbox(world)
dat <- dat %>% 
  filter(latitude >= boundries[2] & latitude <= boundries[4] &
           longitude >= boundries[1] & longitude <= boundries[3])

ui <- fluidPage(sliderInput(inputId = "year",
                            label = "Year Range:",
                            min = min(dat$year),
                            max = max(dat$year),
                            value = c(min(dat$year),
                                      max(dat$year)),
                            sep = ""),
                checkboxGroupInput(inputId = "disaster_type",
                                   label = "Select Type of Disaster:",
                                   choices = unique(dat$disaster_type),
                                   selected = unique(dat$disaster_type)),
                radioButtons(inputId = "stats",
                             label = "Choose Statistic:",
                             choices = c("Total Deaths", "Total Damages"),
                             selected = "Total Deaths"),
                plotOutput(outputId = "history_plot")
                )
server <- function(input, output) {
  output$history_plot <- renderPlot({
    ggplot() +
      geom_sf(data = world) +
      theme_light() +
      geom_point(data = dat, aes(x = longitude,
                                 y = latitude,
                                 color = disaster_type,
                                 size = total_deaths), alpha = 0.7) +
      scale_size_continuous(range = c(1, 10))
  })
}
shinyApp(ui = ui, server = server)
