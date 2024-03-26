library(tidyverse)
library(readxl)
library(janitor)
#fixing Tugs data set ####
#copy from downloads
#file.copy("~/Downloads/CW_CameraData_2019.xlsx",".Data/CW_CameraData_2019.xlsx")

path <- "../../Data/CW_CameraData_2019.xlsx"

site <- c("South Oak Spring Site 2")
trap_days <- read_xlsx(path, sheet = sites[1], range = "B17:I17", col_names = FALSE)

#replaces spaces with _
sites[1] %>% str_replace_all(" ", "_")

#getting/cleaning the data we need
South_Oak_Spring_Site_2 <- 
read_xlsx(path, sheet = site[1], range = "A2:I12") %>%
  pivot_longer(-Species,
               names_to = "month",
               values_to = "obs_count") %>% 
  mutate(site = site [1])

#making df to merge trap days in
South_Oak_Spring_Site_2 <- 
South_Oak_Spring_Site_2 %>% 
  full_join(
    data.frame(month = South_Oak_Spring_Site_2$month %>% unique,
               trap_days[1,] %>% as.numeric())
  )


#making this into a function so we can use on other sheets
read_trap_data <- function(path, sheet, range1, range2){
  trap_days <- read_xlsx(path, sheet = sheet, range = range1, col_names = FALSE)

  x <- 
    read_xlsx(path, sheet = sheet, range = range2) %>%
    clean_names() %>% 
    mutate(across(-Species, as.numeric) %>% 
    pivot_longer(-species,
                 names_to = "month",
                 values_to = "obs_count") %>% 
    mutate(site = sheet,
           month = str_to_sentence(month),
           species = str_to_sentence(species)) %>% 
    mutate(month = case_when(str_detect(month, "[J, j]an" ~ "January",
                             str_detect(month, "[F, f]eb" ~ "February",
                             str_detect(month, "[M, m]ar" ~ "March",
                             str_detect(month, "[A, a]pr" ~ "April",
                             str_detect(month, "[M, m]ay" ~ "May",
                             str_detect(month, "[J, j]un" ~ "June",
                             str_detect(month, "[J, j]ul" ~ "July",
                             str_detect(month, "[A, a]ug" ~ "August",
                             str_detect(month, "[S, s]ep" ~ "September",
                             str_detect(month, "[O, o]ct" ~ "October",
                             str_detect(month, "[N, n]ov" ~ "November",
                             str_detect(month, "[D, d]ec" ~ "December",
                             TRUE ~ month)
  
  x <- 
    x %>% 
    full_join(
      data.frame(month = x$month %>% unique,
                 trap_days[1,] %>% as.numeric())
    )
}
#testing it out
path <- "./Data/CW_CameraData_2019.xlsx"
sites <- c("South Oak Spring Site 2")
read_trap_data(path = path,
               sheet = site[1],
               range1 = "B17:I17",
               range2 = "A2:I12")
