library(tidyverse)
library(readxl)

#Tidy Excel Data####
#Read in XLSX file, skip 3 lines
library(readxl)
dat <- read_xlsx("../../Data/messy_bp.xlsx", skip = 3)
'''Automating how we originally cleaned this '''
#separate blood pressure data into separate d.f
clean_names(dat)

bp<- dat %>% 
  select(-starts_with("HR"))

n.visits <- bp %>% 
  select(starts_with("BP")) %>% 
  length()
#grepl looks for a pattern and returns a true or false
#when you give which a true false question it will return the position of what is true (in this case column numbers)
names(bp)[which(grepl("^BP",names(bp)))] <- paste0("visit", 1:n.visits) #which of the names of BP start with BP, of those positions, turn into visits using paste

bp <- bp %>% 
  pivot_longer(starts_with("visit"),
               names_to = "visit",
               values_to = "bp",
               names_prefix = "visit", 
               names_transform = as.numeric) %>% 
  separate(bp, into = c("systolic", "diastolic"))#tells R they will all start with visit and end with a number

#now doing the same for HR
#separate heart rate data into separate d.f
hr<- dat %>% 
  select(-starts_with("BP"))

n.visits <- hr %>% 
  select(starts_with("HR")) %>% 
  length()
#grepl looks for a pattern and returns a true or false
#when you give which a true false question it will return the position of what is true (in this case column numbers)
names(hr)[which(grepl("^HR",names(hr)))] <- paste0("visit", 1:n.visits)


#join the two d.f
health <- full_join(bp, hr)

health$Race %>% unique

health <- 
  health %>% 
  mutate(Race = case_when(Race == "Caucasian" | Race == "WHITE" ~ "White",
                          TRUE ~ Race)) %>% 
  mutate(birthdate = paste(Year Birth,Month of Birth,Day Birth, sep = "-") %>% as.POSIXct()) %>% 
  mutate(systolic = systolic %>% as.numeric(),
         diastolic = diastolic %>% as.numeric()) %>%
  select(-pat_id, -Month of Birth, -Day Birth, -Year Birth) %>%
  mutate(Hispanic = case_when(Hispanic == "Hispanic" ~ TRUE,
                              TRUE ~ FALSE)) %>% 
  pivot_longer(cols = c("systolic", "diastolic"), names_to = "bp_type", values_to = "bp")

##Plot the Data####
health %>% 
  ggplot(aes(x = visit,
             y = hr,
             color = sex)) +
  geom_path() +
  facet_wrap(~race)

health %>% 
  ggplot(aes(x = visit,
             y = bp,
             color = bp_type)) +
  geom_path() +
  facet_wrap(~hispanic)

#find new terrible data, fresh r script, practice cleaning
#str_squish to remove whitespace in data
