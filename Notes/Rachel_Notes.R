library(gapminder)
library(tidyverse)
library(ggimage)
library(gganimate)
library(patchwork)

#geom_grob takes images, moves them around
#get ggalignment for uglyplot
#look at the r extensions available

names(gapminder)
df <- gapminder

Adf <- df %>% 
  filter(grepl("^A", country))

PlotA1 <- Adf %>% 
  ggplot(mapping = aes(x = year,
                       y = lifeExp,
                       color = country)) +
  geom_point(aes(size = pop)) +
  facet_wrap(~continent) +
  theme_bw()

PlotA2 <- Adf %>% 
  ggplot(mapping = aes(x = year,
                       y = lifeExp,
                       color = country)) +
  geom_point(aes(size = pop)) +
  theme_bw()

PlotA2/PlotA1 + plot_annotation("Comparing with/without Facets")

p.dark <- PlotA +
  theme_dark()

#Patchwork####

PlotA + p.dark
PlotA / p.dark
(PlotA + p.dark)/p.dark + 
  plot_annotation("Main Title") +
  plot_layout(guides = 'collect')
#^ this works due to patchwork

my_countries <- c("Venezuela", "Rwanda", "Nepal")

df <- df %>% 
  mutate(my_countries = case_when(country %in% my_countries ~ country))

p3 <- 
  ggplot(df, aes(x=gdpPercap, y=lifeExp, color=continent)) +
  geom_point(aes(size = pop))+
  geom_text(aes(label = my_countries))

#GGAnimate####

p3 +
  transition_time(time = year) +
  labs(title = "Year: {frame_time}")

#Saving Plots####

anim_save("./Notes/gapminder_animation.gif")
ggsave("./Notes/plot_example.png", plot = p3)
#mess with height, width, dpi in ggsave


#Day 2: Starting Activity ####

library(tidyverse)
df <- read_csv("./Data/wide_income_rent.csv")

#plot rent prices for each state
#state on x-axis, rent on y-axis, bar chart

names(df)
#fix the data!

#Tidying with base R####
#this is the bad way
df2 <- as.data.frame(t(df))
df2 <- df2[-1,]
df2$state <- row.names(df2)
names(df2)
names(df2) <- c("income", "rent", "states")

#Tidying with Tidyverse
##Pivots####
#pivot longer for one variable over multiple, wider to get multiple variables from 1 row
df3 <- df %>% 
  pivot_longer(-variable, names_to = "state", values_to = "amount") %>% 
  pivot_wider(names_from = variable, values_from = amount)


df3 %>% 
  ggplot(aes(x = state, y = rent)) +
  geom_col()+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5, size=8))

#Practice with Tidying: Built-in Data####
#built in data set, is it clean?
table1 #yes, it is clean
table2 #no, cases and population are separate variables. fix it

table2 %>% 
  pivot_wider(names_from = type, values_from = count)

table3 #no, now multiple data is stored badly in rate column. could have been added with a mutate

table3 %>% 
  separate(rate, into = c("cases", "population"))

table4a #no, just cases
table4b #no, just population

#now join them!
x <- table4a %>% 
  pivot_longer(-country, 
               names_to = "year", 
               values_to = "cases")

y <- table4b %>% 
  pivot_longer(-country, 
               names_to = "year", 
               values_to = "population")

full_join(x,y)

table5 #not tidy, bad!

table5 %>% 
  separate(rate, into = c("cases", "population"),
           convert = TRUE) %>% 
  mutate(year = paste0(century,year) %>% as.numeric()) %>% 
  select(-century)

#Tidy Excel Data####
#Read in XLSX file, skip 3 lines
library(readxl)
dat <- read_xlsx("./Data/messy_bp.xlsx", skip = 3)

#separate blood pressure data into separate d.f
bpdat <- 
  dat %>% 
  select(-starts_with("HR"))

#make that d.f tidy
bpdat <- bpdat %>% 
  pivot_longer(starts_with("BP"),
               names_to = "visit",
               values_to = "bp") %>% 
  mutate(visit = case_when(visit == "BP...8" ~ 1,
                           visit == "BP...10" ~ 2,
                           visit == "BP...12" ~ 3)) %>% 
  separate(bp, into = c("systolic", "diastolic"))


#separate heart rate data into separate d.f
hrdat <-
  dat %>% 
  select(-starts_with("BP"))

#make d.f tidy
hrdat <- hrdat %>% 
  pivot_longer(starts_with("HR"),
               names_to = "visit",
               values_to = "hr") %>% 
  mutate(visit = case_when(visit == "HR...9" ~ 1,
                           visit == "HR...11" ~ 2,
                           visit == "HR...13" ~ 3))

##Janitor Package####
library(janitor)

#join the two d.f
health <- full_join(bpdat, hrdat) %>% 
  clean_names()

health$race %>% unique

health <- 
  health %>% 
  mutate(race = case_when(race == "Caucasian" | race == "WHITE" ~ "White",
                          TRUE ~ race)) %>% 
  mutate(birthdate = paste(year_birth,month_of_birth,day_birth, sep = "-") %>% as.POSIXct()) %>% 
  mutate(systolic = systolic %>% as.numeric(),
         diastolic = diastolic %>% as.numeric()) %>%
  select(-pat_id, -month_of_birth, -day_birth, -year_birth) %>%
  mutate(hispanic = case_when(hispanic == "Hispanic" ~ TRUE,
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