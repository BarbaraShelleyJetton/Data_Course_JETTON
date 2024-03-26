library(tidyverse)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)

df <- read_csv("../../Data/Bird_Measurements.csv")
skim(df) #skims the df and tells you things about it
#So what needs to happen make three different df (male, female, unsexed)

keep <- c("Family", "Species_number", "Species_name", "English_name", "Clutch_size", "Egg_mass", "Mating_System") %>% 
  str_to_lower() #these are the columns we want in all the df

male <- 
  df %>% 
  clean_names() %>% #makes names uniformed
  select(keep, starts_with("m_"), -ends_with("_n")) %>% #keeps m_, removes ones that end in _n becuase its bad
  mutate(sex = "male") #adds sex column

names(male) <- names(male) %>% #renames all columns with m_ because these columns are what
  str_remove("m_")

#doing the same for female and unsexed  

female <- 
  df %>% 
  clean_names() %>% 
  select(keep, starts_with("f_"), -ends_with("_n")) %>% 
  mutate(sex = "female")

names(female) <- names(female) %>% 
  str_remove("f_")

unsexed <- 
  df %>% 
  clean_names() %>% 
  select(keep, starts_with("u"), -ends_with("_n")) %>% 
  mutate(sex = "unsexed")

names(unsexed) <- names(unsexed) %>% 
  str_remove("unsexed_")

#joining all the dfs back together 

clean <- male %>% 
  full_join(female) %>% 
  full_join(unsexed)

#messing around with horrible data, there is a variable across multiple columns
#look into read_xlsx
#can read certain sections/sheets