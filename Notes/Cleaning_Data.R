library(tidyverse)
#Cleaning data ####

df <- read_csv("../..//Data/wide_income_rent.csv")

#plot rent prices for each state
#state on x-axis, rent on y-axis, bar chart
#rent and income are in rows while state is in columns, this is nasty
#we need to flip it, so we use transpose (t)

df2 <- (t(df)) %>% as.data.frame

df2 <- df2[-1,]
df2$State <- row.names(df2)
names(df2) <- c("income", "rent", "State")
#this method is tedious and easy to fuck up

df %>% 
  pivot_longer(-variable, names_to = "state", values_to = "amount")#everything except variable and nameing
#I mean techinically this can work, the only thing is that rent and income are in the same column 

df <- df %>% 
  pivot_longer(-variable, names_to = "state", values_to = "amount") %>% 
  pivot_wider(names_from = variable, 
              values_from = amount)
#if one variable is across multiple columns ... pivot longer
#if multiple varibales are in a single column ... pivot wider

## Plotting ####

df %>% 
  pivot_longer(-variable, names_to = "state", values_to = "amount") %>% 
  pivot_wider(names_from = variable, 
              values_from = amount) %>% 
  ggplot(aes(x = state,
             y = rent,
             color = state)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 6), legend.position = "none")

# cleaning data sets ####

table1 #this one is actually right because years are different
table2 #this one is not ... fix it

table2 %>% pivot_wider(names_from = type,
                       values_from = count)

table3 # has two variables in one row (rate)

table3 %>% 
  separate(rate, into =c("cases", "population"))

#joining tables ####

table4a
table4b
# table 4a and 4b are the same dataset made into two different tables, becuase god is dead
#you have to fix tables indivudually first

x <- table4a %>% 
  pivot_longer(-country, names_to = "year",
               values_to = "cases")

y <- table4b %>% 
  pivot_longer(-country, names_to = "year",
               values_to = "population")
#now we can join them together, in order to combine you need to have atleast one common variable 

full_join(x,y)

table5 #absoluley disgusting

table5 %>% 
  separate(rate, into =c("cases", "population"), convert = TRUE) %>% #convert turns things into what they should be (int, dbl, chr)
  mutate(year = paste0(century,year) %>% as.numeric()) %>% 
  select(-century)

# cleaning excek ####
library(readxl)

dat <- read_xlsx("./Data/messy_bp.xlsx") #NEVER use excel and dates that are combined 01/30/2001 seperate by month day year in different columns
#this data is disguting 

dat <- read_xlsx("./Data/messy_bp.xlsx", skip = 3)
# how do we get blood pressure in one column and visit in another, not including Heart rate

bp <- 
  dat %>% 
  select(-starts_with("HR"))
bp %>% 
  pivot_longer(starts_with("BP"),
               names_to = "visit",
               values_to = "BP") %>% 
  mutate(visit = case_when(visit == "BP...8" ~ 1,
                           visit == "BP...10" ~ 2,
                           visit == "BP...12" ~ 3,))

#do this with heart rate and then full join the tables