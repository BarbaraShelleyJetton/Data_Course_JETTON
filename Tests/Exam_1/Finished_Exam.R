library(tidyverse)

# I ####
# Read the cleaned_covid_data.csv file into an R data frame. (20 pts)

df <- read.csv("cleaned_covid_data.csv")

# II ####
# Subset the data set to just show states that begin with "A" and save this as an object called A_states. (20 pts)
# Use the *tidyverse* suite of packages
# Selecting rows where the state starts with "A" is tricky (you can use the grepl() function or just a vector of those states if you prefer)

A_states <- df %>% 
  filter(grepl("^A", Province_State))

# III ####
# Create a plot _of that subset_ showing Deaths over time, with a separate facet for each state. (20 pts)
# Create a scatterplot
# Add loess curves WITHOUT standard error shading
# Keep scales "free" in each facet

str(A_states$Last_Update)

A_states$Last_Update <- as.Date(A_states$Last_Update)

ggplot(A_states, aes(x = Last_Update,
                     y = Deaths)) +
  geom_point() +
  geom_smooth(aes(x = Last_Update, y = Deaths), method = "loess", se = FALSE, span = 0.5) +
  facet_wrap(~Province_State, scales = 'free') +
  labs(title = "Deaths Over Time by State",
       x = "Time",
       y = "Deaths")

# IV #### (Back to the full dataset)
# Find the "peak" of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)
# Im looking for a new data frame with 2 columns:
# "Province_State"
# "Maximum_Fatality_Ratio"
# Arrange the new data frame in descending order by Maximum_Fatality_Ratio
# This might take a few steps. Be careful about how you deal with missing values!

state_max_fatality_rate <- df %>% 
  group_by(Province_State) %>% 
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))

# V ####
# Use that new data frame from task IV to create another plot. (20 pts)
# X-axis is Province_State
# Y-axis is Maximum_Fatality_Ratio
# bar plot
# x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
# X-axis labels turned to 90 deg to be readable
# Even with this partial data set (not current), you should be able to see that (within these dates), different states had very different fatality ratios.

ggplot(state_max_fatality_rate, aes(x = factor(Province_State, levels = rev(Province_State)),
                                    y = Maximum_Fatality_Ratio)) +
  geom_col() + 
  labs(title = "Maximum Fatality Ratio by State",
       x = "Province / State",
       y = "Maximum Fatality Ratio") +
  theme(axis.text = element_text(angle = 90))

# VI (BONUS 10 pts) ####
# Using the FULL data set, plot cumulative deaths for the entire US over time
# Youll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.

df$Last_Update <- as.Date(df$Last_Update)

df_cumulative <- df %>% 
  group_by(Last_Update) %>% 
  summarise(Cumulative_Deaths = cumsum(Deaths))

ggplot(df_cumulative, aes(x = Last_Update,
                     y = Cumulative_Deaths)) +
  geom_line() +
  labs(title = "Cumulative Deaths Over Time in the US",
       x = "Time",
       y = "Cumulative Deaths")
  