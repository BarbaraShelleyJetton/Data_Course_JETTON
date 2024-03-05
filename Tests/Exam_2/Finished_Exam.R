library(tidyverse)
library(easystats)
#Reading in data
data <- read_csv("unicef-u5mr.csv")

#Cleaning data
tidy <- data %>% 
  pivot_longer(starts_with("U5MR."),
               names_to = "Year",
               values_to = "U5MR") %>% 
  mutate(Year = as.numeric(str_replace(Year, "U5MR.", "")))

#Plot 1
tidy %>% 
  ggplot(mapping = aes(x = Year,
                       y = U5MR)) +
  geom_path() +
  theme_minimal()+
  facet_wrap(~Continent)

#Plot 2
mean_tidy <- tidy %>%
  group_by(Continent, Year) %>% 
  summarise(Mean_U5MR = mean(U5MR, na.rm = TRUE))
mean_tidy %>% 
  ggplot(mapping = aes(x = Year,
                       y = Mean_U5MR,
                       color = Continent))+
  geom_line()+
  theme_minimal()
  
#Models
mod1 <- glm(data = tidy, formula = U5MR ~ Year)
mod2 <- glm(data = tidy, formula = U5MR ~ Year + Continent)
mod3 <- glm(data = tidy, formula = U5MR ~ Year * Continent)

#Comparing
compare_performance(mod1, mod2, mod3)
'''I believe mod3 is the best fit model as it has the highest R2 (meaning the
year and continent (and their interaction) explains the variation in the U5MR),
and it has the lowest RMSE (meaning it has the lowest difference in standard
deviation between predicted vs observed values)'''

#Plotting
tidy$pred1 <- predict(mod1, tidy)
tidy$pred2 <- predict(mod2, tidy)
tidy$pred3 <- predict(mod3, tidy)
tidy_2 <- tidy %>% 
  pivot_longer(starts_with("pred"),
               names_to = "Model",
               values_to = "Predicted_U5MR")
tidy_2 %>% 
  ggplot(mapping = aes(x = Year,
                       y = Predicted_U5MR,
                       color = Continent))+
  geom_line() +
  theme_minimal()+
  facet_wrap(~Model)
