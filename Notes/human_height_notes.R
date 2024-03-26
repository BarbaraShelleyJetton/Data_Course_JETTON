# SETUP ####
library(tidyverse)
library(readxl)
library(measurements)

# DATA ####
path <- "./Data/human_heights.xlsx"
dat <- read_xlsx(path)

# CLEAN ####
# we are seeing if there is a difference in heights between males and females
dat <- 
dat %>% 
  pivot_longer(everything(),
               names_to = "sex",
               values_to = "height") %>% 
  separate(height, into = c("feet","inches"),convert = TRUE) %>% 
  mutate(inches = (feet*12) + inches) %>% 
  mutate(cm=conv_unit(inches, from='in',to='cm'))

dat %>% 
  ggplot(aes(x=cm,fill=sex)) +
  geom_density(alpha=.5)
# a T test looks at mean difference and sees if it is significant enough
t.test(dat$cm ~ factor(dat$sex))
?t.test
x ~ y # for formula: lhs = left hand side and rhs = right hand side, the outcome (what you want to look at is lhs (cm), and what you are comparing on rhs (sex))
# p value is that assuming null is true (equal) what are the chances, if picked randomly, there would be a difference
# think of it as surprise value, the smaller this number the more shocked im going to be 
# .05 is the one that we use for some reason which means 5 out of 100 times you will be wrong. 
mod <- glm(data= dat,
    formula = cm ~ sex)
summary(mod)
# glm is the big kid version of a t test
# generized linear model (y = mx + b), y = y, mx = slope, b = y-intercept 

#playing with mpg###
mpg %>% 
  ggplot(aes(x = displ, y = cty)) +
  geom_point(aes(color = class,
                 size = 1.5)) +
  geom_smooth(method = 'glm')
mod <- glm(data = mpg,
           formula = cty ~ displ)

install.packages("easystats")
library(easystats) #helps write models and interprets models in 
report(mod) # writes a report
performance(mod) #give you model preformance
#the higher the R^2 value the closer the data is to the line (model explains data), RMSE root mean square error, higher is worse here how far off is each point from the line
performance::check_model(mod) # looks at if model is valid 
