library(tidyverse)
library(easystats)
library(tidymodels)

#loading data
dat <- read_csv("../../Data/GradSchool_Admissions.csv")
#I am interested in looking at how GRE, GPA, and Rank effects admissions
#lets take a look at the structure of the data
head(dat)
range(dat$gre)
range(dat$gpa)


#visualizing data trends
ggplot(dat, aes(x = gre, y = gpa, color = as.factor(admit), size = rank)) +
  geom_point() +
  labs(x = "GRE Score", y = "GPA", color = "Admission Status", size = "Rank") +
  scale_color_manual(values = c("lightblue", "pink")) + 
  scale_size_continuous(range = c(2, 6)) +  
  theme_minimal()

#how many students were admitted vs not admitted?
table(dat$admit)
table(dat$rank)
#creating models to predict admittance from simple to complex. 
mod1 <- glm(data = dat,
            formula = as.logical(admit) ~ rank,
            family = "binomial")

mod2 <- glm(data = dat,
            formula = as.logical(admit) ~ gpa + gre,
            family = "binomial")

mod3 <- glm(data = dat,
            formula = as.logical(admit) ~ gpa * gre * rank,
            family = "binomial")


#lets look at how well the models predict
predictions_mod1 <- predict(mod1, newdata = dat, type = "response")
predictions_mod2 <- predict(mod2, newdata = dat, type = "response")
predictions_mod3 <- predict(mod3, newdata = dat, type = "response")

#combining predictions with actual outcomes
predictions <- data.frame(
  admit = dat$admit,
  prediction_mod1 = predictions_mod1,
  prediction_mod2 = predictions_mod2,
  prediction_mod3 = predictions_mod3
)

#converting probabilities to 0 and 1
predictions_binary <- predictions %>%
  mutate(
    outcome_mod1 = ifelse(prediction_mod1 >= 0.5, 1, 0),
    outcome_mod2 = ifelse(prediction_mod2 >= 0.5, 1, 0),
    outcome_mod3 = ifelse(prediction_mod3 >= 0.5, 1, 0)
  )

#evaluating model performance
performance_mod1 <- sum(predictions_binary$admit == predictions_binary$outcome_mod1) / nrow(predictions_binary)
performance_mod2 <- sum(predictions_binary$admit == predictions_binary$outcome_mod2) / nrow(predictions_binary)
performance_mod3 <- sum(predictions_binary$admit == predictions_binary$outcome_mod3) / nrow(predictions_binary)

performance_mod1
performance_mod2
performance_mod3


#creating table to compare with data table
prediction_counts <- data.frame(
  Model = c("mod1", "mod2", "mod3"),
  '0' = colSums(predictions_binary[, c("outcome_mod1", "outcome_mod2", "outcome_mod3")] == 0),
  '1' = colSums(predictions_binary[, c("outcome_mod1", "outcome_mod2", "outcome_mod3")] == 1)
)

prediction_counts

#comparing models
compare_performance(mod1, mod2, mod3, rank = TRUE)
compare_performance(mod1, mod2, mod3, rank = TRUE) %>% plot

#retraining models, 70% train, 30% test
set.seed(0)
split <- initial_split(dat, prop = 0.7)
training_dat <- training(split)
test_dat <- testing(split)

mod1 <- glm(data = training_dat,
            formula = as.logical(admit) ~ rank,
            family = "binomial")

mod2 <- glm(data = training_dat,
            formula = as.logical(admit) ~ gpa + gre,
            family = "binomial")

mod3 <- glm(data = training_dat,
            formula = as.logical(admit) ~ gpa * gre * rank,
            family = "binomial")

#looking at predictions again
predictions_mod1 <- predict(mod1, newdata = test_dat, type = "response")
predictions_mod2 <- predict(mod2, newdata = test_dat, type = "response")
predictions_mod3 <- predict(mod3, newdata = test_dat, type = "response")

predictions <- data.frame(
  admit = test_dat$admit,
  prediction_mod1 = predictions_mod1,
  prediction_mod2 = predictions_mod2,
  prediction_mod3 = predictions_mod3
)

predictions_binary <- predictions %>%
  mutate(
    outcome_mod1 = ifelse(prediction_mod1 >= 0.5, 1, 0),
    outcome_mod2 = ifelse(prediction_mod2 >= 0.5, 1, 0),
    outcome_mod3 = ifelse(prediction_mod3 >= 0.5, 1, 0)
  )

performance_mod1 <- sum(predictions_binary$admit == predictions_binary$outcome_mod1) / nrow(predictions_binary)
performance_mod2 <- sum(predictions_binary$admit == predictions_binary$outcome_mod2) / nrow(predictions_binary)
performance_mod3 <- sum(predictions_binary$admit == predictions_binary$outcome_mod3) / nrow(predictions_binary)

performance_mod1
performance_mod2
performance_mod3

prediction_counts <- data.frame(
  Model = c("mod1", "mod2", "mod3"),
  '0' = colSums(predictions_binary[, c("outcome_mod1", "outcome_mod2", "outcome_mod3")] == 0),
  '1' = colSums(predictions_binary[, c("outcome_mod1", "outcome_mod2", "outcome_mod3")] == 1)
)

prediction_counts

#comparing models again
compare_performance(mod1, mod2, mod3, rank = TRUE)
compare_performance(mod1, mod2, mod3, rank = TRUE) %>% plot
