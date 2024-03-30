library(tidyverse)
library(easystats)
install.packages("palmerpenguins")
library(palmerpenguins)

# does body mass vary significantly between penguin species
mod <- glm(data = penguins, formula = body_mass_g ~ species)
performance(mod)
summary(mod)

# Logistical Regression ####
# binary outcome
# family = binomial
# when predicting (type = "response")

# predicting whether a penguin is gentoo based on body mass
names(penguins)

mod2 <- penguins %>%
    mutate(gentoo = case_when(species == "Gentoo" ~ TRUE,
                              TRUE ~ FALSE)) %>%
    glm(data = .,
        formula = gentoo ~ bill_depth_mm + body_mass_g + flipper_length_mm + bill_length_mm,
        family = "binomial") # logistical regression (outcome t/f, family = binomial)
check_model(mod2)
summary(mod2)
penguins$pred <- predict(mod2, penguins, type = 'response')

penguins %>%
    ggplot(aes(x = body_mass_g, y = pred, color = species)) +
    geom_point()

preds <-
penguins %>%
    mutate(outcome = case_when(pred < 0.01 ~ "Not gentoo",
                               pred > 0.75 ~ "Gentoo")) %>%
    select(species, outcome) %>%
    mutate(correct = case_when(species == "Gentoo" & outcome == "Gentoo" ~ TRUE,
                               species != "Gentoo" & outcome == "Not gentoo" ~ TRUE,
                               TRUE ~ FALSE))

preds %>%
    pluck("correct") %>%
    sum() / nrow(preds)

# another

dat <- read_csv("./Data/GradSchool_Admissions.csv")

mod3 <- glm(data = dat,
            formula = as.logical(admit) ~ (gre + gpa) * rank,
            family = "binomial")
dat$pred <- predict(mod3, dat, type = "response")

dat %>%
    ggplot(aes(x = gre, y = pred, color = factor(rank))) +
    geom_point(alpha = .25) +
    geom_smooth()

dat %>%
    ggplot(aes(x = factor(rank), y = pred, color = factor(rank))) +
    geom_jitter(alpha = .25) +
    geom_boxplot()

# image recognition
# library(neuralnets)
# library(keras)

library(ranger)
rf <- ranger(data=dat,
       formula = admit ~ gpa + gre + rank,
       importance = 'permutation')
vip(rf)
summary(rf)
rf
