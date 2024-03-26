library(tidyverse)
library(easystats)
library(MASS)
mod1 <- glm(data = mpg, formula = cty ~ displ) #knows how to predict city with displacement 
class(mod1)
# is a list
mod1$residuals #
mod1$formula #formula is a specific class of an r object. nice way of accessing formula

#new models
mod2 <- glm(data = mpg, formula = cty ~ displ + cyl) #knows how to predict city with cisplacement and cylinder 
mod3 <- glm(data = mpg, formula = cty ~ displ * cyl) #cyl is an interaction variable (how displacement and cylinder interact)
compare_models(mod1, mod2, mod3) #the number change based off the fact that cylinder can explain some of the variation
compare_performance(mod1, mod2, mod3) #mod3 has more explanitaory value (R@), RMSE is better when smaller, AIC smaller is better (looks at how complex the model is, we dont like complex models)
compare_performance(mod1, mod2, mod3) %>% plot #can also visualize it

#looking at different models
#mod1
mpg %>% 
  ggplot(aes(x=displ, y=cty)) +
  geom_smooth(method = 'glm')

#mod3
mpg %>% 
  ggplot(aes(x=displ, y=cty, color = factor(cyl))) +
  geom_smooth(method = 'glm') #can see that there is an interaction coefficient and that engine level is impacted differently

#preictions
predict(mod1, mpg) #looking for displ but doesnt know mpg, he formula shows it only has displ

predict(mod1, data.frame(displ = 1:100)) #predicited mpg for 1-100
# it fucks up the numbers because it thinks its so smart (getting -mpg) as it does not know it cant go past 0. Models only work within the range they were created for. cannot predict anything for outside the data it has seen

mpg$pred <- predict(mod1,mpg)

mpg %>% 
  ggplot(aes(x = cty, y = pred))+
  geom_point() #if you had a perfect model you would see a straight line. (you dont want models to be perfect that would mean it is overfit.)
#still cn improve model

#for model 2
mpg$pred2 <- predict(mod2,mpg)

mpg %>% 
  ggplot(aes(x = cty, y = pred2))+
  geom_point()

#for model 3
mpg$pred3 <- predict(mod3,mpg)

mpg %>% 
  ggplot(aes(x = cty, y = pred3))+
  geom_point()

#showing us all three models overlayed on top of dataset
mpg %>% 
  pivot_longer(starts_with("pred")) %>% 
  ggplot(aes(x = displ, y = cty, color = factor(cyl)))+
  geom_point()+
  geom_point(aes(y=value), color = 'black')+
  facet_wrap(~name)
#mod1 has one slope as it is only looking at displ, mod2 all have the same slope but is added/subtracting depending on cyl, mod3 is allowed different slopes as it looks at the interaction 

#creating a new model
#making a new variable of auto or not
mpg <- 
mpg %>% 
  mutate(auto = grepl("auto",trans))

mod4 <- glm(data = mpg, formula = cty ~ displ * cyl * auto)#if you use * it * for everyvariable so it makes model very complex

compare_models(mod1, mod2, mod3, mod4)
compare_performance(mod1, mod2, mod3, mod4) %>% plot

#lets see if using auto is even important as mod4 is only slightly better
summary(mod4)
#you could write your model only using the onces that are significant
#we are going to learn a shortcut, it will feel like maigc, but know it should be used carefully
mod5 <- glm(data = mpg,cty ~ displ * year + cyl * trans * drv * fl + class)#just making an overly complex model 
step <- stepAIC(mod5) #goes through every step possibility and keeps it in if it improves model, if not it takes it out
step$formula

mod_best <- glm(data = mpg, formula = formula(step))
compare_performance(mod1, mod2, mod3, mod4, mod_best) %>% plot()

mpg$pred_best <- predict(mod_best,mpg)

mpg %>% 
  ggplot(aes(x = cty, y = pred_best))+
  geom_point() #idk why this isnt working D:

#checking models
check_model(mod_best)
#looking at normality residuals, it looks like our mode is not good at predicting 
