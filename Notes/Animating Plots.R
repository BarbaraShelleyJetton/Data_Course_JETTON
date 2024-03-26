library(tidyverse)
library(patchwork)
install.packages("ggimage")
library(ggimage)
install.packages("gganimate")
library(gganimate)
install.packages("gapminder")
library(gapminder)

# Warm up ####

df <- gapminder 
p <- ggplot(df, aes (x = year,
            y = lifeExp,
            color = continent)) +
  geom_point(aes(size = pop)) + #sizing by countries population
  facet_wrap(~continent)

# The patchwork package ####
# lets us stick plots together 
p.dark <- 
  p+
  theme_dark()

p + p.dark #adding plots together
p / p.dark #spliting horitzonatlly 

(p + p.dark) + plot_annotation("MAIN TITLE") +
  patchwork::plot_layout(guides = 'collect')

# can make keys/legends the same if the plots share that information

# how can we show  the effect of gdpPercap and continent on life expectancy including years?
# we cant, that too much info but we can animate it. 
# looking at the plot without year
df$year %>% range # range of year

mycountries <- c("Venezuela", "Rwanda", "Nepal", "Iraq", "Afghanistan", "United States") #making a list of countries I want to look at

df <- 
df %>% 
  mutate(mycountries = case_when(country %in% mycountries ~ country))#adding to df

#plotting
p3 <- 
  ggplot(df, 
         aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point(aes(size = pop)) +
  geom_text(aes(label = mycountries))
p3

#making animation
p3 +
  transition_time(time = year) +
  labs(title = 'Year: {frame_time}')
anim_save("./Notes/gapminder_animation.gif") #use to save an animation
ggsave("./Notes/plot.pdf") #use to save plot
