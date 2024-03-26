library(tidyverse)
library(palmerpenguins)
#subset this penguin data from to only those observations where
#bill_length_mm > 40
penguins %>%
  filter(bill_length_mm > 40 & sex == "female") %>% #find mean of body_mass_g
  pluck("body_mass_g") %>% #pluck takes the data from the specific column you ask for
  mean

#do the same but for each species
penguins %>%
  filter(bill_length_mm > 40 & sex == "female") %>%
  group_by(species,island) %>% #creates a tibble which is the same as a df, commas to seperate grouping
  summarize(mean_body_mass = mean(body_mass_g), #creates a data type called dbl (treat as numeric)
            min_body_mass = min(body_mass_g), 
            max_body_mass = max(body_mass_g),
            sd_body_mass = sd(body_mass_g),
            N = n()) %>% #is number of rows (aka # of penguins)
  arrange(desc(mean_body_mass)) %>% #arranges from low to high (biggest penguin)
  write_csv("./Data/penguin_summary.csv")

#practice
#find the fatty penguins (body_mass > 5000)
#count how many are male and how many are females
#return the max body mass for males and females
#bonus: add new column to penguins that says whether they're fat
penguins %>% 
  filter(body_mass_g > 5000) %>% 
  group_by(sex) %>% 
  summarize(N = n(),
            mean_body_max = max(body_mass_g))
penguins %>% 
  mutate(fat_bitches = body_mass_g > 5000)#changing and making new columns

#another way to make bonus work while adding own flare 
x <- penguins %>% 
  mutate(fatstat = case_when(body_mass_g > 5000 ~ "fat bitch",
                             body_mass_g <= 5000 ~ "skinny bitch"))#case_when only works in mutate

#plotting
x %>% 
  filter(!is.na(sex)) %>% 
  ggplot(mapping = aes(x=body_mass_g, #aes maps varibales where you want
                       y=bill_length_mm,
                       color = fatstat)) + 
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c("red", "blue")) + #can use custom colors
  #scale_color_viridis_d(option = 'plasma', end = 0.8) #or color packs
  theme_dark() +
  theme(axis.text = element_text(angle = 180, face = 'italic'))

# :: is called name place
# + adds new layer
#### Day 2 Plotting ####
names(penguins)
ggplot(penguins, mapping = aes(x = flipper_length_mm, color = species)) +
  geom_bar()
#only uses an x or a y for stat
# essentially makes a histogram
names(penguins)
ggplot(penguins, mapping = aes(x = flipper_length_mm,
                               y = body_mass_g, color = species)) +
  geom_col()
#see now the position stack numbers for body_mass took the sum of the penguins that had x flipper length
#behind the scene there is a stat and position stack
names(penguins)
ggplot(penguins, mapping = aes(x = flipper_length_mm,
                               y = body_mass_g, color = species)) +
  geom_line(aes(group = species))
#makes a horrific line chart
#aes before geom is a universal aes while within the aes it is local and overrides the aes (can be used to add
#more detail)
#lets make somthing real ugly
names(penguins)
ggplot(penguins, mapping = aes(x = flipper_length_mm,
                               y = body_mass_g, color = species)) +
  geom_path(aes(group = species)) +
  stat_ellipse() +
  #geom_point(color = 'gray')
  geom_point(aes(color=sex)) +
#aes uses columns while outside the aes it doesnt (compare gray and color = sex)
  geom_polygon() +
  geom_hex() +
  geom_bin_2d() +
#each geom adds a new layer that builds off the last
  geom_boxplot() +
#some geoms are more complex than others 
  geom_hline(yintercept = 4500, linewidth = 7, color = 'magenta', 
             linetype = '1121', alpha = .50)+
#alpha makes transparent
#can put line_width, alpha, any aes really to a data point (like body_mass)
  geom_point(color = 'yellow', aes(alpha = bill_depth_mm)) +
#the lower the body_mass the more yellow, while the higher the more clear 
#using custom theme
  theme(axis.title = element_text(face = "italic", size = 12, angle = 30),
        legend.background = element_rect(fill = 'hotpink', color = 'blue', linewidth  =5))
#fill is the inside, color is the outsied
#use geomimage to insert your own images

#playing with plots

x %>% 
  filter(!is.na(sex)) %>% 
  ggplot(penguins, mapping = aes(x = bill_depth_mm,
                                 y = body_mass_g)) +
  geom_hex(mapping = aes (color = species, )) +
  xlab("Bill Depth (mm)") +
  ylab("Body Mass (g)")


penguins %>% 
  ggplot(aes(x=body_mass_g, fill = species)) +
  geom_density(alpha=.25)

df <- read.delim("./Data/DatasaurusDozen.tsv")
df %>% 
  group_by(dataset) %>% 
  summarize(meanx = mean(x),
            sdx = sd(x),
            minx = min(x),
            median = median(x))
df %>% 
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  facet_wrap(~dataset)

library(GGally) 
ggpairs(penguins) #ggpairs used to view all variables super fast, gives correlations (birds eye view)

#make a graph of bill_depth and body_mass
penguins %>%
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = body_mass_g,
             y = bill_depth_mm,
             color = sex)) +
  geom_density2d() +
  facet_wrap(~species) +
  labs(x = "Body Mass (g)", y = "Bill Depth (mm)")+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 14))

penguins %>%
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_point(aes(color=sex), size=4, alpha=.75) +
  facet_wrap(~species) +
  scale_colour_viridis_d(end = .8) +
  labs(x = "Bill Depth (mm)", y = "Body mass (g)", color = 'sex') +
  theme(strip.background = element_blank(),
        strip.text = element_text(face='bold', size = 12))

library(gapminder)
