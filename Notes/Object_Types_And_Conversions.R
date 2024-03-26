#Morning Quiz####
#make a df with  mtcars with only rows that have more than 4 cylinders (cyl)
mtcars$cyl > 4
mt_cars <- (mtcars[mtcars$cyl > 4,])

#pull out just the miles per gallon of those cars (mpg) and find the mean, min, and max
mean(mt_cars$mpg)
min(mt_cars$mpg)
max(mt_cars$mpg)

#Object types####
##logical####
c(TRUE, TRUE, FALSE)
##numeric####
1:10
##character####
letters[3]
##interger####
c(1L,2L,3L)
##data.frame####
mtcars[roq,cols]
##factor####
#acts a lot like character, but has levels and numerical assignments
as.factor(letters)
haircolors <- c("brown", "blonde", "black", "red", "red", "black")
haircolors <- as.factor(haircolors)
#can look at levels
levels(haircolors)
#levels will only show the possible options
c(as.factor(haircolors), "purple")
#the levels are not actuallly assigned brown, blonde, etc, it is stored as numbers instead
#have to convert it to a character
as.character(as.factor(haircolors), "purple")

#Typer conversions####
1:5 #numeric
as.character(1:5) #convert to a character
as.numeric(letters) #tries but gives me a bunch on NA becuase what is the numerical value of a letter
as.numeric(c("1", "b","35")) #it does its best and will pull out the numbers 
x <- as.logical(c("true", "t", "F", "False", "T")) #needs to be capital or full true/false 
#false is 0 and true is 1, can be summed or whatever math you want to do with it, however NA 
#will cause is to return NA as is does not know what to do. 
sum(x, na.rm = TRUE)
#na.rm removes NA and allows you to preform math, this is in multiple different functions 

#Data Frames####
str(mtcars) #is currently numeric
names(mtcars)
View(mtcars)
#how to view columns
mtcars[,"mpg"]
#how to turn df to character
#for-loop assigns character version of every column over itself
for(col in names(mtcars)){
  mtcars[,col] <- as.character(mtcars[,col]) 
  # can add () or "" wrap by highlighting what you want to wrap and then using " or (
}
str(mtcars)

data("mtcars") #resets built in data

#using new data
path <- "./Data/cleaned_bird_data.csv"
df <- read.csv(path)
str(df)
#converting to character
for(col in names(df)){
  df[,col] <- as.character(df[,col])
}
str(df)
#write the new file to your computer 
?write.csv
write.csv(df, file = "./Data/character_cleaned_bird_data.csv")

#Apply family functions####
#if you want to do the same function on every column or row use the apply function
apply(mtcars,2,as.character) #2 is columns, 1 is rows 
lapply(list, function)
sapply(list, function)
vapply(list, function, FUN.VALUE = type, ...)
)

#using tidyverse####
library(tidyverse)
#pipeline
mtcars %>% #use ctrl+shift+M to create a pipe (%>%)
  #the thing on the left side of the pipe becomees first argument to thing on the right 
  filter(mpg > 19)
?filter
#another example
mtcars$mpg %>% mean()

abs(mean(mtcars$mpg))

mtcars$mpg %>% 
  mean() %>% 
  abs()
#filters
mtcars %>% 
  filter(mpg > 19 & vs == 1)
