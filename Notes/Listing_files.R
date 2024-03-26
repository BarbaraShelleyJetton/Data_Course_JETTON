getwd()

#lists files in assignments, recursive means it goes through each
list.files(path = "Assignments/", recursive = TRUE)
#can look at help
?list.files()

#lists files that have .csv and saves in x so it can be accessed 
x <- list.files(pattern = ".csv", recursive = TRUE)
x(158)

#readLines goes line by line so one row would be one line. 
readLines("Data/wide_income_rent.csv")

#reads csv
read.csv("Data/wide_income_rent.csv")
#reads whatever number data in x from all .csv files and saves as dat
dat <- read.csv(x[158])
