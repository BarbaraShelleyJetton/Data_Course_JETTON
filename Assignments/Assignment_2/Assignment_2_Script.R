#Assignment 2 (B.Jetton)
#Task 4: find the csv files and save as an object
csv_files <-list.files(path = "Data/", pattern = ".csv", recursive = TRUE)

#Task 5: find length of files that match
y <- length(csv_files)

#Task 6: open specific file and save as object
df <- read.csv(list.files(pattern = "wingspan_vs_mass.csv", recursive = TRUE))

#Task 7: read first 5 lines of file
head(df, n=5)

#Task 8: find files in Data that begins with "b"
b_files <- list.files(path = "Data/", pattern = "^b", recursive = TRUE, full.names = TRUE)

#Task 9: display the first lines of each "b" files
for (x in b_files){
  print(readLines(x, n=1))
}

#Task 10: do the same for files that end in .csv
csv_files <- list.files(pattern =".csv", recursive = TRUE, full.names = TRUE)
for (x in csv_files){
  print(readLines(x, n=1))
}

