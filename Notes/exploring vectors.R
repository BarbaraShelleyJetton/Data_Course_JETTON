x <- c(1,2,3,4,5)
for(i in x){
  print(i + 1)
}
#short cuts for the previous for loop
x + 1

#tells you class, R will treat classes different
class(x)

####vectors####
'''x is a vector. Each vector has a class (numeric, character),
has one dimension, must be all of the same class, 
length can be = 0'''

a <- 1:10
b <- 2:11
c <- letters[1:10]
d <- rep(TRUE, 10)

# to show only the 1,3,5 letter you need to concatenate (c) 
#beacause vectors only have one dimension (column)
w <- letters[c(1,3,5)]

#now how do you combine a,b,c keeping all data and class types
z <- data.frame(a,b,c, d)

#a data frame is a list of vectors, the difference between a 
#df and a matrix is that a df can have different class types

#class, length, dimensions of data frame z
class(z)
length(z)
dim(z)

#testing out logical vectors
1 > 0
0 >= 0
3 < 1
1 == 1
1 != 1
5 > a

#want the rows less than five in the df, if you leave , it will give you all rows
z[5 > a,]
#df have 2 dimensions [row, column] 

#using iris, in the column sepal length, find ones greater than 5
#$ allows to look in whatever column you want
#this returns a logical vector
iris$Sepal.Length > 5
#then we just input it into the below to get rows
iris[iris$Sepal.Length > 5,]

#how many had greater than 5?
nrow(iris[iris$Sepal.Length > 5,])

#turning into df
big_iris <- iris[iris$Sepal.Length > 5,]

#creating a new column in big_iris that is equal to sepal.length * sepal.width
big_iris$Sepal.Area <- big_iris$Sepal.Length * big_iris$Sepal.Width

#how just setosa from big_iris
big_setosa <- big_iris[big_iris$Species == "setosa",]

#mean sepal area from big_setosa
mean(big_setosa$Sepal.Area)

#plotting and math functions
plot(big_setosa$Sepal.Length, big_setosa$Sepal.Width)
sd(big_setosa$Sepal.Length)
sum(big_setosa$Sepal.Length)
min(big_setosa$Sepal.Length)
max(big_setosa$Sepal.Length)
summary(big_setosa$Sepal.Length)
cumsum(big_setosa$Sepal.Length)
cumprod(big_setosa$Sepal.Length)


