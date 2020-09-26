# https://www.datacamp.com/community/tutorials/keras-r-deep-learning
library(tidyverse)
library(keras)

iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) 

names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

plot(iris$Petal.Length, 
     iris$Petal.Width, 
     pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], 
     xlab="Petal Length", 
     ylab="Petal Width")

# Store the overall correlation in `M`
M <- cor(iris[,1:4])

library(corrplot)
# Plot the correlation plot with `M`
corrplot(M, method="circle")

# Data preprocessing
# Build your own `normalize()` function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalize the `iris` data
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))

# Return the first part of `iris` 
head(iris_norm)

iris[,5] <- as.factor(iris[,5]) -1

# Turn `iris` into a matrix
iris <- as.matrix(iris)

# Set `iris` `dimnames` to `NULL`
dimnames(iris) <- NULL

# Normalize the `iris` data
iris <- normalize(iris[,1:4])

# Return the summary of `iris`
summary(iris)


# Determine sample size
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# Split the `iris` data
iris.training <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

# Split the class attribute
iris.trainingtarget <- iris[ind==1, 5]
iris.testtarget <- iris[ind==2, 5]