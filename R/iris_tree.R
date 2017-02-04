# Set random seed. Don't remove this line.
set.seed(1)
library(rpart)
library(rpart.plot)

# Take a look at the iris dataset
str(iris)
summary(iris)

# A decision tree model has been built for you
tree <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
              data = iris, method = "class")

# A dataframe containing unseen observations
unseen <- data.frame(Sepal.Length = c(5.3, 7.2), 
                     Sepal.Width = c(2.9, 3.9), 
                     Petal.Length = c(1.7, 5.4), 
                     Petal.Width = c(0.8, 2.3))

# Predict the label of the unseen observations. Print out the result.
predict(tree, unseen, type = 'class')

rpart.plot(tree)
plot(Petal.Length ~ Petal.Width, data = iris, col = iris$Species)
points(unseen$Petal.Width, unseen$Petal.Length, col = 'blue', bg = 'blue', pch = 19)
