n.points <- 100 #number of rows in the data
sampling.rate <- 0.8

data <- data.frame(age = floor(runif( n.points,18,70)),
                     income = floor(runif(n.points, 25, 150)),
                     credit = rep(NA, n.points))

for(i in 1:length(data$credit)){
  ifelse(data$income[i] > 80, 
         data$credit[i] <- "High",
         data$credit[i] <- "Low")}

# we need the number of points in the test set 
# in order to calculate the misclassification rate
num.test.set.labels <- n.points*(1 - sampling.rate)

# randomly sample which rows will go in the training set
training <- sample(1:n.points, sampling.rate*n.points, replace = F)
# define the training set to be those rows
train <- subset(data[training,], select = c(age, income))

# the other rows are going in the test set 
testing <- setdiff(1:n.points, training)

# define the test set to be those rows
test <- subset(data[testing,], select = c(age, income))

# subset of rows from the training set
cl <- data$credit[training]
# subset of rows we are WITHHOLDING as the test set
true.labels <- data$credit[testing]

train
cl
test
true.labels

# check it!
nrow(train) + nrow(test) == nrow(data)

# k-nn matching (supervised) algo to predict label classes
library(FNN)
knn(train, test, cl, k=3)
