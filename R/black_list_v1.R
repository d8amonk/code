library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


set.seed(1984)
site_list <- data.frame(sites = sample(letters, 1000, replace = T), 
                        CPA = rnorm(1000, 3.50, 1),
                        Clicks = rnorm(1000, 1500, 1000),
                        Imps = rnorm(1000, 50000, 30000),
                        Is_Active = rbinom(1000, 1, 0.7))

n <- nrow(site_list)
shuffled <- site_list[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

tree <-  rpart(Is_Active ~ ., train, method = 'class')
fancyRpartPlot(tree)

pred = predict(tree, test, type = 'class')

# Construct the confusion matrix: conf
conf = table(test$Is_Active, pred)

# Print out the accuracy
sum(diag(conf))/sum(conf)

# pruning 
tree <- rpart(Is_Active ~ ., train, method = "class", control = rpart.control(cp=0.00001))

# Draw the complex tree
fancyRpartPlot(tree)

# Prune the tree: pruned
pruned = prune(tree, cp = 0.01)

# Draw pruned
fancyRpartPlot(pruned)

# Train and test tree with gini criterion
tree_g <- rpart(Is_Active ~ ., train, method = "class")
pred_g <- predict(tree_g, test, type = "class")
conf_g <- table(test$Is_Active, pred_g)
acc_g <- sum(diag(conf_g)) / sum(conf_g)

# Change the first line of code to use information gain as splitting criterion
tree_i <- rpart(Is_Active ~ ., train, method = "class", parms = list(split = "information"), control=rpart.control(minsplit=25, cp=0.005))
pred_i <- predict(tree_i, test, type = "class")
conf_i <- table(test$Is_Active, pred_i)
acc_i <- sum(diag(conf_i)) / sum(conf_i)

# Draw a fancy plot of both tree_g and tree_i
fancyRpartPlot(tree_g)
fancyRpartPlot(tree_i)

# Print out acc_g and acc_i
acc_g
acc_i








