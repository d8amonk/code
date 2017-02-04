# The emails dataset: https://archive.ics.uci.edu/ml/datasets/Spambase
emails.names <- unlist(read.table("c:/Users/jvangeete/Google Drive/Code/R/spambase/spambase_names.csv", stringsAsFactors = F, header = F))
emails <- read.csv("c:/Users/jvangeete/Google Drive/Code/R/spambase/spambase_data.csv")
colnames(emails) <- emails.names

# Show the dimensions of emails
dim(emails)

# Inspect definition of spam_classifier()
spam_classifier <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 4] <- 1
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(prediction)
}

# Apply the classifier to the avg_capital_seq column: spam_pred
spam_pred = spam_classifier(emails$avg_capital_seq)

# Compare spam_pred to emails$spam. Use ==
spam_pred == emails$spam

# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
# set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(titanic)
shuffled <- titanic[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Change the model that has been learned. One of the arguments is incorrect.
tree <- rpart(Survived ~ ., train, method = "class")

# Predict the outcome on the test set with tree: pred
pred = predict(tree, test, type = 'class')

# Calculate the confusion matrix: conf
conf = table(test$Survived, pred)

# Print this confusion matrix
conf