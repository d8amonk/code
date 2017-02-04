library(Amelia)
library(scales)

training.data.raw <- read.csv("c:/Users/jvangeete/Desktop/titanic.csv",
                    header = T, na.strings = c(""))

# check for MVs and subset.  (consider a threshold?)
sapply(training.data.raw, function(x) sum(is.na(x)))
missmap(training.data.raw, main = "Missing Values vs. Observed")
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

# replace remaining MVs "by hand" (can't do Emarbked == CatVar)
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = T)
missmap(data, main = "Missing Values vs. Observed")

# more data checking
sapply(data, function(x) is.factor(x))
contrasts(data$Sex)
contrasts(data$Embarked)

# kill the two rows with Embarked == NA
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

# create train and test sets
train <- data[1:800,]
test <- data[801:889,]

# build the model
summary(model <- glm(Survived ~., 
             family = binomial(link = 'logit'),
             data = train)
        )
# Remember that in the logit model the response variable 
# is log odds: ln(odds) = ln(p/(1-p)) = a*x1 + b*x2 + . + z*xn. 
# Since male is a dummy variable, being male reduces the log 
# odds by 2.75 while a unit increase in age reduces the 
# log odds by 0.037
fitted(model)

logit2prob <- function(l){exp(l)/(1+exp(l))}
# or package arm has the function invlogit
logit2prob(model$coefficients)

# analyze the table of deviance
inspect <- anova(model, test = 'Chisq')
plot(inspect$`Resid. Dev`, type = 'l', col = 'red', lwd = 2,
     main = 'Residual Deviance by Additional Var',
     ylab = 'Deviance')

# no exact equivalent to the R2 of linear regression exists, 
# the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(model)

# run model on test data
fitted.results <- predict(model,
                          newdata = subset(test,
                                         select=c(2,3,4,5,6,7,8)),
                          type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misclass_error <- mean(fitted.results != test$Survived)
print(paste('Accuracy =', percent(1 - misclass_error)))
# Accuracy is dependent on the train/test split
# Consider (k-fold) Cross Validation

# plot the ROC curve and calculate the AUC (area under the curve) 
# which are typical performance measurements for a binary classifier.
library(ROCR)
p <- predict(model, 
             newdata = subset(test,
                              select=c(2,3,4,5,6,7,8)), 
             type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
percent(auc)














