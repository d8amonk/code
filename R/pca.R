require(dplyr)
# require(foba)
# model.foba <- foba(xx[3:10], xx[1], steps = 20)


setwd("D:/")

x <- read.csv("XIV_jvg.csv", na.strings = 0)  
glimpse(x)
summary(x)
x$Date <- as.Date(x$Date, format = '%m/%d/%Y')

xx <- x[, 2:dim(x)[2]]
glimpse(xx)
fit <- lm(Open ~ ., xx, na.action = na.omit)

library(mlbench)
library(caret)
xx[is.na(xx)] <- 0
correlationMatrix <- cor(xx[,2:length(xx)])
correlationMatrix[is.na(correlationMatrix)] <- 0
# write.csv(correlationMatrix, "corrMat_SIC.csv")
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
length(highlyCorrelated)

xx['NA'] <- NULL

control <- trainControl(method="boot", number=10, repeats=3)
# train the model
model <- train(Open ~ ., data=xx, method="bayesglm", preProcess="scale")
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
