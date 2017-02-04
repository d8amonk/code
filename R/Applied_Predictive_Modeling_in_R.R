install.packages(c("caret", "pROC", "rpart", "partykit", 
                   "C50", "kernlab", "AppliedPredictiveModeling",
                   "earth", "mda", "nnet"),
                 dependencies = c("Depends", "Imports", "Suggests"))

library(caret)
data(segmentationData)
# get rid of the cell identifier
segmentationData$Cell <- NULL

training <- subset(segmentationData, Case == "Train")
testing <- subset(segmentationData, Case == "Test")

training$Case <- NULL
testing$Case <- NULL

str(training[,1:9])
cell_lev <- levels(testing$Class) #grabs the levels - use for police data

trainX <- training[, names(training) != "Class"]
# Methods are "BoxCox", "YeoJohnson", center", "scale",
# "range", "knnImpute", "bagImpute", "pca", "ica",
# "spatialSign", "medianImpute", "expoTrans"
preProcValues <- preProcess(trainX, method = c("center", "scale"))
preProcValues

scaledTrain <- predict(preProcValues, trainX)
