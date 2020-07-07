#loading required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randonForest", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")

#importing data
training <- read_csv("rawData/pml-training.csv")
validation <- read_csv("rawData/pml-testing.csv")

#printing the dimensions of training and validation set
dim(training)
dim(validation)

#removing NearlyZeroVariance variables
nzv <- nearZeroVar(training)

training <- training[,-nzv]
validation <- validation[,-nzv]

#printing the new dimensions of training and validation set after removing nearZeroVariance variables
dim(training)
dim(validation)

#Removing variables with large NAs
nasCol <- sapply(training, function(x) mean(is.na(x)) > 0.95)
training <- training[,nasCol == F]
validation <- validation[,nasCol == F]

#printing the new dimensions of training and validation set after removing variables with large NAs
dim(training)
dim(validation)

#structure of the data
str(training)

#removing columns with id information only (col 1 through 5)
training <- training %>% select(6:59)
validation <- validation %>% select(6:59)

#splitting training data into training and test set
inTrain <- createDataPartition(training$classe, p = 0.7, list = F)
test <- training[-inTrain,]
training <- training[inTrain,]

#predicting with random forests
fitRF <- train(classe~., data = training, method = "rf", trControl = trainControl(method = "cv", number = 3))
fitRF$finalModel

#testing on test set
predictTestRF <- predict(fitRF, test)
resultRF <- confusionMatrix(table(predictTestRF, test$classe))

#plotting the result
plot(resultRF$table, col = resultRF$byClass,  main = paste("Random Forest Accuracy =", round(resultRF$overall['Accuracy'], 4)))

#predicting with random forests
fitGbm <- train(classe~., data = training, method = "gbm", trControl = trainControl(method = "repeatedcv", number = 5, repeats = 1))
fitGbm$finalModel

#testing on test set
predictTestGbm <- predict(fitGbm, test)
resultGbm <- confusionMatrix(table(predictTestGbm, test$classe))

#plotting the result
plot(resultGbm$table, col = resultGbm$byClass,  main = paste("GBM Accuracy =", round(resultGbm$overall['Accuracy'], 4)))

#RF accuracy is better. Hence, applying the fitRF model to validation set
predictVal <- predict(fitRF, validation)
predictVal









