bebidas_train$classe <- as.factor(bebidas_train$classe)
trainIndex = createDataPartition(bebidas_train$classe, p=0.7, list=FALSE,times=1)
train_set = bebidas_train[trainIndex,]
test_set = bebidas_train[-trainIndex,]
trainIndex = createDataPartition(test_set$classe, p=0.5, list=FALSE,times=1)
validation_set = test_set[trainIndex,]
test_set = test_set[-trainIndex,]
#validation_set <- validation_set[1:2396,]

set.seed(7)
modelFitRF <- train(classe~., data=train_set, method="rf")
modelFitGBM <- train(classe~., data=train_set, method="gbm", verbose=F)
modelFitKNN <- train(classe~., data=train_set, method="knn")

predRF <- predict(modelFitRF, newdata=validation_set)
predGBM <- predict(modelFitGBM, newdata=validation_set)
predKNN <- predict(modelFitKNN, newdata=validation_set)
predDF <- data.frame(predRF, predGBM, predKNN, classe=validation_set$classe, stringsAsFactors=F)

modelStack <- train(classe~., data=predDF, method="rf")

testPredRF <- predict(modelFitRF, newdata=test_set)
testPredGBM <- predict(modelFitGBM, newdata=test_set)
testPredKNN <- predict(modelFitKNN, newdata=test_set)
testPredLevelOne <- data.frame(testPredRF, testPredGBM, testPredKNN, classe=test_set$classe, stringsAsFactors=F)

combPred <- predict(modelStack, testPredLevelOne)

confusionMatrix(combPred, test_set$classe)$overall[1]
confusionMatrix(testPredRF, test_set$classe)$overall[1]
confusionMatrix(testPredGBM, test_set$classe)$overall[1]
confusionMatrix(testPredKNN, test_set$classe)$overall[1]
