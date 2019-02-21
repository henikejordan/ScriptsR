set.seed(1)
bebidas_train$classe[bebidas_train$classe == '3.5624ppm'] <- 'muitobaixo'
bebidas_train$classe[bebidas_train$classe == '7.1248ppm'] <- 'baixo'
bebidas_train$classe[bebidas_train$classe == '11.9445ppm'] <- 'medio'
bebidas_train$classe[bebidas_train$classe == '23.8889ppm'] <- 'alto'
bebidas_train$classe[bebidas_train$classe == '41.9103ppm'] <- 'muitoalto'

index <- createDataPartition(bebidas_train$classe, p=0.75, list=FALSE)
trainSet <- bebidas_train[ index,]
testSet <- bebidas_train[-index,]
 
fitControl <- trainControl(method="cv", number=5, savePredictions='final', classProbs=T)

model_rf<-train(trainSet[,1:13], trainSet$classe, method='rf', trControl=fitControl, tuneLength=3)
testSet$pred_rf<-predict(object=model_rf, testSet[,1:13])
confusionMatrix(as.factor(testSet$classe), testSet$pred_rf)

model_knn<-train(trainSet[,1:13], trainSet$classe, method='knn', trControl=fitControl, tuneLength=3)
testSet$pred_knn<-predict(object=model_knn, testSet[,1:13])
confusionMatrix(as.factor(testSet$classe), testSet$pred_knn)

model_c50<-train(trainSet[,1:13], trainSet$classe, method='C5.0', trControl=fitControl, tuneLength=3)
testSet$pred_c50<-predict(object=model_c50, testSet[,1:13])
confusionMatrix(as.factor(testSet$classe), testSet$pred_c50)

testSet$pred_rf_prob<-predict(object=model_rf, testSet[,1:13], type='prob')
testSet$pred_knn_prob<-predict(object=model_knn, testSet[,1:13], type='prob')
testSet$pred_c50_prob<-predict(object=model_c50, testSet[,1:13], type='prob')