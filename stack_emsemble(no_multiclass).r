bebidas_train$classe[bebidas_train$classe == '3.5624ppm'] <- 'muitobaixo'
bebidas_train$classe[bebidas_train$classe == '7.1248ppm'] <- 'baixo'
bebidas_train$classe[bebidas_train$classe == '11.9445ppm'] <- 'medio'
bebidas_train$classe[bebidas_train$classe == '23.8889ppm'] <- 'alto'
bebidas_train$classe[bebidas_train$classe == '41.9103ppm'] <- 'muitoalto'

control <- trainControl(method="repeatedcv", number=10, index = createFolds(bebidas_train$classe, 10), repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'knn', 'svmRadial')
set.seed(7)
models <- caretList(classe~., data=bebidas_train, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)
modelCor(results)
splom(results)

stackControl <- trainControl(method="repeatedcv", number=10, index = createFolds(bebidas_train$classe, 10), repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(7)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)