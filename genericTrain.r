library(caret)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
bebidas_n <- as.data.frame(lapply(bebidas[,-27], normalize))
bebidas <- cbind(bebidas_n, bebidas[27])
names(getModelInfo())
#train_control <- trainControl(method="repeatedcv", number=10, repeats=3, 
#preProcOptions = list(k = 5))
model <- train(classe ~ ., data = bebidas, method = "knn",
              trControl = trainControl(method = "LGOCV", p = 0.8, number = 1,
                                       savePredictions = T))
#model <- train(classe~., data=bebidas, trControl=train_control, method="knn", 
#preProcess="knnImpute")
model
confusionMatrix(model, "none")
head(model$pred)
tail(model$pred)