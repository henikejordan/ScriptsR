pred <- predict(model, newdata = bebidas_test)
table(pred, bebidas_test$classe)