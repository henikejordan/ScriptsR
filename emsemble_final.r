library(RPostgreSQL)
library(caret)
con <- dbConnect(PostgreSQL(),user="postgres",password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
classe from gases where data_hora >= '2018-04-01 00:00:00' and data_hora < '2018-11-11 23:00:00'
and classe like '%ppm'
group by data_hora, classe
order by data_hora")
df <- fetch(rs, n = -1)
dados <- df[2:15]
dados$classe[dados$classe == '3.5624ppm'] <- 'muitobaixo'
dados$classe[dados$classe == '7.1248ppm'] <- 'baixo'
dados$classe[dados$classe == '11.9445ppm'] <- 'medio'
dados$classe[dados$classe == '23.8889ppm'] <- 'alto'
dados$classe[dados$classe == '41.9103ppm'] <- 'muitoalto'

index <- createDataPartition(dados$classe,p=0.75,list=FALSE)
trainSet <- dados[ index,]
testSet <- dados[-index,]

fitControl <- trainControl(method = "cv",number = 5,savePredictions = 'final',classProbs = T)
predictors<-c("mq2", "mq3", "mq4", "mq5", "mq6", "mq7", "mq8", "mq9", "mq135", "tgs822", "tgs2600", "tgs2602", "tgs2603")
outcomeName<-'classe'

model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)
testSet$pred_rf<-predict(object = model_rf,testSet[,predictors])
confusionMatrix(as.factor(testSet$classe),testSet$pred_rf)

model_knn<-train(trainSet[,predictors],trainSet[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)
testSet$pred_knn<-predict(object = model_knn,testSet[,predictors])
confusionMatrix(as.factor(testSet$classe),testSet$pred_knn)

model_lda<-train(trainSet[,predictors],trainSet[,outcomeName],method='lda',trControl=fitControl,tuneLength=3)
testSet$pred_lda<-predict(object = model_lda,testSet[,predictors])
confusionMatrix(as.factor(testSet$classe),testSet$pred_lda)

testSet$pred_rf_prob<-predict(object = model_rf,testSet[,predictors],type='prob')
testSet$pred_knn_prob<-predict(object = model_knn,testSet[,predictors],type='prob')
testSet$pred_lda_prob<-predict(object = model_lda,testSet[,predictors],type='prob')

#Averaging
testSet$muitobaixo<-(testSet$pred_rf_prob$muitobaixo+testSet$pred_knn_prob$muitobaixo+testSet$pred_lda_prob$muitobaixo)/3
testSet$baixo<-(testSet$pred_rf_prob$baixo+testSet$pred_knn_prob$baixo+testSet$pred_lda_prob$baixo)/3
testSet$medio<-(testSet$pred_rf_prob$medio+testSet$pred_knn_prob$medio+testSet$pred_lda_prob$medio)/3
testSet$alto<-(testSet$pred_rf_prob$alto+testSet$pred_knn_prob$alto+testSet$pred_lda_prob$alto)/3
testSet$muitoalto<-(testSet$pred_rf_prob$muitoalto+testSet$pred_knn_prob$muitoalto+testSet$pred_lda_prob$muitoalto)/3
maxColumnName <- NULL
for(i in 1:nrow(testSet)) {
  maxColumnName <- rbind(maxColumnName, colnames(testSet[,22:26])[max.col(testSet[i,22:26])])
}
testSet <- cbind(testSet, maxColumnName)
colnames(testSet)[27] <- "pred_avg"
confusionMatrix(as.factor(testSet$classe),testSet$pred_avg)

#Majority Voting
testSet$pred_majority<-as.factor(ifelse(testSet$pred_knn==testSet$pred_lda, as.character(testSet$pred_knn), as.character(testSet$pred_rf)))
confusionMatrix(as.factor(testSet$classe),testSet$pred_majority)

#Weighted Average
wrf<-0.7
wknn<-0.2
wlda<-0.1
testSet$muitobaixo<-((wrf*testSet$pred_rf_prob$muitobaixo)+(wknn*testSet$pred_knn_prob$muitobaixo)+(wlda*testSet$pred_lda_prob$muitobaixo))/(wrf+wknn+wlda)
testSet$baixo<-((wrf*testSet$pred_rf_prob$baixo)+(wknn*testSet$pred_knn_prob$baixo)+(wlda*testSet$pred_lda_prob$baixo))/(wrf+wknn+wlda)
testSet$medio<-((wrf*testSet$pred_rf_prob$medio)+(wknn*testSet$pred_knn_prob$medio)+(wlda*testSet$pred_lda_prob$medio))/(wrf+wknn+wlda)
testSet$alto<-((wrf*testSet$pred_rf_prob$alto)+(wknn*testSet$pred_knn_prob$alto)+(wlda*testSet$pred_lda_prob$alto))/(wrf+wknn+wlda)
testSet$muitoalto<-((wrf*testSet$pred_rf_prob$muitoalto)+(wknn*testSet$pred_knn_prob$muitoalto)+(wlda*testSet$pred_lda_prob$muitoalto))/(wrf+wknn+wlda)
maxColumnName <- NULL
for(i in 1:nrow(testSet)) {
  maxColumnName <- rbind(maxColumnName,colnames(testSet[,22:26])[max.col(testSet[i,22:26])])
}
testSet <- cbind(testSet,maxColumnName)
colnames(testSet)[29] <- "pred_wavg"
confusionMatrix(as.factor(testSet$classe),testSet$pred_wavg)


###########################################################################################################################################################


#Predicting the out of fold prediction probabilities for training data
trainSet$OOF_pred_rf<-model_rf$pred[order(model_rf$pred$rowIndex),4:8]
trainSet$OOF_pred_knn<-model_knn$pred[order(model_knn$pred$rowIndex),4:8]
trainSet$OOF_pred_lda<-model_lda$pred[order(model_lda$pred$rowIndex),4:8]

#Predicting probabilities for the test data
testSet$OOF_pred_rf<-predict(model_rf,testSet[predictors],type='prob')
testSet$OOF_pred_knn<-predict(model_knn,testSet[predictors],type='prob')
testSet$OOF_pred_lda<-predict(model_lda,testSet[predictors],type='prob')

aux_train <- cbind(trainSet$OOF_pred_rf[,1:5],trainSet$OOF_pred_knn[,1:5],trainSet$OOF_pred_lda[,1:5])
colnames(aux_train)[1] <- "OOF_pred_rf_alto"
colnames(aux_train)[2] <- "OOF_pred_rf_baixo"
colnames(aux_train)[3] <- "OOF_pred_rf_medio"
colnames(aux_train)[4] <- "OOF_pred_rf_muitoalto"
colnames(aux_train)[5] <- "OOF_pred_rf_muitobaixo"

colnames(aux_train)[6] <- "OOF_pred_knn_alto"
colnames(aux_train)[7] <- "OOF_pred_knn_baixo"
colnames(aux_train)[8] <- "OOF_pred_knn_medio"
colnames(aux_train)[9] <- "OOF_pred_knn_muitoalto"
colnames(aux_train)[10] <- "OOF_pred_knn_muitobaixo"

colnames(aux_train)[11] <- "OOF_pred_lda_alto"
colnames(aux_train)[12] <- "OOF_pred_lda_baixo"
colnames(aux_train)[13] <- "OOF_pred_lda_medio"
colnames(aux_train)[14] <- "OOF_pred_lda_muitoalto"
colnames(aux_train)[15] <- "OOF_pred_lda_muitobaixo"

aux_test <- cbind(testSet$OOF_pred_rf[,1:5],testSet$OOF_pred_knn[,1:5],testSet$OOF_pred_lda[,1:5])
colnames(aux_test)[1] <- "OOF_pred_rf_alto"
colnames(aux_test)[2] <- "OOF_pred_rf_baixo"
colnames(aux_test)[3] <- "OOF_pred_rf_medio"
colnames(aux_test)[4] <- "OOF_pred_rf_muitoalto"
colnames(aux_test)[5] <- "OOF_pred_rf_muitobaixo"

colnames(aux_test)[6] <- "OOF_pred_knn_alto"
colnames(aux_test)[7] <- "OOF_pred_knn_baixo"
colnames(aux_test)[8] <- "OOF_pred_knn_medio"
colnames(aux_test)[9] <- "OOF_pred_knn_muitoalto"
colnames(aux_test)[10] <- "OOF_pred_knn_muitobaixo"

colnames(aux_test)[11] <- "OOF_pred_lda_alto"
colnames(aux_test)[12] <- "OOF_pred_lda_baixo"
colnames(aux_test)[13] <- "OOF_pred_lda_medio"
colnames(aux_test)[14] <- "OOF_pred_lda_muitoalto"
colnames(aux_test)[15] <- "OOF_pred_lda_muitobaixo"

#GBM as top layer model 
model_gbm<-train(aux_train,trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=3)
testSet$gbm_stacked<-predict(model_gbm, aux_test)
confusionMatrix(as.factor(testSet$classe),testSet$gbm_stacked)
