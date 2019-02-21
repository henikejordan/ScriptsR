maxColumnName <- NULL
maxColumnValue <- NULL
for(i in 1:nrow(bebidas_train)) {
	#column name
	maxColumnName <- rbind(maxColumnName, colnames(bebidas_train)[max.col(bebidas_train[i,])])
	#max column value
	maxColumnValue <- rbind(maxColumnValue, max(bebidas_train[i,]))
}

#sum differents columns
resp <- NULL
resp <- data.frame(bebidas_train$mq2 + bebidas_train$mq3)
colnames(resp)[1] <- "sum"

#Averaging:
#Com todos os modelos(algoritmos), pegar a média de todas as classes (colunas) e então pegar a classe com o maior valor e predizer

#Majority Voting:
#Com todos os modelos(algoritmos), pegar a classe com a maior quantidade de predições

#Weighted Average:
#Igual ao primeiro, mas colocar maiores pesos para os algoritmos com as melhores taxas de validação 

#https://www.analyticsvidhya.com/blog/2017/02/introduction-to-ensembling-along-with-implementation-in-r/