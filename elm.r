library(RPostgreSQL)
library(caret)
# x e o vetor de dados e N o tamanho da janela da media movel
# arrumar para zerar quando muda o label!!!!!
mediaMovel <- function (x, N = 50, classe) {
  label <- classe[1]
  valor <- rep(0, N)
  ret <- rep(0, length(x))
  desvioPadrao <- sd(valor)
  media <- mean(valor)
  total <- 0
  
  #linha por linha
  for (i in 1:length(x)) {
    if(label != classe[i]) {
      valor <- rep(0, N)
      label <- classe[i]
    }
    #atualiza o vetor dos ultimos valores
    for (j in N:2) {
      valor[j] = valor[j - 1];
    }
    valor[1] = x[i];
    
    desvioPadrao <- sd(valor)
    media <- mean(valor)
    total <- 0
    aux = 0;
    
    for (k in 1:N) {
      if (abs(valor[k] - media) < (3 * desvioPadrao)) {
        total <- total + valor[k]
        aux <- aux + 1
      }
    }
    
    if (aux == 0) {
      ret[i] <- valor[1]
    } else {
      ret[i] <- total / aux
    }
  }
  return (ret)
}

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  classe from gases where data_hora >= '2018-08-01 00:00:00' and data_hora < '2018-12-30 23:00:00'
                  and classe like 'Fase%'
                  group by data_hora, classe
                  order by data_hora")
df <- fetch(rs, n = -1)
data <- df[2:15]
data[1:13] <- as.data.frame(lapply(data[1:13], classe = data$classe, mediaMovel))
trainIndex = createDataPartition(data$classe, p=0.7, list=FALSE, times=1)
train_set = data[trainIndex,]
test_set = data[-trainIndex,]
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
i = 1
while (i <= 500) {
	model <- train(classe~., train_set, method = "elm", trControl=train_control, preProcess=c("center", "scale"), tuneGrid = data.frame(.nhid = i,.actfun = "poslin"))
	print(model$results)
	#print(model)
	pred <- predict(model, newdata=test_set)
	matrix <- confusionMatrix(pred, as.factor(test_set$classe))
	print(matrix$overall)
	#print(confusionMatrix(pred, as.factor(test_set$classe)))
	#cat(paste("N hidden", paste(i, "\n", sep=" "), sep=" "))
	if(i < 100) {
	  i = i + 1 
	} else if(i >= 100 & i <= 200) {
	  i = i + 5
	} else {
	  i = i + 10
	}
}
