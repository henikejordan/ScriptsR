library(RPostgreSQL)
library(caret)
library(doParallel)
registerDoParallel(cores=2)

if(F) {
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  normalize_test <- function(x, max, min) {
    ret <- NULL
    for(i in 1:ncol(x)) {
      ret <- cbind(ret, (x[,i] - min[1,i]) / (max[1,i] - min[1,i]))
      colnames(ret)[i] <- colnames(x)[i]
    }
    return(ret)
  }
}

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

integral <- function (x, classe) {
  label <- classe[1]
  sum <- 0
  ret <- rep(0, length(x))
  for(i in 1:length(x)) {
    if(label != classe[i]) {
      sum <- 0
      label <- classe[i]
    }
    sum <- sum + x[i]
    ret[i] <- sum
  }
  return (ret)
}

derivada <- function (x, classe) {
  label <- classe[1]
  dif <- 0
  ret <- rep(0, length(x))
  ret[1] <- dif
  for(i in 2:length(x)) {
    if(label != classe[i]) {
      dif <- 0
      label <- classe[i]
    } else {
      dif <- x[i] - x[i - 1]
    }
    ret[i] <- dif
  }
  return (ret)
}

# x e o vetor de derivadas
maxDerivada <- function (x, classe) {
  label <- classe[1]
  maximo <- x[1]
  primeiro <- 1
  ret <- rep(0, length(x))
  for(i in 1:length(x)) {
	  if(label != classe[i]) {
		  for(j in primeiro:i) {
			  ret[j] <- maximo
		  }
		  primeiro <- i
	  	maximo <- x[i]
	  	label <- classe[i]
    } 
    if(x[i] > maximo) {
      maximo <- x[i]
	  }
  }
  for(j in primeiro:i) {
    ret[j] <- maximo
  }
  return (ret)
}

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
#####Treino#####
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
classe from gases where data_hora >= '2018-04-01 00:00:00' and data_hora < '2018-06-11 23:00:00'
and classe like 'A%'
group by data_hora, classe
order by data_hora")
df <- fetch(rs, n = -1)
bebidas_train <- df[2:15]
bebidas <- as.data.frame(lapply(bebidas_train[1:13], classe = bebidas_train$classe, mediaMovel))
bebidas_train <- cbind(bebidas, bebidas_train[14])
bebidas <- as.data.frame(lapply(bebidas_train[1:13], classe = bebidas_train$classe, derivada))
bebidas <- as.data.frame(lapply(bebidas, classe = bebidas_train$classe, maxDerivada))
bebidas_train <- cbind(bebidas_train[1:13], bebidas, bebidas_train[14])
#train <- bebidas_train[1:13]
#bebidas <- as.data.frame(lapply(bebidas_train[,-14], normalize))
#bebidas_train <- cbind(bebidas, bebidas_train[14])
rm(bebidas)
bebidas_train <- bebidas_train[1:27]
#train_control <- trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions = list(k = 5))
#train_control <- trainControl(method="repeatedcv", number=10, repeats=3, preProcOptions = list(k = 5, pcaComp  = 3))
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
names(getModelInfo())
model <- train(classe~., data=bebidas_train, trControl=train_control, method="kknn")
#model <- train(classe~., data=bebidas_train, trControl=train_control, method="knn", preProcess="knnImpute")
#model <- train(classe~., data=bebidas_train, trControl=train_control, method="kknn", preProcess=c("center", "scale", "pca"))
model
confusionMatrix(model, "none")
#####Teste#####
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
classe from gases where data_hora >= '2018-05-19 18:37:00' and data_hora < '2018-05-19 18:45:00'
--and classe is not null
group by data_hora, classe
order by data_hora")
df <- fetch(rs, n = -1)
bebidas_test <- df[2:15]
bebidas_test$classe <- c('A5')
bebidas <- as.data.frame(lapply(bebidas_test[1:13], classe = bebidas_test$classe, mediaMovel))
bebidas_test <- cbind(bebidas, bebidas_test[14])
bebidas <- as.data.frame(lapply(bebidas_test[1:13], classe = bebidas_test$classe, derivada))
bebidas <- as.data.frame(lapply(bebidas, classe = bebidas_test$classe, maxDerivada))
bebidas_test <- cbind(bebidas_test[1:13], bebidas, bebidas_test[14])
#bebidas <- normalize_test(bebidas_test[-14], as.data.frame(lapply(train, max)), as.data.frame(lapply(train, min)))
#bebidas_test <- cbind(bebidas, bebidas_test[14])
rm(bebidas)
bebidas_test <- bebidas_test[1:27]
pred <- predict(model, newdata = bebidas_test)
table(pred, bebidas_test$classe)
confusionMatrix(data=pred, bebidas_test$classe)

