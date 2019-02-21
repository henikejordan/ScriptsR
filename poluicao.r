library(RPostgreSQL)
library(caret)
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
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
con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
classe from gases where data_hora >= '2018-11-16 17:50:00' and data_hora < '2018-11-16 19:25:00'
group by data_hora, classe
order by data_hora")
df <- fetch(rs, n = -1)
raioMedio <- df[2:15]
raioMedio <- cbind(as.data.frame(lapply(raioMedio[1:13], classe = raioMedio$classe, mediaMovel)), raioMedio[14])
raioMedio <- raioMedio[-c(1:300),]
raioMedio <- as.data.frame(lapply(raioMedio[,-14], normalize))
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
classe from gases where data_hora >= '2018-11-20 15:00:00' and data_hora < '2018-11-20 15:55:00'
group by data_hora, classe
order by data_hora")
df <- fetch(rs, n = -1)
raioAlto <- df[2:15]
raioAlto <- cbind(as.data.frame(lapply(raioAlto[1:13], classe = raioAlto$classe, mediaMovel)), raioAlto[14])
raioAlto <- raioAlto[-c(1:300),]
raioAlto <- as.data.frame(lapply(raioAlto[,-14], normalize))