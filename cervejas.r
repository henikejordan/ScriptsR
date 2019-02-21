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
                  classe from gases where data_hora >= '2018-11-22 16:20:00' and data_hora < '2018-11-22 19:20:00'
                  and classe like 'A%'
                  group by data_hora, classe
                  order by data_hora")
df <- fetch(rs, n = -1)
d1 <- df[2:15]
d1[1:13] <- as.data.frame(lapply(d1[1:13], classe = d1$classe, mediaMovel))

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  classe from gases where data_hora >= '2018-11-29 12:00:00' and data_hora < '2018-11-29 14:40:00'
                  and classe like 'A%'
                  group by data_hora, classe
                  order by data_hora")
df <- fetch(rs, n = -1)
d2 <- df[2:15]
d2[1:13] <- as.data.frame(lapply(d2[1:13], classe = d2$classe, mediaMovel))

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  classe from gases where data_hora >= '2018-11-29 14:40:00' and data_hora < '2018-11-29 17:10:00'
                  and classe like 'A%'
                  group by data_hora, classe
                  order by data_hora")
df <- fetch(rs, n = -1)
d3 <- df[2:15]
d3[1:13] <- as.data.frame(lapply(d3[1:13], classe = d3$classe, mediaMovel))

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  classe from gases where data_hora >= '2018-12-07 11:40:00' and data_hora < '2018-12-07 14:10:00'
                  and classe like 'A%'
                  group by data_hora, classe
                  order by data_hora")
df <- fetch(rs, n = -1)
d4 <- df[2:15]
d4[1:13] <- as.data.frame(lapply(d4[1:13], classe = d4$classe, mediaMovel))

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  classe from gases where data_hora >= '2018-12-07 14:10:00' and data_hora < '2018-12-07 16:40:00'
                  and classe like 'A%'
                  group by data_hora, classe
                  order by data_hora")
df <- fetch(rs, n = -1)
d5 <- df[2:15]
d5[1:13] <- as.data.frame(lapply(d5[1:13], classe = d5$classe, mediaMovel))

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  classe from gases where data_hora >= '2018-12-12 14:40:00' and data_hora < '2018-12-12 17:15:00'
                  and classe like 'A%'
                  group by data_hora, classe
                  order by data_hora")
df <- fetch(rs, n = -1)
d6 <- df[2:15]
d6[1:13] <- as.data.frame(lapply(d6[1:13], classe = d6$classe, mediaMovel))

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  classe from gases where data_hora >= '2018-12-12 17:15:00' and data_hora < '2018-12-12 19:30:00'
                  and classe like 'A%'
                  group by data_hora, classe
                  order by data_hora")
df <- fetch(rs, n = -1)
d7 <- df[2:15]
d7[1:13] <- as.data.frame(lapply(d7[1:13], classe = d7$classe, mediaMovel))

d1_1 <- d1[d1$classe == 'A1',]
d1_2 <- d1[d1$classe == 'A1.5',]
d1_3 <- d1[d1$classe == 'A2',]
d1_4 <- d1[d1$classe == 'A2.5',]
d1_5 <- d1[d1$classe == 'A3',]
d1_6 <- d1[d1$classe == 'A3.5',]
d1_7 <- d1[d1$classe == 'A3.7',]
d1_8 <- d1[d1$classe == 'A3.9',]
d1_9 <- d1[d1$classe == 'A4.1',]
d1_10 <- d1[d1$classe == 'A4.3',]
d1_11 <- d1[d1$classe == 'A4.5',]
d1_12 <- d1[d1$classe == 'A5',]
d1_13 <- d1[d1$classe == 'A5.5',]
d1_14 <- d1[d1$classe == 'A6',]
d1_15 <- d1[d1$classe == 'A8',]
d2_1 <- d2[d2$classe == 'A1',]
d2_2 <- d2[d2$classe == 'A1.5',]
d2_3 <- d2[d2$classe == 'A2',]
d2_4 <- d2[d2$classe == 'A2.5',]
d2_5 <- d2[d2$classe == 'A3',]
d2_6 <- d2[d2$classe == 'A3.5',]
d2_7 <- d2[d2$classe == 'A3.7',]
d2_8 <- d2[d2$classe == 'A3.9',]
d2_9 <- d2[d2$classe == 'A4.1',]
d2_10 <- d2[d2$classe == 'A4.3',]
d2_11 <- d2[d2$classe == 'A4.5',]
d2_12 <- d2[d2$classe == 'A5',]
d2_13 <- d2[d2$classe == 'A5.5',]
d2_14 <- d2[d2$classe == 'A6',]
d2_15 <- d2[d2$classe == 'A8',]
d3_1 <- d3[d3$classe == 'A1',]
d3_2 <- d3[d3$classe == 'A1.5',]
d3_3 <- d3[d3$classe == 'A2',]
d3_4 <- d3[d3$classe == 'A2.5',]
d3_5 <- d3[d3$classe == 'A3',]
d3_6 <- d3[d3$classe == 'A3.5',]
d3_7 <- d3[d3$classe == 'A3.7',]
d3_8 <- d3[d3$classe == 'A3.9',]
d3_9 <- d3[d3$classe == 'A4.1',]
d3_10 <- d3[d3$classe == 'A4.3',]
d3_11 <- d3[d3$classe == 'A4.5',]
d3_12 <- d3[d3$classe == 'A5',]
d3_13 <- d3[d3$classe == 'A5.5',]
d3_14 <- d3[d3$classe == 'A6',]
d3_15 <- d3[d3$classe == 'A8',]
d4_1 <- d4[d4$classe == 'A1',]
d4_2 <- d4[d4$classe == 'A1.5',]
d4_3 <- d4[d4$classe == 'A2',]
d4_4 <- d4[d4$classe == 'A2.5',]
d4_5 <- d4[d4$classe == 'A3',]
d4_6 <- d4[d4$classe == 'A3.5',]
d4_7 <- d4[d4$classe == 'A3.7',]
d4_8 <- d4[d4$classe == 'A3.9',]
d4_9 <- d4[d4$classe == 'A4.1',]
d4_10 <- d4[d4$classe == 'A4.3',]
d4_11 <- d4[d4$classe == 'A4.5',]
d4_12 <- d4[d4$classe == 'A5',]
d4_13 <- d4[d4$classe == 'A5.5',]
d4_14 <- d4[d4$classe == 'A6',]
d4_15 <- d4[d4$classe == 'A8',]
d5_1 <- d5[d5$classe == 'A1',]
d5_2 <- d5[d5$classe == 'A1.5',]
d5_3 <- d5[d5$classe == 'A2',]
d5_4 <- d5[d5$classe == 'A2.5',]
d5_5 <- d5[d5$classe == 'A3',]
d5_6 <- d5[d5$classe == 'A3.5',]
d5_7 <- d5[d5$classe == 'A3.7',]
d5_8 <- d5[d5$classe == 'A3.9',]
d5_9 <- d5[d5$classe == 'A4.1',]
d5_10 <- d5[d5$classe == 'A4.3',]
d5_11 <- d5[d5$classe == 'A4.5',]
d5_12 <- d5[d5$classe == 'A5',]
d5_13 <- d5[d5$classe == 'A5.5',]
d5_14 <- d5[d5$classe == 'A6',]
d5_15 <- d5[d5$classe == 'A8',]
d6_1 <- d6[d6$classe == 'A1',]
d6_2 <- d6[d6$classe == 'A1.5',]
d6_3 <- d6[d6$classe == 'A2',]
d6_4 <- d6[d6$classe == 'A2.5',]
d6_5 <- d6[d6$classe == 'A3',]
d6_6 <- d6[d6$classe == 'A3.5',]
d6_7 <- d6[d6$classe == 'A3.7',]
d6_8 <- d6[d6$classe == 'A3.9',]
d6_9 <- d6[d6$classe == 'A4.1',]
d6_10 <- d6[d6$classe == 'A4.3',]
d6_11 <- d6[d6$classe == 'A4.5',]
d6_12 <- d6[d6$classe == 'A5',]
d6_13 <- d6[d6$classe == 'A5.5',]
d6_14 <- d6[d6$classe == 'A6',]
d6_15 <- d6[d6$classe == 'A8',]
d7_1 <- d7[d7$classe == 'A1',]
d7_2 <- d7[d7$classe == 'A1.5',]
d7_3 <- d7[d7$classe == 'A2',]
d7_4 <- d7[d7$classe == 'A2.5',]
d7_5 <- d7[d7$classe == 'A3',]
d7_6 <- d7[d7$classe == 'A3.5',]
d7_7 <- d7[d7$classe == 'A3.7',]
d7_8 <- d7[d7$classe == 'A3.9',]
d7_9 <- d7[d7$classe == 'A4.1',]
d7_10 <- d7[d7$classe == 'A4.3',]
d7_11 <- d7[d7$classe == 'A4.5',]
d7_12 <- d7[d7$classe == 'A5',]
d7_13 <- d7[d7$classe == 'A5.5',]
d7_14 <- d7[d7$classe == 'A6',]
d7_15 <- d7[d7$classe == 'A8',]

d1_1 <- d1_1[-(1:60),]
d1_2 <- d1_2[-(1:60),]
d1_3 <- d1_3[-(1:60),]
d1_4 <- d1_4[-(1:60),]
d1_5 <- d1_5[-(1:60),]
d1_6 <- d1_6[-(1:60),]
d1_7 <- d1_7[-(1:60),]
d1_8 <- d1_8[-(1:60),]
d1_9 <- d1_9[-(1:60),]
d1_10 <- d1_10[-(1:60),]
d1_11 <- d1_11[-(1:60),]
d1_12 <- d1_12[-(1:60),]
d1_13 <- d1_13[-(1:60),]
d1_14 <- d1_14[-(1:60),]
d1_15 <- d1_15[-(1:60),]
d2_1 <- d2_1[-(1:60),]
d2_2 <- d2_2[-(1:60),]
d2_3 <- d2_3[-(1:60),]
d2_4 <- d2_4[-(1:60),]
d2_5 <- d2_5[-(1:60),]
d2_6 <- d2_6[-(1:60),]
d2_7 <- d2_7[-(1:60),]
d2_8 <- d2_8[-(1:60),]
d2_9 <- d2_9[-(1:60),]
d2_10 <- d2_10[-(1:60),]
d2_11 <- d2_11[-(1:60),]
d2_12 <- d2_12[-(1:60),]
d2_13 <- d2_13[-(1:60),]
d2_14 <- d2_14[-(1:60),]
d2_15 <- d2_15[-(1:60),]
d3_1 <- d3_1[-(1:60),]
d3_2 <- d3_2[-(1:60),]
d3_3 <- d3_3[-(1:60),]
d3_4 <- d3_4[-(1:60),]
d3_5 <- d3_5[-(1:60),]
d3_6 <- d3_6[-(1:60),]
d3_7 <- d3_7[-(1:60),]
d3_8 <- d3_8[-(1:60),]
d3_9 <- d3_9[-(1:60),]
d3_10 <- d3_10[-(1:60),]
d3_11 <- d3_11[-(1:60),]
d3_12 <- d3_12[-(1:60),]
d3_13 <- d3_13[-(1:60),]
d3_14 <- d3_14[-(1:60),]
d3_15 <- d3_15[-(1:60),]
d4_1 <- d4_1[-(1:60),]
d4_2 <- d4_2[-(1:60),]
d4_3 <- d4_3[-(1:60),]
d4_4 <- d4_4[-(1:60),]
d4_5 <- d4_5[-(1:60),]
d4_6 <- d4_6[-(1:60),]
d4_7 <- d4_7[-(1:60),]
d4_8 <- d4_8[-(1:60),]
d4_9 <- d4_9[-(1:60),]
d4_10 <- d4_10[-(1:60),]
d4_11 <- d4_11[-(1:60),]
d4_12 <- d4_12[-(1:60),]
d4_13 <- d4_13[-(1:60),]
d4_14 <- d4_14[-(1:60),]
d4_15 <- d4_15[-(1:60),]
d5_1 <- d5_1[-(1:60),]
d5_2 <- d5_2[-(1:60),]
d5_3 <- d5_3[-(1:60),]
d5_4 <- d5_4[-(1:60),]
d5_5 <- d5_5[-(1:60),]
d5_6 <- d5_6[-(1:60),]
d5_7 <- d5_7[-(1:60),]
d5_8 <- d5_8[-(1:60),]
d5_9 <- d5_9[-(1:60),]
d5_10 <- d5_10[-(1:60),]
d5_11 <- d5_11[-(1:60),]
d5_12 <- d5_12[-(1:60),]
d5_13 <- d5_13[-(1:60),]
d5_14 <- d5_14[-(1:60),]
d5_15 <- d5_15[-(1:60),]
d6_1 <- d6_1[-(1:60),]
d6_2 <- d6_2[-(1:60),]
d6_3 <- d6_3[-(1:60),]
d6_4 <- d6_4[-(1:60),]
d6_5 <- d6_5[-(1:60),]
d6_6 <- d6_6[-(1:60),]
d6_7 <- d6_7[-(1:60),]
d6_8 <- d6_8[-(1:60),]
d6_9 <- d6_9[-(1:60),]
d6_10 <- d6_10[-(1:60),]
d6_11 <- d6_11[-(1:60),]
d6_12 <- d6_12[-(1:60),]
d6_13 <- d6_13[-(1:60),]
d6_14 <- d6_14[-(1:60),]
d6_15 <- d6_15[-(1:60),]
d7_1 <- d7_1[-(1:60),]
d7_2 <- d7_2[-(1:60),]
d7_3 <- d7_3[-(1:60),]
d7_4 <- d7_4[-(1:60),]
d7_5 <- d7_5[-(1:60),]
d7_6 <- d7_6[-(1:60),]
d7_7 <- d7_7[-(1:60),]
d7_8 <- d7_8[-(1:60),]
d7_9 <- d7_9[-(1:60),]
d7_10 <- d7_10[-(1:60),]
d7_11 <- d7_11[-(1:60),]
d7_12 <- d7_12[-(1:60),]
d7_13 <- d7_13[-(1:60),]
d7_14 <- d7_14[-(1:60),]
d7_15 <- d7_15[-(1:60),]

cerveja <- rbind(d1_1, d1_2, d1_3, d1_4, d1_5, d1_6, d1_7, d1_8, d1_9, d1_10, d1_11, d1_12, d1_13, d1_14, d1_15,
                 d2_1, d2_2, d2_3, d2_4, d2_5, d2_6, d2_7, d2_8, d2_9, d2_10, d2_11, d2_12, d2_13, d2_14, d2_15,
                 d3_1, d3_2, d3_3, d3_4, d3_5, d3_6, d3_7, d3_8, d3_9, d3_10, d3_11, d3_12, d3_13, d3_14, d3_15,
                 d4_1, d4_2, d4_3, d4_4, d4_5, d4_6, d4_7, d4_8, d4_9, d4_10, d4_11, d4_12, d4_13, d4_14, d4_15,
                 d5_1, d5_2, d5_3, d5_4, d5_5, d5_6, d5_7, d5_8, d5_9, d5_10, d5_11, d5_12, d5_13, d5_14, d5_15,
                 d6_1, d6_2, d6_3, d6_4, d6_5, d6_6, d6_7, d6_8, d6_9, d6_10, d6_11, d6_12, d6_13, d6_14, d6_15,
                 d7_1, d7_2, d7_3, d7_4, d7_5, d7_6, d7_7, d7_8, d7_9, d7_10, d7_11, d7_12, d7_13, d7_14, d7_15)

cerveja <- rbind(aggregate(. ~ classe, d1_1[1:60,], mean), aggregate(. ~ classe, d1_1[61:120,], mean), aggregate(. ~ classe, d1_1[121:180,], mean),
				 aggregate(. ~ classe, d1_1[181:240,], mean), aggregate(. ~ classe, d1_1[241:300,], mean), aggregate(. ~ classe, d1_1[301:360,], mean),
				 aggregate(. ~ classe, d1_2[1:60,], mean), aggregate(. ~ classe, d1_2[61:120,], mean), aggregate(. ~ classe, d1_2[121:180,], mean),
				 aggregate(. ~ classe, d1_2[181:240,], mean), aggregate(. ~ classe, d1_2[241:300,], mean), aggregate(. ~ classe, d1_2[301:360,], mean),
				 aggregate(. ~ classe, d1_3[1:60,], mean), aggregate(. ~ classe, d1_3[61:120,], mean), aggregate(. ~ classe, d1_3[121:180,], mean),
				 aggregate(. ~ classe, d1_3[181:240,], mean), aggregate(. ~ classe, d1_3[241:300,], mean), aggregate(. ~ classe, d1_3[301:360,], mean),
				 aggregate(. ~ classe, d1_4[1:60,], mean), aggregate(. ~ classe, d1_4[61:120,], mean), aggregate(. ~ classe, d1_4[121:180,], mean),
				 aggregate(. ~ classe, d1_4[181:240,], mean), aggregate(. ~ classe, d1_4[241:300,], mean), aggregate(. ~ classe, d1_4[301:360,], mean),
				 aggregate(. ~ classe, d1_5[1:60,], mean), aggregate(. ~ classe, d1_5[61:120,], mean), aggregate(. ~ classe, d1_5[121:180,], mean),
				 aggregate(. ~ classe, d1_5[181:240,], mean), aggregate(. ~ classe, d1_5[241:300,], mean), aggregate(. ~ classe, d1_5[301:360,], mean),
				 aggregate(. ~ classe, d1_6[1:60,], mean), aggregate(. ~ classe, d1_6[61:120,], mean), aggregate(. ~ classe, d1_6[121:180,], mean),
				 aggregate(. ~ classe, d1_6[181:240,], mean), aggregate(. ~ classe, d1_6[241:300,], mean), aggregate(. ~ classe, d1_6[301:360,], mean),
				 aggregate(. ~ classe, d1_7[1:60,], mean), aggregate(. ~ classe, d1_7[61:120,], mean), aggregate(. ~ classe, d1_7[121:180,], mean),
				 aggregate(. ~ classe, d1_7[181:240,], mean), aggregate(. ~ classe, d1_7[241:300,], mean), aggregate(. ~ classe, d1_7[301:360,], mean),
				 aggregate(. ~ classe, d1_8[1:60,], mean), aggregate(. ~ classe, d1_8[61:120,], mean), aggregate(. ~ classe, d1_8[121:180,], mean),
				 aggregate(. ~ classe, d1_8[181:240,], mean), aggregate(. ~ classe, d1_8[241:300,], mean), aggregate(. ~ classe, d1_8[301:360,], mean),
				 aggregate(. ~ classe, d1_9[1:60,], mean), aggregate(. ~ classe, d1_9[61:120,], mean), aggregate(. ~ classe, d1_9[121:180,], mean),
				 aggregate(. ~ classe, d1_9[181:240,], mean), aggregate(. ~ classe, d1_9[241:300,], mean), aggregate(. ~ classe, d1_9[301:360,], mean),
				 aggregate(. ~ classe, d1_10[1:60,], mean), aggregate(. ~ classe, d1_10[61:120,], mean), aggregate(. ~ classe, d1_10[121:180,], mean),
				 aggregate(. ~ classe, d1_10[181:240,], mean), aggregate(. ~ classe, d1_10[241:300,], mean), aggregate(. ~ classe, d1_10[301:360,], mean),
				 aggregate(. ~ classe, d1_11[1:60,], mean), aggregate(. ~ classe, d1_11[61:120,], mean), aggregate(. ~ classe, d1_11[121:180,], mean),
				 aggregate(. ~ classe, d1_11[181:240,], mean), aggregate(. ~ classe, d1_11[241:300,], mean), aggregate(. ~ classe, d1_11[301:360,], mean),
				 aggregate(. ~ classe, d1_12[1:60,], mean), aggregate(. ~ classe, d1_12[61:120,], mean), aggregate(. ~ classe, d1_12[121:180,], mean),
				 aggregate(. ~ classe, d1_12[181:240,], mean), aggregate(. ~ classe, d1_12[241:300,], mean), aggregate(. ~ classe, d1_12[301:360,], mean),
				 aggregate(. ~ classe, d1_13[1:60,], mean), aggregate(. ~ classe, d1_13[61:120,], mean), aggregate(. ~ classe, d1_13[121:180,], mean),
				 aggregate(. ~ classe, d1_13[181:240,], mean), aggregate(. ~ classe, d1_13[241:300,], mean), aggregate(. ~ classe, d1_13[301:360,], mean),
				 aggregate(. ~ classe, d1_14[1:60,], mean), aggregate(. ~ classe, d1_14[61:120,], mean), aggregate(. ~ classe, d1_14[121:180,], mean),
				 aggregate(. ~ classe, d1_14[181:240,], mean), aggregate(. ~ classe, d1_14[241:300,], mean), aggregate(. ~ classe, d1_14[301:360,], mean),
				 aggregate(. ~ classe, d1_15[1:60,], mean), aggregate(. ~ classe, d1_15[61:120,], mean), aggregate(. ~ classe, d1_15[121:180,], mean),
				 aggregate(. ~ classe, d1_15[181:240,], mean), aggregate(. ~ classe, d1_15[241:300,], mean), aggregate(. ~ classe, d1_15[301:360,], mean),
				 
				 aggregate(. ~ classe, d2_1[1:60,], mean), aggregate(. ~ classe, d2_1[61:120,], mean), aggregate(. ~ classe, d2_1[121:180,], mean),
				 aggregate(. ~ classe, d2_1[181:240,], mean), aggregate(. ~ classe, d2_1[241:300,], mean), aggregate(. ~ classe, d2_1[301:360,], mean),
				 aggregate(. ~ classe, d2_2[1:60,], mean), aggregate(. ~ classe, d2_2[61:120,], mean), aggregate(. ~ classe, d2_2[121:180,], mean),
				 aggregate(. ~ classe, d2_2[181:240,], mean), aggregate(. ~ classe, d2_2[241:300,], mean), aggregate(. ~ classe, d2_2[301:360,], mean),
				 aggregate(. ~ classe, d2_3[1:60,], mean), aggregate(. ~ classe, d2_3[61:120,], mean), aggregate(. ~ classe, d2_3[121:180,], mean),
				 aggregate(. ~ classe, d2_3[181:240,], mean), aggregate(. ~ classe, d2_3[241:300,], mean), aggregate(. ~ classe, d2_3[301:360,], mean),
				 aggregate(. ~ classe, d2_4[1:60,], mean), aggregate(. ~ classe, d2_4[61:120,], mean), aggregate(. ~ classe, d2_4[121:180,], mean),
				 aggregate(. ~ classe, d2_4[181:240,], mean), aggregate(. ~ classe, d2_4[241:300,], mean), aggregate(. ~ classe, d2_4[301:360,], mean),
				 aggregate(. ~ classe, d2_5[1:60,], mean), aggregate(. ~ classe, d2_5[61:120,], mean), aggregate(. ~ classe, d2_5[121:180,], mean),
				 aggregate(. ~ classe, d2_5[181:240,], mean), aggregate(. ~ classe, d2_5[241:300,], mean), aggregate(. ~ classe, d2_5[301:360,], mean),
				 aggregate(. ~ classe, d2_6[1:60,], mean), aggregate(. ~ classe, d2_6[61:120,], mean), aggregate(. ~ classe, d2_6[121:180,], mean),
				 aggregate(. ~ classe, d2_6[181:240,], mean), aggregate(. ~ classe, d2_6[241:300,], mean), aggregate(. ~ classe, d2_6[301:360,], mean),
				 aggregate(. ~ classe, d2_7[1:60,], mean), aggregate(. ~ classe, d2_7[61:120,], mean), aggregate(. ~ classe, d2_7[121:180,], mean),
				 aggregate(. ~ classe, d2_7[181:240,], mean), aggregate(. ~ classe, d2_7[241:300,], mean), aggregate(. ~ classe, d2_7[301:360,], mean),
				 aggregate(. ~ classe, d2_8[1:60,], mean), aggregate(. ~ classe, d2_8[61:120,], mean), aggregate(. ~ classe, d2_8[121:180,], mean),
				 aggregate(. ~ classe, d2_8[181:240,], mean), aggregate(. ~ classe, d2_8[241:300,], mean), aggregate(. ~ classe, d2_8[301:360,], mean),
				 aggregate(. ~ classe, d2_9[1:60,], mean), aggregate(. ~ classe, d2_9[61:120,], mean), aggregate(. ~ classe, d2_9[121:180,], mean),
				 aggregate(. ~ classe, d2_9[181:240,], mean), aggregate(. ~ classe, d2_9[241:300,], mean), aggregate(. ~ classe, d2_9[301:360,], mean),
				 aggregate(. ~ classe, d2_10[1:60,], mean), aggregate(. ~ classe, d2_10[61:120,], mean), aggregate(. ~ classe, d2_10[121:180,], mean),
				 aggregate(. ~ classe, d2_10[181:240,], mean), aggregate(. ~ classe, d2_10[241:300,], mean), aggregate(. ~ classe, d2_10[301:360,], mean),
				 aggregate(. ~ classe, d2_11[1:60,], mean), aggregate(. ~ classe, d2_11[61:120,], mean), aggregate(. ~ classe, d2_11[121:180,], mean),
				 aggregate(. ~ classe, d2_11[181:240,], mean), aggregate(. ~ classe, d2_11[241:300,], mean), aggregate(. ~ classe, d2_11[301:360,], mean),
				 aggregate(. ~ classe, d2_12[1:60,], mean), aggregate(. ~ classe, d2_12[61:120,], mean), aggregate(. ~ classe, d2_12[121:180,], mean),
				 aggregate(. ~ classe, d2_12[181:240,], mean), aggregate(. ~ classe, d2_12[241:300,], mean), aggregate(. ~ classe, d2_12[301:360,], mean),
				 aggregate(. ~ classe, d2_13[1:60,], mean), aggregate(. ~ classe, d2_13[61:120,], mean), aggregate(. ~ classe, d2_13[121:180,], mean),
				 aggregate(. ~ classe, d2_13[181:240,], mean), aggregate(. ~ classe, d2_13[241:300,], mean), aggregate(. ~ classe, d2_13[301:360,], mean),
				 aggregate(. ~ classe, d2_14[1:60,], mean), aggregate(. ~ classe, d2_14[61:120,], mean), aggregate(. ~ classe, d2_14[121:180,], mean),
				 aggregate(. ~ classe, d2_14[181:240,], mean), aggregate(. ~ classe, d2_14[241:300,], mean), aggregate(. ~ classe, d2_14[301:360,], mean),
				 aggregate(. ~ classe, d2_15[1:60,], mean), aggregate(. ~ classe, d2_15[61:120,], mean), aggregate(. ~ classe, d2_15[121:180,], mean),
				 aggregate(. ~ classe, d2_15[181:240,], mean), aggregate(. ~ classe, d2_15[241:300,], mean), aggregate(. ~ classe, d2_15[301:360,], mean),
				 
				 aggregate(. ~ classe, d3_1[1:60,], mean), aggregate(. ~ classe, d3_1[61:120,], mean), aggregate(. ~ classe, d3_1[121:180,], mean),
				 aggregate(. ~ classe, d3_1[181:240,], mean), aggregate(. ~ classe, d3_1[241:300,], mean), aggregate(. ~ classe, d3_1[301:360,], mean),
				 aggregate(. ~ classe, d3_2[1:60,], mean), aggregate(. ~ classe, d3_2[61:120,], mean), aggregate(. ~ classe, d3_2[121:180,], mean),
				 aggregate(. ~ classe, d3_2[181:240,], mean), aggregate(. ~ classe, d3_2[241:300,], mean), aggregate(. ~ classe, d3_2[301:360,], mean),
				 aggregate(. ~ classe, d3_3[1:60,], mean), aggregate(. ~ classe, d3_3[61:120,], mean), aggregate(. ~ classe, d3_3[121:180,], mean),
				 aggregate(. ~ classe, d3_3[181:240,], mean), aggregate(. ~ classe, d3_3[241:300,], mean), aggregate(. ~ classe, d3_3[301:360,], mean),
				 aggregate(. ~ classe, d3_4[1:60,], mean), aggregate(. ~ classe, d3_4[61:120,], mean), aggregate(. ~ classe, d3_4[121:180,], mean),
				 aggregate(. ~ classe, d3_4[181:240,], mean), aggregate(. ~ classe, d3_4[241:300,], mean), aggregate(. ~ classe, d3_4[301:360,], mean),
				 aggregate(. ~ classe, d3_5[1:60,], mean), aggregate(. ~ classe, d3_5[61:120,], mean), aggregate(. ~ classe, d3_5[121:180,], mean),
				 aggregate(. ~ classe, d3_5[181:240,], mean), aggregate(. ~ classe, d3_5[241:300,], mean), aggregate(. ~ classe, d3_5[301:360,], mean),
				 aggregate(. ~ classe, d3_6[1:60,], mean), aggregate(. ~ classe, d3_6[61:120,], mean), aggregate(. ~ classe, d3_6[121:180,], mean),
				 aggregate(. ~ classe, d3_6[181:240,], mean), aggregate(. ~ classe, d3_6[241:300,], mean), aggregate(. ~ classe, d3_6[301:360,], mean),
				 aggregate(. ~ classe, d3_7[1:60,], mean), aggregate(. ~ classe, d3_7[61:120,], mean), aggregate(. ~ classe, d3_7[121:180,], mean),
				 aggregate(. ~ classe, d3_7[181:240,], mean), aggregate(. ~ classe, d3_7[241:300,], mean), aggregate(. ~ classe, d3_7[301:360,], mean),
				 aggregate(. ~ classe, d3_8[1:60,], mean), aggregate(. ~ classe, d3_8[61:120,], mean), aggregate(. ~ classe, d3_8[121:180,], mean),
				 aggregate(. ~ classe, d3_8[181:240,], mean), aggregate(. ~ classe, d3_8[241:300,], mean), aggregate(. ~ classe, d3_8[301:360,], mean),
				 aggregate(. ~ classe, d3_9[1:60,], mean), aggregate(. ~ classe, d3_9[61:120,], mean), aggregate(. ~ classe, d3_9[121:180,], mean),
				 aggregate(. ~ classe, d3_9[181:240,], mean), aggregate(. ~ classe, d3_9[241:300,], mean), aggregate(. ~ classe, d3_9[301:360,], mean),
				 aggregate(. ~ classe, d3_10[1:60,], mean), aggregate(. ~ classe, d3_10[61:120,], mean), aggregate(. ~ classe, d3_10[121:180,], mean),
				 aggregate(. ~ classe, d3_10[181:240,], mean), aggregate(. ~ classe, d3_10[241:300,], mean), aggregate(. ~ classe, d3_10[301:360,], mean),
				 aggregate(. ~ classe, d3_11[1:60,], mean), aggregate(. ~ classe, d3_11[61:120,], mean), aggregate(. ~ classe, d3_11[121:180,], mean),
				 aggregate(. ~ classe, d3_11[181:240,], mean), aggregate(. ~ classe, d3_11[241:300,], mean), aggregate(. ~ classe, d3_11[301:360,], mean),
				 aggregate(. ~ classe, d3_12[1:60,], mean), aggregate(. ~ classe, d3_12[61:120,], mean), aggregate(. ~ classe, d3_12[121:180,], mean),
				 aggregate(. ~ classe, d3_12[181:240,], mean), aggregate(. ~ classe, d3_12[241:300,], mean), aggregate(. ~ classe, d3_12[301:360,], mean),
				 aggregate(. ~ classe, d3_13[1:60,], mean), aggregate(. ~ classe, d3_13[61:120,], mean), aggregate(. ~ classe, d3_13[121:180,], mean),
				 aggregate(. ~ classe, d3_13[181:240,], mean), aggregate(. ~ classe, d3_13[241:300,], mean), aggregate(. ~ classe, d3_13[301:360,], mean),
				 aggregate(. ~ classe, d3_14[1:60,], mean), aggregate(. ~ classe, d3_14[61:120,], mean), aggregate(. ~ classe, d3_14[121:180,], mean),
				 aggregate(. ~ classe, d3_14[181:240,], mean), aggregate(. ~ classe, d3_14[241:300,], mean), aggregate(. ~ classe, d3_14[301:360,], mean),
				 aggregate(. ~ classe, d3_15[1:60,], mean), aggregate(. ~ classe, d3_15[61:120,], mean), aggregate(. ~ classe, d3_15[121:180,], mean),
				 aggregate(. ~ classe, d3_15[181:240,], mean), aggregate(. ~ classe, d3_15[241:300,], mean), aggregate(. ~ classe, d3_15[301:360,], mean),
				 
				 aggregate(. ~ classe, d4_1[1:60,], mean), aggregate(. ~ classe, d4_1[61:120,], mean), aggregate(. ~ classe, d4_1[121:180,], mean),
				 aggregate(. ~ classe, d4_1[181:240,], mean), aggregate(. ~ classe, d4_1[241:300,], mean), aggregate(. ~ classe, d4_1[301:360,], mean),
				 aggregate(. ~ classe, d4_2[1:60,], mean), aggregate(. ~ classe, d4_2[61:120,], mean), aggregate(. ~ classe, d4_2[121:180,], mean),
				 aggregate(. ~ classe, d4_2[181:240,], mean), aggregate(. ~ classe, d4_2[241:300,], mean), aggregate(. ~ classe, d4_2[301:360,], mean),
				 aggregate(. ~ classe, d4_3[1:60,], mean), aggregate(. ~ classe, d4_3[61:120,], mean), aggregate(. ~ classe, d4_3[121:180,], mean),
				 aggregate(. ~ classe, d4_3[181:240,], mean), aggregate(. ~ classe, d4_3[241:300,], mean), aggregate(. ~ classe, d4_3[301:360,], mean),
				 aggregate(. ~ classe, d4_4[1:60,], mean), aggregate(. ~ classe, d4_4[61:120,], mean), aggregate(. ~ classe, d4_4[121:180,], mean),
				 aggregate(. ~ classe, d4_4[181:240,], mean), aggregate(. ~ classe, d4_4[241:300,], mean), aggregate(. ~ classe, d4_4[301:360,], mean),
				 aggregate(. ~ classe, d4_5[1:60,], mean), aggregate(. ~ classe, d4_5[61:120,], mean), aggregate(. ~ classe, d4_5[121:180,], mean),
				 aggregate(. ~ classe, d4_5[181:240,], mean), aggregate(. ~ classe, d4_5[241:300,], mean), aggregate(. ~ classe, d4_5[301:360,], mean),
				 aggregate(. ~ classe, d4_6[1:60,], mean), aggregate(. ~ classe, d4_6[61:120,], mean), aggregate(. ~ classe, d4_6[121:180,], mean),
				 aggregate(. ~ classe, d4_6[181:240,], mean), aggregate(. ~ classe, d4_6[241:300,], mean), aggregate(. ~ classe, d4_6[301:360,], mean),
				 aggregate(. ~ classe, d4_7[1:60,], mean), aggregate(. ~ classe, d4_7[61:120,], mean), aggregate(. ~ classe, d4_7[121:180,], mean),
				 aggregate(. ~ classe, d4_7[181:240,], mean), aggregate(. ~ classe, d4_7[241:300,], mean), aggregate(. ~ classe, d4_7[301:360,], mean),
				 aggregate(. ~ classe, d4_8[1:60,], mean), aggregate(. ~ classe, d4_8[61:120,], mean), aggregate(. ~ classe, d4_8[121:180,], mean),
				 aggregate(. ~ classe, d4_8[181:240,], mean), aggregate(. ~ classe, d4_8[241:300,], mean), aggregate(. ~ classe, d4_8[301:360,], mean),
				 aggregate(. ~ classe, d4_9[1:60,], mean), aggregate(. ~ classe, d4_9[61:120,], mean), aggregate(. ~ classe, d4_9[121:180,], mean),
				 aggregate(. ~ classe, d4_9[181:240,], mean), aggregate(. ~ classe, d4_9[241:300,], mean), aggregate(. ~ classe, d4_9[301:360,], mean),
				 aggregate(. ~ classe, d4_10[1:60,], mean), aggregate(. ~ classe, d4_10[61:120,], mean), aggregate(. ~ classe, d4_10[121:180,], mean),
				 aggregate(. ~ classe, d4_10[181:240,], mean), aggregate(. ~ classe, d4_10[241:300,], mean), aggregate(. ~ classe, d4_10[301:360,], mean),
				 aggregate(. ~ classe, d4_11[1:60,], mean), aggregate(. ~ classe, d4_11[61:120,], mean), aggregate(. ~ classe, d4_11[121:180,], mean),
				 aggregate(. ~ classe, d4_11[181:240,], mean), aggregate(. ~ classe, d4_11[241:300,], mean), aggregate(. ~ classe, d4_11[301:360,], mean),
				 aggregate(. ~ classe, d4_12[1:60,], mean), aggregate(. ~ classe, d4_12[61:120,], mean), aggregate(. ~ classe, d4_12[121:180,], mean),
				 aggregate(. ~ classe, d4_12[181:240,], mean), aggregate(. ~ classe, d4_12[241:300,], mean), aggregate(. ~ classe, d4_12[301:360,], mean),
				 aggregate(. ~ classe, d4_13[1:60,], mean), aggregate(. ~ classe, d4_13[61:120,], mean), aggregate(. ~ classe, d4_13[121:180,], mean),
				 aggregate(. ~ classe, d4_13[181:240,], mean), aggregate(. ~ classe, d4_13[241:300,], mean), aggregate(. ~ classe, d4_13[301:360,], mean),
				 aggregate(. ~ classe, d4_14[1:60,], mean), aggregate(. ~ classe, d4_14[61:120,], mean), aggregate(. ~ classe, d4_14[121:180,], mean),
				 aggregate(. ~ classe, d4_14[181:240,], mean), aggregate(. ~ classe, d4_14[241:300,], mean), aggregate(. ~ classe, d4_14[301:360,], mean),
				 aggregate(. ~ classe, d4_15[1:60,], mean), aggregate(. ~ classe, d4_15[61:120,], mean), aggregate(. ~ classe, d4_15[121:180,], mean),
				 aggregate(. ~ classe, d4_15[181:240,], mean), aggregate(. ~ classe, d4_15[241:300,], mean), aggregate(. ~ classe, d4_15[301:360,], mean),
				 
				 aggregate(. ~ classe, d5_1[1:60,], mean), aggregate(. ~ classe, d5_1[61:120,], mean), aggregate(. ~ classe, d5_1[121:180,], mean),
				 aggregate(. ~ classe, d5_1[181:240,], mean), aggregate(. ~ classe, d5_1[241:300,], mean), aggregate(. ~ classe, d5_1[301:360,], mean),
				 aggregate(. ~ classe, d5_2[1:60,], mean), aggregate(. ~ classe, d5_2[61:120,], mean), aggregate(. ~ classe, d5_2[121:180,], mean),
				 aggregate(. ~ classe, d5_2[181:240,], mean), aggregate(. ~ classe, d5_2[241:300,], mean), aggregate(. ~ classe, d5_2[301:360,], mean),
				 aggregate(. ~ classe, d5_3[1:60,], mean), aggregate(. ~ classe, d5_3[61:120,], mean), aggregate(. ~ classe, d5_3[121:180,], mean),
				 aggregate(. ~ classe, d5_3[181:240,], mean), aggregate(. ~ classe, d5_3[241:300,], mean), aggregate(. ~ classe, d5_3[301:360,], mean),
				 aggregate(. ~ classe, d5_4[1:60,], mean), aggregate(. ~ classe, d5_4[61:120,], mean), aggregate(. ~ classe, d5_4[121:180,], mean),
				 aggregate(. ~ classe, d5_4[181:240,], mean), aggregate(. ~ classe, d5_4[241:300,], mean), aggregate(. ~ classe, d5_4[301:360,], mean),
				 aggregate(. ~ classe, d5_5[1:60,], mean), aggregate(. ~ classe, d5_5[61:120,], mean), aggregate(. ~ classe, d5_5[121:180,], mean),
				 aggregate(. ~ classe, d5_5[181:240,], mean), aggregate(. ~ classe, d5_5[241:300,], mean), aggregate(. ~ classe, d5_5[301:360,], mean),
				 aggregate(. ~ classe, d5_6[1:60,], mean), aggregate(. ~ classe, d5_6[61:120,], mean), aggregate(. ~ classe, d5_6[121:180,], mean),
				 aggregate(. ~ classe, d5_6[181:240,], mean), aggregate(. ~ classe, d5_6[241:300,], mean), aggregate(. ~ classe, d5_6[301:360,], mean),
				 aggregate(. ~ classe, d5_7[1:60,], mean), aggregate(. ~ classe, d5_7[61:120,], mean), aggregate(. ~ classe, d5_7[121:180,], mean),
				 aggregate(. ~ classe, d5_7[181:240,], mean), aggregate(. ~ classe, d5_7[241:300,], mean), aggregate(. ~ classe, d5_7[301:360,], mean),
				 aggregate(. ~ classe, d5_8[1:60,], mean), aggregate(. ~ classe, d5_8[61:120,], mean), aggregate(. ~ classe, d5_8[121:180,], mean),
				 aggregate(. ~ classe, d5_8[181:240,], mean), aggregate(. ~ classe, d5_8[241:300,], mean), aggregate(. ~ classe, d5_8[301:360,], mean),
				 aggregate(. ~ classe, d5_9[1:60,], mean), aggregate(. ~ classe, d5_9[61:120,], mean), aggregate(. ~ classe, d5_9[121:180,], mean),
				 aggregate(. ~ classe, d5_9[181:240,], mean), aggregate(. ~ classe, d5_9[241:300,], mean), aggregate(. ~ classe, d5_9[301:360,], mean),
				 aggregate(. ~ classe, d5_10[1:60,], mean), aggregate(. ~ classe, d5_10[61:120,], mean), aggregate(. ~ classe, d5_10[121:180,], mean),
				 aggregate(. ~ classe, d5_10[181:240,], mean), aggregate(. ~ classe, d5_10[241:300,], mean), aggregate(. ~ classe, d5_10[301:360,], mean),
				 aggregate(. ~ classe, d5_11[1:60,], mean), aggregate(. ~ classe, d5_11[61:120,], mean), aggregate(. ~ classe, d5_11[121:180,], mean),
				 aggregate(. ~ classe, d5_11[181:240,], mean), aggregate(. ~ classe, d5_11[241:300,], mean), aggregate(. ~ classe, d5_11[301:360,], mean),
				 aggregate(. ~ classe, d5_12[1:60,], mean), aggregate(. ~ classe, d5_12[61:120,], mean), aggregate(. ~ classe, d5_12[121:180,], mean),
				 aggregate(. ~ classe, d5_12[181:240,], mean), aggregate(. ~ classe, d5_12[241:300,], mean), aggregate(. ~ classe, d5_12[301:360,], mean),
				 aggregate(. ~ classe, d5_13[1:60,], mean), aggregate(. ~ classe, d5_13[61:120,], mean), aggregate(. ~ classe, d5_13[121:180,], mean),
				 aggregate(. ~ classe, d5_13[181:240,], mean), aggregate(. ~ classe, d5_13[241:300,], mean), aggregate(. ~ classe, d5_13[301:360,], mean),
				 aggregate(. ~ classe, d5_14[1:60,], mean), aggregate(. ~ classe, d5_14[61:120,], mean), aggregate(. ~ classe, d5_14[121:180,], mean),
				 aggregate(. ~ classe, d5_14[181:240,], mean), aggregate(. ~ classe, d5_14[241:300,], mean), aggregate(. ~ classe, d5_14[301:360,], mean),
				 aggregate(. ~ classe, d5_15[1:60,], mean), aggregate(. ~ classe, d5_15[61:120,], mean), aggregate(. ~ classe, d5_15[121:180,], mean),
				 aggregate(. ~ classe, d5_15[181:240,], mean), aggregate(. ~ classe, d5_15[241:300,], mean), aggregate(. ~ classe, d5_15[301:360,], mean),
				 
				 aggregate(. ~ classe, d6_1[1:60,], mean), aggregate(. ~ classe, d6_1[61:120,], mean), aggregate(. ~ classe, d6_1[121:180,], mean),
				 aggregate(. ~ classe, d6_1[181:240,], mean), aggregate(. ~ classe, d6_1[241:300,], mean), aggregate(. ~ classe, d6_1[301:360,], mean),
				 aggregate(. ~ classe, d6_2[1:60,], mean), aggregate(. ~ classe, d6_2[61:120,], mean), aggregate(. ~ classe, d6_2[121:180,], mean),
				 aggregate(. ~ classe, d6_2[181:240,], mean), aggregate(. ~ classe, d6_2[241:300,], mean), aggregate(. ~ classe, d6_2[301:360,], mean),
				 aggregate(. ~ classe, d6_3[1:60,], mean), aggregate(. ~ classe, d6_3[61:120,], mean), aggregate(. ~ classe, d6_3[121:180,], mean),
				 aggregate(. ~ classe, d6_3[181:240,], mean), aggregate(. ~ classe, d6_3[241:300,], mean), aggregate(. ~ classe, d6_3[301:360,], mean),
				 aggregate(. ~ classe, d6_4[1:60,], mean), aggregate(. ~ classe, d6_4[61:120,], mean), aggregate(. ~ classe, d6_4[121:180,], mean),
				 aggregate(. ~ classe, d6_4[181:240,], mean), aggregate(. ~ classe, d6_4[241:300,], mean), aggregate(. ~ classe, d6_4[301:360,], mean),
				 aggregate(. ~ classe, d6_5[1:60,], mean), aggregate(. ~ classe, d6_5[61:120,], mean), aggregate(. ~ classe, d6_5[121:180,], mean),
				 aggregate(. ~ classe, d6_5[181:240,], mean), aggregate(. ~ classe, d6_5[241:300,], mean), aggregate(. ~ classe, d6_5[301:360,], mean),
				 aggregate(. ~ classe, d6_6[1:60,], mean), aggregate(. ~ classe, d6_6[61:120,], mean), aggregate(. ~ classe, d6_6[121:180,], mean),
				 aggregate(. ~ classe, d6_6[181:240,], mean), aggregate(. ~ classe, d6_6[241:300,], mean), aggregate(. ~ classe, d6_6[301:360,], mean),
				 aggregate(. ~ classe, d6_7[1:60,], mean), aggregate(. ~ classe, d6_7[61:120,], mean), aggregate(. ~ classe, d6_7[121:180,], mean),
				 aggregate(. ~ classe, d6_7[181:240,], mean), aggregate(. ~ classe, d6_7[241:300,], mean), aggregate(. ~ classe, d6_7[301:360,], mean),
				 aggregate(. ~ classe, d6_8[1:60,], mean), aggregate(. ~ classe, d6_8[61:120,], mean), aggregate(. ~ classe, d6_8[121:180,], mean),
				 aggregate(. ~ classe, d6_8[181:240,], mean), aggregate(. ~ classe, d6_8[241:300,], mean), aggregate(. ~ classe, d6_8[301:360,], mean),
				 aggregate(. ~ classe, d6_9[1:60,], mean), aggregate(. ~ classe, d6_9[61:120,], mean), aggregate(. ~ classe, d6_9[121:180,], mean),
				 aggregate(. ~ classe, d6_9[181:240,], mean), aggregate(. ~ classe, d6_9[241:300,], mean), aggregate(. ~ classe, d6_9[301:360,], mean),
				 aggregate(. ~ classe, d6_10[1:60,], mean), aggregate(. ~ classe, d6_10[61:120,], mean), aggregate(. ~ classe, d6_10[121:180,], mean),
				 aggregate(. ~ classe, d6_10[181:240,], mean), aggregate(. ~ classe, d6_10[241:300,], mean), aggregate(. ~ classe, d6_10[301:360,], mean),
				 aggregate(. ~ classe, d6_11[1:60,], mean), aggregate(. ~ classe, d6_11[61:120,], mean), aggregate(. ~ classe, d6_11[121:180,], mean),
				 aggregate(. ~ classe, d6_11[181:240,], mean), aggregate(. ~ classe, d6_11[241:300,], mean), aggregate(. ~ classe, d6_11[301:360,], mean),
				 aggregate(. ~ classe, d6_12[1:60,], mean), aggregate(. ~ classe, d6_12[61:120,], mean), aggregate(. ~ classe, d6_12[121:180,], mean),
				 aggregate(. ~ classe, d6_12[181:240,], mean), aggregate(. ~ classe, d6_12[241:300,], mean), aggregate(. ~ classe, d6_12[301:360,], mean),
				 aggregate(. ~ classe, d6_13[1:60,], mean), aggregate(. ~ classe, d6_13[61:120,], mean), aggregate(. ~ classe, d6_13[121:180,], mean),
				 aggregate(. ~ classe, d6_13[181:240,], mean), aggregate(. ~ classe, d6_13[241:300,], mean), aggregate(. ~ classe, d6_13[301:360,], mean),
				 aggregate(. ~ classe, d6_14[1:60,], mean), aggregate(. ~ classe, d6_14[61:120,], mean), aggregate(. ~ classe, d6_14[121:180,], mean),
				 aggregate(. ~ classe, d6_14[181:240,], mean), aggregate(. ~ classe, d6_14[241:300,], mean), aggregate(. ~ classe, d6_14[301:360,], mean),
				 aggregate(. ~ classe, d6_15[1:60,], mean), aggregate(. ~ classe, d6_15[61:120,], mean), aggregate(. ~ classe, d6_15[121:180,], mean),
				 aggregate(. ~ classe, d6_15[181:240,], mean), aggregate(. ~ classe, d6_15[241:300,], mean), aggregate(. ~ classe, d6_15[301:360,], mean),
				 
				 aggregate(. ~ classe, d7_1[1:60,], mean), aggregate(. ~ classe, d7_1[61:120,], mean), aggregate(. ~ classe, d7_1[121:180,], mean),
				 aggregate(. ~ classe, d7_1[181:240,], mean), aggregate(. ~ classe, d7_1[241:300,], mean), aggregate(. ~ classe, d7_1[301:360,], mean),
				 aggregate(. ~ classe, d7_2[1:60,], mean), aggregate(. ~ classe, d7_2[61:120,], mean), aggregate(. ~ classe, d7_2[121:180,], mean),
				 aggregate(. ~ classe, d7_2[181:240,], mean), aggregate(. ~ classe, d7_2[241:300,], mean), aggregate(. ~ classe, d7_2[301:360,], mean),
				 aggregate(. ~ classe, d7_3[1:60,], mean), aggregate(. ~ classe, d7_3[61:120,], mean), aggregate(. ~ classe, d7_3[121:180,], mean),
				 aggregate(. ~ classe, d7_3[181:240,], mean), aggregate(. ~ classe, d7_3[241:300,], mean), aggregate(. ~ classe, d7_3[301:360,], mean),
				 aggregate(. ~ classe, d7_4[1:60,], mean), aggregate(. ~ classe, d7_4[61:120,], mean), aggregate(. ~ classe, d7_4[121:180,], mean),
				 aggregate(. ~ classe, d7_4[181:240,], mean), aggregate(. ~ classe, d7_4[241:300,], mean), aggregate(. ~ classe, d7_4[301:360,], mean),
				 aggregate(. ~ classe, d7_5[1:60,], mean), aggregate(. ~ classe, d7_5[61:120,], mean), aggregate(. ~ classe, d7_5[121:180,], mean),
				 aggregate(. ~ classe, d7_5[181:240,], mean), aggregate(. ~ classe, d7_5[241:300,], mean), aggregate(. ~ classe, d7_5[301:360,], mean),
				 aggregate(. ~ classe, d7_6[1:60,], mean), aggregate(. ~ classe, d7_6[61:120,], mean), aggregate(. ~ classe, d7_6[121:180,], mean),
				 aggregate(. ~ classe, d7_6[181:240,], mean), aggregate(. ~ classe, d7_6[241:300,], mean), aggregate(. ~ classe, d7_6[301:360,], mean),
				 aggregate(. ~ classe, d7_7[1:60,], mean), aggregate(. ~ classe, d7_7[61:120,], mean), aggregate(. ~ classe, d7_7[121:180,], mean),
				 aggregate(. ~ classe, d7_7[181:240,], mean), aggregate(. ~ classe, d7_7[241:300,], mean), aggregate(. ~ classe, d7_7[301:360,], mean),
				 aggregate(. ~ classe, d7_8[1:60,], mean), aggregate(. ~ classe, d7_8[61:120,], mean), aggregate(. ~ classe, d7_8[121:180,], mean),
				 aggregate(. ~ classe, d7_8[181:240,], mean), aggregate(. ~ classe, d7_8[241:300,], mean), aggregate(. ~ classe, d7_8[301:360,], mean),
				 aggregate(. ~ classe, d7_9[1:60,], mean), aggregate(. ~ classe, d7_9[61:120,], mean), aggregate(. ~ classe, d7_9[121:180,], mean),
				 aggregate(. ~ classe, d7_9[181:240,], mean), aggregate(. ~ classe, d7_9[241:300,], mean), aggregate(. ~ classe, d7_9[301:360,], mean),
				 aggregate(. ~ classe, d7_10[1:60,], mean), aggregate(. ~ classe, d7_10[61:120,], mean), aggregate(. ~ classe, d7_10[121:180,], mean),
				 aggregate(. ~ classe, d7_10[181:240,], mean), aggregate(. ~ classe, d7_10[241:300,], mean), aggregate(. ~ classe, d7_10[301:360,], mean),
				 aggregate(. ~ classe, d7_11[1:60,], mean), aggregate(. ~ classe, d7_11[61:120,], mean), aggregate(. ~ classe, d7_11[121:180,], mean),
				 aggregate(. ~ classe, d7_11[181:240,], mean), aggregate(. ~ classe, d7_11[241:300,], mean), aggregate(. ~ classe, d7_11[301:360,], mean),
				 aggregate(. ~ classe, d7_12[1:60,], mean), aggregate(. ~ classe, d7_12[61:120,], mean), aggregate(. ~ classe, d7_12[121:180,], mean),
				 aggregate(. ~ classe, d7_12[181:240,], mean), aggregate(. ~ classe, d7_12[241:300,], mean), aggregate(. ~ classe, d7_12[301:360,], mean),
				 aggregate(. ~ classe, d7_13[1:60,], mean), aggregate(. ~ classe, d7_13[61:120,], mean), aggregate(. ~ classe, d7_13[121:180,], mean),
				 aggregate(. ~ classe, d7_13[181:240,], mean), aggregate(. ~ classe, d7_13[241:300,], mean), aggregate(. ~ classe, d7_13[301:360,], mean),
				 aggregate(. ~ classe, d7_14[1:60,], mean), aggregate(. ~ classe, d7_14[61:120,], mean), aggregate(. ~ classe, d7_14[121:180,], mean),
				 aggregate(. ~ classe, d7_14[181:240,], mean), aggregate(. ~ classe, d7_14[241:300,], mean), aggregate(. ~ classe, d7_14[301:360,], mean),
				 aggregate(. ~ classe, d7_15[1:60,], mean), aggregate(. ~ classe, d7_15[61:120,], mean), aggregate(. ~ classe, d7_15[121:180,], mean),
				 aggregate(. ~ classe, d7_15[181:240,], mean), aggregate(. ~ classe, d7_15[241:300,], mean), aggregate(. ~ classe, d7_15[301:360,], mean)
				 )

cerveja$classe[cerveja$classe == 'A1'] <- 1
cerveja$classe[cerveja$classe == 'A1.5'] <- 1.5
cerveja$classe[cerveja$classe == 'A2'] <- 2
cerveja$classe[cerveja$classe == 'A2.5'] <- 2.5
cerveja$classe[cerveja$classe == 'A3'] <- 3
cerveja$classe[cerveja$classe == 'A3.5'] <- 3.5
cerveja$classe[cerveja$classe == 'A3.7'] <- 3.7
cerveja$classe[cerveja$classe == 'A3.9'] <- 3.9
cerveja$classe[cerveja$classe == 'A4.1'] <- 4.1
cerveja$classe[cerveja$classe == 'A4.3'] <- 4.3
cerveja$classe[cerveja$classe == 'A4.5'] <- 4.5
cerveja$classe[cerveja$classe == 'A5'] <- 5
cerveja$classe[cerveja$classe == 'A5.5'] <- 5.5
cerveja$classe[cerveja$classe == 'A6'] <- 6
cerveja$classe[cerveja$classe == 'A8'] <- 8

cerveja$classe <- as.numeric(cerveja$classe)

rm(d1, d2, d3, d4, d5, d6, d7)
rm(d1_1, d1_2, d1_3, d1_4, d1_5, d1_6, d1_7, d1_8, d1_9, d1_10, d1_11, d1_12, d1_13, d1_14, d1_15)
rm(d2_1, d2_2, d2_3, d2_4, d2_5, d2_6, d2_7, d2_8, d2_9, d2_10, d2_11, d2_12, d2_13, d2_14, d2_15)
rm(d3_1, d3_2, d3_3, d3_4, d3_5, d3_6, d3_7, d3_8, d3_9, d3_10, d3_11, d3_12, d3_13, d3_14, d3_15)
rm(d4_1, d4_2, d4_3, d4_4, d4_5, d4_6, d4_7, d4_8, d4_9, d4_10, d4_11, d4_12, d4_13, d4_14, d4_15)
rm(d5_1, d5_2, d5_3, d5_4, d5_5, d5_6, d5_7, d5_8, d5_9, d5_10, d5_11, d5_12, d5_13, d5_14, d5_15)
rm(d6_1, d6_2, d6_3, d6_4, d6_5, d6_6, d6_7, d6_8, d6_9, d6_10, d6_11, d6_12, d6_13, d6_14, d6_15)
rm(d7_1, d7_2, d7_3, d7_4, d7_5, d7_6, d7_7, d7_8, d7_9, d7_10, d7_11, d7_12, d7_13, d7_14, d7_15)
rm(con, rs, df, mediaMovel)

write.table(cerveja, file = "cerveja.csv", row.names=FALSE, sep=";")

names(getModelInfo())