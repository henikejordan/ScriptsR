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

#tensao <- function(x) {
#  return ((x * 5) / (1023))
#}

f_mq2 <- function(rs, temperatura, umidade) {
  return (rs / (1.414e+00 + 3.258e-04*I(temperatura^2) -2.663e-02*temperatura -2.010e-05*I(umidade^2) + 0*umidade + 1.282e-05*temperatura*umidade))
}
f_mq3 <- function(rs, temperatura, umidade) {
  return (rs / (1.378e+00 + 2.386e-04*I(temperatura^2) -2.297e-02*temperatura -3.068e-05*I(umidade^2) + 0*umidade + 4.970e-05*temperatura*umidade))
}
f_mq4 <- function(rs, temperatura, umidade) {
  return (rs / (1.196e+00 + 9.849e-05*I(temperatura^2) -1.034e-02*temperatura -2.826e-05*I(umidade^2) + 0*umidade + 1.099e-06*temperatura*umidade))
}
f_mq5 <- function(rs, temperatura, umidade) {
  return (rs / (1.238e+00 + 1.311e-04*I(temperatura^2) -1.452e-02*temperatura -3.434e-05*I(umidade^2) + 0*umidade + 2.747e-05*temperatura*umidade))
}
f_mq6 <- function(rs, temperatura, umidade) {
  return (rs / (1.210e+00 + 1.268e-04*I(temperatura^2) -1.261e-02*temperatura -2.946e-05*I(umidade^2) + 0*umidade + 2.161e-05*temperatura*umidade))
}
f_mq7 <- function(rs, temperatura, umidade) {
  return (rs / (1.271e+00 + 1.460e-04*I(temperatura^2) -1.579e-02*temperatura -3.110e-05*I(umidade^2) + 0*umidade + 2.912e-05*temperatura*umidade))
}
f_mq8 <- function(rs, temperatura, umidade) {
  return (rs / (1.058e+00 + 1.343e-04*I(temperatura^2) -1.064e-02*temperatura -1.076e-05*I(umidade^2) + 0*umidade + 1.154e-05*temperatura*umidade))
}
f_mq9 <- function(rs, temperatura, umidade) {
  return (rs / (1.271e+00 + 1.470e-04*I(temperatura^2) -1.585e-02*temperatura -3.138e-05*I(umidade^2) + 0*umidade + 2.967e-05*temperatura*umidade))
}
f_mq135 <- function(rs, temperatura, umidade) {
  return (rs / (1.411e+00 + 3.369e-04*I(temperatura^2) -2.582e-02*temperatura -1.956e-05*I(umidade^2) + 0*umidade -3.211e-19*temperatura*umidade))
}
f_tgs822 <- function(rs, temperatura, umidade) {
  return (rs / (2.739e+00 + 4.970e-04*I(temperatura^2) -5.767e-02*temperatura + 5.695e-05*I(umidade^2) -1.791e-02*umidade + 1.539e-04*temperatura*umidade))
}
f_tgs2600 <- function(rs, temperatura, umidade) {
  return (rs / (3.114e+00 + 7.730e-04*I(temperatura^2) -8.252e-02*temperatura + 2.381e-05*I(umidade^2) -1.875e-02*umidade + 3.071e-04*temperatura*umidade))
}
f_tgs2602 <- function(rs, temperatura, umidade) {
  return (rs / (2.248e+00 + 4.016e-04*I(temperatura^2) -5.665e-02*temperatura + 1.079e-05*I(umidade^2) -7.007e-03*umidade + 1.184e-04*temperatura*umidade))
}
f_tgs2603 <- function(rs, temperatura, umidade) {
  return (rs / (1.731e+00 + 2.286e-04*I(temperatura^2) -4.000e-02*temperatura + 0*I(umidade^2) + 0*umidade + 0*temperatura*umidade))
}

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                  max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora 
                  join umidade u on g.data_hora = u.data_hora 
                  where g.data_hora >= '2018-08-01 00:00:00' and g.data_hora < '2018-12-30 23:00:00' 
                  and g.classe like 'Fase%' 
                  group by g.data_hora, g.classe, t.temperatura, u.umidade 
                  order by g.data_hora")
df <- fetch(rs, n = -1)
data <- df[2:17]
data[1:15] <- as.data.frame(lapply(data[1:15], classe = data$classe, mediaMovel))
#data[1:15] <- as.data.frame(lapply(data[1:15], classe = data$classe, tensao))
data[1] <- as.data.frame(lapply(data[1], temperatura=data$temperatura, umidade=data$umidade, f_mq2))
data[2] <- as.data.frame(lapply(data[2], temperatura=data$temperatura, umidade=data$umidade, f_mq3))
data[3] <- as.data.frame(lapply(data[3], temperatura=data$temperatura, umidade=data$umidade, f_mq4))
data[4] <- as.data.frame(lapply(data[4], temperatura=data$temperatura, umidade=data$umidade, f_mq5))
data[5] <- as.data.frame(lapply(data[5], temperatura=data$temperatura, umidade=data$umidade, f_mq6))
data[6] <- as.data.frame(lapply(data[6], temperatura=data$temperatura, umidade=data$umidade, f_mq7))
data[7] <- as.data.frame(lapply(data[7], temperatura=data$temperatura, umidade=data$umidade, f_mq8))
data[8] <- as.data.frame(lapply(data[8], temperatura=data$temperatura, umidade=data$umidade, f_mq9))
data[9] <- as.data.frame(lapply(data[9], temperatura=data$temperatura, umidade=data$umidade, f_mq135))
data[10] <- as.data.frame(lapply(data[10], temperatura=data$temperatura, umidade=data$umidade, f_tgs822))
data[11] <- as.data.frame(lapply(data[11], temperatura=data$temperatura, umidade=data$umidade, f_tgs2600))
data[12] <- as.data.frame(lapply(data[12], temperatura=data$temperatura, umidade=data$umidade, f_tgs2602))
data[13] <- as.data.frame(lapply(data[13], temperatura=data$temperatura, umidade=data$umidade, f_tgs2603))
data <- cbind(data[1:13], data[16])

f1 <- data[data$classe == 'Fase 1',]
f2 <- data[data$classe == 'Fase 2',]
f3 <- data[data$classe == 'Fase 3',]
f4 <- data[data$classe == 'Fase 4',]
f5 <- data[data$classe == 'Fase 5',]
f6 <- data[data$classe == 'Fase 6',]
f7 <- data[data$classe == 'Fase 7',]
f8 <- data[data$classe == 'Fase 8',]
f9 <- data[data$classe == 'Fase 9',]
f10 <- data[data$classe == 'Fase 10',]
f11 <- data[data$classe == 'Fase 11',]
f12 <- data[data$classe == 'Fase 12',]

f1 <- f1[-(1:600),]
f2 <- f2[-(1:600),]
f3 <- f3[-(1:600),]
f4 <- f4[-(1:600),]
f5 <- f5[-(1:600),]
f6 <- f6[-(1:600),]
f7 <- f7[-(1:600),]
f8 <- f8[-(1:600),]
f9 <- f9[-(1:600),]
f10 <- f10[-(1:600),]
f11 <- f11[-(1:600),]
f12 <- f12[-(1:600),]

f1_1 <- f1[1:120,]
f1_2 <- f1[121:240,]
f1_3 <- f1[241:360,]
f1_4 <- f1[361:480,]
f1_5 <- f1[481:600,]
f1_6 <- f1[601:720,]
f1_7 <- f1[721:840,]
f1_8 <- f1[841:960,]
f1_9 <- f1[961:1080,]
f1_10 <- f1[1081:1200,]
f1_11 <- f1[1201:1320,]
f1_12 <- f1[1321:1440,]
f1_13 <- f1[1441:1560,]
f1_14 <- f1[1561:1680,]
f1_15 <- f1[1681:1800,]
f2_1 <- f2[1:120,]
f2_2 <- f2[121:240,]
f2_3 <- f2[241:360,]
f2_4 <- f2[361:480,]
f2_5 <- f2[481:600,]
f2_6 <- f2[601:720,]
f2_7 <- f2[721:840,]
f2_8 <- f2[841:960,]
f2_9 <- f2[961:1080,]
f2_10 <- f2[1081:1200,]
f2_11 <- f2[1201:1320,]
f2_12 <- f2[1321:1440,]
f2_13 <- f2[1441:1560,]
f2_14 <- f2[1561:1680,]
f2_15 <- f2[1681:1800,]
f3_1 <- f3[1:120,]
f3_2 <- f3[121:240,]
f3_3 <- f3[241:360,]
f3_4 <- f3[361:480,]
f3_5 <- f3[481:600,]
f3_6 <- f3[601:720,]
f3_7 <- f3[721:840,]
f3_8 <- f3[841:960,]
f3_9 <- f3[961:1080,]
f3_10 <- f3[1081:1200,]
f3_11 <- f3[1201:1320,]
f3_12 <- f3[1321:1440,]
f3_13 <- f3[1441:1560,]
f3_14 <- f3[1561:1680,]
f3_15 <- f3[1681:1800,]
f4_1 <- f4[1:120,]
f4_2 <- f4[121:240,]
f4_3 <- f4[241:360,]
f4_4 <- f4[361:480,]
f4_5 <- f4[481:600,]
f4_6 <- f4[601:720,]
f4_7 <- f4[721:840,]
f4_8 <- f4[841:960,]
f4_9 <- f4[961:1080,]
f4_10 <- f4[1081:1200,]
f4_11 <- f4[1201:1320,]
f4_12 <- f4[1321:1440,]
f4_13 <- f4[1441:1560,]
f4_14 <- f4[1561:1680,]
f4_15 <- f4[1681:1800,]
f5_1 <- f5[1:120,]
f5_2 <- f5[121:240,]
f5_3 <- f5[241:360,]
f5_4 <- f5[361:480,]
f5_5 <- f5[481:600,]
f5_6 <- f5[601:720,]
f5_7 <- f5[721:840,]
f5_8 <- f5[841:960,]
f5_9 <- f5[961:1080,]
f5_10 <- f5[1081:1200,]
f5_11 <- f5[1201:1320,]
f5_12 <- f5[1321:1440,]
f5_13 <- f5[1441:1560,]
f5_14 <- f5[1561:1680,]
f5_15 <- f5[1681:1800,]
f6_1 <- f6[1:120,]
f6_2 <- f6[121:240,]
f6_3 <- f6[241:360,]
f6_4 <- f6[361:480,]
f6_5 <- f6[481:600,]
f6_6 <- f6[601:720,]
f6_7 <- f6[721:840,]
f6_8 <- f6[841:960,]
f6_9 <- f6[961:1080,]
f6_10 <- f6[1081:1200,]
f6_11 <- f6[1201:1320,]
f6_12 <- f6[1321:1440,]
f6_13 <- f6[1441:1560,]
f6_14 <- f6[1561:1680,]
f6_15 <- f6[1681:1800,]
f7_1 <- f7[1:120,]
f7_2 <- f7[121:240,]
f7_3 <- f7[241:360,]
f7_4 <- f7[361:480,]
f7_5 <- f7[481:600,]
f7_6 <- f7[601:720,]
f7_7 <- f7[721:840,]
f7_8 <- f7[841:960,]
f7_9 <- f7[961:1080,]
f7_10 <- f7[1081:1200,]
f7_11 <- f7[1201:1320,]
f7_12 <- f7[1321:1440,]
f7_13 <- f7[1441:1560,]
f7_14 <- f7[1561:1680,]
f7_15 <- f7[1681:1800,]
f8_1 <- f8[1:120,]
f8_2 <- f8[121:240,]
f8_3 <- f8[241:360,]
f8_4 <- f8[361:480,]
f8_5 <- f8[481:600,]
f8_6 <- f8[601:720,]
f8_7 <- f8[721:840,]
f8_8 <- f8[841:960,]
f8_9 <- f8[961:1080,]
f8_10 <- f8[1081:1200,]
f8_11 <- f8[1201:1320,]
f8_12 <- f8[1321:1440,]
f8_13 <- f8[1441:1560,]
f8_14 <- f8[1561:1680,]
f8_15 <- f8[1681:1800,]
f9_1 <- f9[1:120,]
f9_2 <- f9[121:240,]
f9_3 <- f9[241:360,]
f9_4 <- f9[361:480,]
f9_5 <- f9[481:600,]
f9_6 <- f9[601:720,]
f9_7 <- f9[721:840,]
f9_8 <- f9[841:960,]
f9_9 <- f9[961:1080,]
f9_10 <- f9[1081:1200,]
f9_11 <- f9[1201:1320,]
f9_12 <- f9[1321:1440,]
f9_13 <- f9[1441:1560,]
f9_14 <- f9[1561:1680,]
f9_15 <- f9[1681:1800,]
f10_1 <- f10[1:120,]
f10_2 <- f10[121:240,]
f10_3 <- f10[241:360,]
f10_4 <- f10[361:480,]
f10_5 <- f10[481:600,]
f10_6 <- f10[601:720,]
f10_7 <- f10[721:840,]
f10_8 <- f10[841:960,]
f10_9 <- f10[961:1080,]
f10_10 <- f10[1081:1200,]
f10_11 <- f10[1201:1320,]
f10_12 <- f10[1321:1440,]
f10_13 <- f10[1441:1560,]
f10_14 <- f10[1561:1680,]
f10_15 <- f10[1681:1800,]
f11_1 <- f11[1:120,]
f11_2 <- f11[121:240,]
f11_3 <- f11[241:360,]
f11_4 <- f11[361:480,]
f11_5 <- f11[481:600,]
f11_6 <- f11[601:720,]
f11_7 <- f11[721:840,]
f11_8 <- f11[841:960,]
f11_9 <- f11[961:1080,]
f11_10 <- f11[1081:1200,]
f11_11 <- f11[1201:1320,]
f11_12 <- f11[1321:1440,]
f11_13 <- f11[1441:1560,]
f11_14 <- f11[1561:1680,]
f11_15 <- f11[1681:1800,]
f12_1 <- f12[1:120,]
f12_2 <- f12[121:240,]
f12_3 <- f12[241:360,]
f12_4 <- f12[361:480,]
f12_5 <- f12[481:600,]
f12_6 <- f12[601:720,]
f12_7 <- f12[721:840,]
f12_8 <- f12[841:960,]
f12_9 <- f12[961:1080,]
f12_10 <- f12[1081:1200,]
f12_11 <- f12[1201:1320,]
f12_12 <- f12[1321:1440,]
f12_13 <- f12[1441:1560,]
f12_14 <- f12[1561:1680,]
f12_15 <- f12[1681:1800,]

pessego <- rbind(aggregate(. ~ classe, f1_1, mean), aggregate(. ~ classe, f1_2, mean), aggregate(. ~ classe, f1_3, mean), 
                 aggregate(. ~ classe, f1_4, mean), aggregate(. ~ classe, f1_5, mean), aggregate(. ~ classe, f1_6, mean), 
                 aggregate(. ~ classe, f1_7, mean), aggregate(. ~ classe, f1_8, mean), aggregate(. ~ classe, f1_9, mean), 
                 aggregate(. ~ classe, f1_10, mean), aggregate(. ~ classe, f1_11, mean), aggregate(. ~ classe, f1_12, mean), 
                 aggregate(. ~ classe, f1_13, mean), aggregate(. ~ classe, f1_14, mean), aggregate(. ~ classe, f1_15, mean), 
                 aggregate(. ~ classe, f2_1, mean), aggregate(. ~ classe, f2_2, mean), aggregate(. ~ classe, f2_3, mean), 
                 aggregate(. ~ classe, f2_4, mean), aggregate(. ~ classe, f2_5, mean), aggregate(. ~ classe, f2_6, mean), 
                 aggregate(. ~ classe, f2_7, mean), aggregate(. ~ classe, f2_8, mean), aggregate(. ~ classe, f2_9, mean), 
                 aggregate(. ~ classe, f2_10, mean), aggregate(. ~ classe, f2_11, mean), aggregate(. ~ classe, f2_12, mean), 
                 aggregate(. ~ classe, f2_13, mean), aggregate(. ~ classe, f2_14, mean), aggregate(. ~ classe, f2_15, mean),
                 aggregate(. ~ classe, f3_1, mean), aggregate(. ~ classe, f3_2, mean), aggregate(. ~ classe, f3_3, mean), 
                 aggregate(. ~ classe, f3_4, mean), aggregate(. ~ classe, f3_5, mean), aggregate(. ~ classe, f3_6, mean), 
                 aggregate(. ~ classe, f3_7, mean), aggregate(. ~ classe, f3_8, mean), aggregate(. ~ classe, f3_9, mean), 
                 aggregate(. ~ classe, f3_10, mean), aggregate(. ~ classe, f3_11, mean), aggregate(. ~ classe, f3_12, mean), 
                 aggregate(. ~ classe, f3_13, mean), aggregate(. ~ classe, f3_14, mean), aggregate(. ~ classe, f3_15, mean),
                 aggregate(. ~ classe, f4_1, mean), aggregate(. ~ classe, f4_2, mean), aggregate(. ~ classe, f4_3, mean), 
                 aggregate(. ~ classe, f4_4, mean), aggregate(. ~ classe, f4_5, mean), aggregate(. ~ classe, f4_6, mean), 
                 aggregate(. ~ classe, f4_7, mean), aggregate(. ~ classe, f4_8, mean), aggregate(. ~ classe, f4_9, mean), 
                 aggregate(. ~ classe, f4_10, mean), aggregate(. ~ classe, f4_11, mean), aggregate(. ~ classe, f4_12, mean), 
                 aggregate(. ~ classe, f4_13, mean), aggregate(. ~ classe, f4_14, mean), aggregate(. ~ classe, f4_15, mean),
                 aggregate(. ~ classe, f5_1, mean), aggregate(. ~ classe, f5_2, mean), aggregate(. ~ classe, f5_3, mean), 
                 aggregate(. ~ classe, f5_4, mean), aggregate(. ~ classe, f5_5, mean), aggregate(. ~ classe, f5_6, mean), 
                 aggregate(. ~ classe, f5_7, mean), aggregate(. ~ classe, f5_8, mean), aggregate(. ~ classe, f5_9, mean), 
                 aggregate(. ~ classe, f5_10, mean), aggregate(. ~ classe, f5_11, mean), aggregate(. ~ classe, f5_12, mean), 
                 aggregate(. ~ classe, f5_13, mean), aggregate(. ~ classe, f5_14, mean), aggregate(. ~ classe, f5_15, mean),
                 aggregate(. ~ classe, f6_1, mean), aggregate(. ~ classe, f6_2, mean), aggregate(. ~ classe, f6_3, mean), 
                 aggregate(. ~ classe, f6_4, mean), aggregate(. ~ classe, f6_5, mean), aggregate(. ~ classe, f6_6, mean), 
                 aggregate(. ~ classe, f6_7, mean), aggregate(. ~ classe, f6_8, mean), aggregate(. ~ classe, f6_9, mean), 
                 aggregate(. ~ classe, f6_10, mean), aggregate(. ~ classe, f6_11, mean), aggregate(. ~ classe, f6_12, mean), 
                 aggregate(. ~ classe, f6_13, mean), aggregate(. ~ classe, f6_14, mean), aggregate(. ~ classe, f6_15, mean),
                 aggregate(. ~ classe, f7_1, mean), aggregate(. ~ classe, f7_2, mean), aggregate(. ~ classe, f7_3, mean), 
                 aggregate(. ~ classe, f7_4, mean), aggregate(. ~ classe, f7_5, mean), aggregate(. ~ classe, f7_6, mean), 
                 aggregate(. ~ classe, f7_7, mean), aggregate(. ~ classe, f7_8, mean), aggregate(. ~ classe, f7_9, mean), 
                 aggregate(. ~ classe, f7_10, mean), aggregate(. ~ classe, f7_11, mean), aggregate(. ~ classe, f7_12, mean), 
                 aggregate(. ~ classe, f7_13, mean), aggregate(. ~ classe, f7_14, mean), aggregate(. ~ classe, f7_15, mean),
                 aggregate(. ~ classe, f8_1, mean), aggregate(. ~ classe, f8_2, mean), aggregate(. ~ classe, f8_3, mean), 
                 aggregate(. ~ classe, f8_4, mean), aggregate(. ~ classe, f8_5, mean), aggregate(. ~ classe, f8_6, mean), 
                 aggregate(. ~ classe, f8_7, mean), aggregate(. ~ classe, f8_8, mean), aggregate(. ~ classe, f8_9, mean), 
                 aggregate(. ~ classe, f8_10, mean), aggregate(. ~ classe, f8_11, mean), aggregate(. ~ classe, f8_12, mean), 
                 aggregate(. ~ classe, f8_13, mean), aggregate(. ~ classe, f8_14, mean), aggregate(. ~ classe, f8_15, mean),
                 aggregate(. ~ classe, f9_1, mean), aggregate(. ~ classe, f9_2, mean), aggregate(. ~ classe, f9_3, mean), 
                 aggregate(. ~ classe, f9_4, mean), aggregate(. ~ classe, f9_5, mean), aggregate(. ~ classe, f9_6, mean), 
                 aggregate(. ~ classe, f9_7, mean), aggregate(. ~ classe, f9_8, mean), aggregate(. ~ classe, f9_9, mean), 
                 aggregate(. ~ classe, f9_10, mean), aggregate(. ~ classe, f9_11, mean), aggregate(. ~ classe, f9_12, mean), 
                 aggregate(. ~ classe, f9_13, mean), aggregate(. ~ classe, f9_14, mean), aggregate(. ~ classe, f9_15, mean),
                 aggregate(. ~ classe, f10_1, mean), aggregate(. ~ classe, f10_2, mean), aggregate(. ~ classe, f10_3, mean), 
                 aggregate(. ~ classe, f10_4, mean), aggregate(. ~ classe, f10_5, mean), aggregate(. ~ classe, f10_6, mean), 
                 aggregate(. ~ classe, f10_7, mean), aggregate(. ~ classe, f10_8, mean), aggregate(. ~ classe, f10_9, mean), 
                 aggregate(. ~ classe, f10_10, mean), aggregate(. ~ classe, f10_11, mean), aggregate(. ~ classe, f10_12, mean), 
                 aggregate(. ~ classe, f10_13, mean), aggregate(. ~ classe, f10_14, mean), aggregate(. ~ classe, f10_15, mean),
                 aggregate(. ~ classe, f11_1, mean), aggregate(. ~ classe, f11_2, mean), aggregate(. ~ classe, f11_3, mean), 
                 aggregate(. ~ classe, f11_4, mean), aggregate(. ~ classe, f11_5, mean), aggregate(. ~ classe, f11_6, mean), 
                 aggregate(. ~ classe, f11_7, mean), aggregate(. ~ classe, f11_8, mean), aggregate(. ~ classe, f11_9, mean), 
                 aggregate(. ~ classe, f11_10, mean), aggregate(. ~ classe, f11_11, mean), aggregate(. ~ classe, f11_12, mean), 
                 aggregate(. ~ classe, f11_13, mean), aggregate(. ~ classe, f11_14, mean), aggregate(. ~ classe, f11_15, mean),
                 aggregate(. ~ classe, f12_1, mean), aggregate(. ~ classe, f12_2, mean), aggregate(. ~ classe, f12_3, mean), 
                 aggregate(. ~ classe, f12_4, mean), aggregate(. ~ classe, f12_5, mean), aggregate(. ~ classe, f12_6, mean), 
                 aggregate(. ~ classe, f12_7, mean), aggregate(. ~ classe, f12_8, mean), aggregate(. ~ classe, f12_9, mean), 
                 aggregate(. ~ classe, f12_10, mean), aggregate(. ~ classe, f12_11, mean), aggregate(. ~ classe, f12_12, mean), 
                 aggregate(. ~ classe, f12_13, mean), aggregate(. ~ classe, f12_14, mean), aggregate(. ~ classe, f12_15, mean))

rm(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
rm(f1_1, f1_2, f1_3, f1_4, f1_5, f1_6, f1_7, f1_8, f1_9, f1_10, f1_11, f1_12, f1_13, f1_14, f1_15)
rm(f2_1, f2_2, f2_3, f2_4, f2_5, f2_6, f2_7, f2_8, f2_9, f2_10, f2_11, f2_12, f2_13, f2_14, f2_15)
rm(f3_1, f3_2, f3_3, f3_4, f3_5, f3_6, f3_7, f3_8, f3_9, f3_10, f3_11, f3_12, f3_13, f3_14, f3_15)
rm(f4_1, f4_2, f4_3, f4_4, f4_5, f4_6, f4_7, f4_8, f4_9, f4_10, f4_11, f4_12, f4_13, f4_14, f4_15)
rm(f5_1, f5_2, f5_3, f5_4, f5_5, f5_6, f5_7, f5_8, f5_9, f5_10, f5_11, f5_12, f5_13, f5_14, f5_15)
rm(f6_1, f6_2, f6_3, f6_4, f6_5, f6_6, f6_7, f6_8, f6_9, f6_10, f6_11, f6_12, f6_13, f6_14, f6_15)
rm(f7_1, f7_2, f7_3, f7_4, f7_5, f7_6, f7_7, f7_8, f7_9, f7_10, f7_11, f7_12, f7_13, f7_14, f7_15)
rm(f8_1, f8_2, f8_3, f8_4, f8_5, f8_6, f8_7, f8_8, f8_9, f8_10, f8_11, f8_12, f8_13, f8_14, f8_15)
rm(f9_1, f9_2, f9_3, f9_4, f9_5, f9_6, f9_7, f9_8, f9_9, f9_10, f9_11, f9_12, f9_13, f9_14, f9_15)
rm(f10_1, f10_2, f10_3, f10_4, f10_5, f10_6, f10_7, f10_8, f10_9, f10_10, f10_11, f10_12, f10_13, f10_14, f10_15)
rm(f11_1, f11_2, f11_3, f11_4, f11_5, f11_6, f11_7, f11_8, f11_9, f11_10, f11_11, f11_12, f11_13, f11_14, f11_15)
rm(f12_1, f12_2, f12_3, f12_4, f12_5, f12_6, f12_7, f12_8, f12_9, f12_10, f12_11, f12_12, f12_13, f12_14, f12_15)
rm(f_mq2, f_mq3, f_mq4, f_mq5, f_mq6, f_mq7, f_mq8, f_mq9, f_mq135, f_tgs822, f_tgs2600, f_tgs2602, f_tgs2603)

#write.table(pessego, file = "pessego.csv", row.names=FALSE, sep=";")

pessego$classe[pessego$classe == 'Fase 1'] <- 'Estagio 1'
pessego$classe[pessego$classe == 'Fase 2'] <- 'Estagio 1'
pessego$classe[pessego$classe == 'Fase 3'] <- 'Estagio 1'
pessego$classe[pessego$classe == 'Fase 4'] <- 'Estagio 2'
pessego$classe[pessego$classe == 'Fase 5'] <- 'Estagio 2'
pessego$classe[pessego$classe == 'Fase 6'] <- 'Estagio 2'
pessego$classe[pessego$classe == 'Fase 7'] <- 'Estagio 3'
pessego$classe[pessego$classe == 'Fase 8'] <- 'Estagio 3'
pessego$classe[pessego$classe == 'Fase 9'] <- 'Estagio 3'
pessego$classe[pessego$classe == 'Fase 10'] <- 'Estagio 4'
pessego$classe[pessego$classe == 'Fase 11'] <- 'Estagio 4'
pessego$classe[pessego$classe == 'Fase 12'] <- 'Estagio 4'

library(FSelector)

weights <- chi.squared(classe~., pessego)
weights

#sensores utilizados com a compensação de temperatura e umidade
pessego_reduzido <- pessego[,c(1,3,4,5,14)]

require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)

#######################TUDO######################
#################################################
pca <- prcomp(pessego[,-1],
              center = TRUE,
              scale. = TRUE) 

prop.pca = pca$sdev^2/sum(pca$sdev^2)

lda <- lda(classe ~ ., 
           pessego, 
           prior = c(1,1,1,1)/4)

prop.lda = lda$svd^2/sum(lda$svd^2)

plda <- predict(object = lda,
                newdata = pessego)

dataset = data.frame(classe = pessego$classe, pca = pca$x, lda = plda$x)

p1 <- ggplot(dataset, aes(lda.LD1, lda.LD2, col=classe, fill=classe)) +
  stat_ellipse(geom="polygon", col="black", alpha=0.5) + 
  geom_point(aes(lda.LD1, lda.LD2, colour = classe, shape = classe), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

p2 <- ggplot(dataset, aes(pca.PC1, pca.PC2, col=classe, fill=classe)) +
  stat_ellipse(geom="polygon", col="black", alpha=0.5) + 
  geom_point(aes(pca.PC1, pca.PC2, colour = classe, shape = classe), size = 2.5) +
  labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

grid.arrange(p1, p2)

library("car")

scatter3d(x = dataset$pca.PC1, y = dataset$pca.PC2, z = dataset$pca.PC3, 
          groups = dataset$classe, surface=FALSE, ellipsoid = TRUE, 
          xlab = paste("PC1 (", percent(prop.pca[1]), ")", sep=""), 
          ylab = paste("PC2 (", percent(prop.pca[2]), ")", sep=""), 
          zlab = paste("PC3 (", percent(prop.pca[3]), ")", sep=""))

scatter3d(x = dataset$lda.LD1, y = dataset$lda.LD2, z = dataset$lda.LD3, 
          groups = dataset$classe, surface=FALSE, ellipsoid = TRUE, 
          xlab = paste("LD1 (", percent(prop.lda[1]), ")", sep=""), 
          ylab = paste("LD2 (", percent(prop.lda[2]), ")", sep=""), 
          zlab = paste("LD3 (", percent(prop.lda[3]), ")", sep=""))

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

trainIndex = createDataPartition(pessego$classe, p=0.7, list=FALSE, times=1)
train_set = pessego[trainIndex,]
test_set = pessego[-trainIndex,]

pca <- dataset[,1:4]
trainIndex = createDataPartition(pca$classe, p=0.7, list=FALSE, times=1)
train_set_pca = pca[trainIndex,]
test_set_pca = pca[-trainIndex,]

lda <- dataset[,c(1,15,16,17)]
trainIndex = createDataPartition(lda$classe, p=0.7, list=FALSE, times=1)
train_set_lda = lda[trainIndex,]
test_set_lda = lda[-trainIndex,]

######################KNN########################
model <- train(classe~., train_set, method = "knn", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set)
confusionMatrix(pred, as.factor(test_set$classe))

model <- train(classe~., train_set_pca, method = "knn", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_pca)
confusionMatrix(pred, as.factor(test_set_pca$classe))

model <- train(classe~., train_set_lda, method = "knn", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_lda)
confusionMatrix(pred, as.factor(test_set_lda$classe))
#################################################


######################SVM########################
model <- train(classe~., train_set, method = "svmLinear", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set)
confusionMatrix(pred, as.factor(test_set$classe))

model <- train(classe~., train_set_pca, method = "svmLinear", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_pca)
confusionMatrix(pred, as.factor(test_set_pca$classe))

model <- train(classe~., train_set_lda, method = "svmLinear", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_lda)
confusionMatrix(pred, as.factor(test_set_lda$classe))
#################################################


######################RF#########################
model <- train(classe~., train_set, method = "rf", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set)
confusionMatrix(pred, as.factor(test_set$classe))

model <- train(classe~., train_set_pca, method = "rf", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_pca)
confusionMatrix(pred, as.factor(test_set_pca$classe))

model <- train(classe~., train_set_lda, method = "rf", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_lda)
confusionMatrix(pred, as.factor(test_set_lda$classe))
#################################################


######################ELM########################
model <- train(classe~., train_set, method = "elm", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set)
confusionMatrix(pred, as.factor(test_set$classe))

model <- train(classe~., train_set_pca, method = "elm", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_pca)
confusionMatrix(pred, as.factor(test_set_pca$classe))

model <- train(classe~., train_set_lda, method = "elm", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_lda)
confusionMatrix(pred, as.factor(test_set_lda$classe))
#################################################



#####################REDUZIDO####################
#################################################
pca <- prcomp(pessego_reduzido[,-1],
              center = TRUE,
              scale. = TRUE) 

prop.pca = pca$sdev^2/sum(pca$sdev^2)

lda <- lda(classe ~ ., 
           pessego_reduzido, 
           prior = c(1,1,1,1)/4)

prop.lda = lda$svd^2/sum(lda$svd^2)

plda <- predict(object = lda,
                newdata = pessego_reduzido)

dataset = data.frame(classe = pessego_reduzido$classe, pca = pca$x, lda = plda$x)

p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = classe, shape = classe), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))

p2 <- ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, colour = classe, shape = classe), size = 2.5) +
  labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

grid.arrange(p1, p2)

library("car")

scatter3d(x = dataset$pca.PC1, y = dataset$pca.PC2, z = dataset$pca.PC3, 
          groups = dataset$classe, surface=FALSE, ellipsoid = TRUE, 
          xlab = paste("PC1 (", percent(prop.pca[1]), ")", sep=""), 
          ylab = paste("PC2 (", percent(prop.pca[2]), ")", sep=""), 
          zlab = paste("PC3 (", percent(prop.pca[3]), ")", sep=""))

scatter3d(x = dataset$lda.LD1, y = dataset$lda.LD2, z = dataset$lda.LD3, 
          groups = dataset$classe, surface=FALSE, ellipsoid = TRUE, 
          xlab = paste("LD1 (", percent(prop.lda[1]), ")", sep=""), 
          ylab = paste("LD2 (", percent(prop.lda[2]), ")", sep=""), 
          zlab = paste("LD3 (", percent(prop.lda[3]), ")", sep=""))

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

trainIndex = createDataPartition(pessego_reduzido$classe, p=0.7, list=FALSE, times=1)
train_set = pessego_reduzido[trainIndex,]
test_set = pessego_reduzido[-trainIndex,]

pca <- dataset[,1:4]
trainIndex = createDataPartition(pca$classe, p=0.7, list=FALSE, times=1)
train_set_pca = pca[trainIndex,]
test_set_pca = pca[-trainIndex,]

lda <- dataset[,c(1,6,7,8)]
trainIndex = createDataPartition(lda$classe, p=0.7, list=FALSE, times=1)
train_set_lda = lda[trainIndex,]
test_set_lda = lda[-trainIndex,]

######################KNN########################
model <- train(classe~., train_set, method = "knn", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set)
confusionMatrix(pred, as.factor(test_set$classe))

model <- train(classe~., train_set_pca, method = "knn", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_pca)
confusionMatrix(pred, as.factor(test_set_pca$classe))

model <- train(classe~., train_set_lda, method = "knn", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_lda)
confusionMatrix(pred, as.factor(test_set_lda$classe))
#################################################


######################SVM########################
model <- train(classe~., train_set, method = "svmLinear", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set)
confusionMatrix(pred, as.factor(test_set$classe))

model <- train(classe~., train_set_pca, method = "svmLinear", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_pca)
confusionMatrix(pred, as.factor(test_set_pca$classe))

model <- train(classe~., train_set_lda, method = "svmLinear", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_lda)
confusionMatrix(pred, as.factor(test_set_lda$classe))
#################################################


######################RF#########################
model <- train(classe~., train_set, method = "rf", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set)
confusionMatrix(pred, as.factor(test_set$classe))

model <- train(classe~., train_set_pca, method = "rf", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_pca)
confusionMatrix(pred, as.factor(test_set_pca$classe))

model <- train(classe~., train_set_lda, method = "rf", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_lda)
confusionMatrix(pred, as.factor(test_set_lda$classe))
#################################################


######################ELM########################
model <- train(classe~., train_set, method = "elm", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set)
confusionMatrix(pred, as.factor(test_set$classe))

model <- train(classe~., train_set_pca, method = "elm", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_pca)
confusionMatrix(pred, as.factor(test_set_pca$classe))

model <- train(classe~., train_set_lda, method = "elm", trControl=train_control, preProcess=c("center", "scale"))
print(model)
pred <- predict(model, newdata=test_set_lda)
confusionMatrix(pred, as.factor(test_set_lda$classe))
#################################################
