library(RPostgreSQL)
library(caret)

#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}

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
				          where g.data_hora >= '2018-11-22 16:20:00' and g.data_hora < '2018-11-22 19:20:00'
                  and g.classe like 'A%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
d1 <- df[2:17]
d1[1:15] <- as.data.frame(lapply(d1[1:15], classe = d1$classe, mediaMovel))

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
				          where g.data_hora >= '2018-11-29 12:00:00' and g.data_hora < '2018-11-29 14:40:00'
                  and g.classe like 'A%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
d2 <- df[2:17]
d2[1:15] <- as.data.frame(lapply(d2[1:15], classe = d2$classe, mediaMovel))

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
				          where g.data_hora >= '2018-11-29 14:40:00' and g.data_hora < '2018-11-29 17:10:00'
                  and g.classe like 'A%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
d3 <- df[2:17]
d3[1:15] <- as.data.frame(lapply(d3[1:15], classe = d3$classe, mediaMovel))

con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-07 11:40:00' and g.data_hora < '2018-12-07 14:10:00'
                   and g.classe like 'A%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
d4 <- df[2:17]
d4[1:15] <- as.data.frame(lapply(d4[1:15], classe = d4$classe, mediaMovel))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-07 14:10:00' and g.data_hora < '2018-12-07 16:40:00'
                   and g.classe like 'A%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
d5 <- df[2:17]
d5[1:15] <- as.data.frame(lapply(d5[1:15], classe = d5$classe, mediaMovel))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-12 14:40:00' and g.data_hora < '2018-12-12 17:15:00'
                   and g.classe like 'A%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
d6 <- df[2:17]
d6[1:15] <- as.data.frame(lapply(d6[1:15], classe = d6$classe, mediaMovel))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-12 17:15:00' and g.data_hora < '2018-12-12 19:30:00'
                   and g.classe like 'A%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
d7 <- df[2:17]
d7[1:15] <- as.data.frame(lapply(d7[1:15], classe = d7$classe, mediaMovel))

d1[1] <- as.data.frame(lapply(d1[1], temperatura=d1$temperatura, umidade=d1$umidade, f_mq2))
d1[2] <- as.data.frame(lapply(d1[2], temperatura=d1$temperatura, umidade=d1$umidade, f_mq3))
d1[3] <- as.data.frame(lapply(d1[3], temperatura=d1$temperatura, umidade=d1$umidade, f_mq4))
d1[4] <- as.data.frame(lapply(d1[4], temperatura=d1$temperatura, umidade=d1$umidade, f_mq5))
d1[5] <- as.data.frame(lapply(d1[5], temperatura=d1$temperatura, umidade=d1$umidade, f_mq6))
d1[6] <- as.data.frame(lapply(d1[6], temperatura=d1$temperatura, umidade=d1$umidade, f_mq7))
d1[7] <- as.data.frame(lapply(d1[7], temperatura=d1$temperatura, umidade=d1$umidade, f_mq8))
d1[8] <- as.data.frame(lapply(d1[8], temperatura=d1$temperatura, umidade=d1$umidade, f_mq9))
d1[9] <- as.data.frame(lapply(d1[9], temperatura=d1$temperatura, umidade=d1$umidade, f_mq135))
d1[10] <- as.data.frame(lapply(d1[10], temperatura=d1$temperatura, umidade=d1$umidade, f_tgs822))
d1[11] <- as.data.frame(lapply(d1[11], temperatura=d1$temperatura, umidade=d1$umidade, f_tgs2600))
d1[12] <- as.data.frame(lapply(d1[12], temperatura=d1$temperatura, umidade=d1$umidade, f_tgs2602))
d1[13] <- as.data.frame(lapply(d1[13], temperatura=d1$temperatura, umidade=d1$umidade, f_tgs2603))

d2[1] <- as.data.frame(lapply(d2[1], temperatura=d2$temperatura, umidade=d2$umidade, f_mq2))
d2[2] <- as.data.frame(lapply(d2[2], temperatura=d2$temperatura, umidade=d2$umidade, f_mq3))
d2[3] <- as.data.frame(lapply(d2[3], temperatura=d2$temperatura, umidade=d2$umidade, f_mq4))
d2[4] <- as.data.frame(lapply(d2[4], temperatura=d2$temperatura, umidade=d2$umidade, f_mq5))
d2[5] <- as.data.frame(lapply(d2[5], temperatura=d2$temperatura, umidade=d2$umidade, f_mq6))
d2[6] <- as.data.frame(lapply(d2[6], temperatura=d2$temperatura, umidade=d2$umidade, f_mq7))
d2[7] <- as.data.frame(lapply(d2[7], temperatura=d2$temperatura, umidade=d2$umidade, f_mq8))
d2[8] <- as.data.frame(lapply(d2[8], temperatura=d2$temperatura, umidade=d2$umidade, f_mq9))
d2[9] <- as.data.frame(lapply(d2[9], temperatura=d2$temperatura, umidade=d2$umidade, f_mq135))
d2[10] <- as.data.frame(lapply(d2[10], temperatura=d2$temperatura, umidade=d2$umidade, f_tgs822))
d2[11] <- as.data.frame(lapply(d2[11], temperatura=d2$temperatura, umidade=d2$umidade, f_tgs2600))
d2[12] <- as.data.frame(lapply(d2[12], temperatura=d2$temperatura, umidade=d2$umidade, f_tgs2602))
d2[13] <- as.data.frame(lapply(d2[13], temperatura=d2$temperatura, umidade=d2$umidade, f_tgs2603))

d3[1] <- as.data.frame(lapply(d3[1], temperatura=d3$temperatura, umidade=d3$umidade, f_mq2))
d3[2] <- as.data.frame(lapply(d3[2], temperatura=d3$temperatura, umidade=d3$umidade, f_mq3))
d3[3] <- as.data.frame(lapply(d3[3], temperatura=d3$temperatura, umidade=d3$umidade, f_mq4))
d3[4] <- as.data.frame(lapply(d3[4], temperatura=d3$temperatura, umidade=d3$umidade, f_mq5))
d3[5] <- as.data.frame(lapply(d3[5], temperatura=d3$temperatura, umidade=d3$umidade, f_mq6))
d3[6] <- as.data.frame(lapply(d3[6], temperatura=d3$temperatura, umidade=d3$umidade, f_mq7))
d3[7] <- as.data.frame(lapply(d3[7], temperatura=d3$temperatura, umidade=d3$umidade, f_mq8))
d3[8] <- as.data.frame(lapply(d3[8], temperatura=d3$temperatura, umidade=d3$umidade, f_mq9))
d3[9] <- as.data.frame(lapply(d3[9], temperatura=d3$temperatura, umidade=d3$umidade, f_mq135))
d3[10] <- as.data.frame(lapply(d3[10], temperatura=d3$temperatura, umidade=d3$umidade, f_tgs822))
d3[11] <- as.data.frame(lapply(d3[11], temperatura=d3$temperatura, umidade=d3$umidade, f_tgs2600))
d3[12] <- as.data.frame(lapply(d3[12], temperatura=d3$temperatura, umidade=d3$umidade, f_tgs2602))
d3[13] <- as.data.frame(lapply(d3[13], temperatura=d3$temperatura, umidade=d3$umidade, f_tgs2603))

d4[1] <- as.data.frame(lapply(d4[1], temperatura=d4$temperatura, umidade=d4$umidade, f_mq2))
d4[2] <- as.data.frame(lapply(d4[2], temperatura=d4$temperatura, umidade=d4$umidade, f_mq3))
d4[3] <- as.data.frame(lapply(d4[3], temperatura=d4$temperatura, umidade=d4$umidade, f_mq4))
d4[4] <- as.data.frame(lapply(d4[4], temperatura=d4$temperatura, umidade=d4$umidade, f_mq5))
d4[5] <- as.data.frame(lapply(d4[5], temperatura=d4$temperatura, umidade=d4$umidade, f_mq6))
d4[6] <- as.data.frame(lapply(d4[6], temperatura=d4$temperatura, umidade=d4$umidade, f_mq7))
d4[7] <- as.data.frame(lapply(d4[7], temperatura=d4$temperatura, umidade=d4$umidade, f_mq8))
d4[8] <- as.data.frame(lapply(d4[8], temperatura=d4$temperatura, umidade=d4$umidade, f_mq9))
d4[9] <- as.data.frame(lapply(d4[9], temperatura=d4$temperatura, umidade=d4$umidade, f_mq135))
d4[10] <- as.data.frame(lapply(d4[10], temperatura=d4$temperatura, umidade=d4$umidade, f_tgs822))
d4[11] <- as.data.frame(lapply(d4[11], temperatura=d4$temperatura, umidade=d4$umidade, f_tgs2600))
d4[12] <- as.data.frame(lapply(d4[12], temperatura=d4$temperatura, umidade=d4$umidade, f_tgs2602))
d4[13] <- as.data.frame(lapply(d4[13], temperatura=d4$temperatura, umidade=d4$umidade, f_tgs2603))

d5[1] <- as.data.frame(lapply(d5[1], temperatura=d5$temperatura, umidade=d5$umidade, f_mq2))
d5[2] <- as.data.frame(lapply(d5[2], temperatura=d5$temperatura, umidade=d5$umidade, f_mq3))
d5[3] <- as.data.frame(lapply(d5[3], temperatura=d5$temperatura, umidade=d5$umidade, f_mq4))
d5[4] <- as.data.frame(lapply(d5[4], temperatura=d5$temperatura, umidade=d5$umidade, f_mq5))
d5[5] <- as.data.frame(lapply(d5[5], temperatura=d5$temperatura, umidade=d5$umidade, f_mq6))
d5[6] <- as.data.frame(lapply(d5[6], temperatura=d5$temperatura, umidade=d5$umidade, f_mq7))
d5[7] <- as.data.frame(lapply(d5[7], temperatura=d5$temperatura, umidade=d5$umidade, f_mq8))
d5[8] <- as.data.frame(lapply(d5[8], temperatura=d5$temperatura, umidade=d5$umidade, f_mq9))
d5[9] <- as.data.frame(lapply(d5[9], temperatura=d5$temperatura, umidade=d5$umidade, f_mq135))
d5[10] <- as.data.frame(lapply(d5[10], temperatura=d5$temperatura, umidade=d5$umidade, f_tgs822))
d5[11] <- as.data.frame(lapply(d5[11], temperatura=d5$temperatura, umidade=d5$umidade, f_tgs2600))
d5[12] <- as.data.frame(lapply(d5[12], temperatura=d5$temperatura, umidade=d5$umidade, f_tgs2602))
d5[13] <- as.data.frame(lapply(d5[13], temperatura=d5$temperatura, umidade=d5$umidade, f_tgs2603))

d6[1] <- as.data.frame(lapply(d6[1], temperatura=d6$temperatura, umidade=d6$umidade, f_mq2))
d6[2] <- as.data.frame(lapply(d6[2], temperatura=d6$temperatura, umidade=d6$umidade, f_mq3))
d6[3] <- as.data.frame(lapply(d6[3], temperatura=d6$temperatura, umidade=d6$umidade, f_mq4))
d6[4] <- as.data.frame(lapply(d6[4], temperatura=d6$temperatura, umidade=d6$umidade, f_mq5))
d6[5] <- as.data.frame(lapply(d6[5], temperatura=d6$temperatura, umidade=d6$umidade, f_mq6))
d6[6] <- as.data.frame(lapply(d6[6], temperatura=d6$temperatura, umidade=d6$umidade, f_mq7))
d6[7] <- as.data.frame(lapply(d6[7], temperatura=d6$temperatura, umidade=d6$umidade, f_mq8))
d6[8] <- as.data.frame(lapply(d6[8], temperatura=d6$temperatura, umidade=d6$umidade, f_mq9))
d6[9] <- as.data.frame(lapply(d6[9], temperatura=d6$temperatura, umidade=d6$umidade, f_mq135))
d6[10] <- as.data.frame(lapply(d6[10], temperatura=d6$temperatura, umidade=d6$umidade, f_tgs822))
d6[11] <- as.data.frame(lapply(d6[11], temperatura=d6$temperatura, umidade=d6$umidade, f_tgs2600))
d6[12] <- as.data.frame(lapply(d6[12], temperatura=d6$temperatura, umidade=d6$umidade, f_tgs2602))
d6[13] <- as.data.frame(lapply(d6[13], temperatura=d6$temperatura, umidade=d6$umidade, f_tgs2603))

d7[1] <- as.data.frame(lapply(d7[1], temperatura=d7$temperatura, umidade=d7$umidade, f_mq2))
d7[2] <- as.data.frame(lapply(d7[2], temperatura=d7$temperatura, umidade=d7$umidade, f_mq3))
d7[3] <- as.data.frame(lapply(d7[3], temperatura=d7$temperatura, umidade=d7$umidade, f_mq4))
d7[4] <- as.data.frame(lapply(d7[4], temperatura=d7$temperatura, umidade=d7$umidade, f_mq5))
d7[5] <- as.data.frame(lapply(d7[5], temperatura=d7$temperatura, umidade=d7$umidade, f_mq6))
d7[6] <- as.data.frame(lapply(d7[6], temperatura=d7$temperatura, umidade=d7$umidade, f_mq7))
d7[7] <- as.data.frame(lapply(d7[7], temperatura=d7$temperatura, umidade=d7$umidade, f_mq8))
d7[8] <- as.data.frame(lapply(d7[8], temperatura=d7$temperatura, umidade=d7$umidade, f_mq9))
d7[9] <- as.data.frame(lapply(d7[9], temperatura=d7$temperatura, umidade=d7$umidade, f_mq135))
d7[10] <- as.data.frame(lapply(d7[10], temperatura=d7$temperatura, umidade=d7$umidade, f_tgs822))
d7[11] <- as.data.frame(lapply(d7[11], temperatura=d7$temperatura, umidade=d7$umidade, f_tgs2600))
d7[12] <- as.data.frame(lapply(d7[12], temperatura=d7$temperatura, umidade=d7$umidade, f_tgs2602))
d7[13] <- as.data.frame(lapply(d7[13], temperatura=d7$temperatura, umidade=d7$umidade, f_tgs2603))

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

data <- rbind(d1_1, d1_2, d1_3, d1_4, d1_5, d1_6, d1_7, d1_8, d1_9, d1_10, d1_11, d1_12, d1_13, d1_14, d1_15,
				d2_1, d2_2, d2_3, d2_4, d2_5, d2_6, d2_7, d2_8, d2_9, d2_10, d2_11, d2_12, d2_13, d2_14, d2_15,
				d3_1, d3_2, d3_3, d3_4, d3_5, d3_6, d3_7, d3_8, d3_9, d3_10, d3_11, d3_12, d3_13, d3_14, d3_15,
				d4_1, d4_2, d4_3, d4_4, d4_5, d4_6, d4_7, d4_8, d4_9, d4_10, d4_11, d4_12, d4_13, d4_14, d4_15,
                d5_1, d5_2, d5_3, d5_4, d5_5, d5_6, d5_7, d5_8, d5_9, d5_10, d5_11, d5_12, d5_13, d5_14, d5_15,
                d6_1, d6_2, d6_3, d6_4, d6_5, d6_6, d6_7, d6_8, d6_9, d6_10, d6_11, d6_12, d6_13, d6_14, d6_15,
                d7_1, d7_2, d7_3, d7_4, d7_5, d7_6, d7_7, d7_8, d7_9, d7_10, d7_11, d7_12, d7_13, d7_14, d7_15)

data$classe[data$classe == 'A1'] <- 1
data$classe[data$classe == 'A1.5'] <- 1.5
data$classe[data$classe == 'A2'] <- 2
data$classe[data$classe == 'A2.5'] <- 2.5
data$classe[data$classe == 'A3'] <- 3
data$classe[data$classe == 'A3.5'] <- 3.5
data$classe[data$classe == 'A3.7'] <- 3.7
data$classe[data$classe == 'A3.9'] <- 3.9
data$classe[data$classe == 'A4.1'] <- 4.1
data$classe[data$classe == 'A4.3'] <- 4.3
data$classe[data$classe == 'A4.5'] <- 4.5
data$classe[data$classe == 'A5'] <- 5
data$classe[data$classe == 'A5.5'] <- 5.5
data$classe[data$classe == 'A6'] <- 6
data$classe[data$classe == 'A8'] <- 8

data$classe <- as.numeric(data$classe)

rm(d1, d2, d3, d4, d5, d6, d7)
rm(d1_1, d1_2, d1_3, d1_4, d1_5, d1_6, d1_7, d1_8, d1_9, d1_10, d1_11, d1_12, d1_13, d1_14, d1_15)
rm(d2_1, d2_2, d2_3, d2_4, d2_5, d2_6, d2_7, d2_8, d2_9, d2_10, d2_11, d2_12, d2_13, d2_14, d2_15)
rm(d3_1, d3_2, d3_3, d3_4, d3_5, d3_6, d3_7, d3_8, d3_9, d3_10, d3_11, d3_12, d3_13, d3_14, d3_15)
rm(d4_1, d4_2, d4_3, d4_4, d4_5, d4_6, d4_7, d4_8, d4_9, d4_10, d4_11, d4_12, d4_13, d4_14, d4_15)
rm(d5_1, d5_2, d5_3, d5_4, d5_5, d5_6, d5_7, d5_8, d5_9, d5_10, d5_11, d5_12, d5_13, d5_14, d5_15)
rm(d6_1, d6_2, d6_3, d6_4, d6_5, d6_6, d6_7, d6_8, d6_9, d6_10, d6_11, d6_12, d6_13, d6_14, d6_15)
rm(d7_1, d7_2, d7_3, d7_4, d7_5, d7_6, d7_7, d7_8, d7_9, d7_10, d7_11, d7_12, d7_13, d7_14, d7_15)

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 14:20:00' and g.data_hora < '2018-12-17 14:30:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b4_1 <- df[2:17]
b4_1[1:15] <- as.data.frame(lapply(b4_1[1:15], classe = b4_1$classe, mediaMovel))
b4_1$classe[b4_1$classe == 'B4'] <- 4
b4_1$classe <- as.numeric(b4_1$classe)
b4_1[1] <- as.data.frame(lapply(b4_1[1], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_mq2))
b4_1[2] <- as.data.frame(lapply(b4_1[2], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_mq3))
b4_1[3] <- as.data.frame(lapply(b4_1[3], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_mq4))
b4_1[4] <- as.data.frame(lapply(b4_1[4], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_mq5))
b4_1[5] <- as.data.frame(lapply(b4_1[5], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_mq6))
b4_1[6] <- as.data.frame(lapply(b4_1[6], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_mq7))
b4_1[7] <- as.data.frame(lapply(b4_1[7], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_mq8))
b4_1[8] <- as.data.frame(lapply(b4_1[8], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_mq9))
b4_1[9] <- as.data.frame(lapply(b4_1[9], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_mq135))
b4_1[10] <- as.data.frame(lapply(b4_1[10], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_tgs822))
b4_1[11] <- as.data.frame(lapply(b4_1[11], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_tgs2600))
b4_1[12] <- as.data.frame(lapply(b4_1[12], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_tgs2602))
b4_1[13] <- as.data.frame(lapply(b4_1[13], temperatura=b4_1$temperatura, umidade=b4_1$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 14:30:00' and g.data_hora < '2018-12-17 14:40:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b4_2 <- df[2:17]
b4_2[1:15] <- as.data.frame(lapply(b4_2[1:15], classe = b4_2$classe, mediaMovel))
b4_2$classe[b4_2$classe == 'B4'] <- 4
b4_2$classe <- as.numeric(b4_2$classe)
b4_2[1] <- as.data.frame(lapply(b4_2[1], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_mq2))
b4_2[2] <- as.data.frame(lapply(b4_2[2], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_mq3))
b4_2[3] <- as.data.frame(lapply(b4_2[3], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_mq4))
b4_2[4] <- as.data.frame(lapply(b4_2[4], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_mq5))
b4_2[5] <- as.data.frame(lapply(b4_2[5], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_mq6))
b4_2[6] <- as.data.frame(lapply(b4_2[6], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_mq7))
b4_2[7] <- as.data.frame(lapply(b4_2[7], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_mq8))
b4_2[8] <- as.data.frame(lapply(b4_2[8], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_mq9))
b4_2[9] <- as.data.frame(lapply(b4_2[9], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_mq135))
b4_2[10] <- as.data.frame(lapply(b4_2[10], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_tgs822))
b4_2[11] <- as.data.frame(lapply(b4_2[11], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_tgs2600))
b4_2[12] <- as.data.frame(lapply(b4_2[12], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_tgs2602))
b4_2[13] <- as.data.frame(lapply(b4_2[13], temperatura=b4_2$temperatura, umidade=b4_2$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 14:40:00' and g.data_hora < '2018-12-17 14:50:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b4_3 <- df[2:17]
b4_3[1:15] <- as.data.frame(lapply(b4_3[1:15], classe = b4_3$classe, mediaMovel))
b4_3$classe[b4_3$classe == 'B4'] <- 4
b4_3$classe <- as.numeric(b4_3$classe)
b4_3[1] <- as.data.frame(lapply(b4_3[1], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_mq2))
b4_3[2] <- as.data.frame(lapply(b4_3[2], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_mq3))
b4_3[3] <- as.data.frame(lapply(b4_3[3], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_mq4))
b4_3[4] <- as.data.frame(lapply(b4_3[4], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_mq5))
b4_3[5] <- as.data.frame(lapply(b4_3[5], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_mq6))
b4_3[6] <- as.data.frame(lapply(b4_3[6], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_mq7))
b4_3[7] <- as.data.frame(lapply(b4_3[7], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_mq8))
b4_3[8] <- as.data.frame(lapply(b4_3[8], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_mq9))
b4_3[9] <- as.data.frame(lapply(b4_3[9], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_mq135))
b4_3[10] <- as.data.frame(lapply(b4_3[10], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_tgs822))
b4_3[11] <- as.data.frame(lapply(b4_3[11], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_tgs2600))
b4_3[12] <- as.data.frame(lapply(b4_3[12], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_tgs2602))
b4_3[13] <- as.data.frame(lapply(b4_3[13], temperatura=b4_3$temperatura, umidade=b4_3$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 14:50:00' and g.data_hora < '2018-12-17 15:00:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b4m_1 <- df[2:17]
b4m_1[1:15] <- as.data.frame(lapply(b4m_1[1:15], classe = b4m_1$classe, mediaMovel))
b4m_1$classe[b4m_1$classe == 'B4M'] <- 4
b4m_1$classe <- as.numeric(b4m_1$classe)
b4m_1[1] <- as.data.frame(lapply(b4m_1[1], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_mq2))
b4m_1[2] <- as.data.frame(lapply(b4m_1[2], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_mq3))
b4m_1[3] <- as.data.frame(lapply(b4m_1[3], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_mq4))
b4m_1[4] <- as.data.frame(lapply(b4m_1[4], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_mq5))
b4m_1[5] <- as.data.frame(lapply(b4m_1[5], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_mq6))
b4m_1[6] <- as.data.frame(lapply(b4m_1[6], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_mq7))
b4m_1[7] <- as.data.frame(lapply(b4m_1[7], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_mq8))
b4m_1[8] <- as.data.frame(lapply(b4m_1[8], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_mq9))
b4m_1[9] <- as.data.frame(lapply(b4m_1[9], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_mq135))
b4m_1[10] <- as.data.frame(lapply(b4m_1[10], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_tgs822))
b4m_1[11] <- as.data.frame(lapply(b4m_1[11], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_tgs2600))
b4m_1[12] <- as.data.frame(lapply(b4m_1[12], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_tgs2602))
b4m_1[13] <- as.data.frame(lapply(b4m_1[13], temperatura=b4m_1$temperatura, umidade=b4m_1$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 15:02:00' and g.data_hora < '2018-12-17 15:10:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b4m_2 <- df[2:17]
b4m_2[1:15] <- as.data.frame(lapply(b4m_2[1:15], classe = b4m_2$classe, mediaMovel))
b4m_2$classe[b4m_2$classe == 'B4M'] <- 4
b4m_2$classe <- as.numeric(b4m_2$classe)
b4m_2[1] <- as.data.frame(lapply(b4m_2[1], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_mq2))
b4m_2[2] <- as.data.frame(lapply(b4m_2[2], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_mq3))
b4m_2[3] <- as.data.frame(lapply(b4m_2[3], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_mq4))
b4m_2[4] <- as.data.frame(lapply(b4m_2[4], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_mq5))
b4m_2[5] <- as.data.frame(lapply(b4m_2[5], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_mq6))
b4m_2[6] <- as.data.frame(lapply(b4m_2[6], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_mq7))
b4m_2[7] <- as.data.frame(lapply(b4m_2[7], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_mq8))
b4m_2[8] <- as.data.frame(lapply(b4m_2[8], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_mq9))
b4m_2[9] <- as.data.frame(lapply(b4m_2[9], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_mq135))
b4m_2[10] <- as.data.frame(lapply(b4m_2[10], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_tgs822))
b4m_2[11] <- as.data.frame(lapply(b4m_2[11], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_tgs2600))
b4m_2[12] <- as.data.frame(lapply(b4m_2[12], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_tgs2602))
b4m_2[13] <- as.data.frame(lapply(b4m_2[13], temperatura=b4m_2$temperatura, umidade=b4m_2$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 15:12:00' and g.data_hora < '2018-12-17 15:20:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b4m_3 <- df[2:17]
b4m_3[1:15] <- as.data.frame(lapply(b4m_3[1:15], classe = b4m_3$classe, mediaMovel))
b4m_3$classe[b4m_3$classe == 'B4M'] <- 4
b4m_3$classe <- as.numeric(b4m_3$classe)
b4m_3[1] <- as.data.frame(lapply(b4m_3[1], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_mq2))
b4m_3[2] <- as.data.frame(lapply(b4m_3[2], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_mq3))
b4m_3[3] <- as.data.frame(lapply(b4m_3[3], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_mq4))
b4m_3[4] <- as.data.frame(lapply(b4m_3[4], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_mq5))
b4m_3[5] <- as.data.frame(lapply(b4m_3[5], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_mq6))
b4m_3[6] <- as.data.frame(lapply(b4m_3[6], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_mq7))
b4m_3[7] <- as.data.frame(lapply(b4m_3[7], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_mq8))
b4m_3[8] <- as.data.frame(lapply(b4m_3[8], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_mq9))
b4m_3[9] <- as.data.frame(lapply(b4m_3[9], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_mq135))
b4m_3[10] <- as.data.frame(lapply(b4m_3[10], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_tgs822))
b4m_3[11] <- as.data.frame(lapply(b4m_3[11], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_tgs2600))
b4m_3[12] <- as.data.frame(lapply(b4m_3[12], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_tgs2602))
b4m_3[13] <- as.data.frame(lapply(b4m_3[13], temperatura=b4m_3$temperatura, umidade=b4m_3$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 15:22:00' and g.data_hora < '2018-12-17 15:32:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b45_1 <- df[2:17]
b45_1[1:15] <- as.data.frame(lapply(b45_1[1:15], classe = b45_1$classe, mediaMovel))
b45_1$classe[b45_1$classe == 'B4.5'] <- 4.5
b45_1$classe <- as.numeric(b45_1$classe)
b45_1[1] <- as.data.frame(lapply(b45_1[1], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_mq2))
b45_1[2] <- as.data.frame(lapply(b45_1[2], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_mq3))
b45_1[3] <- as.data.frame(lapply(b45_1[3], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_mq4))
b45_1[4] <- as.data.frame(lapply(b45_1[4], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_mq5))
b45_1[5] <- as.data.frame(lapply(b45_1[5], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_mq6))
b45_1[6] <- as.data.frame(lapply(b45_1[6], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_mq7))
b45_1[7] <- as.data.frame(lapply(b45_1[7], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_mq8))
b45_1[8] <- as.data.frame(lapply(b45_1[8], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_mq9))
b45_1[9] <- as.data.frame(lapply(b45_1[9], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_mq135))
b45_1[10] <- as.data.frame(lapply(b45_1[10], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_tgs822))
b45_1[11] <- as.data.frame(lapply(b45_1[11], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_tgs2600))
b45_1[12] <- as.data.frame(lapply(b45_1[12], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_tgs2602))
b45_1[13] <- as.data.frame(lapply(b45_1[13], temperatura=b45_1$temperatura, umidade=b45_1$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 15:32:00' and g.data_hora < '2018-12-17 15:42:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b45_2 <- df[2:17]
b45_2[1:15] <- as.data.frame(lapply(b45_2[1:15], classe = b45_2$classe, mediaMovel))
b45_2$classe[b45_2$classe == 'B4.5'] <- 4.5
b45_2$classe <- as.numeric(b45_2$classe)
b45_2[1] <- as.data.frame(lapply(b45_2[1], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_mq2))
b45_2[2] <- as.data.frame(lapply(b45_2[2], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_mq3))
b45_2[3] <- as.data.frame(lapply(b45_2[3], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_mq4))
b45_2[4] <- as.data.frame(lapply(b45_2[4], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_mq5))
b45_2[5] <- as.data.frame(lapply(b45_2[5], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_mq6))
b45_2[6] <- as.data.frame(lapply(b45_2[6], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_mq7))
b45_2[7] <- as.data.frame(lapply(b45_2[7], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_mq8))
b45_2[8] <- as.data.frame(lapply(b45_2[8], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_mq9))
b45_2[9] <- as.data.frame(lapply(b45_2[9], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_mq135))
b45_2[10] <- as.data.frame(lapply(b45_2[10], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_tgs822))
b45_2[11] <- as.data.frame(lapply(b45_2[11], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_tgs2600))
b45_2[12] <- as.data.frame(lapply(b45_2[12], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_tgs2602))
b45_2[13] <- as.data.frame(lapply(b45_2[13], temperatura=b45_2$temperatura, umidade=b45_2$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 15:42:00' and g.data_hora < '2018-12-17 15:52:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b45_3 <- df[2:17]
b45_3[1:15] <- as.data.frame(lapply(b45_3[1:15], classe = b45_3$classe, mediaMovel))
b45_3$classe[b45_3$classe == 'B4.5'] <- 4.5
b45_3$classe <- as.numeric(b45_3$classe)
b45_3[1] <- as.data.frame(lapply(b45_3[1], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_mq2))
b45_3[2] <- as.data.frame(lapply(b45_3[2], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_mq3))
b45_3[3] <- as.data.frame(lapply(b45_3[3], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_mq4))
b45_3[4] <- as.data.frame(lapply(b45_3[4], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_mq5))
b45_3[5] <- as.data.frame(lapply(b45_3[5], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_mq6))
b45_3[6] <- as.data.frame(lapply(b45_3[6], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_mq7))
b45_3[7] <- as.data.frame(lapply(b45_3[7], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_mq8))
b45_3[8] <- as.data.frame(lapply(b45_3[8], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_mq9))
b45_3[9] <- as.data.frame(lapply(b45_3[9], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_mq135))
b45_3[10] <- as.data.frame(lapply(b45_3[10], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_tgs822))
b45_3[11] <- as.data.frame(lapply(b45_3[11], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_tgs2600))
b45_3[12] <- as.data.frame(lapply(b45_3[12], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_tgs2602))
b45_3[13] <- as.data.frame(lapply(b45_3[13], temperatura=b45_3$temperatura, umidade=b45_3$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 15:52:00' and g.data_hora < '2018-12-17 16:02:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b5_1 <- df[2:17]
b5_1[1:15] <- as.data.frame(lapply(b5_1[1:15], classe = b5_1$classe, mediaMovel))
b5_1$classe[b5_1$classe == 'B5'] <- 5
b5_1$classe <- as.numeric(b5_1$classe)
b5_1[1] <- as.data.frame(lapply(b5_1[1], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_mq2))
b5_1[2] <- as.data.frame(lapply(b5_1[2], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_mq3))
b5_1[3] <- as.data.frame(lapply(b5_1[3], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_mq4))
b5_1[4] <- as.data.frame(lapply(b5_1[4], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_mq5))
b5_1[5] <- as.data.frame(lapply(b5_1[5], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_mq6))
b5_1[6] <- as.data.frame(lapply(b5_1[6], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_mq7))
b5_1[7] <- as.data.frame(lapply(b5_1[7], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_mq8))
b5_1[8] <- as.data.frame(lapply(b5_1[8], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_mq9))
b5_1[9] <- as.data.frame(lapply(b5_1[9], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_mq135))
b5_1[10] <- as.data.frame(lapply(b5_1[10], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_tgs822))
b5_1[11] <- as.data.frame(lapply(b5_1[11], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_tgs2600))
b5_1[12] <- as.data.frame(lapply(b5_1[12], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_tgs2602))
b5_1[13] <- as.data.frame(lapply(b5_1[13], temperatura=b5_1$temperatura, umidade=b5_1$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 16:06:00' and g.data_hora < '2018-12-17 16:13:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b5_2 <- df[2:17]
b5_2[1:15] <- as.data.frame(lapply(b5_2[1:15], classe = b5_2$classe, mediaMovel))
b5_2$classe[b5_2$classe == 'B5'] <- 5
b5_2$classe <- as.numeric(b5_2$classe)
b5_2[1] <- as.data.frame(lapply(b5_2[1], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_mq2))
b5_2[2] <- as.data.frame(lapply(b5_2[2], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_mq3))
b5_2[3] <- as.data.frame(lapply(b5_2[3], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_mq4))
b5_2[4] <- as.data.frame(lapply(b5_2[4], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_mq5))
b5_2[5] <- as.data.frame(lapply(b5_2[5], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_mq6))
b5_2[6] <- as.data.frame(lapply(b5_2[6], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_mq7))
b5_2[7] <- as.data.frame(lapply(b5_2[7], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_mq8))
b5_2[8] <- as.data.frame(lapply(b5_2[8], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_mq9))
b5_2[9] <- as.data.frame(lapply(b5_2[9], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_mq135))
b5_2[10] <- as.data.frame(lapply(b5_2[10], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_tgs822))
b5_2[11] <- as.data.frame(lapply(b5_2[11], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_tgs2600))
b5_2[12] <- as.data.frame(lapply(b5_2[12], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_tgs2602))
b5_2[13] <- as.data.frame(lapply(b5_2[13], temperatura=b5_2$temperatura, umidade=b5_2$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 16:13:00' and g.data_hora < '2018-12-17 16:23:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b5_3 <- df[2:17]
b5_3[1:15] <- as.data.frame(lapply(b5_3[1:15], classe = b5_3$classe, mediaMovel))
b5_3$classe[b5_3$classe == 'B5'] <- 5
b5_3$classe <- as.numeric(b5_3$classe)
b5_3[1] <- as.data.frame(lapply(b5_3[1], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_mq2))
b5_3[2] <- as.data.frame(lapply(b5_3[2], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_mq3))
b5_3[3] <- as.data.frame(lapply(b5_3[3], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_mq4))
b5_3[4] <- as.data.frame(lapply(b5_3[4], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_mq5))
b5_3[5] <- as.data.frame(lapply(b5_3[5], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_mq6))
b5_3[6] <- as.data.frame(lapply(b5_3[6], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_mq7))
b5_3[7] <- as.data.frame(lapply(b5_3[7], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_mq8))
b5_3[8] <- as.data.frame(lapply(b5_3[8], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_mq9))
b5_3[9] <- as.data.frame(lapply(b5_3[9], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_mq135))
b5_3[10] <- as.data.frame(lapply(b5_3[10], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_tgs822))
b5_3[11] <- as.data.frame(lapply(b5_3[11], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_tgs2600))
b5_3[12] <- as.data.frame(lapply(b5_3[12], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_tgs2602))
b5_3[13] <- as.data.frame(lapply(b5_3[13], temperatura=b5_3$temperatura, umidade=b5_3$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 16:24:00' and g.data_hora < '2018-12-17 16:33:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b46_1 <- df[2:17]
b46_1[1:15] <- as.data.frame(lapply(b46_1[1:15], classe = b46_1$classe, mediaMovel))
b46_1$classe[b46_1$classe == 'B4.6'] <- 4.6
b46_1$classe <- as.numeric(b46_1$classe)
b46_1[1] <- as.data.frame(lapply(b46_1[1], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_mq2))
b46_1[2] <- as.data.frame(lapply(b46_1[2], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_mq3))
b46_1[3] <- as.data.frame(lapply(b46_1[3], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_mq4))
b46_1[4] <- as.data.frame(lapply(b46_1[4], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_mq5))
b46_1[5] <- as.data.frame(lapply(b46_1[5], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_mq6))
b46_1[6] <- as.data.frame(lapply(b46_1[6], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_mq7))
b46_1[7] <- as.data.frame(lapply(b46_1[7], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_mq8))
b46_1[8] <- as.data.frame(lapply(b46_1[8], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_mq9))
b46_1[9] <- as.data.frame(lapply(b46_1[9], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_mq135))
b46_1[10] <- as.data.frame(lapply(b46_1[10], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_tgs822))
b46_1[11] <- as.data.frame(lapply(b46_1[11], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_tgs2600))
b46_1[12] <- as.data.frame(lapply(b46_1[12], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_tgs2602))
b46_1[13] <- as.data.frame(lapply(b46_1[13], temperatura=b46_1$temperatura, umidade=b46_1$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 16:35:00' and g.data_hora < '2018-12-17 16:43:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b46_2 <- df[2:17]
b46_2[1:15] <- as.data.frame(lapply(b46_2[1:15], classe = b46_2$classe, mediaMovel))
b46_2$classe[b46_2$classe == 'B4.6'] <- 4.6
b46_2$classe <- as.numeric(b46_2$classe)
b46_2[1] <- as.data.frame(lapply(b46_2[1], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_mq2))
b46_2[2] <- as.data.frame(lapply(b46_2[2], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_mq3))
b46_2[3] <- as.data.frame(lapply(b46_2[3], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_mq4))
b46_2[4] <- as.data.frame(lapply(b46_2[4], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_mq5))
b46_2[5] <- as.data.frame(lapply(b46_2[5], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_mq6))
b46_2[6] <- as.data.frame(lapply(b46_2[6], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_mq7))
b46_2[7] <- as.data.frame(lapply(b46_2[7], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_mq8))
b46_2[8] <- as.data.frame(lapply(b46_2[8], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_mq9))
b46_2[9] <- as.data.frame(lapply(b46_2[9], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_mq135))
b46_2[10] <- as.data.frame(lapply(b46_2[10], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_tgs822))
b46_2[11] <- as.data.frame(lapply(b46_2[11], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_tgs2600))
b46_2[12] <- as.data.frame(lapply(b46_2[12], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_tgs2602))
b46_2[13] <- as.data.frame(lapply(b46_2[13], temperatura=b46_2$temperatura, umidade=b46_2$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 16:45:00' and g.data_hora < '2018-12-17 16:55:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b46_3 <- df[2:17]
b46_3[1:15] <- as.data.frame(lapply(b46_3[1:15], classe = b46_3$classe, mediaMovel))
b46_3$classe[b46_3$classe == 'B4.6'] <- 4.6
b46_3$classe <- as.numeric(b46_3$classe)
b46_3[1] <- as.data.frame(lapply(b46_3[1], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_mq2))
b46_3[2] <- as.data.frame(lapply(b46_3[2], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_mq3))
b46_3[3] <- as.data.frame(lapply(b46_3[3], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_mq4))
b46_3[4] <- as.data.frame(lapply(b46_3[4], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_mq5))
b46_3[5] <- as.data.frame(lapply(b46_3[5], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_mq6))
b46_3[6] <- as.data.frame(lapply(b46_3[6], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_mq7))
b46_3[7] <- as.data.frame(lapply(b46_3[7], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_mq8))
b46_3[8] <- as.data.frame(lapply(b46_3[8], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_mq9))
b46_3[9] <- as.data.frame(lapply(b46_3[9], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_mq135))
b46_3[10] <- as.data.frame(lapply(b46_3[10], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_tgs822))
b46_3[11] <- as.data.frame(lapply(b46_3[11], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_tgs2600))
b46_3[12] <- as.data.frame(lapply(b46_3[12], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_tgs2602))
b46_3[13] <- as.data.frame(lapply(b46_3[13], temperatura=b46_3$temperatura, umidade=b46_3$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 16:55:00' and g.data_hora < '2018-12-17 17:05:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b54_1 <- df[2:17]
b54_1[1:15] <- as.data.frame(lapply(b54_1[1:15], classe = b54_1$classe, mediaMovel))
b54_1$classe[b54_1$classe == 'B5.4'] <- 5.4
b54_1$classe <- as.numeric(b54_1$classe)
b54_1[1] <- as.data.frame(lapply(b54_1[1], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_mq2))
b54_1[2] <- as.data.frame(lapply(b54_1[2], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_mq3))
b54_1[3] <- as.data.frame(lapply(b54_1[3], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_mq4))
b54_1[4] <- as.data.frame(lapply(b54_1[4], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_mq5))
b54_1[5] <- as.data.frame(lapply(b54_1[5], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_mq6))
b54_1[6] <- as.data.frame(lapply(b54_1[6], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_mq7))
b54_1[7] <- as.data.frame(lapply(b54_1[7], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_mq8))
b54_1[8] <- as.data.frame(lapply(b54_1[8], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_mq9))
b54_1[9] <- as.data.frame(lapply(b54_1[9], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_mq135))
b54_1[10] <- as.data.frame(lapply(b54_1[10], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_tgs822))
b54_1[11] <- as.data.frame(lapply(b54_1[11], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_tgs2600))
b54_1[12] <- as.data.frame(lapply(b54_1[12], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_tgs2602))
b54_1[13] <- as.data.frame(lapply(b54_1[13], temperatura=b54_1$temperatura, umidade=b54_1$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 17:05:00' and g.data_hora < '2018-12-17 17:15:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b54_2 <- df[2:17]
b54_2[1:15] <- as.data.frame(lapply(b54_2[1:15], classe = b54_2$classe, mediaMovel))
b54_2$classe[b54_2$classe == 'B5.4'] <- 5.4
b54_2$classe <- as.numeric(b54_2$classe)
b54_2[1] <- as.data.frame(lapply(b54_2[1], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_mq2))
b54_2[2] <- as.data.frame(lapply(b54_2[2], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_mq3))
b54_2[3] <- as.data.frame(lapply(b54_2[3], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_mq4))
b54_2[4] <- as.data.frame(lapply(b54_2[4], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_mq5))
b54_2[5] <- as.data.frame(lapply(b54_2[5], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_mq6))
b54_2[6] <- as.data.frame(lapply(b54_2[6], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_mq7))
b54_2[7] <- as.data.frame(lapply(b54_2[7], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_mq8))
b54_2[8] <- as.data.frame(lapply(b54_2[8], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_mq9))
b54_2[9] <- as.data.frame(lapply(b54_2[9], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_mq135))
b54_2[10] <- as.data.frame(lapply(b54_2[10], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_tgs822))
b54_2[11] <- as.data.frame(lapply(b54_2[11], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_tgs2600))
b54_2[12] <- as.data.frame(lapply(b54_2[12], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_tgs2602))
b54_2[13] <- as.data.frame(lapply(b54_2[13], temperatura=b54_2$temperatura, umidade=b54_2$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 17:15:00' and g.data_hora < '2018-12-17 17:25:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b54_3 <- df[2:17]
b54_3[1:15] <- as.data.frame(lapply(b54_3[1:15], classe = b54_3$classe, mediaMovel))
b54_3$classe[b54_3$classe == 'B5.4'] <- 5.4
b54_3$classe <- as.numeric(b54_3$classe)
b54_3[1] <- as.data.frame(lapply(b54_3[1], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_mq2))
b54_3[2] <- as.data.frame(lapply(b54_3[2], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_mq3))
b54_3[3] <- as.data.frame(lapply(b54_3[3], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_mq4))
b54_3[4] <- as.data.frame(lapply(b54_3[4], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_mq5))
b54_3[5] <- as.data.frame(lapply(b54_3[5], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_mq6))
b54_3[6] <- as.data.frame(lapply(b54_3[6], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_mq7))
b54_3[7] <- as.data.frame(lapply(b54_3[7], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_mq8))
b54_3[8] <- as.data.frame(lapply(b54_3[8], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_mq9))
b54_3[9] <- as.data.frame(lapply(b54_3[9], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_mq135))
b54_3[10] <- as.data.frame(lapply(b54_3[10], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_tgs822))
b54_3[11] <- as.data.frame(lapply(b54_3[11], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_tgs2600))
b54_3[12] <- as.data.frame(lapply(b54_3[12], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_tgs2602))
b54_3[13] <- as.data.frame(lapply(b54_3[13], temperatura=b54_3$temperatura, umidade=b54_3$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 17:28:00' and g.data_hora < '2018-12-17 17:38:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b79_1 <- df[2:17]
b79_1[1:15] <- as.data.frame(lapply(b79_1[1:15], classe = b79_1$classe, mediaMovel))
b79_1$classe[b79_1$classe == 'B7.9'] <- 7.9
b79_1$classe <- as.numeric(b79_1$classe)
b79_1[1] <- as.data.frame(lapply(b79_1[1], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_mq2))
b79_1[2] <- as.data.frame(lapply(b79_1[2], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_mq3))
b79_1[3] <- as.data.frame(lapply(b79_1[3], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_mq4))
b79_1[4] <- as.data.frame(lapply(b79_1[4], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_mq5))
b79_1[5] <- as.data.frame(lapply(b79_1[5], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_mq6))
b79_1[6] <- as.data.frame(lapply(b79_1[6], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_mq7))
b79_1[7] <- as.data.frame(lapply(b79_1[7], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_mq8))
b79_1[8] <- as.data.frame(lapply(b79_1[8], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_mq9))
b79_1[9] <- as.data.frame(lapply(b79_1[9], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_mq135))
b79_1[10] <- as.data.frame(lapply(b79_1[10], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_tgs822))
b79_1[11] <- as.data.frame(lapply(b79_1[11], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_tgs2600))
b79_1[12] <- as.data.frame(lapply(b79_1[12], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_tgs2602))
b79_1[13] <- as.data.frame(lapply(b79_1[13], temperatura=b79_1$temperatura, umidade=b79_1$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                   max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                   t.temperatura, u.umidade, g.classe from gases g 
                   join temperatura t on g.data_hora = t.data_hora
                   join umidade u on g.data_hora = u.data_hora
                   where g.data_hora >= '2018-12-17 17:38:00' and g.data_hora < '2018-12-17 17:48:00'
                   and g.classe like 'B%'
                   group by g.data_hora, g.classe, t.temperatura, u.umidade
                   order by g.data_hora")
df <- fetch(rs, n = -1)
b79_2 <- df[2:17]
b79_2[1:15] <- as.data.frame(lapply(b79_2[1:15], classe = b79_2$classe, mediaMovel))
b79_2$classe[b79_2$classe == 'B7.9'] <- 7.9
b79_2$classe <- as.numeric(b79_2$classe)
b79_2[1] <- as.data.frame(lapply(b79_2[1], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_mq2))
b79_2[2] <- as.data.frame(lapply(b79_2[2], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_mq3))
b79_2[3] <- as.data.frame(lapply(b79_2[3], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_mq4))
b79_2[4] <- as.data.frame(lapply(b79_2[4], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_mq5))
b79_2[5] <- as.data.frame(lapply(b79_2[5], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_mq6))
b79_2[6] <- as.data.frame(lapply(b79_2[6], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_mq7))
b79_2[7] <- as.data.frame(lapply(b79_2[7], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_mq8))
b79_2[8] <- as.data.frame(lapply(b79_2[8], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_mq9))
b79_2[9] <- as.data.frame(lapply(b79_2[9], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_mq135))
b79_2[10] <- as.data.frame(lapply(b79_2[10], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_tgs822))
b79_2[11] <- as.data.frame(lapply(b79_2[11], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_tgs2600))
b79_2[12] <- as.data.frame(lapply(b79_2[12], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_tgs2602))
b79_2[13] <- as.data.frame(lapply(b79_2[13], temperatura=b79_2$temperatura, umidade=b79_2$umidade, f_tgs2603))

rs <- dbSendQuery(con, "select g.data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
                   max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
                  max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
                  t.temperatura, u.umidade, g.classe from gases g 
                  join temperatura t on g.data_hora = t.data_hora
                  join umidade u on g.data_hora = u.data_hora
                  where g.data_hora >= '2018-12-17 17:48:00' and g.data_hora < '2018-12-17 17:58:00'
                  and g.classe like 'B%'
                  group by g.data_hora, g.classe, t.temperatura, u.umidade
                  order by g.data_hora")
df <- fetch(rs, n = -1)
b79_3 <- df[2:17]
b79_3[1:15] <- as.data.frame(lapply(b79_3[1:15], classe = b79_3$classe, mediaMovel))
b79_3$classe[b79_3$classe == 'B7.9'] <- 7.9
b79_3$classe <- as.numeric(b79_3$classe)
b79_3[1] <- as.data.frame(lapply(b79_3[1], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_mq2))
b79_3[2] <- as.data.frame(lapply(b79_3[2], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_mq3))
b79_3[3] <- as.data.frame(lapply(b79_3[3], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_mq4))
b79_3[4] <- as.data.frame(lapply(b79_3[4], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_mq5))
b79_3[5] <- as.data.frame(lapply(b79_3[5], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_mq6))
b79_3[6] <- as.data.frame(lapply(b79_3[6], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_mq7))
b79_3[7] <- as.data.frame(lapply(b79_3[7], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_mq8))
b79_3[8] <- as.data.frame(lapply(b79_3[8], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_mq9))
b79_3[9] <- as.data.frame(lapply(b79_3[9], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_mq135))
b79_3[10] <- as.data.frame(lapply(b79_3[10], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_tgs822))
b79_3[11] <- as.data.frame(lapply(b79_3[11], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_tgs2600))
b79_3[12] <- as.data.frame(lapply(b79_3[12], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_tgs2602))
b79_3[13] <- as.data.frame(lapply(b79_3[13], temperatura=b79_3$temperatura, umidade=b79_3$umidade, f_tgs2603))

#aux <- rbind(data, b4_1, b4_2, b4_3, b4m_1, b4m_2, b4m_3, b45_1, b45_2, b45_3,
#             b46_1, b46_2, b46_3, b5_1, b5_2, b5_3, b54_1, b54_2, b54_3,
#             b79_1, b79_2, b79_3)

#aux[,1:13] <- as.data.frame(lapply(aux[,-14], normalize))

#data <- aux[1:25090,]
#b4_1 <- aux[25091:25582,]
#b4_2 <- aux[25583:26067,]
#b4_3 <- aux[26068:26525,]
#b4m_1 <- aux[26526:26992,]
#b4m_2 <- aux[26993:27472,]
#b4m_3 <- aux[27473:27952,]
#b45_1 <- aux[27953:28424,]
#b45_2 <- aux[28425:28905,]
#b45_3 <- aux[28906:29408,]
#b46_1 <- aux[29409:29904,]
#b46_2 <- aux[29905:30377,]
#b46_3 <- aux[30378:30858,]
#b5_1 <- aux[30859:31329,]
#b5_2 <- aux[31330:31706,]
#b5_3 <- aux[31707:32210,]
#b54_1 <- aux[32211:32681,]
#b54_2 <- aux[32682:33164,]
#b54_3 <- aux[33165:33628,]
#b79_1 <- aux[33629:34109,]
#b79_2 <- aux[34110:34573,]
#b79_3 <- aux[34574:35064,]

rm(con, df, rs, mediaMovel, normalize)
rm(f_mq2, f_mq3, f_mq4, f_mq5, f_mq6, f_mq7, f_mq8, f_mq9, f_mq135, f_tgs822, f_tgs2600, f_tgs2602, f_tgs2603)

lapply(dbListConnections(PostgreSQL()), dbDisconnect)

rmse = function (y_true, y_pred) {
  out = sqrt(mean((y_true - y_pred)^2))
  out
}

data <- data[,-c(14,15)]
b4_1 <- b4_1[-(1:60),-c(14,15)]
b4_2 <- b4_2[-(1:60),-c(14,15)]
b4_3 <- b4_3[-(1:60),-c(14,15)]
b4m_1 <- b4m_1[-(1:60),-c(14,15)]
b4m_2 <- b4m_2[-(1:60),-c(14,15)]
b4m_3 <- b4m_3[-(1:60),-c(14,15)]
b45_1 <- b45_1[-(1:60),-c(14,15)]
b45_2 <- b45_2[-(1:60),-c(14,15)]
b45_3 <- b45_3[-(1:60),-c(14,15)]
b46_1 <- b46_1[-(1:60),-c(14,15)]
b46_2 <- b46_2[-(1:60),-c(14,15)]
b46_3 <- b46_3[-(1:60),-c(14,15)]
b5_1 <- b5_1[-(1:60),-c(14,15)]
b5_2 <- b5_2[-(1:60),-c(14,15)]
b5_3 <- b5_3[-(1:60),-c(14,15)]
b54_1 <- b54_1[-(1:60),-c(14,15)]
b54_2 <- b54_2[-(1:60),-c(14,15)]
b54_3 <- b54_3[-(1:60),-c(14,15)]
b79_1 <- b79_1[-(1:60),-c(14,15)]
b79_2 <- b79_2[-(1:60),-c(14,15)]
b79_3 <- b79_3[-(1:60),-c(14,15)]

fit_lm = lm(classe~., data = data)
#validation
#Randomly shuffle the data
data <- data[sample(nrow(data)),]
#Create 10 equally size folds
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
#Perform 10 fold cross validation
validation_rmse_lm <- 0
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  pr_lm = as.matrix(predict(fit_lm, newdata = testData))
  validation_rmse_lm <- validation_rmse_lm + rmse(testData$classe, pr_lm[, 1])
}
validation_rmse_lm <- validation_rmse_lm / 10
print(validation_rmse_lm)
#test
pred_lm_b4_1 = as.matrix(predict(fit_lm, newdata = b4_1))
mean(pred_lm_b4_1)
rmse(b4_1$classe, pred_lm_b4_1[, 1])
pred_lm_b4_2 = as.matrix(predict(fit_lm, newdata = b4_2))
mean(pred_lm_b4_2)
rmse(b4_2$classe, pred_lm_b4_2[, 1])
pred_lm_b4_3 = as.matrix(predict(fit_lm, newdata = b4_3))
mean(pred_lm_b4_3)
rmse(b4_3$classe, pred_lm_b4_3[, 1])
pred_lm_b4m_1 = as.matrix(predict(fit_lm, newdata = b4m_1))
mean(pred_lm_b4m_1)
rmse(b4m_1$classe, pred_lm_b4m_1[, 1])
pred_lm_b4m_2 = as.matrix(predict(fit_lm, newdata = b4m_2))
mean(pred_lm_b4m_2)
rmse(b4m_2$classe, pred_lm_b4m_2[, 1])
pred_lm_b4m_3 = as.matrix(predict(fit_lm, newdata = b4m_3))
mean(pred_lm_b4m_3)
rmse(b4m_3$classe, pred_lm_b4m_3[, 1])
pred_lm_b45_1 = as.matrix(predict(fit_lm, newdata = b45_1))
mean(pred_lm_b45_1)
rmse(b45_1$classe, pred_lm_b45_1[, 1])
pred_lm_b45_2 = as.matrix(predict(fit_lm, newdata = b45_2))
mean(pred_lm_b45_2)
rmse(b45_2$classe, pred_lm_b45_2[, 1])
pred_lm_b45_3 = as.matrix(predict(fit_lm, newdata = b45_3))
mean(pred_lm_b45_3)
rmse(b45_3$classe, pred_lm_b45_3[, 1])
pred_lm_b46_1 = as.matrix(predict(fit_lm, newdata = b46_1))
mean(pred_lm_b46_1)
rmse(b46_1$classe, pred_lm_b46_1[, 1])
pred_lm_b46_2 = as.matrix(predict(fit_lm, newdata = b46_2))
mean(pred_lm_b46_2)
rmse(b46_2$classe, pred_lm_b46_2[, 1])
pred_lm_b46_3 = as.matrix(predict(fit_lm, newdata = b46_3))
mean(pred_lm_b46_3)
rmse(b46_3$classe, pred_lm_b46_3[, 1])
pred_lm_b5_1 = as.matrix(predict(fit_lm, newdata = b5_1))
mean(pred_lm_b5_1)
rmse(b5_1$classe, pred_lm_b5_1[, 1])
pred_lm_b5_2 = as.matrix(predict(fit_lm, newdata = b5_2))
mean(pred_lm_b5_2)
rmse(b5_2$classe, pred_lm_b5_2[, 1])
pred_lm_b5_3 = as.matrix(predict(fit_lm, newdata = b5_3))
mean(pred_lm_b5_3)
rmse(b5_3$classe, pred_lm_b5_3[, 1])
pred_lm_b54_1 = as.matrix(predict(fit_lm, newdata = b54_1))
mean(pred_lm_b54_1)
rmse(b54_1$classe, pred_lm_b54_1[, 1])
pred_lm_b54_2 = as.matrix(predict(fit_lm, newdata = b54_2))
mean(pred_lm_b54_2)
rmse(b54_2$classe, pred_lm_b54_2[, 1])
pred_lm_b54_3 = as.matrix(predict(fit_lm, newdata = b54_3))
mean(pred_lm_b54_3)
rmse(b54_3$classe, pred_lm_b54_3[, 1])
pred_lm_b79_1 = as.matrix(predict(fit_lm, newdata = b79_1))
mean(pred_lm_b79_1)
rmse(b79_1$classe, pred_lm_b79_1[, 1])
pred_lm_b79_2 = as.matrix(predict(fit_lm, newdata = b79_2))
mean(pred_lm_b79_2)
rmse(b79_2$classe, pred_lm_b79_2[, 1])
pred_lm_b79_3 = as.matrix(predict(fit_lm, newdata = b79_3))
mean(pred_lm_b79_3)
rmse(b79_3$classe, pred_lm_b79_3[, 1])

library(elmNNRcpp)
fit_elm = elm_train(matrix(cbind(data$mq2, data$mq3, data$mq4, data$mq5, data$mq6, 
                                 data$mq7, data$mq8, data$mq9, data$mq135, data$tgs822, 
                                 data$tgs2600, data$tgs2602, data$tgs2603), ncol=13), 
                    matrix(data$classe), 
                    nhid = 15, actfun = 'purelin', init_weights = "uniform_negative", 
                    bias = TRUE, verbose = T)
#validation
#Randomly shuffle the data
data <- data[sample(nrow(data)),]
#Create 10 equally size folds
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
#Perform 10 fold cross validation
validation_rmse_elm <- 0
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  pr_elm = elm_predict(fit_elm, as.matrix(testData[,-14]))
  validation_rmse_elm <- validation_rmse_elm + rmse(testData$classe, pr_elm[, 1])
}
validation_rmse_elm <- validation_rmse_elm / 10
print(validation_rmse_elm)
#test
pred_elm_b4_1 = elm_predict(fit_elm, as.matrix(b4_1[,-c(14)]))
mean(pred_elm_b4_1)
rmse(b4_1$classe, pred_elm_b4_1[, 1])
pred_elm_b4_2 = elm_predict(fit_elm, as.matrix(b4_2[,-c(14)]))
mean(pred_elm_b4_2)
rmse(b4_2$classe, pred_elm_b4_2[, 1])
pred_elm_b4_3 = elm_predict(fit_elm, as.matrix(b4_3[,-c(14)]))
mean(pred_elm_b4_3)
rmse(b4_3$classe, pred_elm_b4_3[, 1])
pred_elm_b4m_1 = elm_predict(fit_elm, as.matrix(b4m_1[,-c(14)]))
mean(pred_elm_b4m_1)
rmse(b4m_1$classe, pred_elm_b4m_1[, 1])
pred_elm_b4m_2 = elm_predict(fit_elm, as.matrix(b4m_2[,-c(14)]))
mean(pred_elm_b4m_2)
rmse(b4m_2$classe, pred_elm_b4m_2[, 1])
pred_elm_b4m_3 = elm_predict(fit_elm, as.matrix(b4m_3[,-c(14)]))
mean(pred_elm_b4m_3)
rmse(b4m_3$classe, pred_elm_b4m_3[, 1])
pred_elm_b45_1 = elm_predict(fit_elm, as.matrix(b45_1[,-c(14)]))
mean(pred_elm_b45_1)
rmse(b45_1$classe, pred_elm_b45_1[, 1])
pred_elm_b45_2 = elm_predict(fit_elm, as.matrix(b45_2[,-c(14)]))
mean(pred_elm_b45_2)
rmse(b45_2$classe, pred_elm_b45_2[, 1])
pred_elm_b45_3 = elm_predict(fit_elm, as.matrix(b45_3[,-c(14)]))
mean(pred_elm_b45_3)
rmse(b45_3$classe, pred_elm_b45_3[, 1])
pred_elm_b46_1 = elm_predict(fit_elm, as.matrix(b46_1[,-c(14)]))
mean(pred_elm_b46_1)
rmse(b46_1$classe, pred_elm_b46_1[, 1])
pred_elm_b46_2 = elm_predict(fit_elm, as.matrix(b46_2[,-c(14)]))
mean(pred_elm_b46_2)
rmse(b46_2$classe, pred_elm_b46_2[, 1])
pred_elm_b46_3 = elm_predict(fit_elm, as.matrix(b46_3[,-c(14)]))
mean(pred_elm_b46_3)
rmse(b46_3$classe, pred_elm_b46_3[, 1])
pred_elm_b5_1 = elm_predict(fit_elm, as.matrix(b5_1[,-c(14)]))
mean(pred_elm_b5_1)
rmse(b5_1$classe, pred_elm_b5_1[, 1])
pred_elm_b5_2 = elm_predict(fit_elm, as.matrix(b5_2[,-c(14)]))
mean(pred_elm_b5_2)
rmse(b5_2$classe, pred_elm_b5_2[, 1])
pred_elm_b5_3 = elm_predict(fit_elm, as.matrix(b5_3[,-c(14)]))
mean(pred_elm_b5_3)
rmse(b5_3$classe, pred_elm_b5_3[, 1])
pred_elm_b54_1 = elm_predict(fit_elm, as.matrix(b54_1[,-c(14)]))
mean(pred_elm_b54_1)
rmse(b54_1$classe, pred_elm_b54_1[, 1])
pred_elm_b54_2 = elm_predict(fit_elm, as.matrix(b54_2[,-c(14)]))
mean(pred_elm_b54_2)
rmse(b54_2$classe, pred_elm_b54_2[, 1])
pred_elm_b54_3 = elm_predict(fit_elm, as.matrix(b54_3[,-c(14)]))
mean(pred_elm_b54_3)
rmse(b54_3$classe, pred_elm_b54_3[, 1])
pred_elm_b79_1 = elm_predict(fit_elm, as.matrix(b79_1[,-c(14)]))
mean(pred_elm_b79_1)
rmse(b79_1$classe, pred_elm_b79_1[, 1])
pred_elm_b79_2 = elm_predict(fit_elm, as.matrix(b79_2[,-c(14)]))
mean(pred_elm_b79_2)
rmse(b79_2$classe, pred_elm_b79_2[, 1])
pred_elm_b79_3 = elm_predict(fit_elm, as.matrix(b79_3[,-c(14)]))
mean(pred_elm_b79_3)
rmse(b79_3$classe, pred_elm_b79_3[, 1])

library(randomForest)
fit_rf <- randomForest(classe~., data=data)
#validation
#Randomly shuffle the data
data <- data[sample(nrow(data)),]
#Create 10 equally size folds
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
#Perform 10 fold cross validation
validation_rmse_rf <- 0
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  pr_rf = as.matrix(predict(fit_rf, newdata = testData))
  validation_rmse_rf <- validation_rmse_rf + rmse(testData$classe, pr_rf[, 1])
}
validation_rmse_rf <- validation_rmse_rf / 10
print(validation_rmse_rf)
#test
pred_rf_b4_1 = as.matrix(predict(fit_rf, newdata = b4_1))
mean(pred_rf_b4_1)
rmse(b4_1$classe, pred_rf_b4_1[, 1])
pred_rf_b4_2 = as.matrix(predict(fit_rf, newdata = b4_2))
mean(pred_rf_b4_2)
rmse(b4_2$classe, pred_rf_b4_2[, 1])
pred_rf_b4_3 = as.matrix(predict(fit_rf, newdata = b4_3))
mean(pred_rf_b4_3)
rmse(b4_3$classe, pred_rf_b4_3[, 1])
pred_rf_b4m_1 = as.matrix(predict(fit_rf, newdata = b4m_1))
mean(pred_rf_b4m_1)
rmse(b4m_1$classe, pred_rf_b4m_1[, 1])
pred_rf_b4m_2 = as.matrix(predict(fit_rf, newdata = b4m_2))
mean(pred_rf_b4m_2)
rmse(b4m_2$classe, pred_rf_b4m_2[, 1])
pred_rf_b4m_3 = as.matrix(predict(fit_rf, newdata = b4m_3))
mean(pred_rf_b4m_3)
rmse(b4m_3$classe, pred_rf_b4m_3[, 1])
pred_rf_b45_1 = as.matrix(predict(fit_rf, newdata = b45_1))
mean(pred_rf_b45_1)
rmse(b45_1$classe, pred_rf_b45_1[, 1])
pred_rf_b45_2 = as.matrix(predict(fit_rf, newdata = b45_2))
mean(pred_rf_b45_2)
rmse(b45_2$classe, pred_rf_b45_2[, 1])
pred_rf_b45_3 = as.matrix(predict(fit_rf, newdata = b45_3))
mean(pred_rf_b45_3)
rmse(b45_3$classe, pred_rf_b45_3[, 1])
pred_rf_b46_1 = as.matrix(predict(fit_rf, newdata = b46_1))
mean(pred_rf_b46_1)
rmse(b46_1$classe, pred_rf_b46_1[, 1])
pred_rf_b46_2 = as.matrix(predict(fit_rf, newdata = b46_2))
mean(pred_rf_b46_2)
rmse(b46_2$classe, pred_rf_b46_2[, 1])
pred_rf_b46_3 = as.matrix(predict(fit_rf, newdata = b46_3))
mean(pred_rf_b46_3)
rmse(b46_3$classe, pred_rf_b46_3[, 1])
pred_rf_b5_1 = as.matrix(predict(fit_rf, newdata = b5_1))
mean(pred_rf_b5_1)
rmse(b5_1$classe, pred_rf_b5_1[, 1])
pred_rf_b5_2 = as.matrix(predict(fit_rf, newdata = b5_2))
mean(pred_rf_b5_2)
rmse(b5_2$classe, pred_rf_b5_2[, 1])
pred_rf_b5_3 = as.matrix(predict(fit_rf, newdata = b5_3))
mean(pred_rf_b5_3)
rmse(b5_3$classe, pred_rf_b5_3[, 1])
pred_rf_b54_1 = as.matrix(predict(fit_rf, newdata = b54_1))
mean(pred_rf_b54_1)
rmse(b54_1$classe, pred_rf_b54_1[, 1])
pred_rf_b54_2 = as.matrix(predict(fit_rf, newdata = b54_2))
mean(pred_rf_b54_2)
rmse(b54_2$classe, pred_rf_b54_2[, 1])
pred_rf_b54_3 = as.matrix(predict(fit_rf, newdata = b54_3))
mean(pred_rf_b54_3)
rmse(b54_3$classe, pred_rf_b54_3[, 1])
pred_rf_b79_1 = as.matrix(predict(fit_rf, newdata = b79_1))
mean(pred_rf_b79_1)
rmse(b79_1$classe, pred_rf_b79_1[, 1])
pred_rf_b79_2 = as.matrix(predict(fit_rf, newdata = b79_2))
mean(pred_rf_b79_2)
rmse(b79_2$classe, pred_rf_b79_2[, 1])
pred_rf_b79_3 = as.matrix(predict(fit_rf, newdata = b79_3))
mean(pred_rf_b79_3)
rmse(b79_3$classe, pred_rf_b79_3[, 1])

library(party)
fit_ctree = ctree(classe~., data = data)
#validation
#Randomly shuffle the data
data <- data[sample(nrow(data)),]
#Create 10 equally size folds
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
#Perform 10 fold cross validation
validation_rmse_ctree <- 0
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  pr_ctree = as.matrix(predict(fit_ctree, newdata = testData))
  validation_rmse_ctree <- validation_rmse_ctree + rmse(testData$classe, pr_ctree[, 1])
}
validation_rmse_ctree <- validation_rmse_ctree / 10
print(validation_rmse_ctree)
#test
pred_ctree_b4_1 = as.matrix(predict(fit_ctree, newdata = b4_1))
mean(pred_ctree_b4_1)
rmse(b4_1$classe, pred_ctree_b4_1[, 1])
pred_ctree_b4_2 = as.matrix(predict(fit_ctree, newdata = b4_2))
mean(pred_ctree_b4_2)
rmse(b4_2$classe, pred_ctree_b4_2[, 1])
pred_ctree_b4_3 = as.matrix(predict(fit_ctree, newdata = b4_3))
mean(pred_ctree_b4_3)
rmse(b4_3$classe, pred_ctree_b4_3[, 1])
pred_ctree_b4m_1 = as.matrix(predict(fit_ctree, newdata = b4m_1))
mean(pred_ctree_b4m_1)
rmse(b4m_1$classe, pred_ctree_b4m_1[, 1])
pred_ctree_b4m_2 = as.matrix(predict(fit_ctree, newdata = b4m_2))
mean(pred_ctree_b4m_2)
rmse(b4m_2$classe, pred_ctree_b4m_2[, 1])
pred_ctree_b4m_3 = as.matrix(predict(fit_ctree, newdata = b4m_3))
mean(pred_ctree_b4m_3)
rmse(b4m_3$classe, pred_ctree_b4m_3[, 1])
pred_ctree_b45_1 = as.matrix(predict(fit_ctree, newdata = b45_1))
mean(pred_ctree_b45_1)
rmse(b45_1$classe, pred_ctree_b45_1[, 1])
pred_ctree_b45_2 = as.matrix(predict(fit_ctree, newdata = b45_2))
mean(pred_ctree_b45_2)
rmse(b45_2$classe, pred_ctree_b45_2[, 1])
pred_ctree_b45_3 = as.matrix(predict(fit_ctree, newdata = b45_3))
mean(pred_ctree_b45_3)
rmse(b45_3$classe, pred_ctree_b45_3[, 1])
pred_ctree_b46_1 = as.matrix(predict(fit_ctree, newdata = b46_1))
mean(pred_ctree_b46_1)
rmse(b46_1$classe, pred_ctree_b46_1[, 1])
pred_ctree_b46_2 = as.matrix(predict(fit_ctree, newdata = b46_2))
mean(pred_ctree_b46_2)
rmse(b46_2$classe, pred_ctree_b46_2[, 1])
pred_ctree_b46_3 = as.matrix(predict(fit_ctree, newdata = b46_3))
mean(pred_ctree_b46_3)
rmse(b46_3$classe, pred_ctree_b46_3[, 1])
pred_ctree_b5_1 = as.matrix(predict(fit_ctree, newdata = b5_1))
mean(pred_ctree_b5_1)
rmse(b5_1$classe, pred_ctree_b5_1[, 1])
pred_ctree_b5_2 = as.matrix(predict(fit_ctree, newdata = b5_2))
mean(pred_ctree_b5_2)
rmse(b5_2$classe, pred_ctree_b5_2[, 1])
pred_ctree_b5_3 = as.matrix(predict(fit_ctree, newdata = b5_3))
mean(pred_ctree_b5_3)
rmse(b5_3$classe, pred_ctree_b5_3[, 1])
pred_ctree_b54_1 = as.matrix(predict(fit_ctree, newdata = b54_1))
mean(pred_ctree_b54_1)
rmse(b54_1$classe, pred_ctree_b54_1[, 1])
pred_ctree_b54_2 = as.matrix(predict(fit_ctree, newdata = b54_2))
mean(pred_ctree_b54_2)
rmse(b54_2$classe, pred_ctree_b54_2[, 1])
pred_ctree_b54_3 = as.matrix(predict(fit_ctree, newdata = b54_3))
mean(pred_ctree_b54_3)
rmse(b54_3$classe, pred_ctree_b54_3[, 1])
pred_ctree_b79_1 = as.matrix(predict(fit_ctree, newdata = b79_1))
mean(pred_ctree_b79_1)
rmse(b79_1$classe, pred_ctree_b79_1[, 1])
pred_ctree_b79_2 = as.matrix(predict(fit_ctree, newdata = b79_2))
mean(pred_ctree_b79_2)
rmse(b79_2$classe, pred_ctree_b79_2[, 1])
pred_ctree_b79_3 = as.matrix(predict(fit_ctree, newdata = b79_3))
mean(pred_ctree_b79_3)
rmse(b79_3$classe, pred_ctree_b79_3[, 1])

library(rpart)
fit_rpart <- rpart(classe~., method="anova", data=data)
#validation
#Randomly shuffle the data
data <- data[sample(nrow(data)),]
#Create 10 equally size folds
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
#Perform 10 fold cross validation
validation_rmse_rpart <- 0
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  pr_rpart = as.matrix(predict(fit_rpart, newdata = testData))
  validation_rmse_rpart <- validation_rmse_rpart + rmse(testData$classe, pr_rpart[, 1])
}
validation_rmse_rpart <- validation_rmse_rpart / 10
print(validation_rmse_rpart)
#test
pred_rpart_b4_1 = as.matrix(predict(fit_rpart, newdata = b4_1))
mean(pred_rpart_b4_1)
rmse(b4_1$classe, pred_rpart_b4_1[, 1])
pred_rpart_b4_2 = as.matrix(predict(fit_rpart, newdata = b4_2))
mean(pred_rpart_b4_2)
rmse(b4_2$classe, pred_rpart_b4_2[, 1])
pred_rpart_b4_3 = as.matrix(predict(fit_rpart, newdata = b4_3))
mean(pred_rpart_b4_3)
rmse(b4_3$classe, pred_rpart_b4_3[, 1])
pred_rpart_b4m_1 = as.matrix(predict(fit_rpart, newdata = b4m_1))
mean(pred_rpart_b4m_1)
rmse(b4m_1$classe, pred_rpart_b4m_1[, 1])
pred_rpart_b4m_2 = as.matrix(predict(fit_rpart, newdata = b4m_2))
mean(pred_rpart_b4m_2)
rmse(b4m_2$classe, pred_rpart_b4m_2[, 1])
pred_rpart_b4m_3 = as.matrix(predict(fit_rpart, newdata = b4m_3))
mean(pred_rpart_b4m_3)
rmse(b4m_3$classe, pred_rpart_b4m_3[, 1])
pred_rpart_b45_1 = as.matrix(predict(fit_rpart, newdata = b45_1))
mean(pred_rpart_b45_1)
rmse(b45_1$classe, pred_rpart_b45_1[, 1])
pred_rpart_b45_2 = as.matrix(predict(fit_rpart, newdata = b45_2))
mean(pred_rpart_b45_2)
rmse(b45_2$classe, pred_rpart_b45_2[, 1])
pred_rpart_b45_3 = as.matrix(predict(fit_rpart, newdata = b45_3))
mean(pred_rpart_b45_3)
rmse(b45_3$classe, pred_rpart_b45_3[, 1])
pred_rpart_b46_1 = as.matrix(predict(fit_rpart, newdata = b46_1))
mean(pred_rpart_b46_1)
rmse(b46_1$classe, pred_rpart_b46_1[, 1])
pred_rpart_b46_2 = as.matrix(predict(fit_rpart, newdata = b46_2))
mean(pred_rpart_b46_2)
rmse(b46_2$classe, pred_rpart_b46_2[, 1])
pred_rpart_b46_3 = as.matrix(predict(fit_rpart, newdata = b46_3))
mean(pred_rpart_b46_3)
rmse(b46_3$classe, pred_rpart_b46_3[, 1])
pred_rpart_b5_1 = as.matrix(predict(fit_rpart, newdata = b5_1))
mean(pred_rpart_b5_1)
rmse(b5_1$classe, pred_rpart_b5_1[, 1])
pred_rpart_b5_2 = as.matrix(predict(fit_rpart, newdata = b5_2))
mean(pred_rpart_b5_2)
rmse(b5_2$classe, pred_rpart_b5_2[, 1])
pred_rpart_b5_3 = as.matrix(predict(fit_rpart, newdata = b5_3))
mean(pred_rpart_b5_3)
rmse(b5_3$classe, pred_rpart_b5_3[, 1])
pred_rpart_b54_1 = as.matrix(predict(fit_rpart, newdata = b54_1))
mean(pred_rpart_b54_1)
rmse(b54_1$classe, pred_rpart_b54_1[, 1])
pred_rpart_b54_2 = as.matrix(predict(fit_rpart, newdata = b54_2))
mean(pred_rpart_b54_2)
rmse(b54_2$classe, pred_rpart_b54_2[, 1])
pred_rpart_b54_3 = as.matrix(predict(fit_rpart, newdata = b54_3))
mean(pred_rpart_b54_3)
rmse(b54_3$classe, pred_rpart_b54_3[, 1])
pred_rpart_b79_1 = as.matrix(predict(fit_rpart, newdata = b79_1))
mean(pred_rpart_b79_1)
rmse(b79_1$classe, pred_rpart_b79_1[, 1])
pred_rpart_b79_2 = as.matrix(predict(fit_rpart, newdata = b79_2))
mean(pred_rpart_b79_2)
rmse(b79_2$classe, pred_rpart_b79_2[, 1])
pred_rpart_b79_3 = as.matrix(predict(fit_rpart, newdata = b79_3))
mean(pred_rpart_b79_3)
rmse(b79_3$classe, pred_rpart_b79_3[, 1])

fit_nls <- lm(classe ~ log(mq3+0.01) + log(mq4+0.01) + log(mq5+0.01) + 
                 log(mq6+0.01)+ log(mq8+0.01) + log(mq9+0.01) + 
                 log(mq135+0.01) + log(tgs822+0.01) + log(tgs2600+0.01) + 
                 log(tgs2602+0.01) + log(tgs2603+0.01), data=data)
#validation
#Randomly shuffle the data
data <- data[sample(nrow(data)),]
#Create 10 equally size folds
folds <- cut(seq(1, nrow(data)), breaks=10, labels=FALSE)
#Perform 10 fold cross validation
validation_rmse_nls <- 0
for(i in 1:10){
  #Segment your data by fold using the which() function 
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  pr_nls = as.matrix(predict(fit_nls, newdata = testData))
  validation_rmse_nls <- validation_rmse_nls + rmse(testData$classe, pr_nls[, 1])
}
validation_rmse_nls <- validation_rmse_nls / 10
print(validation_rmse_nls)
#test
pred_nls_b4_1 = as.matrix(predict(fit_nls, newdata = b4_1))
mean(pred_nls_b4_1)
rmse(b4_1$classe, pred_nls_b4_1[, 1])
pred_nls_b4_2 = as.matrix(predict(fit_nls, newdata = b4_2))
mean(pred_nls_b4_2)
rmse(b4_2$classe, pred_nls_b4_2[, 1])
pred_nls_b4_3 = as.matrix(predict(fit_nls, newdata = b4_3))
mean(pred_nls_b4_3)
rmse(b4_3$classe, pred_nls_b4_3[, 1])
pred_nls_b4m_1 = as.matrix(predict(fit_nls, newdata = b4m_1))
mean(pred_nls_b4m_1)
rmse(b4m_1$classe, pred_nls_b4m_1[, 1])
pred_nls_b4m_2 = as.matrix(predict(fit_nls, newdata = b4m_2))
mean(pred_nls_b4m_2)
rmse(b4m_2$classe, pred_nls_b4m_2[, 1])
pred_nls_b4m_3 = as.matrix(predict(fit_nls, newdata = b4m_3))
mean(pred_nls_b4m_3)
rmse(b4m_3$classe, pred_nls_b4m_3[, 1])
pred_nls_b45_1 = as.matrix(predict(fit_nls, newdata = b45_1))
mean(pred_nls_b45_1)
rmse(b45_1$classe, pred_nls_b45_1[, 1])
pred_nls_b45_2 = as.matrix(predict(fit_nls, newdata = b45_2))
mean(pred_nls_b45_2)
rmse(b45_2$classe, pred_nls_b45_2[, 1])
pred_nls_b45_3 = as.matrix(predict(fit_nls, newdata = b45_3))
mean(pred_nls_b45_3)
rmse(b45_3$classe, pred_nls_b45_3[, 1])
pred_nls_b46_1 = as.matrix(predict(fit_nls, newdata = b46_1))
mean(pred_nls_b46_1)
rmse(b46_1$classe, pred_nls_b46_1[, 1])
pred_nls_b46_2 = as.matrix(predict(fit_nls, newdata = b46_2))
mean(pred_nls_b46_2)
rmse(b46_2$classe, pred_nls_b46_2[, 1])
pred_nls_b46_3 = as.matrix(predict(fit_nls, newdata = b46_3))
mean(pred_nls_b46_3)
rmse(b46_3$classe, pred_nls_b46_3[, 1])
pred_nls_b5_1 = as.matrix(predict(fit_nls, newdata = b5_1))
mean(pred_nls_b5_1)
rmse(b5_1$classe, pred_nls_b5_1[, 1])
pred_nls_b5_2 = as.matrix(predict(fit_nls, newdata = b5_2))
mean(pred_nls_b5_2)
rmse(b5_2$classe, pred_nls_b5_2[, 1])
pred_nls_b5_3 = as.matrix(predict(fit_nls, newdata = b5_3))
mean(pred_nls_b5_3)
rmse(b5_3$classe, pred_nls_b5_3[, 1])
pred_nls_b54_1 = as.matrix(predict(fit_nls, newdata = b54_1))
mean(pred_nls_b54_1)
rmse(b54_1$classe, pred_nls_b54_1[, 1])
pred_nls_b54_2 = as.matrix(predict(fit_nls, newdata = b54_2))
mean(pred_nls_b54_2)
rmse(b54_2$classe, pred_nls_b54_2[, 1])
pred_nls_b54_3 = as.matrix(predict(fit_nls, newdata = b54_3))
mean(pred_nls_b54_3)
rmse(b54_3$classe, pred_nls_b54_3[, 1])
pred_nls_b79_1 = as.matrix(predict(fit_nls, newdata = b79_1))
mean(pred_nls_b79_1)
rmse(b79_1$classe, pred_nls_b79_1[, 1])
pred_nls_b79_2 = as.matrix(predict(fit_nls, newdata = b79_2))
mean(pred_nls_b79_2)
rmse(b79_2$classe, pred_nls_b79_2[, 1])
pred_nls_b79_3 = as.matrix(predict(fit_nls, newdata = b79_3))
mean(pred_nls_b79_3)
rmse(b79_3$classe, pred_nls_b79_3[, 1])

