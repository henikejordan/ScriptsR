#####Remove Redundant Features#####
set.seed(7)
library(mlbench)
library(RPostgreSQL)
library(caret)
library(doParallel)
registerDoParallel(cores=2)
con <- dbConnect(PostgreSQL(), user="postgres", password="12345",dbname="postgres")
rs <- dbSendQuery(con, "select data_hora, max(mq2) as mq2, max(mq3) as mq3, max(mq4) as mq4, max(mq5) as mq5,
max(mq6) as mq6, max(mq7) as mq7, max(mq8) as mq8, max(mq9) as mq9, max(mq135) as mq135,
max(tgs822) as tgs822, max(tgs2600) as tgs2600, max(tgs2602) as tgs2602, max(tgs2603) as tgs2603,
classe from gases where data_hora >= '2018-04-01 00:00:00' and data_hora < '2018-12-31 23:00:00' 
and classe like '%ppm'
group by data_hora, classe
order by data_hora")
df <- fetch(rs, n = -1)
bebidas_train <- df[2:15]
correlationMatrix <- cor(bebidas_train[,1:13])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
print(highlyCorrelated)

#####Rank Features By Importance#####
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(classe~., data=bebidas_train, method="rf", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)