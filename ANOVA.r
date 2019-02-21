sensores <- gl(13,nrow(bebidas_train),labels = c("mq2","mq3","mq4","mq5","mq6","mq7","mq8",
                                            "mq9","mq135","tgs822","tgs2600","tgs2602","tgs2603"))
head(sensores)
bebidas.vert<-data.frame(sensores,
                         valor=c(bebidas_train$mq2,bebidas_train$mq3,bebidas_train$mq4,bebidas_train$mq5,bebidas_train$mq6,
                                 bebidas_train$mq7,bebidas_train$mq8,bebidas_train$mq9,bebidas_train$mq135,
                                 bebidas_train$tgs822,bebidas_train$tgs2600,bebidas_train$tgs2602,bebidas_train$tgs2603))
head(bebidas.vert)
ANOVA=aov(valor~sensores,bebidas.vert)
summary(ANOVA)
TukeyHSD(ANOVA)
