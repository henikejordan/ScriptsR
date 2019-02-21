y <- function(model, dados) {
    ret <- NULL
    for(i in 1:nrow(dados)) {
        ret <- rbind(ret, sum(dados[i,1:ncol(dados)-1] * coef(model)[2:ncol(dados)]) + coef(model)[1])
    }
    return (ret)
}

bebidas_train$classe <- strtoi(gsub('A', '', bebidas_train$classe))
model <- lm(classe~mq2+mq3+mq4+mq5+mq6+mq7+mq8+mq9+mq135+tgs822+tgs2600+tgs2602+tgs2603, data=bebidas_train)
