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