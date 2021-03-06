library(ggplot2)
ggplot(NULL, aes(c(-5, 5))) + stat_function(fun=dnorm)

x <- seq(-3, 3, length=200)
ggplot(NULL, aes(x)) + stat_function(fun=dnorm)


x <- seq(-3, 3, length=200)
plot(x, dnorm(x, mean=0, sd=1), type='l', main="Normal distribution, X~N(0,1)")
