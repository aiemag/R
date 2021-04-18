library(tidyverse)
library(latex2exp)


#################################
# Normal Distribution

# case 1
x <- rnorm(10000, mean=0, sd=1)
ggplot(NULL, aes(x=x, y=..density..)) +
  geom_histogram(alpha=.2, bins = 100) +
  geom_line(stat="density", size=1, alpha=.7)

# case 2
x <- seq(-3, 3, length=200)
ggplot(data=data.frame(x), aes(x=x)) + 
  stat_function(fun=dnorm)

# case 3
x <- seq(-3, 3, length=200)
plot(x, dnorm(x, mean=0, sd=1), type='l', main="Normal distribution, X~N(0,1)")

#################################
# Normal Distribution comparison by m, sd

# mean = fixed, sd = varaible
x1 <- rnorm(10000, mean=0, sd=1)
x2 <- rnorm(10000, mean=0, sd=.5)
x3 <- rnorm(10000, mean=0, sd=2)
ggplot() +
  geom_histogram(aes(x=x1, y=..density..), alpha=.3, bins = 100) +
  geom_line(aes(x=x1, y=..density..), stat="density", size=1, alpha=.7) + 
  
  geom_histogram(aes(x=x2, y=..density..), fill='blue', alpha=.3, bins = 100) +
  geom_line(aes(x=x2, y=..density..), color='blue', stat="density", size=1, alpha=.7) +
  
  geom_histogram(aes(x=x3, y=..density..), fill='red', alpha=.3, bins = 100) +
  geom_line(aes(x=x3, y=..density..), color='red', stat="density", size=1, alpha=.7) +

  geom_text(aes(x=2, y=.4, label = "m=0, sd=1")) +
  geom_text(aes(x=2, y=.8, label = "m=0, sd=1/2"), color='blue') +
  geom_text(aes(x=2, y=.2, label = "m=0, sd=2"), color='red')


# mean = variable, sd = fixed
x1 <- rnorm(10000, mean=0, sd=1)
x2 <- rnorm(10000, mean=2, sd=1)
x3 <- rnorm(10000, mean=4, sd=1)
ggplot() +
  geom_histogram(aes(x=x1, y=..density..), alpha=.3, bins = 100) +
  geom_line(aes(x=x1, y=..density..), stat="density", size=1, alpha=.7) + 
  
  geom_histogram(aes(x=x2, y=..density..), fill='blue', alpha=.3, bins = 100) +
  geom_line(aes(x=x2, y=..density..), color='blue', stat="density", size=1, alpha=.7) +
  
  geom_histogram(aes(x=x3, y=..density..), fill='red', alpha=.3, bins = 100) +
  geom_line(aes(x=x3, y=..density..), color='red', stat="density", size=1, alpha=.7) +
  
  geom_text(aes(x=0, y=.4, label = "m=0, sd=1")) +
  geom_text(aes(x=2, y=.4, label = "m=2, sd=1"), color='blue') +
  geom_text(aes(x=4, y=.4, label = "m=4, sd=1"), color='red')


#################################
# Standard Normal Distribution painting specific range
dnorm_func <- function(x){
  y <- dnorm(x)
  y[x < 0 | x > 1] <- NA
  return(y)
}

ggplot(NULL, aes(x=c(-3, 3))) +
  ggtitle("Standard Normal Distribution") +
  stat_function(fun=dnorm) +
  stat_function(fun=dnorm_func, geom="area", fill="gray", alpha=0.5) +
  ylab("f(z)") +
  xlab("z") +
  geom_text(aes(x=1, y=.0, label = "z")) +
  geom_text(aes(x=1.5, y=.25, label = "P(0<=Z<=z)")) +
  geom_text(aes(x=0, y=.41, label = "1/root(2π)"))


#################################
# PDF
dnorm_func <- function(x){
  y <- dnorm(x)
  y[x < 1 | x > 2] <- NA
  return(y)
}

dnorm(1)

ggplot(data.frame(x=c(-3, 3)), aes(x=x)) +
  ggtitle("PDF(from 1 to 2) at X~N(0,1), ") +
  stat_function(fun=dnorm) +
  stat_function(fun=dnorm_func, geom="area", fill="gray", alpha=0.5) +
  ylab("f(x)") +
  xlab("x")

#################################
# CDF
ggplot(NULL, aes(x=c(-3,3))) +
  ggtitle("CDF at X~N(0,1), ") +
  stat_function(fun=pnorm) +
  ylab("F(x)") +
  xlab("x")


#---------------------------------------------------
#################################
# PMF
pmf_data = data.frame(x=c(1,2,3,4,5,6), y=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
ggplot(data=pmf_data) +
  ggtitle("PMF from 1 to 6") +
  geom_point(aes(x, y)) +
  geom_bar(stat='identity', width=0.02, aes(x,y)) +
  scale_x_continuous(breaks=seq(0, 7, 1)) +
  ylim(0, 0.2) +
  ylab("f(x)") +
  xlab("x") +
  geom_text(aes(x=1, y=1.1/6, label = "1/6" ))


#################################
# CMF
cmf_func <- function(x){
  y <- x*1/6
  return(y)
}

cmf_data = data.frame(x=c(1,2,3,4,5,6), y=cmf_func(c(1,2,3,4,5,6)))
ggplot(data=cmf_data) +
  ggtitle("CMF from 1 to 6") +
  geom_point(aes(x, y)) +
  geom_bar(stat='identity', width=0.02, aes(x,y)) +
  scale_x_continuous(breaks=seq(0, 7, 1)) +
  #ylim(0, 0.2) +
  ylab("F(x)") +
  xlab("x")
  #geom_text(aes(x=1, y=1.2/6, label = "1/6" ))



#################################
# Statistical Estimation
dnorm_func <- function(x){
  y <- dnorm(x)
  y[x < -1.96 | x > 1.96] <- NA
  return(y)
}

ggplot(NULL, aes(x=c(-3, 3))) +
  ggtitle("95%(from -1.96 to 1.96) at X~N(0,1), ") +
  stat_function(fun=dnorm) +
  stat_function(fun=dnorm_func, geom="area", fill="gray", alpha=0.5) +
  ylab("y") + 
  xlab("z") +
  annotate(
    geom = 'text',
    x = 1, y = 0.3,
    size=4,
    label = TeX("\\int_{-1.96}^{1.96}f(z)dz")
  )
#latex2exp_examples()
