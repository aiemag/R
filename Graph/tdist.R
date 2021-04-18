library(tidyverse)
library(latex2exp)


# T distribution (t value  > 2.015, α = 0.05, DOF = 10)
alpha_td_func <- function(x){
  y <- dt(x, 10) # 10 is DOF
  y[x < 2.015] <- NA
  return(y)
}

# T distribution (t value  > 3.167, p = ?, DOF = 10)
pvalue_td_func <- function(x){
  y <- dt(x, 10) # 10 is DOF
  y[x < 3.167] <- NA
  return(y)
}


t <- seq(-5, 5, length.out = 101)
y1 <- dt(t, v)

ggplot() +
  ggtitle("T Distribution(DOF=10)") +
  geom_line(aes(x=t, y=y1)) +
  stat_function(fun=alpha_td_func, geom="area", fill="skyblue", alpha=0.5) +
  stat_function(fun=pvalue_td_func, geom="area", fill="red", alpha=0.5) +
  ylab("f(t)") +
  xlab("t") +
  geom_text(aes(x=2.7, y=0.05, label = "α=0.05")) +
  geom_text(aes(x=4.3, y=0.02, label = "p-value=0.012449"))