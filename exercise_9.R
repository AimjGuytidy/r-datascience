rm(list = ls())
library(tidyverse)
library(ipred)
library(stats)

set.seed(42)

x <- sample(replicate(10000,c(0,1)),1000)

y <- (x * rnorm(1) + runif(length(x))**2) * 100

model_x <- lm(y ~ x, data = data.frame(x = x, y = y))

summary(model_x)


daty <- data.frame(x = x, y = y) |>
  mutate(z = ifelse(x == 1, "TRUE","FALSE"))


model_x1 <- lm(y ~ z, data = daty)

summary(model_x1)

mean(filter(daty,z == "TRUE")$y)
sum(model_x1$coefficients)

mean(filter(daty,z == "FALSE")$y)
