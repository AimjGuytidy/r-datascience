rm(list = ls())
library(tidyverse)
library(ipred)
library(stats)

nlsw <- read_csv("data/nlsw88.csv")

model_reg <- lm(lwage ~ yrs_school,nlsw)
summary(model_reg)
ci <- confint(model_reg, level = 0.9)
ci
new <- data.frame(yrs_school=c(mean(nlsw$yrs_school,na.rm = TRUE)))
predict(model_reg,newdata = new)
mean(nlsw$lwage)
sum(model_reg$residuals)
