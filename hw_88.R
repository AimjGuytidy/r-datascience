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
nlsw$other <- 1-nlsw$black
model_reg_mult <- lm(lwage ~ black + other, nlsw)
summary(model_reg_mult)

model_mult <- lm(lwage ~ yrs_school + ttl_exp, nlsw)
summary(model_mult)

model_mult1 <- lm(lwage ~ I(yrs_school + 2*ttl_exp), nlsw)
summary(model_mult1)

nlsw$sumy <- nlsw$yrs_school+2*nlsw$ttl_exp

model_mult10 <- lm(lwage ~ sumy, nlsw)
summary(model_mult10)

anova(model_mult1,model_mult)
anova(model_mult,model_mult1)
