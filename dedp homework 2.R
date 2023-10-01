library(haven)
library(ICC)
library(randomizr)
library(multiwayvcov)
library(lmtest)
library(knitr)
library(pwrcalc)
library(Hmisc)

data("balsakhi")
view(balsakhi)
baseline_sd <- sd(balsakhi$pre_mathnorm,na.rm = TRUE)
baseline_mean <- mean(balsakhi$pre_mathnorm,na.rm = TRUE)
effect_mde <- baseline_sd/15
treated_meana<-baseline_mean + effect_mde
mde <- twomeans(m1 = baseline_mean,m2 = treated_meana,nratio = 1, sd = baseline_sd,
                sig.level = 0.05, power = .8)
mde
alpha <- .05
power <- .8
sample_sizer <- 6800
t_power = qt(power, df=2*(sample_sizer-1))
t_alpha = qt(1-alpha/2, df=2*(sample_sizer-1))
t_to <- t_power + t_alpha
p <- .5
p_star <- sqrt(1/(p*(1-p)))
denom <- sqrt(1/sample_sizer) * baseline_sd
mde_school <- t_to * p_star * denom
factor_mde <- baseline_sd/mde_school
