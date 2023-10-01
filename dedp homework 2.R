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


takeup_tr <- 0.5
takeup_cont <- 0
eff_tu <- takeup_tr - takeup_cont
eff_tu_final <- (baseline_sd*(1/15)) * eff_tu
tr_eff_tu <- baseline_mean + eff_tu_final
eff_tu_mde <- twomeans(m1 = baseline_mean, m2 = tr_eff_tu, nratio = nratio,
                       power = power, sig.level = alpha, sd = baseline_sd)
eff_tu_mde
