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


bal_subset <- subset(balsakhi,!is.na(pre_mathnorm))
cluster_sub <- as.factor(bal_subset$schoolid)
out_sub <- bal_subset$pre_mathnorm
icc2 <- ICCest(cluster_sub,out_sub,data = bal_subset)
rho2 <- icc2$ICC
tr_effect <- 0.4 * baseline_sd
tr_mean <- baseline_mean + tr_effect
tot_clusters <- 36
clus_size_model <- twomeans(m1 = baseline_mean, m2 = tr_mean,sd = baseline_sd,
                            power = power, sig.level = alpha,nratio = nratio)|>
  clustered(numclus = tot_clusters, rho = rho2, )

clus_size_model



t_power = qt(power, df=2*(36-1))
t_alpha = qt(1-alpha/2, df=2*(36-1))

t_stat <- t_alpha + t_power 

# and 50% of the study population is assigned to treatment and 50% to control:
p <- .5

# Now, we have Duflo et al.'s power adjustment:
mde <- t_stat * sqrt(1 / (p * (1 - p) * total_clusters)) * sqrt(rho + (1 - rho) / cluster_size) * baseline_sd
mde_check <- t_stat * sqrt(1/(p*(1-p)))*sqrt(1/(total_clusters*cluster_size*(1+(rho*(cluster_size-1)))))

cluster_sizee <- ((t_stat/(baseline_sd*0.4)) * sqrt(1 / (p * (1 - p) * 36)) * sqrt(rho2 + (1 - rho2)) * baseline_sd)**2

clus_size_model1 <- twomeans(m1 = baseline_mean, m2 = tr_mean,sd = baseline_sd,
                            power = power, sig.level = alpha,nratio = nratio)|>
  clustered(numclus  = 65, rho = rho2, )

clus_size_model1
# And lastly, the total sample size of our study and the number who are treated, respectively:
n <- total_clusters * cluster_size
treated <- n * p