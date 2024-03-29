rm(list = ls())
#install.packages(c("coda","mvtnorm","devtools","dagitty"))
#library(devtools)
#devtools::install_github("rmcelreath/rethinking")
# installing stan
# run the next line if you already have rstan installed
# remove.packages(c("StanHeaders", "rstan"))

#install.packages("rstan",
#                 repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# from counts to probability

ways <- c( 0, 3, 8, 9, 0 )
priors <- ways/sum(ways)

# binomial distribution
dbinom(6, size = 9, prob = 0.5) #prob to get 6 waters if prob of water is 0.5

# Grid approximation method ####
###############################

# define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# define priors
prior <-rep(1, 20)

# compute likelihood at each value
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior

unstd.posterior <- likelihood * prior

# standardize the posterior

posterior <- unstd.posterior/sum(unstd.posterior) 

# let's visualize this 
# 1st case
plot(p_grid,posterior,type = "b",xlab = "Probability of water",
     ylab = "Posterior Probability")
mtext("20 points")

# 2nd case
prior2 <- ifelse(p_grid < .5, 0, 1)
unstd.posterior2 <- likelihood * prior2
posterior2 <- unstd.posterior2/sum(unstd.posterior2)
plot(p_grid,posterior2,type = "b",xlab = "Probability of water",
     ylab = "Posterior Probability")
mtext("20 points")

# 3rd case

prior3 <- exp(-5 * abs(p_grid -.5))
unstd.posterior3 <- likelihood * prior3
posterior3 <- unstd.posterior3/sum(unstd.posterior3)
plot(p_grid,posterior3,type = "b",xlab = "Probability of water",
     ylab = "Posterior Probability")
mtext("20 points")

# Quadratic approximation ####
##############################

# install.packages("remotes")
# remotes::install_github("stan-dev/cmdstanr")
# 
# cmdstanr::check_cmdstan_toolchain(fix = TRUE)
# cmdstanr::install_cmdstan()

# install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
# devtools::install_github("rmcelreath/rethinking")
library(posterior)
library(rethinking)

globe.qa <- quap(alist(
                      W ~ dbinom(W+L,p), # binomial likelihood
                      p ~ dunif(0,1) # uniform prior
                 ),
                 data = list(W = 6, L = 3) )

# display summary of quadratic approximation
precis( globe.qa )

# analytical calculation
W <- 6
L <- 3

curve(dbeta(x, W+1, L+1), from = 0, to = 1)

# Quadratic approximation
curve(dnorm(x, mean = 0.67, sd = 0.16), lty = 2, add = T)

# Markov chain Monte Carlo #####
################################

n_samples <- 100000 # specify number of samples
p <- rep(NA, n_samples) # replicate NA n_samples time, initiating the parameter
p[1] <- 0.5 # assign value to parameter
W <- 6 # amount of water draw from tossing 9 times
L <- 3 # amount of land

for (i in 2:n_samples) {
  p_new <- rnorm( 1, p[i-1], 0.1) # draw from a random sample that's normal
  if ( p_new < 0) p_new <- abs(p_new)
  if ( p_new > 1) p_new <- 2 - p_new
  q0 <- dbinom( W, W+L, p[i-1]) #prior likelihood
  q1 <- dbinom( W, W+L, p_new) #updated likelihood
  p[i] <- ifelse( runif(1) < q1/q0, p_new, p[i-1])
}

# let's visualize the density distribution of our parameter from MCMC
dens(p, xlim = c(0,1))
curve( dbeta(x, W+1, L+1), lty = 2, add = T)

# Sampling ####
###############

# Testing Vampirism!!!

pr_positive_vamp <- 0.95
pr_vamp <- 0.001
pr_mortal <- 1 - pr_vamp
pr_positive_mortal <- 0.01
pr_positive <- (pr_positive_vamp * pr_vamp) + (pr_positive_mortal * pr_mortal) 
pr_vamp_positive <- (pr_positive_vamp * pr_vamp) / pr_positive
cat(paste("The probability for vampirism given positive test is:",pr_vamp_positive))

# sampling from a grid approximation posteriors

grid_parameter <- seq(0,1, length.out=1000)
grid_prior <- rep(1,1000)
prob_data <- dbinom(6, 9, prob = grid_parameter)
unstd_post <- prob_data * grid_prior
std_post <- unstd_post / sum(unstd_post)
plot(grid_parameter,std_post, type = "l")
hist(grid_parameter)

# sampling with replacement based on posterior probabilities

samples <- sample(grid_parameter ,size = 1e4, replace = T, prob = std_post)

hist(samples)
dens(samples,add = F)
plot(samples)

# intervals of defined boundaries
std_post
std_post[grid_parameter<.5]
sum(std_post[grid_parameter <= .5])

length(samples)
sum(samples < .5) / length(samples)
sum(samples > .5 & samples < .75) / length(samples)

# intervals of defined mass (confindence interval)

quantile(samples,.8) # 80th percentile
quantile(samples,c(.1,.9)) # middle 80%
quantile(samples,.2)


# a different scenario
grid_par <- seq(0,1,length.out = 1000)
prior_grid <- rep(1, 1000)
likelihood_grid <- dbinom(3,size = 3, prob = grid_par)
post_unstd <- likelihood_grid * prior_grid
post_std <- post_unstd/sum(post_unstd)
plot(grid_par,post_std, type = "l")
samples1 <- sample(grid_par,1e4,replace = T, prob = post_std)
dens(samples1)
PI(samples1, prob = .5) # 50% percentile compatibility interval

# Highest Posterior Density Interval
# the narrowest interval containing the specified probability mass.
# this captures the parameters with highest posterior probability, as well as 
# being narrower

HPDI(samples1, prob = .5)
HPDI(samples1, prob = .1)

HPDI(samples, prob = .8)
PI(samples = samples, prob = .8)
HPDI(samples, prob = .95)
PI(samples, prob = .95)

#point estimates
grid_par[which.max(post_std)] # this translate to find grid_par value where the 
                              # post_std is at the maximum

# point estimates when you have samples
hist(samples1)
chainmode(samples1, adj = .01)
mean(samples1)
median(samples1)

# function to find statistical mode. source: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(samples1)

# Loss function

sum(std_post * abs(.5 - grid_parameter)) # this is the loss fct if we set our 
                                         # parameter value to .5 this formula 
                                         # calculate the weighted loss fct

# loss function having a vector of probable values
# we will use sapply
loss <-
  sapply(grid_parameter, function(d)
    sum(std_post * abs(d - grid_parameter))) # in this scenario we are interested
grid_parameter[which.min(loss)]              # in finding the parameter with the 
                                             # lowest loss function!

# let's replicate the method on the 3 tosses and 3 water parts
sum(post_std * abs(.5 - grid_par))
loss <-
  sapply(grid_par, function(d)
    sum(post_std * abs(d - grid_par)))
grid_par[which.min(loss)]
median(samples1)

# we can use (d-p)**2 as a loss function which is equivalent to mean(samples1)

# Sampling to simulate prediction ####
######################################

# Dummy data ####

# bayesian models are generative!!! we can get data from the likelihood fct
dbinom(0:2,size = 2, prob = .7) # this can be interpreted as making 2 tosses
#with the prob of water being .7 and we are interested in the prob of having no 
#water, 1 water or all tosses landing on water.

dbinom(1, size = 1, prob = .7)

rbinom(1, size = 1, prob = .7) # this means that we perform the tossing activity
#once and the activity consist of only 1 toss (size) where water has .7 prob, the 
#result indicate how many time we stumbled on water.

rbinom(2, size = 3, prob = .7) # this example is trying to determine how many 
#times we hit water if we conduct the experiment 2 times with each time consisting
#3 tosses!! this means water can be hit either o times, 1 time, 2 times, or 3 times

dummy_w <- rbinom(1e5, size = 2, prob = .7)
table(dummy_w)/1e5 # this is to see the proportional of each possible event
hist(dummy_w)
dummy_w_9 <- rbinom(1e5, size = 9, prob = .7)
hist(dummy_w_9)
simplehist(dummy_w_9, xlab = "dummy water count")

dummy_w_1000 <- rbinom(1e8, size = 1000, prob = .7)
#simplehist(dummy_w_1000)
hist(dummy_w_1000)

# Model checking ####
#####################


# did the software work?
# is the model adequate?


# Posterior predictive distribution: distribution of distributions in bayesian 
# sense

# we start with the p param and how it is ditributed (posterior distribution)
# and then we create sampling distribution for each possible value of p ( eg. 
# looking at p = .1 we check the likelihood distribution rbin(x,size = 9,prob = .1))
# after the sampling distribution, we move on to average over the sampling dist
# this is done by computing the weighted average frequency of each possible obs.
# and then we get uncertainty about prediction

# predicted obs with single p value (.6)
w <- rbinom(1e4, size = 9, prob = .6)
simplehist(w)

# predicted obs, taking into account the parameter uncertainty
w <- rbinom(1e4, size = 9, prob = samples) # here the prob is based on many p values
simplehist(w)

# Practice ####

# initial values for reference
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, 9, prob = p_grid)
post_unstd <- likelihood * prior
post_std <- post_unstd/sum(post_unstd) #standardizing

# using the sampling method
set.seed(100)
samples <- sample(p_grid, size = 10000, prob = post_std,replace = T)
hist(samples)
sum(samples<.2)/length(samples)
quantile(samples,0.000349) # this indicates that only 0.0349% lies below .2
quantile(samples,0.0004)

sum(samples>.8)*100/length(samples)
PI(samples = samples,prob = .8)
quantile(samples,0.8889) # approx 11.11% lies above .8
quantile(samples,(1-0.1116))

sum(samples>=.2 & samples<=.8)/length(samples)
PI(samples, .888)
quantile(samples,.888)
sum(samples>=.2) / length(samples)
quantile(samples,(1-0.9996))
btn_2_8<-(88.8-.04) # approx 88.76% lie btn .2 and .8

quantile(samples,.2) # 20% of post lies below .5185
quantile(samples,.8) # 20% of post lies above .75576

HPDI(samples,prob = .66) # narrowes int. [.5085, .7738]

# creating posterior dist. using grid approx.
likelihood1 <- dbinom(8, 15, prob = p_grid)
hist(likelihood1)
post_unstd1 <- likelihood1 * prior
post_std1 <- post_unstd1/sum(post_unstd1)
hist(post_std1)
hist(post_std)
samples1 <- sample(p_grid,1e4,replace = T,prob = post_std1)
hist(samples1)
median(samples1)
HPDI(samples = samples1, prob = .9)

w <- rbinom(1e5, 15, prob = samples1)
table(w)/1e5 # the prob for 8 out of 15 is .14594

prob_6_9 <- dbinom(6, 9, prob = samples1)
k <- samples1[which.max(post_std1)]
prob_6_9_new <- dbinom(6, 9, prob = k) # prob of observing 6/9 based on 8/15 data
# is 0.2132718 this is incorrect!!
# the correct answer
prob_6 <- rbinom(1e4, 9, prob = samples1)
prob_6_tab <- table(prob_6)/1e4
prob_6_tab["6"]
# scenario with some knowledge on the prior

prior2 <- ifelse(p_grid < .5, 0,1)
post_unstd2 <- likelihood * prior2
post_std2 <- post_unstd2 / sum(post_unstd2)

set.seed(100)
samples2 <- sample(p_grid,1e4,replace = T, prob = post_std2)
hist(samples2)
median(samples2) 
median(samples)
median(samples1)

loss2 <- sapply(p_grid, function(d) sum(post_std2 * abs(p_grid-d)))
p_grid[which.min(loss2)] # the p with the least loss is 0.6766767
median(samples2)


loss0 <- sapply(p_grid, function(d) sum(post_std * abs(p_grid-d)))
p_grid[which.min(loss0)] # the p with the least loss is 0.6446446

# the p obtained after considering that we new the real p was not below .5 is 
# closer to the real value

# we need to toss the globe at least 30 times to have precise p

# Hard Problems!

# data
data(homeworkch3)
sum_boys <- sum(birth1,birth2)
sum_boys
length(birth1) + length(birth2)
birth_likelihood <- dbinom(111,200,prob = p_grid)
birth_post_un <- birth_likelihood * prior
birth_post <- birth_post_un / sum(birth_post_un)
p_grid[which.max(birth_post)] # a p of 0.5545546 has a max post

birth_sample <- sample(p_grid,1e4, replace = T, prob = birth_post)
HPDI(birth_sample,prob = .5)
HPDI(birth_sample,prob = .89)
PI(birth_sample)
HPDI(birth_sample, prob = .97)
hist(birth_sample)
median(birth_sample)

birth_rep <- rbinom(1e4,200,birth_sample)
table(birth_rep)/1e4
median(birth_rep)
birth_table <- table(birth_rep)/1e4
birth_table["111"]
birth_table[which.max(birth_table)] # based on this there are 109 boys

dens(birth_rep) # the model does a great job in replicaating the data


sum(birth1)
birth1_likelihood <- dbinom(51, 100, prob = p_grid)
birth1_post_un <- birth1_likelihood * prior
birth1_post <- birth1_post_un / sum(birth1_post_un)
birth1_sample <- sample(p_grid, 1e4, replace = T, prob = birth1_post)
dens(birth1_sample)
birth1_rep <- rbinom(1e4, 100, birth1_sample)
birth1_table <- table(birth1_rep)/1e4
birth1_table[which.max(birth1_table)] # from the sim first borns the number of boys
# is 52!!
# the model looks a little bit bumpy tho!
sum(birth1)
rbinom(10000,49,prob = (1-median(birth1_sample)))



# Geocentric Models ####

pos <- replicate(1000, sum(runif(16,-1,1)))
dens(pos)
hist(pos)
plot(density(pos))

plot(density(replicate(1000,sum(runif(8,-1,1)))))
plot(density(replicate(1000,sum(runif(4,-1,1)))))
temp <- replicate(1000, runif(16, -1, 1))
temp_df <- as.data.frame(temp)
temp_df[1,] <- 0
temp_df <- data.table::as.data.table(temp_df)
temp_df[,"x1"] <- seq(1,16)
temp_long <- data.table::melt(temp_df,id.vars = "x1")
ggplot(data = temp_long,aes(x1,value,group=variable))+
  geom_line()

