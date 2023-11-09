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

#install.packages("remotes")
#remotes::install_github("stan-dev/cmdstanr")

#cmdstanr::check_cmdstan_toolchain(fix = TRUE)
#cmdstanr::install_cmdstan()

#install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
#devtools::install_github("rmcelreath/rethinking")
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
