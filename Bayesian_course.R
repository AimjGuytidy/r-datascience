rm(list = ls())
#install.packages(c("coda","mvtnorm","devtools","dagitty"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
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

install.packages("remotes")
remotes::install_github("stan-dev/cmdstanr")

cmdstanr::check_cmdstan_toolchain(fix = TRUE)
cmdstanr::install_cmdstan()

install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")
library(posterior)
library(rethinking)

globe.qa <- quap(alist(
                      W ~ dbinom(W+L,p), # binomial likelihood
                      p ~ dunif(0,1) # uniform prior
                 ),
                 data = list(W = 6, L = 3) )

# display summary of quadratic approximation
precis( globe.qa )
