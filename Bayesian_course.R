rm(list = ls())
#install.packages(c("coda","mvtnorm","devtools","dagitty"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
# installing stan
# run the next line if you already have rstan installed
# remove.packages(c("StanHeaders", "rstan"))

install.packages("rstan",
                 repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
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
