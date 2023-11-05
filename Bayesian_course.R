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
