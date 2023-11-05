rm(list = ls())
#install.packages(c("coda","mvtnorm","devtools","dagitty"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")

# from counts to probability

ways <- c( 0, 3, 8, 9, 0 )
priors <- ways/sum(ways)

# binomial distribution
dbinom(6, size = 9, prob = 0.5) #prob to get 6 waters if prob of water is 0.5
