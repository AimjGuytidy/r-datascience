rm(list = ls())
#install.packages(c("coda","mvtnorm","devtools","dagitty"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")

# from counts to probability

ways <- c( 0, 3, 8, 9, 0 )
priors <- ways/sum(ways)
