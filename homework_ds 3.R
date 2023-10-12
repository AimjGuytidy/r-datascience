# set the library with the packages we use
library(ggplot2)
library(tidyverse)
require(cowplot)

# Scenario  1 where we don't accept defects
N <- 100
m <- 6
n <- N - m
k <- 0
x <- 0
prob <- dhyper(x,m,n,k)
while (prob>.1){
  
  k <- k + 1
  prob <- dhyper(x,m,n,k)
}

# Scenario 2 where we accept at most 1 defect
k <- 0
x <- 1
prob <- phyper(x,m,n,k)
while (prob>.1) {
  k <- k + 1
  prob <- phyper(x,m,n,k)
}

# poisson distribution
x <- 0
expy <- function(lambda) exp(-lambda) + lambda * exp(-lambda) + (lambda**2 * exp(-lambda))/factorial(2)
trial <- expy(x)
while (trial > 0.01) {
  x <- x + .1
  trial <- expy(x)
}