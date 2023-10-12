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
expy <- function(lambda) exp(-lambda) + lambda * exp(-lambda) 
trial <- expy(x)
while (trial > 0.01) {
  x <- x + .1
  trial <- expy(x)
}

# simulations and estimators
n_obs <- 100
n_samples <- 1000
theta <- 5
theta_mean <- c()
theta_med <- c()
# simulating the 1000 samples and producing their means
for (samp in seq(1,n_samples)) {
  n_vec <- runif(n_obs,min = 0,max = theta)
  theta_mean <- append(theta_mean,mean(n_vec,na.rm=TRUE))
  theta_med <- append(theta_med, median(n_vec,na.rm = TRUE))
}

# Plotting the distribution
theta_mean <- as.data.frame(theta_mean)
theta_med <- as.data.frame(theta_med)

ggplot() +
  geom_histogram(data = theta_mean,
                 mapping = aes(x = theta_mean,y=..density..),color = "darkblue",
                 fill = "green",alpha=.4)+
  geom_histogram(data = theta_med, 
                 mapping = aes(x=theta_med,y=..density..), color = "darkblue",
                 fill = "yellow", alpha=.2)


ggplot() +
  geom_histogram(data = theta_mean,
                 mapping = aes(x = theta_mean*2,y=..density..),color = "darkblue",
                 fill = "maroon",alpha=.4)+
  geom_histogram(data = theta_med, 
                 mapping = aes(x=theta_med*2,y=..density..), color = "darkblue",
                 fill = "orange", alpha=.2)
