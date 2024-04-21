# example of probability integral transformation
rm(list = ls())
# start by drawing random values from uniform distribution

u <- runif(1000)

# After selecting your distribution of choice (you must know its CDF!)
# input the uniform random varialbes in the inverse function of the CDF
# the distribution must be continuous!!

# exponential distribution

expy <- qexp(u)

# normal distribution

normy <- qnorm(u)

# beta distribution

bety <- qbeta(u,0.3,0.6)


# let's visualize the distributions

# uniform####
par(mfrow=c(2,2))
# PDF

hist(u)
plot(density(u))

# CDF

plot(ecdf(u))

# exponential ####
par(mfrow=c(2,2))
# PDF

hist(expy)
plot(density(expy))

# CDF

plot(ecdf(expy))

# normal ####
par(mfrow = c(2,2))
# PDF

hist(normy)
plot(density(normy))

# CDF

plot(ecdf(normy))

# beta ####
par(mfrow = c(2,2))
# PDF

hist(bety)
plot(density(bety))

# CDF

plot(ecdf(bety))