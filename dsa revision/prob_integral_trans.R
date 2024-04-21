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


# Probability integral transformation eg: exponential
set.seed(42)
draws <- seq(0,1000,length.out = 100000)
temp <- -log(1-draws) / 1.5
par(mfrow=c(2,2))
hist(qexp(draws))
plot(density(qexp(draws)))
plot(sort(runif(1000)),qexp(sort(runif(1000))))
abline(v = mean(qexp(runif(1000))))

plot(qexp(draws),xlim = c(0,100))
