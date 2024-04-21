rm(list = ls())

# we have a uniform distribution U[0,1]
 
u <- runif(1000)

# for n = 5

# nth order stats pdf

draws <- seq(0,1,length.out = 1000)
y5 <- 5 * (draws^4)
y1 <- 5 * (1-draws)^4
# plots
par(mfrow = c(1,2))
plot(draws, y5, main = "nth order stat")
plot(draws, y1, main = "1st order stat")