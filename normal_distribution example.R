set.seed(42)
x <- runif(1000)
mu <- mean(x)
sigma <- sd(x)
y <- (1/(sigma*sqrt(2*pi))) * exp(-0.5*(((x-mu)/sigma)^2))
plot(x,y)

# the log of gaussian forms a parabola which is a quadratic function
plot(x,log(y))
