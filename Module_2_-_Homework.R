rm(list = ls())

#rbinom(n, size, prob)
#where n refers to number of observations, size refers to number of trials, 
#and prob refers to probability of success on each trial
library(tidyverse)
#Question 9&10
successes <- rbinom(1000,8,0.2)
hist(successes)
hist(rbinom(1000,8,1))
k <- as.tibble(data.frame(successes))

k|>
  group_by(successes)|>
  summarise(n=n())|>
  mutate(freq=n/sum(n))|>
  ggplot(aes(x=successes,y=freq)) +
  geom_col() +
  ylab("Estimated Density")
#Question 12
?dbinom
?pbinom
sum(dbinom(0:7,10,0.65))
sum(dbinom(6:10,10,0.65))


#Question 15
#Create a tibble with x and the analytical probability densities.
n = 1000
p = .2
my_binom<-as_tibble(list(x=0:n, prob=dbinom(0:n, n, p)))

#Plot the computed theoretical density.
ggplot(my_binom, aes(x=x, y=prob)) + geom_col() +
  ylab("Analytical Density")

calculated_cdf <- my_binom %>%
  mutate(cdf=cumsum(prob))

#Plot the computed cdf
ggplot(calculated_cdf, aes(x=x, y=cdf)) + geom_step() + 
  ylab("CDF")