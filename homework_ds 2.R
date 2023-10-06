library(ggplot2)
library(tidyverse)
# creating a random draw of 1000 numbers from a uniform distribution
set.seed(42)
u <- runif(1000)
max(u)*3 + 2 # converting the draw from 0 to 1 to 2 to 5
min(u)*3 + 2
maxa <- list()
mina <- list()
ndraws <- list()
for (draw in seq(1,100)) {
  t <- runif(draw)
  maxa<-append(maxa,max(t))
  mina<-append(mina,min(t))
  ndraws <- append(ndraws,draw)
  rm(t)
} 

dada <- data.frame(cbind(as.vector(maxa),as.vector(mina),as.vector(ndraws)))
ggplot(dada) +
  geom_point(aes(y = X2, x = X3,color="purple"))
pnorm(u)
hist(pnorm(u))
hist(dnorm(u))
hist(qnorm(u)) #this is the right answer when you want to convert uniform distribution to normal distribution
