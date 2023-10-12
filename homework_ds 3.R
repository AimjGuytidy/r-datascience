# set the library with the packages we use
library(ggplot2)
library(tidyverse)
require(cowplot)
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