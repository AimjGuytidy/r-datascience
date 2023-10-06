#Preliminaries
#---------------------------------------
rm(list=ls())
library("utils")
#install.packages('plot3D')
library(plot3D)
#setwd()

#Creating the vector x and y
M <- mesh(seq(0,1,length=100), seq(0,1,length=100))
x<-M$x
y<-M$y
z<-6/5*(M$x+M$y^2)

#Plotting this pdf
persp3D(x, y, z, xlab='X variable', ylab= 'Y variable', xlim = c(0,1), 
        main= "Plotting joint pdf")
set.seed(42)
x <- seq(0,1,length = 1000)
y <- seq(0,1,length = 1000)
X <- function(x){(6/5) *(x**2/2 + (x*1/3))}
Y <- function(y){(6/5) *(y**3/3 + (y*1/2))}
ecdfx <- ecdf(X)
ecdfy <- ecdf(Y)
plot(ecdf(X))
plot(ecdf(Y))
hold
ggplot(data = data.frame(x), aes(x)) +
  stat_function(fun = X,color="blue") + 
  stat_function(fun = Y, color = "red")
