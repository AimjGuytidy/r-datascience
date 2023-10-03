#Preliminaries
#---------------------------------------
rm(list=ls())
library("utils")
#install.packages('plot3D')
library(plot3D)
setwd()

#Creating the vector x and y
M <- mesh(seq(0,1,length=100), seq(0,1,length=100))
x<-M$x
y<-M$y
z<-6/5*(M$x+M$y^2)

#Plotting this pdf
persp3D(x, y, z, xlab='X variable', ylab= 'Y variable', xlim = c(0,1), main= "Plotting joint pdf")