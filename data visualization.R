#we will use ggplot2
install.packages("tidyverse")
require(tidyverse)
print(mpg)
head(mpg,5)
str(mpg)
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,color=class,size=class,alpha=class,shape=class))
# ggplot(data = mpg)
nrow(mtcars)
ncol(mtcars)
?geom_point
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,color=displ<5))+
  facet_wrap(~trans,nrow = 2)
# vignette("ggplot2-specs")
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(drv ~ cyl)

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(.~ cyl)

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy))+
  facet_grid(drv ~ .)

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,color=displ<5))+
  facet_wrap(~class,nrow = 2)
