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
# page 16
#Geometric Objects
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy),color='blue')

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x=displ,y=hwy))

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x=displ,y=hwy),color='orange')

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x=displ,y=hwy,linetype=drv,color=drv))+
  geom_point(mapping = aes(x=displ,y=hwy,color=drv))

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x=displ,y=hwy,group=drv),color='orange')

ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+
  geom_smooth(mapping = aes(linetype=drv,color=drv))+
  geom_point(mapping = aes(color=drv))

ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+
  geom_smooth(data = filter(mpg,class=='subcompact'))+
  geom_point(mapping = aes(color=class))

ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+
  geom_smooth(data = filter(mpg,class=='subcompact'),se=FALSE)+
  geom_point(mapping = aes(color=class))

ggplot(data = mpg,mapping = aes(x=displ,y=hwy,color=drv))+
  geom_point()+
  geom_smooth(se=FALSE)

# Statistical transformations

head(diamonds,5)

#Bar chart
ggplot(data = diamonds,mapping = aes(x=cut))+
  geom_bar()

ggplot(data = diamonds,mapping = aes(x=cut))+
  stat_count()

trial <- tribble(
  ~a,    ~b,
  'bar1',25,
  'bar2',35,
  'bar3',75
  )
ggplot(data = trial,mapping = aes(x=a,y=b))+
  geom_bar(stat = 'identity')

ggplot(data = diamonds,mapping = aes(x=cut))+
  geom_bar(mapping = aes(y=..prop..,group=1))

ggplot(data = diamonds,mapping = aes(x=cut,y=depth))+
  stat_summary(fun.max = max,fun.min = min,fun = median)
#Position Adjustments
ggplot(data = mpg)+
  geom_bar(mapping = aes(x=hwy),color='blue')

ggplot(data = mpg)+
  geom_bar(mapping = aes(x=hwy,fill='yellow'))

ggplot(data = mpg)+
  geom_bar(mapping = aes(x=hwy,fill=drv))

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut,fill=cut))

ggplot(data = diamonds,mapping = aes(x=cut,color=cut))+
  geom_bar(fill=NA,position='identity')

ggplot(data = diamonds,mapping = aes(x=cut,fill=clarity))+
  geom_bar(position='dodge')

ggplot(data = diamonds,mapping = aes(x=cut,fill=clarity))+
  geom_bar(alpha=1/5,position='identity')

ggplot(data = diamonds,mapping = aes(x=cut,fill=clarity))+
  geom_bar(position='fill')

ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+
  geom_point(position='jitter')

?geom_count
?geom_jitter
?geom_boxplot

ggplot(data = mpg)+
  geom_boxplot(mapping = aes(x=hwy,y=drv))

#Coordinate Systems

ggplot(data = mpg)+
  geom_boxplot(mapping = aes(y=hwy,x=class))+
  coord_flip()

rw<-map_data("nz")
ggplot(data=rw,mapping = aes(long,lat,group=group))+
  geom_polygon(fill='white',color='magenta')

ggplot(data=rw,mapping = aes(long,lat,group=group))+
  geom_polygon(fill='white',color='magenta')+
  coord_quickmap()

?labs

bar<-ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut,fill=cut))

bar + coord_flip()
bar+coord_polar()

install.packages("mapproj")
require("mapproj")
require("maps")
mapy<-ggplot(data=rw,mapping = aes(long,lat,group=group))+
  geom_polygon(fill='white',color='magenta')
  
mapy+coord_map()
?coord_fixed
?geom_abline


ggplot(data = mpg,aes(x=cty,y=hwy))+
  geom_point()+
  geom_abline()+
  coord_fixed()

#The Layered Grammar of Graphics
#########################################

#####################
#2.Workflow:Basics#####
#####################
