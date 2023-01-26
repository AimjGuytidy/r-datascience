#we will use ggplot2
# install.packages("tidyverse")
require(tidyverse)
print(mpg)
head(mpg,5)
str(mpg)
 
colnames(mpg)
dplyr::count(mpg[,c("class","displ")],class,sort=TRUE)%>%
  ggplot()+
  geom_bar(mapping = aes(x=class,y=n,fill=class),stat = "identity")
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,color=class,size=class,alpha=class,shape=class))
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,size=class))+
    facet_grid(cols = vars(class))

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,color=class))+
  facet_wrap(facets = vars(class))

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,size=class))+
  facet_grid(.~cyl)

nrow(mtcars)
ncol(mtcars)
# ?geom_point
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

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,color=displ<5))+
  facet_wrap(~class,ncol = 4)

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ,y=hwy),color="green")+
  facet_wrap(~class,nrow = 2)

# ggplot(data = mpg)+
#   geom_point(mapping = aes(x=displ,y=hwy,color=class,size=class,alpha=class,shape=class))
# # ggplot(data = mpg)
# nrow(mtcars)
# ncol(mtcars)
# ?geom_point
# ggplot(data = mpg)+
#   geom_point(mapping = aes(x=displ,y=hwy,color=displ<5))+
#   facet_wrap(~trans,nrow = 2)
# # vignette("ggplot2-specs")
# ggplot(data = mpg)+
#   geom_point(mapping = aes(x=displ,y=hwy))+
#   facet_grid(drv ~ cyl)
# 
# ggplot(data = mpg)+
#   geom_point(mapping = aes(x=displ,y=hwy))+
#   facet_grid(.~ cyl)
# 
# ggplot(data = mpg)+
#   geom_point(mapping = aes(x=displ,y=hwy))+
#   facet_grid(drv ~ .)
# 
# ggplot(data = mpg)+
#   geom_point(mapping = aes(x=displ,y=hwy,color=displ<5))+
#   facet_wrap(~class,nrow = 2)
# 
# ggplot(data = mpg)+
#   geom_point(mapping = aes(x=displ,y=hwy,color=displ<5))+
#   facet_wrap(~class,ncol = 4)
# 
# ggplot(data = mpg)+
#   geom_point(mapping = aes(x=displ,y=hwy),color="green")+
#   facet_wrap(~class,nrow = 2)

ggplot(mpg)+
  geom_smooth(mapping = aes(x=displ,y=hwy,linetype=drv,color=drv))+
  geom_point(aes(x=displ,y=hwy,color=drv))

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
view(diamonds)
#Bar chart
ggplot(data = diamonds,mapping = aes(x=cut))+
  geom_bar()

ggplot(data = diamonds,mapping = aes(x=cut))+
  stat_count()

?tribble
a <- 1:5
tibble(a,a^2)
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

ggplot(data = diamonds,mapping = aes(x=cut))+
  geom_bar(mapping = aes(y=..prop..,group=price))

ggplot(data = diamonds,mapping = aes(x=cut,y=depth))+
  stat_summary(fun.max = max,fun.min = min,fun = median)

ggplot(data = diamonds,mapping = aes(x=cut,y=price))+
  stat_summary(fun.max = max,fun.min = min,fun = mean)
?stat_summary
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

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut))+
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

# install.packages("mapproj")
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
#2.Workflow:Basics###
#####################
# this_is_a_really_long_name <- 2.5
# this_is_a_really_long_name
# This_is_a_really_long_name
r_rocks <- 2^3
seq(1,10)
x <- "hello world"
(y <- seq(1,10,length.out=5))

# ggplot(data = mpg)+
#   geom_point(mapping = aes(x=displ,y=hwy),color='blue')
# 
# ggplot(data = mpg)+
#   geom_smooth(mapping = aes(x=displ,y=hwy))
# 
# ggplot(data = mpg)+
#   geom_smooth(mapping = aes(x=displ,y=hwy),color='orange')
# 
# ggplot(data = mpg)+
#   geom_smooth(mapping = aes(x=displ,y=hwy,linetype=drv,color=drv))+
#   geom_point(mapping = aes(x=displ,y=hwy,color=drv))
# 
# ggplot(data = mpg)+
#   geom_smooth(mapping = aes(x=displ,y=hwy,group=drv),color='orange')
# 
# ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+
#   geom_smooth(mapping = aes(linetype=drv,color=drv))+
#   geom_point(mapping = aes(color=drv))
# 
# ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+
#   geom_smooth(data = filter(mpg,class=='subcompact'))+
#   geom_point(mapping = aes(color=class))
# 
# ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+
#   geom_smooth(data = filter(mpg,class=='subcompact'),se=FALSE)+
#   geom_point(mapping = aes(color=class))
# 
# ggplot(data = mpg,mapping = aes(x=displ,y=hwy,color=drv))+
#   geom_point()+
#   geom_smooth(se=FALSE)
# 
# # Statistical transformations
# 
# head(diamonds,5)
# view(diamonds)
# #Bar chart
# ggplot(data = diamonds,mapping = aes(x=cut))+
#   geom_bar()
# 
# ggplot(data = diamonds,mapping = aes(x=cut))+
#   stat_count()
# 
# ?tribble
# a <- 1:5
# tibble(a,a^2)
# trial <- tribble(
#   ~a,    ~b,
#   'bar1',25,
#   'bar2',35,
#   'bar3',75
#   )
# ggplot(data = trial,mapping = aes(x=a,y=b))+
#   geom_bar(stat = 'identity')
# 
# ggplot(data = diamonds,mapping = aes(x=cut))+
#   geom_bar(mapping = aes(y=..prop..,group=1))
# 
# ggplot(data = diamonds,mapping = aes(x=cut))+
#   geom_bar(mapping = aes(y=..prop..,group=price))
# 
# ggplot(data = diamonds,mapping = aes(x=cut,y=depth))+
#   stat_summary(fun.max = max,fun.min = min,fun = median)
# 
# ggplot(data = diamonds,mapping = aes(x=cut,y=price))+
#   stat_summary(fun.max = max,fun.min = min,fun = mean)
# ?stat_summary
# #Position Adjustments
# ggplot(data = mpg)+
#   geom_bar(mapping = aes(x=hwy),color='blue')
# 
# ggplot(data = mpg)+
#   geom_bar(mapping = aes(x=hwy,fill='yellow'))
# 
# ggplot(data = mpg)+
#   geom_bar(mapping = aes(x=hwy,fill=drv))
# 
# ggplot(data = diamonds)+
#   geom_bar(mapping = aes(x=cut,fill=cut))
# 
# ggplot(data = diamonds,mapping = aes(x=cut,color=cut))+
#   geom_bar(fill=NA,position='identity')
# 
# ggplot(data = diamonds,mapping = aes(x=cut,fill=clarity))+
#   geom_bar(position='dodge')
# 
# ggplot(data = diamonds,mapping = aes(x=cut,fill=clarity))+
#   geom_bar(alpha=1/5,position='identity')
# 
# ggplot(data = diamonds,mapping = aes(x=cut,fill=clarity))+
#   geom_bar(position='fill')
# 
# ggplot(data = mpg,mapping = aes(x=displ,y=hwy))+
#   geom_point(position='jitter')
# 
# ?geom_count
# ?geom_jitter
# ?geom_boxplot
# 
# ggplot(data = mpg)+
#   geom_boxplot(mapping = aes(x=hwy,y=drv))
# 
# #Coordinate Systems
# 
# ggplot(data = mpg)+
#   geom_boxplot(mapping = aes(y=hwy,x=class))+
#   coord_flip()
# 
# ggplot(data = diamonds)+
#   geom_bar(mapping = aes(x=cut))+
#   coord_flip()
# 
# rw<-map_data("nz")
# ggplot(data=rw,mapping = aes(long,lat,group=group))+
#   geom_polygon(fill='white',color='magenta')
# 
# ggplot(data=rw,mapping = aes(long,lat,group=group))+
#   geom_polygon(fill='white',color='magenta')+
#   coord_quickmap()
# 
# ?labs
# 
# bar<-ggplot(data = diamonds)+
#   geom_bar(mapping = aes(x=cut,fill=cut))
# 
# bar + coord_flip()
# bar+coord_polar()
# 
# # install.packages("mapproj")
# require("mapproj")
# require("maps")
# mapy<-ggplot(data=rw,mapping = aes(long,lat,group=group))+
#   geom_polygon(fill='white',color='magenta')
#   
# mapy+coord_map()
# ?coord_fixed
# ?geom_abline
# 
# 
# ggplot(data = mpg,aes(x=cty,y=hwy))+
#   geom_point()+
#   geom_abline()+
#   coord_fixed()
# 
# #The Layered Grammar of Graphics
# #########################################
# 
# #####################
# #2.Workflow:Basics###
# #####################
# # this_is_a_really_long_name <- 2.5
# # this_is_a_really_long_name
# # This_is_a_really_long_name
# r_rocks <- 2^3
# seq(1,10)
# x <- "hello world"
# (y <- seq(1,10,length.out=5))



##################################
#3.Data Transformation with dplyr#
##################################

require(tidyverse)
# install.packages("nycflights13")
require(nycflights13)

 
head(flights,5)

# view(flights)
View(flights)
flights%>%
  arrange(dep_time)
flights%>%
  filter(month==1,day==1)
flights%>%
  filter(month %in% c(11,10))
tribble(
  ~x,
  1,
  NA,
  4
)
tibble(x = c(1,NA,4))

flights%>%
  filter(arr_delay>=2)%>%
  select(flight,dep_time,dep_delay,arr_delay,arr_time)

flights%>%
  filter(between(dep_time,000,600))%>%
  arrange(desc(dep_time))%>%
  select(flight,dep_time,dep_delay,arr_delay,arr_time)


# head(flights,5)
# 
# # view(flights)
# View(flights)
   

# Filter()
(jan1u <- flights%>%
  filter(day==22,month==10))
jan1 <- filter(flights,month==1,day==1)
   
View(jan1)
(dec25 <- filter(flights,month==12,day==25))
# page 48
View(jan1)
filter(flights,month ==10|month==5)
octmay <- filter(flights,month %in% c(5,10))
View(octmay)
   
#View(jan1)
(dec25 <- filter(flights,month==12,day==25))
# page 48
#View(jan1)
filter(flights,month ==10|month==5)
octmay <- filter(flights,month %in% c(5,10))
#View(octmay)
   
sum(is.na(flights))
df <- tibble(x=c(1,NA,3))
filter(df,x>1)
filter(df,is.na(x))
filter(df,is.na(x)|x>1)
#filter exercise
arr_delay0 <- filter(flights,arr_delay >=120)
View(arr_delay0)

dest_hous <- filter(flights,dest %in% c('IAH','HOU'))
View(dest_hous)

# unique(flights[c('carrier')])
carrier_air <- filter(flights,carrier %in% c('UA','AA','DL'))
View(carrier_air)

dep_time <- filter(flights,month %in% c(7,8,9))
View(dep_time)

arr_left <- filter(flights,arr_delay > 120 & dep_delay <= 0)
View(arr_left)

delayed_made <- filter(flights,dep_delay >= 60 & air_time >= 30)
View(delayed_made)

midnight_dep <- filter(flights,dep_time <= 600)
View(midnight_dep)

# ?between

missing_deptime <- filter(flights,is.na(dep_time))
View(missing_deptime)
(missing_deptimeu <- flights%>%
    filter(is.na(dep_time)))
# NA^0
# NA*0

#Arrange()
?arrange
arrange(flights,year,month,desc(day))
(flights%>%
    arrange(year,month,desc(day)))
View(arrange(flights,desc(is.na(dep_time))))

View(arrange(flights,desc(dep_delay)))

View(arrange(flights,dep_time))

View(arrange(flights,air_time))

View(arrange(flights,distance))

View(arrange(flights,desc(distance)))

# Select()
?select
select(flights,year,air_time,distance)
(flights%>%
    select(year,air_time,distance))
select(flights,year:dep_delay)

select(flights,-(year:dep_delay))

View(rename(flights,tail_num=tailnum))

View(select(flights,air_time,everything()))

View(select(flights,air_time,dep_time,air_time))

# ?select

vars <- c(
  "year", "month", "day", "dep_delay", "arr_delay"
)

View(select(flights,one_of(vars)))

View(select(flights,contains('TIME')))


#Add a new variable with mutate()

flights_sml <- select(flights,year:day,ends_with('delay'),air_time,distance)
View(flights_sml)

View(mutate(flights_sml,
       gain=arr_delay - dep_delay,
       speed=distance/air_time*60,
       hours=air_time/60,
       gain_per_hour = gain/hours))
(flights_sml%>%
  mutate(gain=arr_delay - dep_delay,
         speed=distance/air_time*60,
         hours=air_time/60,
         gain_per_hour = gain/hours))
View(transmute(flights_sml,
            gain=arr_delay - dep_delay,
            speed=distance/air_time*60,
            hours=air_time/60,
            gain_per_hour = gain/hours))

#Useful creation Functions

   
View(transmute(flights,
               air_time,
               hours=air_time%/%60,
               mins=air_time%%60))

(x <- 1:10)
lag(x)
lead(x)
(x-lag(x))
(x-lead(x))

cumsum(x)
cummin(x)
cumprod(x)
cummean(x)
cummax(x)
y <- c(1,2,2,NA,3,4)
min_rank(y)
row_number(y)

View(transmute(flights,
          dep_time,
          sched_dep_time,
          depy = (dep_time%/%100)*60 + (dep_time%%100),
          sch_depy = (sched_dep_time%/%100)*60 + (sched_dep_time%%100)))

View(transmute(flights,
               air_time,
               dep_time,
               arr_time,
               diff = (arr_time-dep_time),
               compare_two = (diff != air_time)))

View(transmute(flights,
               air_time,
               dep_time,
               arr_time,
               depy = (dep_time%/%100)*60 + (dep_time%%100),
               arry = (arr_time%/%100)*60 + (arr_time%%100),
               diff = (arry-depy),
               compare_two = (diff != air_time)))

View(transmute(flights,dep_delay,ranky=min_rank(dep_delay)))
View(arrange(transmute(flights,dep_delay,ranky=row_number(dep_delay)),ranky))

#Grouped Summaries with summarize()

summarize(flights,delay=mean(dep_delay,na.rm=TRUE))
(by_day <- group_by(flights,year,month,day))
view(summarize(by_day,delay=mean(dep_delay,na.rm=TRUE)))


View(group_by(flights,year,month,day)%>%
  summarize(delay=mean(dep_delay,na.rm=TRUE)))
   
# View(transmute(flights,
#                air_time,
#                hours=air_time%/%60,
#                mins=air_time%%60))
# 
# (x <- 1:10)
# lag(x)
# lead(x)
# (x-lag(x))
# (x-lead(x))
# 
# cumsum(x)
# cummin(x)
# cumprod(x)
# cummean(x)
# cummax(x)
# y <- c(1,2,2,NA,3,4)
# min_rank(y)
# row_number(y)
# 
# View(transmute(flights,
#           dep_time,
#           sched_dep_time,
#           depy = (dep_time%/%100)*60 + (dep_time%%100),
#           sch_depy = (sched_dep_time%/%100)*60 + (sched_dep_time%%100)))
# 
# View(transmute(flights,
#                air_time,
#                dep_time,
#                arr_time,
#                diff = (arr_time-dep_time),
#                compare_two = (diff != air_time)))
# 
# View(transmute(flights,
#                air_time,
#                dep_time,
#                arr_time,
#                depy = (dep_time%/%100)*60 + (dep_time%%100),
#                arry = (arr_time%/%100)*60 + (arr_time%%100),
#                diff = (arry-depy),
#                compare_two = (diff != air_time)))
# 
# View(transmute(flights,dep_delay,ranky=min_rank(dep_delay)))
# View(arrange(transmute(flights,dep_delay,ranky=row_number(dep_delay)),ranky))
# 
# #Grouped Summaries with summarize()
# 
# summarize(flights,delay=mean(dep_delay,na.rm=TRUE))
# (by_day <- group_by(flights,year,month,day))
# view(summarize(by_day,delay=mean(dep_delay,na.rm=TRUE)))
# 
# 
# View(group_by(flights,year,month,day)%>%
#   summarize(delay=mean(dep_delay,na.rm=TRUE)))
   

#Combining Multiple Operations with the Pipe

by_dest <- group_by(flights,dest)
delay <- summarise(by_dest,
                   count=n(),
                   dist=mean(distance,na.rm=TRUE),
                   delay=mean(arr_delay,na.rm=TRUE))
delay <- filter(delay,count>=20,dest != 'HNL')
   
View(delay)
   
#View(delay)
   

ggplot(delay,mapping = aes(x=dist,y=delay))+
  geom_point(mapping = aes(size=count),alpha=1/3)+
  geom_smooth(se=FALSE)

#using pipe %>%

delay <- flights%>%
  group_by(dest)%>%
  summarise(count=n(),
            dist=mean(distance,na.rm=TRUE),
            delay=mean(arr_delay,na.rm=TRUE))%>%
  filter(count>=20,dest != 'HNL')
   
View(delay)

View(flights%>%
       group_by(year,month,day)%>%
       summarise(mean=mean(dep_delay)))

View(flights%>%
       group_by(year,month,day)%>%
       summarise(mean=mean(dep_delay,na.rm=TRUE)))
   
# View(delay)
# 
# View(flights%>%
#        group_by(year,month,day)%>%
#        summarise(mean=mean(dep_delay)))
# 
# View(flights%>%
#        group_by(year,month,day)%>%
#        summarise(mean=mean(dep_delay,na.rm=TRUE)))
   
not_cancelled <- flights%>%
  filter(!is.na(dep_delay),!is.na(arr_delay))

not_cancelled%>%
  group_by(year,month,day)%>%
  summarise(mean=mean(dep_delay))

   
View(select(flights,tailnum,everything())%>%
       group_by(tailnum)%>%
       summarise(count=n()))
   
#View(select(flights,tailnum,everything())%>%
       # group_by(tailnum)%>%
       # summarise(count=n()))
   

delays <- not_cancelled%>%
            group_by(tailnum)%>%
            summarise(delay=mean(arr_delay))

   
View(delays)
   
#View(delays)
   

ggplot(data = delays)+
  geom_freqpoly(mapping = aes(x=delay),binwidth=10)

delays <- not_cancelled%>%
  group_by(tailnum)%>%
  summarise(delay=mean(arr_delay,na.rm=TRUE),
            count=n())

ggplot(data = delays,mapping = aes(y=delay,x=count))+
  geom_point(alpha=1/10)
ggplot(data = delays,mapping = aes(x=delay,y=count))+
  geom_point(alpha=1/10)+
  coord_flip()
delays%>%
  filter(count>25)%>%
  ggplot(mapping = aes(x=count,y=delay))+
    geom_point(alpha=1/10)

# install.packages("Lahman")

require(Lahman)
?as_tibble
view(Batting)
batting <- as_tibble(Batting)
View(batting)

batters <- batting%>%
            group_by(playerID)%>%
            summarise(ba=sum(H,na.rm = TRUE)/sum(AB,na.rm = TRUE),
                      ab=sum(AB,na.rm = TRUE))

batters %>% 
  filter(ab > 100) %>%
  ggplot(mapping = aes(x=ab,y=ba))+
    geom_point()+
    geom_smooth(se=FALSE)

batters %>%
  arrange(desc(ba))%>%
  filter(ab>100)%>%
  ggplot(mapping = aes(x=ab,y=ba))+
    geom_point()+
    geom_smooth(se=FALSE)

#Summary Functions

#measures of location

View(not_cancelled %>%
       group_by(year,month,day)%>%
       summarise(avg_delay1=mean(arr_delay),
                 avg_delay2=mean(arr_delay[arr_delay>0])))

#measures of spread

View(not_cancelled %>%
       group_by(dest)%>%
       summarise(distance_sd = sd(distance)) %>%
       arrange(desc(distance_sd)))

#measures of rank

View(not_cancelled %>%
       group_by(year,month,day)%>%
       summarise(first=min(dep_time),
                 last=max(dep_time)))

# measure of position

View(not_cancelled %>%
       group_by(year,month,day)%>%
       summarise(first_dep=first(dep_time),
                 last_dep=last(dep_time)))

   
View(not_cancelled %>%
       group_by(year,month,day)%>%
       mutate(r=min_rank(desc(dep_time)))%>%
       filter(r %in% range(r)))

#counts

View(not_cancelled %>%
       group_by(dest)%>%
       summarise(carriers = n_distinct(carrier)) %>%
       arrange(desc(carriers)))

View(not_cancelled %>%
       count(dest))
view(not_cancelled %>%
  group_by(dest)%>%
  summarise(count = n())%>%
  arrange(desc(count)))

View(not_cancelled %>%
       count(tailnum,wt=distance))

View(not_cancelled %>%
       group_by(year,month,day)%>%
       summarise(n_early = sum(dep_time<500)))

View(not_cancelled %>%
       group_by(year,month,day)%>%
       summarise(hour_perc = mean(arr_delay>60)))
   
# View(not_cancelled %>%
#        group_by(year,month,day)%>%
#        mutate(r=min_rank(desc(dep_time)))%>%
#        filter(r %in% range(r)))
# 
# #counts
# 
# View(not_cancelled %>%
#        group_by(dest)%>%
#        summarise(carriers = n_distinct(carrier)) %>%
#        arrange(desc(carriers)))
# 
# View(not_cancelled %>%
#        count(dest))
# view(not_cancelled %>%
#   group_by(dest)%>%
#   summarise(count = n())%>%
#   arrange(desc(count)))
# 
# View(not_cancelled %>%
#        count(tailnum,wt=distance))
# 
# View(not_cancelled %>%
#        group_by(year,month,day)%>%
#        summarise(n_early = sum(dep_time<500)))
# 
# View(not_cancelled %>%
#        group_by(year,month,day)%>%
#        summarise(hour_perc = mean(arr_delay>60)))
   

# Grouping by multiple values

daily <- group_by(flights,year,month,day)
(per_day <- summarise(daily,flights_count=n()))

(per_month <- summarise(per_day,flights_count=sum(flights_count)))

(per_year <- summarise(per_month,flights_count=sum(flights_count)))

#Ungrouping

   
View(daily %>%
  ungroup() %>%
  summarise(flights=n()))
view(flights)

view(filter(flights,arr_delay == -15, dep_delay == -15))

View(flights %>%
       group_by(tailnum)%>%
       summarise(mean_arr = mean(!is.na(arr_delay[arr_delay == -15])))%>%
       filter(mean_arr==0.5))

View(not_cancelled%>%
       count(dest))
View(not_cancelled%>%
       group_by(dest)%>%
       summarise(count=n()))

View(not_cancelled%>%
       count(tailnum, wt=distance))
View(not_cancelled%>%
       group_by(tailnum)%>%
       summarise(distancy=sum(distance)))

View(flights %>%
       group_by(year,month,day)%>%
       summarise(canc = mean((is.na(arr_delay)|is.na(dep_delay))),
                 counta = n()))
   
# View(daily %>%
#   ungroup() %>%
#   summarise(flights=n()))
# view(flights)
# 
# view(filter(flights,arr_delay == -15, dep_delay == -15))
# 
# View(flights %>%
#        group_by(tailnum)%>%
#        summarise(mean_arr = mean(!is.na(arr_delay[arr_delay == -15])))%>%
#        filter(mean_arr==0.5))
# 
# View(not_cancelled%>%
#        count(dest))
# View(not_cancelled%>%
#        group_by(dest)%>%
#        summarise(count=n()))
# 
# View(not_cancelled%>%
#        count(tailnum, wt=distance))
# View(not_cancelled%>%
#        group_by(tailnum)%>%
#        summarise(distancy=sum(distance)))
# 
# View(flights %>%
#        group_by(year,month,day)%>%
#        summarise(canc = mean((is.na(arr_delay)|is.na(dep_delay))),
#                  counta = n()))
   

flights %>%
  group_by(year,month,day)%>%
  summarise(canc = mean((is.na(arr_delay)|is.na(dep_delay))),
            counta = n())%>%
  ggplot(mapping = aes(x=counta,y=canc))+
    geom_point()


   
View(
  cancel_grouped <-
  flights%>%
       mutate(cancelled = (is.na(arr_delay)|is.na(dep_delay)))%>%
       group_by(year,month,day)%>%
       summarise(canc_prop = mean(cancelled),
                 avg_dep_del = mean(dep_delay,na.rm=TRUE),
                 avg_arr_del = mean(arr_delay,na.rm=TRUE)))

View(
  cancel_ungrouped <- 
  flights%>%
       mutate(cancelled = (is.na(arr_delay)|is.na(dep_delay)))%>%
       group_by(year,month,day)%>%
       summarise(canc_prop = mean(cancelled),
                 avg_dep_del = mean(dep_delay,na.rm=TRUE),
                 avg_arr_del = mean(arr_delay,na.rm=TRUE))%>%
       ungroup())
   
# View(
#   cancel_grouped <-
#   flights%>%
#        mutate(cancelled = (is.na(arr_delay)|is.na(dep_delay)))%>%
#        group_by(year,month,day)%>%
#        summarise(canc_prop = mean(cancelled),
#                  avg_dep_del = mean(dep_delay,na.rm=TRUE),
#                  avg_arr_del = mean(arr_delay,na.rm=TRUE)))
# 
# View(
#   cancel_ungrouped <- 
#   flights%>%
#        mutate(cancelled = (is.na(arr_delay)|is.na(dep_delay)))%>%
#        group_by(year,month,day)%>%
#        summarise(canc_prop = mean(cancelled),
#                  avg_dep_del = mean(dep_delay,na.rm=TRUE),
#                  avg_arr_del = mean(arr_delay,na.rm=TRUE))%>%
#        ungroup())
   

cancel_ungrouped %>%
  ggplot(mapping = aes(x=avg_dep_del,y=canc_prop))+
    geom_point()


   
View(flights %>%
  filter(between(dep_delay,left = 0,right = 60))%>%
    group_by(tailnum)%>%
    count())

View(flights%>%
       group_by(carrier,dest)%>%
       summarise(n(),
                 max_delay = max(dep_delay,na.rm = TRUE),
                 delay_count = mean(dep_delay,na.rm=TRUE))%>%
       arrange(desc(delay_count)))
?count

# Grouped Mutates (and Filters)

View(flights_sml%>%
       group_by(year,month,day)%>%
       filter(rank(desc(arr_delay))<10))

View(popular_dest <- flights%>%
       group_by(dest)%>%
       filter(n()>365))
   
# View(flights %>%
#   filter(between(dep_delay,left = 0,right = 60))%>%
#     group_by(tailnum)%>%
#     count())
# 
# View(flights%>%
#        group_by(carrier,dest)%>%
#        summarise(n(),
#                  max_delay = max(dep_delay,na.rm = TRUE),
#                  delay_count = mean(dep_delay,na.rm=TRUE))%>%
#        arrange(desc(delay_count)))
# ?count

# Grouped Mutates (and Filters)

# View(flights_sml%>%
#        group_by(year,month,day)%>%
#        filter(rank(desc(arr_delay))<10))
# 
# View(popular_dest <- flights%>%
#        group_by(dest)%>%
#        filter(n()>365))
   
length(flights)
nrow(flights)
ncol(flights)
#page 74
   
View(flights%>%
       group_by(year,month,day)%>%
       filter(rank(desc(dep_delay))<=5))

View(popular_dest%>%
  filter(arr_delay > 0)%>%
  mutate(pop_del = arr_delay / sum(arr_delay))%>%
  select(year:day,dest,arr_delay,pop_del))

vignette('window-functions')

View(flights%>%
       group_by(year,month,day,tailnum)%>%
       filter(rank(arr_delay)<=5)%>%
       select(year:day,tailnum,arr_delay))
   
# View(flights%>%
#        group_by(year,month,day)%>%
#        filter(rank(desc(dep_delay))<=5))
# 
# View(popular_dest%>%
#   filter(arr_delay > 0)%>%
#   mutate(pop_del = arr_delay / sum(arr_delay))%>%
#   select(year:day,dest,arr_delay,pop_del))
# 
# vignette('window-functions')
# 
# View(flights%>%
#        group_by(year,month,day,tailnum)%>%
#        filter(rank(arr_delay)<=5)%>%
#        select(year:day,tailnum,arr_delay))
   
#dont skip the exercises!!



# Exploratory Data Analysis

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut))

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut,fill=cut))
?geom_bar
View(diamonds %>%
  group_by(cut)%>%
  summarise(count=n()))

ggplot(data=diamonds)+
  geom_histogram(mapping = aes(x=carat),binwidth = 0.5)

ggplot(data=diamonds)+
  geom_histogram(mapping = aes(x=carat),binwidth = 0.5)+
  geom_freqpoly(mapping = aes(x=carat),color='blue')

   
View(diamonds %>%
  count(cut_width(carat,0.5)))

View(diamonds %>%
       count(cut_width(carat,0.2)))

diamonds %>%
  filter(carat<3)%>%
  ggplot(mapping = aes(x=carat))+
    geom_histogram(binwidth = 0.1)


diamonds %>%
  filter(carat<3)%>%
  ggplot(mapping = aes(x=carat,color=cut))+
    geom_freqpoly(binwidth = 0.1)

diamonds %>%
  filter(carat<3)%>%
  ggplot(mapping = aes(x=carat))+
    geom_freqpoly(binwidth = 0.1)

diamonds %>%
  filter(carat<3)%>%
  ggplot(mapping = aes(x=carat))+
    geom_histogram(binwidth = 0.01)

diamonds %>%
  filter(carat<3)%>%
  ggplot(mapping = aes(x=carat,color=cut))+
    geom_histogram(binwidth = 0.01)

View(faithful)

ggplot(data = diamonds,mapping = aes(x=y))+
  geom_histogram(binwidth = 0.25)+
  coord_cartesian(ylim = c(0,50))
   
# View(diamonds %>%
#   count(cut_width(carat,0.5)))
# 
# View(diamonds %>%
#        count(cut_width(carat,0.2)))
# 
# diamonds %>%
#   filter(carat<3)%>%
#   ggplot(mapping = aes(x=carat))+
#     geom_histogram(binwidth = 0.1)
# 
# 
# diamonds %>%
#   filter(carat<3)%>%
#   ggplot(mapping = aes(x=carat,color=cut))+
#     geom_freqpoly(binwidth = 0.1)
# 
# diamonds %>%
#   filter(carat<3)%>%
#   ggplot(mapping = aes(x=carat))+
#     geom_freqpoly(binwidth = 0.1)
# 
# diamonds %>%
#   filter(carat<3)%>%
#   ggplot(mapping = aes(x=carat))+
#     geom_histogram(binwidth = 0.01)
# 
# diamonds %>%
#   filter(carat<3)%>%
#   ggplot(mapping = aes(x=carat,color=cut))+
#     geom_histogram(binwidth = 0.01)
# 
# View(faithful)
# 
# ggplot(data = diamonds,mapping = aes(x=y))+
#   geom_histogram(binwidth = 0.25)+
#   coord_cartesian(ylim = c(0,50))
   

View(unusual <- diamonds%>%
                  filter(y<3|y>20)%>%
                  arrange(y))

#Exercises

ggplot(data = diamonds)+
  geom_freqpoly(mapping = aes(x=x,color='blue'),binwidth=0.25)+
  geom_freqpoly(mapping = aes(x=y,color='green',binwidth=0.25))+
  geom_freqpoly(mapping = aes(x=z,color='magenta',binwidth=0.25))

ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=x),binwidth=0.25)
  
ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=y),binwidth=0.25)

ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=z),binwidth=0.25)

View(diamonds)

ggplot(data = diamonds)+
  geom_histogram(mapping = aes(x=price),binwidth=500)

View(diamonds%>%
  filter(carat==0.99|carat==1)%>%
  count(carat))
#page 91
head(batters,12)
head(not_cancelled,4)
filter(diamonds,between(y,5,15))
diamonds2 <- diamonds%>%
  mutate(y=ifelse(y<3|y>20,NA,y))
sum(is.na(diamonds2$y))
sum(is.na(diamonds$y))
   
sum(is.na(diamonds2[y]))
   
   
ggplot(data = diamonds2)+
  geom_point(mapping = aes(x=x,y=y))

ggplot(data = diamonds2)+
  geom_point(mapping = aes(x=x,y=y),na.rm = TRUE)
flights%>%
  mutate(cancelled=is.na(dep_time),
         sched_hour = sched_dep_time%/%100,
         sched_min = sched_dep_time%%100,
         sched_time = sched_hour + sched_min/60)%>%
  ggplot(mapping = aes(sched_time))+
  geom_freqpoly(mapping=aes(color=cancelled),binwidth=1/4)
ggplot(data = flights)+
  geom_bar(mapping = aes(x=dep_time))
  
ggplot(data = flights)+
  geom_histogram(mapping = aes(x=dep_time))

#Covariation

flights%>%
  mutate(cancelled=is.na(dep_time),
         sched_hour = sched_dep_time%/%100,
         sched_min = sched_dep_time%%100,
         sched_time = sched_hour + sched_min/60)%>%
  ggplot(mapping = aes(y=sched_time,x=dep_delay))+
  geom_point(mapping=aes(color=cancelled))

ggplot(data = diamonds,mapping = aes(x = price))+
  geom_freqpoly(mapping = aes(color=cut))

ggplot(data = diamonds,mapping = aes(x = price,y=..density..))+
  geom_freqpoly(mapping = aes(color=cut),binwidth=500)

ggplot(data = diamonds,mapping = aes(x = price,y=cut))+
  geom_boxplot()

ggplot(data = mpg)+
  geom_boxplot(mapping = aes(
    x=reorder(class,hwy,FUN = median),
    y=hwy
  ))+coord_flip()

?reorder

#exercises

flights%>%
  mutate(cancelled=is.na(dep_time),
         sched_hour = sched_dep_time%/%100,
         sched_min = sched_dep_time%%100,
         sched_time = sched_hour + sched_min/60)%>%
  ggplot(mapping = aes(x=sched_time,y=..density..))+
  geom_freqpoly(mapping=aes(color=cancelled),binwidth=1/4)

ggplot(data = diamonds,mapping = aes(x=carat,y=..density..))+
  geom_freqpoly(aes(color=cut))

#install.packages('lvplot')
require(ggbeeswarm)
require(ggstance)
require(lvplot)

?`ggstance-ggproto`
ggplot(data = mpg)+
  geom_boxplot(mapping = aes(
    x=reorder(class,hwy,FUN = median),
    y=hwy
  ))+coord_flip()

ggplot(data = diamonds)+
  geom_lv(mapping = aes(
    y=price,
    x=reorder(cut,price,FUN = median),fill=..LV..
  ))+coord_flip()
?geom_lv


ggplot(data = diamonds,mapping = aes(x=carat,y=..density..))+
  geom_histogram(aes(fill=cut))

ggplot(data = diamonds,mapping = aes(x=carat,y=..density..))+
  geom_freqpoly(aes(color=cut))

ggplot(data = diamonds,mapping = aes(x=carat,y=cut))+
  geom_violin(aes(fill=cut))

?ggbeeswarm

#Two categorical variables

ggplot(data=diamonds)+
  geom_count(mapping=aes(x=cut,y=color))

diamonds%>%
  count(cut,color)%>%
  ggplot()+
  geom_tile(mapping = aes(x=color,y=cut,fill=n))
#101
?filter
view(flights)
flights%>%
  ggplot()+
  geom_tile(mapping = aes(x=month,y=dest,fill=dep_delay))
n_distinct(flights$dest)

# view(diamonds%>%
#        count(cut,color)%>%
#        group_by(cut)%>%
#        mutate(prop= n/sum(n)))

diamonds%>%
  count(cut,color)%>%
  group_by(cut)%>%
  mutate(prop= n/sum(n))%>%
  ggplot()+
  geom_tile(mapping = aes(x=color,y=cut,fill=prop))+
  coord_flip()

?factor
# kaki0<-flights%>%
#   group_by(month,dest)%>%
#   summarise(dep_delay=mean(dep_delay,na.rm=TRUE))%>%
#   group_by(dest)%>%
#   mutate(count=n())
flights%>%
  group_by(month,dest)%>%
  summarise(dep_delay=mean(dep_delay,na.rm=TRUE))%>%
  group_by(dest)%>%
  filter(n() == 12)%>%
  ungroup()%>%
  mutate(dest=reorder(dest,dep_delay))%>%
  ggplot()+
  geom_tile(mapping = aes(x=factor(month),y=dest,fill=dep_delay))
#page 101
ggplot(data=diamonds)+
  geom_point(mapping = aes(x=carat, y = price),alpha=1/100)

#using bin2d()
ggplot(data=diamonds)+
  geom_bin2d(mapping = aes(x=carat,y=price))
# install.packages("hexbin")
ggplot(data = diamonds)+
  geom_hex(mapping = aes(x=carat,y=price))
smaller<- diamonds%>%filter(carat<3)
ggplot(data = smaller,mapping = aes(x=carat,y=price))+
  geom_boxplot(mapping = aes(group=cut_width(carat,0.1)))
ggplot(data = smaller,mapping = aes(x=carat,y=price))+
  geom_boxplot(mapping = aes(group=cut_number(carat,20)))
view(smaller%>%group_by(cut,color)%>%summarise(color_count=n()))

#Exercises

ggplot(data = smaller,mapping = aes(x=price))+
  geom_freqpoly()
?geom_freqpoly
# ggplot(data = smaller,mapping = aes(x=carat))+
#   geom_histogram()+
#   facet_grid(.~price)
bigger <- diamonds%>%filter(carat>=3)     
ggplot(data = smaller,mapping = aes(color=cut_number(carat,20),x=price))+
  geom_freqpoly()

ggplot(data = smaller,mapping = aes(color=cut_width(carat,1,boundary = 0),x=price))+
  geom_freqpoly()

ggplot(data = smaller,mapping = aes(x=price))+
  geom_histogram()

ggplot(data = bigger,mapping = aes(x=price))+
  geom_histogram()

ggplot(data = smaller,mapping = aes(x=carat,y=cut_width(price,2000,boundary = 0)))+
         geom_boxplot(varwidth = TRUE)

ggplot(data = diamonds,mapping = aes(x=cut_number(carat,5),y=price,color=cut))+
  geom_boxplot()

ggplot(data = diamonds,mapping = aes(x=cut,y=price,color=cut_number(carat,7)))+
  geom_boxplot()

ggplot(data = diamonds,mapping = aes(x=carat,y=price))+
  geom_hex()+
  facet_wrap(~cut,ncol=1)

#Pattern and Models

ggplot(faithful)+
  geom_point(aes(eruptions,waiting))

#modelling example
?lm
library(modelr)
mod<-lm(log(price)~log(carat),data = diamonds)
str(mod)
diamonds3<-diamonds%>%
  add_residuals(mod)%>%
  mutate(resid=exp(resid))
view(diamonds3)
ggplot(diamonds3)+
  geom_point(aes(carat,resid))
ggplot(diamonds3)+
  geom_boxplot(aes(cut,resid))
ggplot(diamonds3)+
  geom_boxplot(aes(cut,resid,color=cut_number(carat,3)))
# ggsave("diamonds.pdf")
# write_csv(diamonds,"diamonds.csv")
#Chap 6 Workflow:Projects#
##########################
?geom_pointrange
ggplot(diamonds,mapping = aes(cut,depth))+
  geom_pointrange(aes(ymin=min(depth),ymax=max(depth)))

#Part II Wrangle#
#################

#Chapter 7 Tibbles with tibble#
###############################

data.frame(iris)
as_tibble(iris)
tribble(~a,~b,
        1, 3,
        3, 4)
?tribble
tibble(x=1:5,
       y=1,
       z=x^2 + y)
tb <- tibble(
  ':)'="smile",
  ' '="space",
  '2000'="number"
)
tb
#tribble--->transposed tibble
?runif
tibble(
  a = lubridate::now() + runif(1e3)*86400,
  b = lubridate::today() + runif(1e3)*30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters,1e3,replace = TRUE)
)
?lubridate

view(data.frame(
  a = lubridate::now() + runif(1e3)*86400,
  b = lubridate::today() + runif(1e3)*30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters,1e3,replace = TRUE)
))
flights%>%
  print(n=4,width=Inf)
df <- tibble(
  x=runif(5),
  y=rnorm(5)
)
df[["x"]]
df$x
df%>%.$x
dff <- as.data.frame(df)
class(dff)
dff$x
dff[["x"]]
dff["x"]
df["x"]
mtcars
as_tibble(mtcars)

#Exercises

df <- data.frame(abc=1,xyz="a")
df$x
df[,"xyz"]
df[,c("abc","xyz")]
dft <- as_tibble(df)
dft$x
dft[,"xyz"]
dft[,c("abc","xyz")]
vari <- "mpg"
class(mtcars)
cara <- as_tibble(mtcars)
cara$vari$mpg
mtcars[[vari]]
cara[[vari]]
cara[vari]
mtcars[vari]
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
# ?tibble::enframe()
annoying$`1`
ggplot(annoying)+
  geom_point(aes(`1`,`2`))
annoying$`3` <- annoying$`2`/annoying$`1`
   
annoying<-annoying %>% select(-`\`3\``)
annoying
?tibble
   
#annoying<-annoying %>% select(-`\`3\``)
annoying
#?tibble
   
annoying<-annoying %>% rename(one="1",two="2",three="3")
annoying

#Chapter 8 Data Import with readr#
##################################page 151
   

   
# gh <- heights
# write_csv(gh,"data/heights.csv")
   
heights0 <- read_csv("data/heights.csv")
view(heights)
read_csv("a,b,c
         1,2,3
         4,5,6")
read_csv("The first line of metadata
         The second line of metadata
         x,y,z
         1,2,3",skip=2)
read_csv("# A comment I want to skip
         x,y,z
         1,2,3",comment="#")
view(df)
read_csv("1,2,3\n4,5,6",col_names =FALSE)
read_csv("a,b,c\n1,2,.",na=".")
?read_csv
read_delim("x,y\n1,'a,b'",delim = ",",quote = "")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n1,2\na,b")
str(parse_logical(c("TRUE","FALSE","NA")))
parse_double("1.32")
parse_double("1,32")
parse_double("1,32",locale = locale(decimal_mark = ","))
#page 137
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE","FALSE"))
str(parse_guess("2010-10-01"))
challlenge <-read_csv(readr_example("challenge.csv"))
challlenge
tail(challlenge,1000)
guess_parser(challlenge)
challlenge[999:1010,]
problems(challlenge)

#exercises#
##########
?locale

k <- parse_number("133,311.4",locale=locale(decimal_mark=".",grouping_mark = ","))
k
parse_time("172309",locale=locale(time_format = "%H%M%S"))
parse_date("00/12/23",locale=locale(date_format = "%y/%m/%d"))
date_names_langs()
parse_date("1 nyakanga 2015", "%d %B %Y", locale = locale("rw"))
?read_csv2
read_csv2(I("a;b\n1,0;2,0"))
#read_csv(I("a;b\n1,0;2,0"))
?I
parse_date("January 1, 2010","%B %d, %Y",locale=locale())
parse_date(c("August 19 (2015)", "July 1 (2015)"),"%B %d (%Y)")
parse_date("(2010)","%AD",locale=locale())


#Parsing a file#
###############
read_csv(readr_example("challenge.csv"))
problems(read_csv(readr_example("challenge.csv")))
chall <- read_csv(readr_example("challenge.csv"),
                  col_types = cols(
                    x=col_double(),
                    y=col_date()
                  ))
problems(chall)
tail(chall)

chall2 <- read_csv(readr_example("challenge.csv"),guess_max = 1001)
chall2
chall3 <- read_csv(readr_example("challenge.csv"),
                   col_types = cols(.default=col_character()))
chall3
type_convert(chall3)

#writing to a file#
##################

write_csv(chall3,"chally.csv")
chall3
read_csv("chally.csv")
write_rds(chall3,"chally1.rds")
read_rds("chally1.rds")
#install.packages("feather")
library(feather)
write_feather(chall3,"chally2.feather")
read_feather("chally2.feather")


#chap 9 Tidy data with tidyr#
############################

table1
view(table2)
view(table4a)
table3
table1%>% mutate(rate=cases/population*10000)
table1%>%count(year,wt=cases)

ggplot(data=table1,aes(year,cases))+
  geom_line(aes(group=country),color="grey50")+
  geom_point(aes(color=country))

#exercises#
##########


table2$casesy <- ifelse(table2$type=="cases",table2$count,0)
table2$pop <- ifelse(table2$type=="population",table2$count,0)
df_tab<-table2%>%select(country,year,casesy,pop)%>%group_by(country,year)%>%summarise(casesy=sum(casesy),
                                                                              pop=sum(pop))
view(df_tab)
#page 151
table2
tab_cas<-table2%>%filter(table2$type=="cases")%>%rename(cases=count)%>%
  arrange(country,year)
tab_cas
tab_pop <- table2%>%filter(table2$type=="population")%>%
  rename(population=count)%>%
  arrange(country,year)
tab_pop
?bind_rows
tab_cas_pop <- tibble("country" = tab_cas$country,
                         "year" = tab_cas$year,
                         "cases" = tab_cas$cases,
                         "population" = tab_pop$population)
view(tab_cas_pop)
#bind_rows(table2,tab_cas_pop)
view(table2)
view(table1)
ggplot(table2[table2$type=="cases",],aes(year,count))+
  geom_line(aes(group=country))+
  geom_point(aes(color=country))

#spreading and gathering#
########################
#page 151 

#gather#
#######

# use this when you have values as columns

view(table4a)
gather(table4a,c('1999','2000'),key="year",value = "cases")
table4a%>%gather('1999','2000',key = "year",value = "cases")
view(table4b)
table4b%>%gather('1999','2000',key = "year",value = "population")
tidy4a <- table4a%>%gather('1999','2000',key = "year",value = "cases")
tidy4b <- table4b%>%gather('1999','2000',key = "year",value = "population")

#spreading#
##########

# use this when you have column names as values

table2%>%spread(key = "type",value = "count")
table2a <- table2%>%select(country,year,type,count)

tidy2b <- table2a%>%spread(key="type",value="count")
view(tidy2b)

#exercises#
##########
stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c( 1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks
stock_ex <- stocks%>%spread(key = "year",value = "return")%>%
  gather('2015','2016',key = "year",value = "return",convert = TRUE)
#stock_ex$year<-as.integer(stock_ex$year)
stock_ex
people <- tribble(
  ~name, ~key, ~value,
  #-----------------|--------|------
  "Phillip Woods", "age", 45,
  "Phillip Woods", "height", 186,
  "Phillip Woods", "age", 50,
  "Jessica Cordero", "age", 37,
  "Jessica Cordero", "height", 156
)
?spread
people
people$id_num <- c(1:5)
people%>%spread(key = "key",value = "value",fill = 0)

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)
preg
preg%>%gather(male,female,key = "sex",value = "count")

# separating and pull#
#####################
#page 156

#separate: pulls apart one column into multiple columns.

view(table3)

separate(table3,col = rate,into = c("cases","population"),sep = "/",convert = TRUE)

#unite: 

table5
?unite

table5%>%unite(col = "year",century,year,sep = "")

#EXERCISES#
###########
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j"))%>%
  separate(x, c("one", "two", "three"),extra = "drop")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"),fill = "right")

#Missing values#
###############

   
   
stocks <- tibble(
  year = c(2015,2015,2015,2015,2016,2016,2016),
  qtr = c(1,2,3,4,2,3,4),
  return = c(1.88,0.59,0.35, NA,0.92,0.17,2.66)
)
stocks
stocks %>% spread(key="year",value = "return")%>%
  gather("2015","2016",key = "year",value = "return",na.rm = TRUE)
?complete
stocks%>%complete(year,qtr)
treatment <- tribble(
  ~person,   ~treatment, ~response,
  "Derrick Whitmore", 1,7,
  NA,   2,10,
  NA,   3,9,
  "Katherine Burke", 1,4
)
treatment
?fill
treatment %>% fill(person)

#Exercises#
##########

treatment %>% fill(person,.direction="up")

#case study#
###########

view(who)
#start with gather by making columns values
?gather
who1 <- who %>% gather(new_sp_m014:newrel_f65,key = "key",value = "cases",na.rm = TRUE)
view(who1)

#we count the values of the column key to check the structure of the values
require(stringr)
who1 %>% count(key)
who2<-who1 %>% mutate(key = str_replace(key,"newrel","new_rel"))
view(who2 %>% separate(key,c("status","type","demographic"),sep = "_")%>%separate(demographic,into = c("gender","age group"),sep = 1))
who3<-who2 %>% separate(key,c("status","type","demographic"),sep = "_")%>%separate(demographic,into = c("gender","age group"),sep = 1)
who3%>%count(status)
who4 <- who3%>%select(-status,-iso2,-iso3)
who4

#putting everything in a pipeline

who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(
    code = str_replace(code, "newrel", "new_rel")
  ) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)
#summarize by country
who_count <- who4%>%group_by(country)%>%
  summarise(value_sum = sum(cases))%>%
  select(country,value_sum)
who_count
?geom_bar
ggplot(who_count%>%filter(country%in%c("Rwanda","France","Kenya","United States of America","Canada")))+
  geom_bar(aes(country,value_sum,fill=country),stat = "identity")+
  coord_flip()+
  theme(legend.position = "none")
#page 168
#summarize by year
show_year <- who4%>%group_by(year,country,gender)%>%summarise(value_sum=sum(cases))%>%
  select(country,year,gender,value_sum)
who4
show_year
ggplot(show_year,aes(year,value_sum))+
  geom_line(aes(group=country,color=gender))


#summarize by sex
show_gender <- who4%>%group_by(gender)%>%
  summarise(value_sum=sum(cases))%>%
  select(gender,value_sum)
ggplot(show_gender,aes(gender,value_sum))+
  geom_bar(stat="identity",aes(fill=gender))+
  geom_text(aes(label=signif(value_sum)),vjust=-0.6)

#Nontidy Data#
#############

#you might not use tidy data because some 
#computations are more efficient if the data 
#is in a different format. 


#Chapter 10 Relational data with dplyr#
######################################

#mutating joins, filtering joins, set operations

colnames(airlines)
colnames(airports)
colnames(planes)
colnames(weather)
colnames(flights)

#keys: a variable or set of variables that uniquely identifies an observation.

#primary keys and foreign keys

planes%>%
  count(tailnum)%>%
  filter(n>1)

airlines%>%
  count(carrier)%>%
  filter(n>1)
#view(flights)
flights%>%
  count(flight)%>%
  filter(n>1)

airports%>%
  count(faa)%>%
  filter(n>1)

weather%>%
  count(year,month,day,hour,origin)%>%
  filter(n>1)
weather %>% 
  filter(year==2013 & month == 11 & day == 3 & hour ==1)

#surrogate key: an engineered key
flights_test <- flights 
flights_test$prim_key <- as.numeric(rownames(flights_test))
flights_test <- flights_test%>%
  select(prim_key,year:time_hour)
flights_test %>%
  count(prim_key)%>%
  filter(n>1)


# Mutating Joins
airlines

flights2<-flights%>%
  select(year:day,hour,origin,dest,tailnum,carrier)
flights2
flights2 %>%
  inner_join(airlines,by="carrier")

flights2 %>%
  left_join(airlines,by="carrier")

flights2 %>%
  select(-origin,-dest)%>%
  inner_join(airlines,by="carrier")

flights2 %>%
  select(-origin,-dest)%>%
  mutate(name=airlines$name[match(carrier,airlines$carrier)])

#understanding joins
x<-tibble(key=c(1,2,3),val_x=c("x1","x2","x3"))
y <- tribble(~key,  ~val_y,
             1,    "y1",
             2,    "y2",
             4,    "y3")
x
y
#table(x$key)
#?table
x%>%mutate(matchy=y$val_y[match(key,x$key)])
x%>%
  left_join(y,by="key")#unmatched rows are included in the results and keeps all observations in x.

x %>% 
  inner_join(y,by="key")#unmatched rows are not included in the results

x %>% 
  right_join(y,by="key")#unmatched rows are included in the results and keep all the observations in y!!

x %>% 
  full_join(y,by="key")#unmatched rows are included in the results and keep all observations in x and y

#Duplicate keys#
###############

x<-tibble(key=c(1,2,2,1),val_x=c("x1","x2","x3","x4"))
y <- tribble(~key,  ~val_y,
             1,    "y1",
             2,    "y2"
             )
x
y
x %>%
  inner_join(y,by="key")
x%>%
  left_join(y,by="key")


x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

x %>%
  left_join(y,by="key")


#Defining the Key columns

view(flights2 %>%
  left_join(weather))

view(flights2 %>%
       left_join(planes,by="tailnum"))

flights2 %>%
  left_join(airports,c("dest"="faa")) # the keys to match are  specified and the x key will be the one to be shown

flights2 %>%
  left_join(airports,c("origin"="faa")) # the keys to match are  specified and the x key will be the one to be shown
colnames(flights2)
colnames(flights)
fli1 <- flights%>%
  group_by(dest)%>%
  summarise(avg_delay=mean(dep_delay,na.rm=TRUE),
            avg_arr_delay=mean(arr_delay,na.rm=TRUE))
fli1
(delays <- airports %>%
            inner_join(fli1,by=c("faa"="dest")))%>%
   # drop_na(avg_arr_delay)%>%
                  ggplot(aes(lon,lat))+
                  borders("state")+
                  geom_point(aes(color=avg_arr_delay))+
                  coord_quickmap()
view(delays)
#page 213 
colnames(flights2)
colnames(airports)
view(flights %>%
  inner_join(airports,by=c("dest"="faa"))%>%
  inner_join(airports,by=c("origin"="faa")))

flights %>%
  select(flight_year=year,tailnum,arr_delay)%>%
  inner_join(planes,by="tailnum")%>%
  mutate(age=flight_year-year)%>%
  group_by(age)%>%
  summarise(avg_delay=mean(arr_delay,na.rm=TRUE))%>%
  ggplot(aes(age,avg_delay))+
  geom_point()+
  geom_line()
colnames(weather)

flights %>%
  left_join(weather,by=c("year","month","day","hour","origin"))%>%
    select(hour,temp,dewp,humid,wind_dir,wind_speed,wind_gust,precip,pressure,visib)%>%
  group_by(hour)%>%
    summarise(mean(temp,na.rm=TRUE),mean(dewp,na.rm=TRUE),mean(humid,na.rm=TRUE),mean(wind_dir,na.rm=TRUE))

flights %>%
  filter(year==2013,month==6,day==13)%>%
  group_by(dest)%>%
  summarise(delaysa=mean(arr_delay,na.rm=TRUE))%>%
  left_join(airports,by=c("dest"="faa"))%>%
  ggplot(aes(lon,lat))+
  borders("state")+
  geom_point(aes(size=delaysa,color=delaysa))+
  coord_quickmap()+
  scale_color_viridis_b()


#Filter joins: these affect the observations not variables

#semi_join: keeps all observations in x that have a match in y
#anti_join: drops all observations in x that have a match in y

top_destinations <- flights%>%
  count(dest,sort = TRUE)%>%
  head(10)
top_destinations

flights %>%
  filter(dest %in% top_destinations$dest)

flights %>%
  semi_join(top_destinations,by="dest")

unmatched_planes<-flights %>%
  anti_join(planes,by="tailnum")%>%
  count(tailnum,sort = TRUE)

#exercises#
##########

view(flights %>%
  filter(tailnum %in% unmatched_planes$tailnum))

sum(unmatched_planes$n)
view(flights %>%
  filter(tailnum %in% unmatched_planes$tailnum) %>%
  select(carrier,tailnum)%>%
  count(tailnum)%>%
  mutate(propo=n*100/sum(n)))

view(flights %>%
  left_join(planes)%>%
  select(dep_time,tailnum)%>%
  count(tailnum)%>%
  filter(n>=100)%>%
  inner_join(planes,by="tailnum"))

#install.packages("fueleconomy")

require(fueleconomy)

colnames(vehicles)
colnames(common)
view(vehicles %>%
  semi_join(common,by=c("make","model")))
view(vehicles)
worse_hour<-flights %>%
  group_by(year,month,day,hour)%>%
  summarise(delay_sum=sum(arr_delay))%>%
  drop_na(delay_sum)%>%
  arrange(delay_sum)%>%
  tail(48)%>%
  left_join(weather)# this is not correct

anti_join(flights,airports,by=c("dest"="faa"))
anti_join(airports,flights,by=c("faa"="dest"))
planes %>%
  anti_join(airlines)
colnames(airlines)
colnames(planes)
flights %>%
  inner_join(planes,by="tailnum")%>%
  inner_join(airlines)

#reject the hypothesis

worst_hour <- flights %>%
  mutate(hour_delay=dep_delay%/%100)%>%
  group_by(origin,month,day,hour)%>%
  summarise(avg_delay=mean(dep_delay,na.rm=TRUE))%>%
  ungroup()%>%
  arrange(desc(avg_delay))%>%
  slice(1:48)#very interesting

# Join Problems

#Set Operations#
###############

#intersect(x,y)
#union(x,y)
#setdiff(x,y)


#Chapter 11: Strings with stringr#
#################################

library(stringr)

x <- c("\"","\\")
x
writeLines(x)
"\u00b5"
#String Length
str_length(c("a","R for data science",NA))
str_c("x","y")
str_c("x","y",sep = ",")
x<-c("abc",NA)
str_c("|-",x,"-|")
str_c("|-",str_replace_na(x),"-|")
str_c("prefix",c("a","b","c"),"suffix")
name<- "Parfait"
time_of_day <- "afternoon"
birthday<-FALSE
str_c(
  "Good ", time_of_day," ",name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

str_c(c("x","y","z"),collapse = ",")

#Subsetting strings

x <- c("Apple","Banana","Pear")
str_sub(x,1,3)
str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1))
x

#Locales

str_to_upper(c("i","l"))
str_to_upper(c("i","l"),locale = "tr")
sort(x)
str_sort(x)
str_sort(x,locale="haw")

#Exercises#
##########

?paste
paste(c("abantu ","batatu ", "beza"))
paste0(c("abantu ","batatu ", "beza"))
?paste0
acc <- 564
paste("the number that I wrote is ",acc)
paste0("the number that he wrote is ",acc)
str_c("the number that he wrote is ",acc)
stringle <- function(l){
  if ((str_length(l) %/% 2) == 0 ){
    print(str_sub(l,str_length(l)/2,str_length(l)/2))
  }else{
    print(str_sub(l,(str_length(l) + 1)/2,(str_length(l) + 1)/2))
  }
}

funky <- function(c){
  return(c^2)
}
trial0 <- function(a,b,c) {
  result <- a * b + c
  print(result)
}

?str_wrap
?str_trim
str_wrap(c("in this world there is no free lunch","the field is looking somehow messy"))
str_trim(" in the car\t")
str_squish(" in the car ")
str_pad(" in this car ",6)
str_pad(c("a","b","c"))

#return after covering the function sections#
############################################

#Matching patterns with regular expressions#
###########################################

library("htmlwidgets")

x <- c("apple","banana","pear")

str_view(x,"an")

str_view(x,".a.")
dot <- "\\."
writeLines(dot)
str_view(c("abc","a.c","bef"),"a\\.c")
x <- "a\\b"
writeLines(x)
str_view("a\\b","\\\\")
x<-"\"\'\\"
writeLines(x)
str_view(x,"\\\"\\'\\\\")
x <- "\\..\\..\\.."
library(foreign)
writeLines(x)
str_view(x,"\\\\..\\\\..\\\\..")

#Anchors#
########

x <- c("apple","banana","pear")

str_view(x,"^a") #view the start
str_view(x,"a$")#view the end

x <- c("apple pie", "apple", "apple cake")
str_view(x,"apple")
str_view(x,"^apple$")

#use \b for boundary

#exercises#
##########

x <- "$^$"
writeLines(x)
str_view(x,"\\$\\^\\$")


words
str_view_all(words,"^y",match = TRUE)
str_view(words,"x$",match = TRUE)
str_view(words,"^...$",match=TRUE)
str_view(words,".......",match=TRUE)

# Character classes and Alternatives#
####################################

str_view(c("grey", "gray"), "gr(e|a)y")

str_view(c("grey", "gray"), "[^a]",match = TRUE)

str_view(words,"^(a|e|u|i|o)",match = TRUE)

str_view(words,"^(a|e|u|i|o)",match = FALSE)
str_view(words,"[^e]ed$",match = TRUE)

str_view(words,"(ing|ize)$",match = TRUE)
trialy <- c("1.5ha","0,002","3.5","9","1000hector")
trialy
str_view(trialy,"([0-9][.,])?[0-9]+",match = TRUE)
kk <- str_remove(trialy,"[^([0-9][.,])?[0-9]+]")
as_tibble(str_remove(kk,"[^([0-9][.,])?[0-9]+]"))
str_remove_all(trialy,"[^([0-9][.,])?[0-9]+]")

?str_remove_all
kukii<- as_tibble(str_remove_all(trialy,"[^([0-9][.,])?[0-9]+]"))
kukii%>%parse_number(as.character(kukii$value),locale = locale(decimal_mark = ","))
#if ()
is_character(1.324)
for (i in kukii$value){
  if(is_character(i)){
    print(parse_number(i))
  }
  else{
    print("nothing to show here")
  }
}

for (i in kukii$value){
  if(is_character(i)){
    print(parse_number(i))
  }
  else{
    print("nothing to show here")
  }
}
   
