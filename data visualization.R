#we will use ggplot2
# install.packages("tidyverse")
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


##################################
#3.Data Transformation with dplyr#
##################################

require(tidyverse)
# install.packages("nycflights13")
require(nycflights13)

head(flights,5)

# view(flights)
View(flights)

# Filter()

jan1 <- filter(flights,month==1,day==1)
View(jan1)
(dec25 <- filter(flights,month==12,day==25))
# page 48
View(jan1)
filter(flights,month ==10|month==5)
octmay <- filter(flights,month %in% c(5,10))
View(octmay)
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

# NA^0
# NA*0

#Arrange()
arrange(flights,year,month,desc(day))

View(arrange(flights,desc(is.na(dep_time))))

View(arrange(flights,desc(dep_delay)))

View(arrange(flights,dep_time))

View(arrange(flights,air_time))

View(arrange(flights,distance))

View(arrange(flights,desc(distance)))

# Select()

select(flights,year,air_time,distance)

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

#Combining Multiple Operations with the Pipe

by_dest <- group_by(flights,dest)
delay <- summarise(by_dest,
                   count=n(),
                   dist=mean(distance,na.rm=TRUE),
                   delay=mean(arr_delay,na.rm=TRUE))
delay <- filter(delay,count>=20,dest != 'HNL')
View(delay)

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
not_cancelled <- flights%>%
  filter(!is.na(dep_delay),!is.na(arr_delay))

not_cancelled%>%
  group_by(year,month,day)%>%
  summarise(mean=mean(dep_delay))

View(select(flights,tailnum,everything())%>%
       group_by(tailnum)%>%
       summarise(count=n()))

delays <- not_cancelled%>%
            group_by(tailnum)%>%
            summarise(delay=mean(arr_delay))

View(delays)

ggplot(data = delays)+
  geom_freqpoly(mapping = aes(x=delay),binwidth=10)

delays <- not_cancelled%>%
  group_by(tailnum)%>%
  summarise(delay=mean(arr_delay,na.rm=TRUE),
            count=n())

ggplot(data = delays,mapping = aes(y=delay,x=count))+
  geom_point(alpha=1/10)

delays%>%
  filter(count>25)%>%
  ggplot(mapping = aes(x=count,y=delay))+
    geom_point(alpha=1/10)

# install.packages("Lahman")

require(Lahman)
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

View(not_cancelled %>%
       count(tailnum,wt=distance))

View(not_cancelled %>%
       group_by(year,month,day)%>%
       summarise(n_early = sum(dep_time<500)))

View(not_cancelled %>%
       group_by(year,month,day)%>%
       summarise(hour_perc = mean(arr_delay>60)))

# Grouping by multiple values

daily <- group_by(flights,year,month,day)
(per_day <- summarise(daily,flights_count=n()))

(per_month <- summarise(per_day,flights_count=sum(flights_count)))

(per_year <- summarise(per_month,flights_count=sum(flights_count)))

#Ungrouping

View(daily %>%
  ungroup() %>%
  summarise(flights=n()))
