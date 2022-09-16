library(dplyr)
library(haven)
library(readr)
library(readxl)
df <- read_dta("data/dataset_join.dta")
df%>%
  group_by(gender,geo_entity)%>%
  summarise(sumy = n())

df%>%
  group_by(gender)%>%
  summarise(avy = mean(age))

df%>%
  group_by(generation)%>%
  summarise(sumy = n())

df%>%
  filter(improved_source=="Improved")%>%
  group_by(geo_entity,improved_source)%>%
  summarise(sumy = n())
df%>%
  group_by(gender)%>%
  summarise(avy = median(age))
anscombes <- read.csv("data/anscombes.csv")


df1 <- anscombes%>%
  filter(dataset=="I")
df2 <- anscombes%>%
  filter(dataset=="II")
View(anscombe)
summary(anscombe)

plot(df1$x,df1$y)
abline(a=df1$x[1],b=slope)
