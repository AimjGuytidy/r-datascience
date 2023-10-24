# Preliminaries

rm(list = ls()) # removing all objects to start with a clean slate
#install.packages("ggvis")
#install.packages("bdscale")
library("bdscale")
library("utils")
library("tidyverse")
library("readr")
library("openxlsx")
library("readxl")
library("haven")
library("scales")
setwd("C:/Users/HP/source/repos/r-datascience")


Light_grey <- c("#FFFFDD") #Light grey for the background
Blue <- c("#279EFF") #Blue
Light_blue <- c("#9DC3E5") #Light blue
Dark_blue <- c("#0C356A") #Dark blue
Green <- c("#016A70") #Green
yellow_green <- c("#D2DE32") #yellow green
Dark_green <- c("#A2C579") #Dark green
dark_greenish <- c("#164B60")
whitish <- c("#F9F9F9")
orange <- c("#ED7D31")
red <- c("#D80032")
maroon <- c("#3D0C11")



df <- read_excel("data/fredgraph.xls",skip = 11)
df$observation_date <- as.Date.POSIXct(df$observation_date)
df <- df |>
  mutate(scaled_oil_price = (DCOILWTICO-mean(DCOILWTICO,na.rm = T))/sd(DCOILWTICO,na.rm=T),
         scaled_breakeven_rates = (T5YIE-mean(T5YIE,na.rm = T)/sd(T5YIE,na.rm = T)),
         breakeven_rates_scaledup = T5YIE * max(DCOILWTICO)/max(T5YIE))
ggplot(data = filter(df,T5YIE!=0), aes(x = observation_date)) + 
  geom_line(aes(y = DCOILWTICO/46.28), color = Blue,linewidth = 1)+
  #scale_x_bd(business.dates=nyse, labels=date_format('%d%b'), max.major.breaks=10) +
  geom_line(aes(y = T5YIE), color = red,linewidth = 1)+ 
  scale_y_continuous(
    "Oil Prices", 
    sec.axis = sec_axis(~ ./46.28, name = "Breakeven rates")
  ) +
  scale_x_bd(business.dates=nyse, labels=date_format('%Y-%m-%d'), max.major.breaks=10,
             max.minor.breaks = 10)



ggplot(data = filter(df,T5YIE!=0), aes(x = observation_date)) + 
  geom_line(aes(y = DCOILWTICO), color = Blue,linewidth = 1)+
  #scale_x_bd(business.dates=nyse, labels=date_format('%d%b'), max.major.breaks=10) +
  geom_line(aes(y = T5YIE*46.28), color = red,linewidth = 1)+ 
  scale_y_continuous(
    "Oil Prices", 
    sec.axis = sec_axis(~ .*46.28, name = "Breakeven rates",
                        labels = function(x) {
                          paste0(round(x/4628,2), "%")})
  ) +
  scale_x_bd(business.dates=nyse, labels=date_format('%Y-%m-%d'), max.major.breaks=10,
             max.minor.breaks = 10)


ggplot(data = filter(df,T5YIE!=0), aes(x = observation_date)) + 
  geom_line(aes(y = DCOILWTICO), color = Blue,linewidth = 1)+
  #scale_x_bd(business.dates=nyse, labels=date_format('%d%b'), max.major.breaks=10) +
  geom_line(aes(y = breakeven_rates_scaledup), color = red,linewidth = 1)+
  scale_x_bd(business.dates=nyse, labels=date_format('%Y-%m-%d'), max.major.breaks=20,
             max.minor.breaks = 20)
