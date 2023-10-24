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
ggplot(data = df, aes(x = observation_date)) + 
  geom_line(aes(y = DCOILWTICO, color = Blue)) +
  scale_x_date(date_breaks  ="5 days")+
  coord_flip()


plot(df$observation_date, df$DCOILWTICO, xaxt = "n", type = "l", xlab = "Date", ylab = "y")
axis.POSIXct(1, at = seq(min(df$observation_date), max(df$observation_date), by = "week"), format="%b %d")


library(ggvis)

ggvis(df, ~observation_date, ~DCOILWTICO) %>% layer_lines()