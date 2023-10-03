# Preliminaries

rm(list = ls()) # removing all objects to start with a clean slate
library("utils")
library("tidyverse")

getwd() # make sure I am in the right directory

# getting the data
gender_data <- as_tibble(read_csv("data/Gender_StatsData.csv"))

view(head(gender_data,3))

# creating a tibble with only adolescent fertility rate indicator
teenager_fr <- filter(gender_data, Indicator.Code=="SP.ADO.TFRT") # we can use subset too!

