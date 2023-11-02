#######################
# TMIS ANALYSIS#
####################
rm(list = ls()) # removing all objects to start with a clean slate
library(foreign)
library(haven)
library(tidyverse)
library(VIM)
library(outliers)
library(ggplot2)
library(scales) 
library(grid)
library(RColorBrewer)
library(psych)
#install.packages("sjlabelled")
library(testit)
library(matrixStats)
library(rio)
library(labelled)
library(readxl)
library(data.table)
library(sjlabelled)
library(plotly)

df <- read_xlsx("data/tmis_data_october_2023.xlsx")

df_filter <- df|>
  select(employeeid,lastName,firstName,gender,civilStatus,year_birth,position,qualificationLevel,
         role,schoolName:districtName)|>
  unite("Name",lastName:firstName,sep = " ") |>
  mutate(age = as.integer(format(Sys.Date(), "%Y")) - as.integer(year_birth),
         age_brackets = if_else(age >= 66, "66 and above",
                        if_else(age >= 61, "61-65",
                        if_else(age >= 56, "56-60",
                        if_else(age >= 51, "51-55",
                        if_else(age >= 46, "46-50",
                        if_else(age >= 41, "41-45",
                        if_else(age >= 36, "36-40",
                        if_else(age >= 31, "31-35",
                        if_else(age >= 26, "26-30",
                        if_else(age >= 21, "21-25",
                                "17-20")))))))))))


# let's look at the maximum age of the teachers
max(df_filter$age,na.rm = T) # the maximum age is 68
sum(is.na(df_filter$age)) # 6 teachers have missing age values

