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
library(openxlsx)
library(data.table)
library(sjlabelled)

df <- readxl::read_excel("data/tmis_data_october_2023.xlsx")
df1 <- read.xlsx("data/tmis_data_october_2023.xlsx")

df_filter <- df|>
  select(employeeid,gender,civilStatus,year_birth,position,qualificationLevel,
         role,schoolName:specialization)
df_filter$age <- as.integer(format(Sys.Date(), "%Y")) - as.integer(df_filter$year_birth)

# let's look at the maximum age of the teachers
max(df_filter$age,na.rm = T) # the maximum age is 68
